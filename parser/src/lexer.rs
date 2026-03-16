use crate::Span;
use crate::errors::{ParseError, ParseErrors};
use crate::marker::Marker;
use crate::options::ParseOptions;

// #[derive(Default, Debug, Clone, Copy)]
// pub struct FunctionModifiers {
// 	pub in_async: bool,
// 	pub in_generator: bool,
// }

#[derive(Default, Debug, Clone, Copy)]
pub struct ParsingFlags {
	pub(crate) strict_mode: bool,
	pub(crate) in_async: bool,
	pub(crate) in_generator: bool,
	/// TODO invert
	pub(crate) top_level: bool,
	/// for `return` checking
	pub(crate) function: Option<crate::functions::FunctionKind>,
	pub(crate) in_class: bool,
	/// for `arguments` checking
	pub(crate) in_ternary_or_class_field: bool,
}

// TODO Vec<u32> for keywords?
#[derive(Default, Debug)]
pub struct ParseState {
	blank_lines: u32,
	comment_lines: u32,
	pub markers: Vec<Span>,
	pub constant_imports: Vec<String>,
	pub errors: Vec<ParseError>,
	/// label chains break via classes functions etc
	pub(crate) label_boundary: usize,
	pub(crate) labels: Vec<(String, bool)>,

	pub(crate) flags: ParsingFlags,

	/// the current position into the script
	pub head: u32,
	pub last: u32,
}

pub struct Lexer<'a> {
	/// the original source, must start with the content... (aka offset has no effect)
	script: &'a str,
	/// Used to offset position markers.
	/// For example parsing the contents of a script tag need the positions shifted
	offset: u32,
	/// options
	options: ParseOptions,
	pub(crate) state: ParseState,
}

const fn is_whitespace_ascii(byte: u8) -> bool {
	matches!(byte, b'\t' | b' ' | 0b0000_1011 | 0b0000_1100)
}

const fn is_whitespace_char_three_bytes(chr: char) -> bool {
	matches!(
		chr,
		'\u{FEFF}'
			| '\u{1680}'
			| '\u{2000}'
			| '\u{2001}'
			| '\u{2002}'
			| '\u{2003}'
			| '\u{2004}'
			| '\u{2005}'
			| '\u{2006}'
			| '\u{2007}'
			| '\u{2008}'
			| '\u{2009}'
			| '\u{200A}'
			| '\u{202F}'
			| '\u{205F}'
			| '\u{3000}'
	)
}

#[allow(clippy::manual_find)]
impl<'a> Lexer<'a> {
	#[must_use]
	pub(crate) fn new(script: &'a str, offset: u32, options: ParseOptions) -> Self {
		if script.len() > u32::MAX as usize {
			todo!()
			// return Err((LexingErrors::CannotLoadLargeFile(script.len()), source_map::Nullable::NULL));
		}

		let mut state = ParseState::default();

		// TODO what about nested
		state.flags.top_level = true;
		state.flags.in_async = options.top_level_await;
		state.flags.strict_mode = options.strict_mode;

		let mut reader = Lexer { script, offset, options, state };
		reader.skip_including_comments();
		reader
	}

	/// This is for lookahead
	pub(crate) fn try_parse<T, U>(
		&mut self,
		cb: impl for<'b> FnOnce(&'b mut Lexer<'a>) -> Result<T, U>,
	) -> Result<T, U> {
		let mut forked = Lexer {
			script: self.script,
			offset: self.offset,
			options: self.options,
			state: ParseState {
				head: self.state.head,
				flags: self.state.flags.clone(),
				..ParseState::default()
			},
		};
		let result = cb(&mut forked);
		match result {
			Ok(node) => {
				// If okay, we merge the good parse state here
				let ParseState {
					head,
					blank_lines,
					comment_lines,
					last,
					mut markers,
					mut constant_imports,
					labels: _,
					label_boundary: _,
					errors: _,
					flags: _,
				} = forked.state;
				self.state.head = head;
				self.state.blank_lines = blank_lines;
				self.state.comment_lines = comment_lines;
				self.state.last = last;
				self.state.markers.append(&mut markers);
				self.state.constant_imports.append(&mut constant_imports);
				Ok(node)
			}
			Err(err) => Err(err),
		}
	}

	#[must_use]
	pub(crate) fn get_options(&self) -> &ParseOptions {
		&self.options
	}

	pub(crate) fn strict_mode(&self) -> bool {
		self.state.flags.strict_mode || self.state.flags.in_class
	}

	pub(crate) fn new_partial_point_marker<T>(&mut self, span: Span) -> Marker<T> {
		let idx = self.state.markers.len() as u8;
		self.state.markers.push(span);
		Marker(idx, std::marker::PhantomData)
	}

	/// Just used for specific things, not all annotations
	#[must_use]
	pub(crate) fn parse_type_annotations(&self) -> bool {
		self.options.type_annotations.type_annotations()
	}

	#[must_use]
	pub(crate) fn get_current(&self) -> &'a str {
		unsafe { self.script.get_unchecked(self.state.head as usize..) }
	}

	#[must_use]
	#[allow(unused)]
	#[cfg(debug_assertions)]
	pub(crate) fn get_current_short(&self) -> &'a str {
		&self.script
			[self.state.head as usize..(self.state.head as usize + 8).min(self.script.len())]
	}

	#[must_use]
	pub(crate) fn source_size(&self) -> u32 {
		self.script.len() as u32
	}

	#[must_use]
	pub(crate) fn is_finished(&self) -> bool {
		self.state.head >= self.source_size()
	}

	#[must_use]
	pub(crate) fn last_was_from_new_line(&self) -> u32 {
		self.state.blank_lines
	}

	fn skip_including_comments(&mut self) {
		self.state.last = self.state.head;
		while (self.state.head as usize) < self.script.len() {
			let first_byte: u8 =
				unsafe { *self.script.as_bytes().get_unchecked(self.state.head as usize) };
			if is_whitespace_ascii(first_byte) {
				self.state.head += 1;
			} else if let b'\r' = first_byte {
				self.state.head += 1;
				if !self.get_current().starts_with('\n') {
					self.state.blank_lines += 1;
				}
			} else if let b'\n' = first_byte {
				self.state.head += 1;
				self.state.blank_lines += 1;
			} else if first_byte >= 0x80 {
				let current = self.get_current();
				if current.starts_with('\u{00A0}') {
					self.state.head += 2;
				} else if current.starts_with(is_whitespace_char_three_bytes) {
					self.state.head += 3;
				} else if current.starts_with('\u{FEFF}') {
					self.state.head += 3;
				} else if current.starts_with(['\u{2028}', '\u{2029}']) {
					// Line Separator <LS> or Paragraph Separator <LS>
					self.state.blank_lines += 1;
					self.state.head += 3;
				} else {
					break;
				}
			} else if first_byte == b'/' {
				let current = self.get_current();
				if let Some(rest) = current.strip_prefix("//") {
					let idx = rest.find('\n').unwrap_or(rest.len());
					self.state.head += 2 + idx as u32;
					self.state.comment_lines += 1;

					let _comment = &rest[..idx];
				} else if let Some(rest) = current.strip_prefix("/*") {
					if let Some(idx) = rest.find("*/") {
						self.state.head += 4 + idx as u32;
						let comment = &rest[..idx];
						if comment.contains(NEW_LINE_CHARACTERS) {
							self.state.comment_lines += 1;
						}
						// return ParseError::new(ParseErrors::UnexpectedEnd, position)
					} else {
						// TODO error
						self.state.head += current.len() as u32;
					}
				} else {
					break;
				}
			} else if first_byte == b'<' {
				let current = self.get_current();
				if let Some(rest) = current.strip_prefix("<!--") {
					// TODO last was new line?
					let idx = rest.find('\n').unwrap_or(rest.len());
					self.state.comment_lines += 1;

					self.state.head += 4 + idx as u32;
					let _comment = &rest[..idx];
				} else {
					break;
				}
			} else if first_byte == b'-' {
				let current = self.get_current();
				if let Some(rest) = current.strip_prefix("-->") {
					// TODO last was new line?
					let idx = rest.find('\n').unwrap_or(rest.len());
					self.state.comment_lines += 1;

					self.state.head += 3 + idx as u32;
					let _comment = &rest[..idx];
				} else {
					break;
				}
			} else {
				break;
			}
		}
	}

	/// TODO wip
	pub(crate) fn last_was_whitespace(&self) -> bool {
		if let Some(before) = self.script.get(..self.state.head as usize) {
			before.ends_with(char::is_whitespace)
		} else {
			false
		}
	}

	pub(crate) fn is_keyword(&self, keyword: &str) -> bool {
		let current = self.get_current();
		if let Some(rest) = current.strip_prefix(keyword) {
			!rest.starts_with(|chr: char| utilities::is_identifier_continutation(chr))
		} else {
			false
		}
	}

	pub(crate) fn is_keyword_advance(&mut self, keyword: &str) -> bool {
		let current = self.get_current();
		if let Some(rest) = current.strip_prefix(keyword)
			&& !rest.starts_with(|chr: char| utilities::is_identifier_continutation(chr))
		{
			self.state.blank_lines = 0;
			self.state.comment_lines = 0;
			self.state.head += keyword.len() as u32;
			self.skip_including_comments();
			true
		} else {
			false
		}
	}

	pub(crate) fn is_operator(&self, operator: &str) -> bool {
		self.get_current().starts_with(operator)
	}

	pub(crate) fn is_operator_advance(&mut self, operator: &str) -> bool {
		let current = self.get_current();
		if current.starts_with(operator) {
			self.state.blank_lines = 0;
			self.state.comment_lines = 0;
			self.state.head += operator.len() as u32;
			self.skip_including_comments();
			true
		} else {
			false
		}
	}

	pub(crate) fn expect_chr(&mut self, chr: char) -> Result<source_map::End, ParseError> {
		let current = self.get_current();
		if current.starts_with(chr) {
			self.state.head += chr.len_utf8() as u32;
			let end = self.state.head;
			self.skip_including_comments();
			Ok(source_map::End(end))
		} else {
			let position = self.get_start().with_length(chr.len_utf8());
			let reason = ParseErrors::UnexpectedCharacter {
				// TODO single version
				expected: &['?'],
				found: current.chars().next(),
			};
			Err(ParseError::new(reason, position))
		}
	}

	/// Same as above, but do not advance as to lose any whitespace in the literal parts
	/// of template literals and JSX children
	pub(crate) fn expect_closing_bracket(&mut self) -> Result<source_map::End, ParseError> {
		if self.get_current().starts_with('}') {
			self.state.head += 1;
			// self.skip_including_comments();
			Ok(source_map::End(self.state.head))
		} else {
			let position = self.get_start().with_length(1);
			let reason = ParseErrors::UnexpectedCharacter {
				expected: &['}'],
				found: self.get_current().chars().next(),
			};
			Err(ParseError::new(reason, position))
		}
	}

	pub(crate) fn expect_operator(&mut self, expected: &'static str) -> Result<(), ParseError> {
		let current = self.get_current();
		if current.starts_with(expected) {
			self.state.head += expected.len() as u32;
			self.skip_including_comments();
			Ok(())
		} else {
			let (found, position) = utilities::next_item(self);
			let reason = ParseErrors::ExpectedOperator { expected, found };
			Err(ParseError::new(reason, position))
		}
	}

	pub(crate) fn expect_keyword(
		&mut self,
		expected: &'static str,
	) -> Result<source_map::Start, ParseError> {
		let current = self.get_current();
		if current.starts_with(expected) {
			let start = source_map::Start(self.offset + self.state.head);
			self.state.head += expected.len() as u32;
			self.skip_including_comments();
			Ok(start)
		} else {
			let (found, position) = utilities::next_item(self);
			let reason = ParseErrors::ExpectedKeyword { expected, found };
			Err(ParseError::new(reason, position))
		}
	}

	#[must_use]
	pub(crate) fn is_one_of<'b>(&self, items: &[&'b str]) -> Option<&'b str> {
		let current = self.get_current();
		for item in items {
			if current.starts_with(item) {
				return Some(item);
			}
		}
		None
	}

	// Does not advance
	#[must_use]
	pub(crate) fn is_one_of_operators<'b>(&self, operators: &'static [&'b str]) -> Option<&'b str> {
		let current = self.get_current();
		for item in operators {
			if current.starts_with(item) {
				return Some(item);
			}
		}
		None
	}

	#[must_use]
	pub(crate) fn starts_with(&self, chr: char) -> bool {
		self.get_current().starts_with(chr)
	}

	#[must_use]
	pub(crate) fn starts_with_slice(&self, slice: &str) -> bool {
		self.get_current().starts_with(slice)
	}

	/// Can't do `-` and `+` because they are valid expression prefixed
	/// TODO `.` if not number etc.
	#[must_use]
	#[allow(clippy::match_like_matches_macro)]
	pub(crate) fn starts_with_expression_delimiter(&self) -> bool {
		let current = self.get_current();
		if current.starts_with(['=', ',', ':', '?', ';', '.', ']', ')', '}']) {
			true
		} else {
			self.is_keyword("instanceof")
		}
	}

	#[must_use]
	#[allow(clippy::match_like_matches_macro)]
	pub(crate) fn starts_with_expression_delimiter_or_open_bracket(&self) -> bool {
		let current = self.get_current();
		if current.starts_with(['=', ',', ':', ';', '.', '?', ']', ')', '}', '[', '(', '{']) {
			true
		} else {
			self.is_keyword("instanceof")
		}
	}

	#[must_use]
	pub(crate) fn starts_with_statement_or_declaration_on_new_line(&self) -> bool {
		let current = self.get_current();
		if (self.state.blank_lines + self.state.comment_lines) > 0 {
			// `class` and `function` are actual expressions...
			let statement_or_declaration_prefixes =
				&["const", "let", "function", "class", "if", "for", "while"];
			for prefix in statement_or_declaration_prefixes {
				// Starts with prefix and is not other identifier
				let not_identifier = current.starts_with(prefix)
					&& !current[prefix.len()..].starts_with(utilities::is_identifier_start);
				if not_identifier {
					return true;
				}
			}
			false
		} else {
			false
		}
	}

	pub(crate) fn if_not_expression_like(&self) -> bool {
		self.starts_with_expression_delimiter()
			|| self.starts_with_statement_or_declaration_on_new_line()
	}

	#[must_use]
	pub(crate) fn get_start(&self) -> source_map::Start {
		source_map::Start(self.offset + self.state.head)
	}

	/// use last rather than head for whitespace and comment reasons
	#[must_use]
	pub(crate) fn get_end(&self) -> source_map::End {
		source_map::End(self.offset + self.state.last)
	}

	pub(crate) fn advance(&mut self, count: u32) {
		self.state.blank_lines = 0;
		self.state.comment_lines = 0;
		self.state.head += count;
		self.skip_including_comments();
	}

	pub(crate) fn parse_identifier(
		&mut self,
		location: &'static str,
		check_reserved: bool,
	) -> Result<std::borrow::Cow<'a, str>, ParseError> {
		fn valid_start_character(chr: char) -> bool {
			unicode_id_start::is_id_start(chr) || matches!(chr, '\\' | '_' | '$')
		}

		fn valid_continue_character(chr: char) -> bool {
			unicode_id_start::is_id_continue(chr) || chr == '$'
		}

		let start = self.get_start();
		let current = self.get_current();

		if !current.starts_with(valid_start_character) {
			return Err(ParseError::new(
				ParseErrors::ExpectedIdentifier { location },
				start.with_length(1),
			));
		}

		let mut last = 0;
		let mut value = std::borrow::Cow::Borrowed("");
		for (idx, chr) in current.char_indices() {
			if !valid_continue_character(chr) {
				if idx < last {
					continue;
				}
				value += &current[last..idx];
				if let '\\' = chr {
					if let Some(after) = &current[idx + 1..].strip_prefix('u')
						&& let Ok((chr, width)) =
							crate::strings::parse_unicode_escape_sequence(after)
					{
						if valid_continue_character(chr) {
							value.to_mut().push(chr);
							last = idx + 2 + width;
						} else {
							// TODO invalid char etc
							return Err(ParseError::new(
								ParseErrors::ExpectedIdentifier { location },
								start.with_length(idx),
							));
						}
					} else {
						return Err(ParseError::new(
							ParseErrors::ExpectedIdentifier { location },
							start.with_length(idx),
						));
					}
				} else {
					last = idx;
					break;
				}
			}
		}
		// if not advanced
		if last == 0 {
			value += current;
			last = current.len();
		}
		self.advance(last as u32);
		// if check_reserved {
		// 	let is_invalid = if self.strict_mode() && utilities::is_strict_mode_reserved_word(&value) {
		// 		true
		// 	} else {
		// 		"enum" == value
		// 	};
		// 	if is_invalid {
		// 		return Err(ParseError::new(
		// 			ParseErrors::ReservedIdentifier,
		// 			start.with_length(value.len()),
		// 		));
		// 	}
		// }

		if check_reserved {
			// dbg!(&value);
			let is_reserved = utilities::is_reserved_word(&value, self.strict_mode())
				|| (self.state.flags.in_async && value == "await")
				|| ((self.state.flags.in_generator && self.strict_mode()) && value == "yield");

			if is_reserved {
				return Err(ParseError::new(
					ParseErrors::ReservedIdentifier,
					start.with_length(last),
				));
			}
		}

		Ok(value)
	}

	// Will append the length on `until`
	pub(crate) fn parse_until(&mut self, until: &str) -> Result<&'a str, ()> {
		let current = self.get_current();
		if let "\n" = until {
			// TODO WIP
			let idx = current.find(NEW_LINE_CHARACTERS).unwrap_or(current.len());
			self.state.head += idx as u32;
			self.skip_including_comments();
			Ok(&current[..idx])
		} else {
			let idx = current.find(until);
			if let Some(idx) = idx {
				self.state.head += (idx + until.len()) as u32;
				self.skip_including_comments();
				// TODO temp fix
				Ok(&current[..idx])
			} else {
				Err(())
			}
		}
	}

	// For JSX attributes and content. Also returns which one of `possibles` matched
	pub(crate) fn parse_until_one_of_advance(
		&mut self,
		possibles: &[char],
	) -> Result<(&'a str, &'a str), ()> {
		let current = self.get_current();
		if let Some((idx, until)) = current.match_indices(possibles).next() {
			self.state.head += (idx + 1) as u32;
			self.skip_including_comments();
			Ok((&current[..idx], until))
		} else {
			Err(())
		}
	}

	pub(crate) fn parse_until_one_of_no_advance(
		&mut self,
		possibles: &[char],
	) -> Result<(&'a str, &'a str), ()> {
		let current = self.get_current();
		if let Some((idx, until)) = current.match_indices(possibles).next() {
			self.state.head += idx as u32;
			self.skip_including_comments();
			Ok((&current[..idx], until))
		} else {
			Err(())
		}
	}

	#[must_use]
	pub(crate) fn starts_with_number(&self) -> bool {
		let bytes = self.get_current().as_bytes();
		if let Some(start) = bytes.first() {
			if start.is_ascii_digit() {
				true
			} else if let b'.' = start {
				if let Some(after) = bytes.get(1) {
					after.is_ascii_digit() || *after == b'_'
				} else {
					false
				}
			} else {
				false
			}
		} else {
			false
		}
	}

	#[allow(clippy::single_match_else)]
	pub(crate) fn parse_number_literal(
		&mut self,
	) -> Result<(crate::numbers::ParsedNumberLiteral<'a>, u32), ParseError> {
		let current = self.get_current();
		let result = crate::numbers::parse_number(current);
		match result {
			Ok((value, count)) => {
				self.advance(count);
				Ok((value, count))
			}
			Err(()) => {
				// TODO ...
				let span = self.get_start().with_length(1);
				Err(ParseError::new(ParseErrors::InvalidNumberLiteral, span))
			}
		}
	}

	#[must_use]
	pub(crate) fn starts_with_string_delimeter(&self) -> bool {
		self.get_current().starts_with(['"', '\''])
	}

	/// expects current to start with string delimeter
	#[allow(clippy::single_match_else)]
	pub(crate) fn parse_string_literal(
		&mut self,
	) -> Result<(std::borrow::Cow<'a, str>, crate::strings::Quoting, u32), ParseError> {
		let value = self.get_current();
		let result = crate::strings::parse_string(value);
		let position = self.get_start().with_length(1);

		match result {
			Ok(crate::strings::ParseStringOutput {
				value,
				quoting,
				source_length,
				unknown_escapes,
				uses_octal,
			}) => {
				if uses_octal && self.strict_mode() {
					// TODO better
					return Err(ParseError::new(ParseErrors::InvalidStringLiteral, position));
				}

				if !unknown_escapes.is_empty() {
					// TODO better
					return Err(ParseError::new(ParseErrors::InvalidStringLiteral, position));
				}

				// TODO add unknown escapes to warnings (or errors on strict mode)
				self.advance(source_length);
				Ok((value, quoting, source_length))
			}
			Err(_) => {
				// TODO ...
				Err(ParseError::new(ParseErrors::InvalidStringLiteral, position))
			}
		}
	}

	/// Returns content and flags. Flags can be empty
	pub(crate) fn parse_regex_literal(&mut self) -> Result<(&'a str, &'a str), ParseError> {
		fn valid_regexp_flag(chr: char) -> bool {
			// TODO specify via reader.get_options()
			const EXTRA_REGEX_FLAGS: bool = true;

			if let 'd' | 'g' | 'i' | 'm' | 's' | 'u' | 'y' = chr {
				true
			} else if let 'v' = chr
				&& EXTRA_REGEX_FLAGS
			{
				true
			} else {
				false
			}
		}

		fn find_end_of_regexp(on: &str) -> Option<usize> {
			fn is_escaped_at(on: &str, at: usize) -> bool {
				on[..at].bytes().rev().take_while(|chr: &u8| *chr == b'\\').count() % 2 == 1
			}

			// For character classes
			let mut upto = 0;
			for (idx, matched) in on.match_indices(['[', '/']) {
				if upto > idx || is_escaped_at(on, idx) {
					continue;
				} else if let "[" = matched {
					let after = &on[idx..][1..];
					let (offset, _) =
						after.match_indices(']').find(|(idx, _)| !is_escaped_at(after, *idx))?;
					upto = idx + offset + 2;
				} else {
					return Some(idx);
				}
			}
			None
		}

		let on = self.get_current();
		let Some(on) = on.strip_prefix('/') else {
			return Err(ParseError::new(
				ParseErrors::TODO("expected /"),
				self.get_start().with_length(1),
			));
		};

		let Some(advance) = find_end_of_regexp(on) else {
			return Err(ParseError::new(
				ParseErrors::TODO("no end to regexp"),
				self.get_start().with_length(1),
			));
		};

		let regex = &on[..advance];
		self.state.head += 2 + advance as u32;

		let on = self.get_current();
		let (flags, _) = on.split_once(|c: char| !c.is_ascii_lowercase()).unwrap_or((on, ""));

		let invalid_flag = flags.contains(|chr: char| !valid_regexp_flag(chr));
		if invalid_flag {
			Err(ParseError::new(
				ParseErrors::InvalidRegexFlag,
				self.get_start().with_length(flags.len()),
			))
		} else {
			self.state.head += flags.len() as u32;
			self.skip_including_comments();
			Ok((regex, flags))
		}
	}

	/// Expects that `//` or `/*` has been parsed
	pub(crate) fn parse_comment_literal(
		&mut self,
		is_multiline: bool,
	) -> Result<&'a str, ParseError> {
		if is_multiline {
			let result = self.parse_until("*/");
			if let Ok(content) = result {
				// WIP
				if content.contains(NEW_LINE_CHARACTERS) {
					self.state.comment_lines += 1;
				}
				Ok(content)
			} else {
				// TODO might be a problem
				let position = self.get_start().with_length(self.get_current().len());
				Err(ParseError::new(ParseErrors::UnexpectedEnd, position))
			}
		} else {
			self.state.comment_lines += 1;
			Ok(self.parse_until("\n").expect("Always should have found end of line or file"))
		}
	}

	pub(crate) fn parse_html_comment_literal(&mut self) -> Result<&'a str, ParseError> {
		Ok(self.parse_until("\n").expect("Always should have found end of line or file"))
	}

	#[must_use]
	pub(crate) fn after_identifier(&self) -> &'a str {
		self.after_identifier_offset(0)
	}

	#[must_use]
	pub(crate) fn after_identifier_offset(&self, offset: usize) -> &'a str {
		let current = &self.get_current().trim_start()[offset..];

		if let Some(idx) = current.find(|chr: char| !(chr.is_alphanumeric() || chr == '_')) {
			current[idx..].trim_start()
		} else {
			// Return empty slice
			Default::default()
		}
	}

	/// Part of [ASI](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#automatic_semicolon_insertion)
	pub(crate) fn expect_semi_colon(&mut self) -> Result<(), ParseError> {
		let semi_colon_like = self.state.blank_lines > 0
			|| self.state.comment_lines > 0
			|| self.is_finished()
			|| self.starts_with_slice("//")
			|| self.starts_with_slice("}")
			|| self.is_operator_advance(";");

		if semi_colon_like {
			Ok(())
		} else {
			let (found, position) = utilities::next_item(self);
			let error = ParseErrors::ExpectedOperator { expected: ";", found };
			Err(ParseError::new(error, position))
		}
	}

	pub(crate) fn is_semi_colon(&self) -> bool {
		self.starts_with('}')
			|| self.starts_with(';')
			|| self.last_was_from_new_line() > 0
			|| self.is_finished()
	}

	pub(crate) fn accept_semi_colon(&mut self) {
		self.is_operator_advance(";");
		self.state.blank_lines = 1;
	}

	pub(crate) fn starts_with_function_header(&self) -> bool {
		if self.is_keyword("async") || self.is_keyword("function") {
			true
		} else {
			#[cfg(feature = "extras")]
			if self.get_options().extras.custom_function_headers {
				return self.is_keyword("generator")
					|| self.is_keyword("worker")
					|| self.is_keyword("server")
					|| self.is_keyword("test");
			}
			false
		}
	}

	pub(crate) fn push_label(&mut self, name: String, on_iteration_item: bool) {
		self.state.labels.push((name, on_iteration_item));
	}

	pub(crate) fn pop_label(&mut self) {
		self.state.labels.pop();
	}

	/// returns `on_iteration_item`
	pub(crate) fn contains_label(&self, label: &str) -> Option<bool> {
		self.state.labels.iter().skip(self.state.label_boundary).find_map(
			|(name, on_iteration_item)| (name.as_str() == label).then_some(*on_iteration_item),
		)
	}
}

const NEW_LINE_CHARACTERS: [char; 4] = ['\n', '\r', '\u{2028}', '\u{2029}'];

pub(crate) mod utilities {
	pub(crate) fn is_identifier_start(chr: char) -> bool {
		unicode_id_start::is_id_start(chr) || chr == '$'
	}

	pub(crate) fn is_identifier_continutation(chr: char) -> bool {
		// TODO `\\` for unicode identifiers
		unicode_id_start::is_id_continue(chr) || chr == '$' || chr == '\\'
	}

	pub(crate) fn is_reserved_word(identifier: &str, strict_mode: bool) -> bool {
		// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#reserved_words
		let reserved = matches!(
			identifier,
			"break"
				| "case" | "catch"
				| "class" | "const"
				| "continue" | "debugger"
				| "default" | "delete"
				| "do" | "else"
				| "export" | "extends"
				| "false" | "finally"
				| "for" | "function"
				| "if" | "import"
				| "in" | "instanceof"
				| "new" | "null"
				| "return" | "super"
				| "switch" | "this"
				| "throw" | "true"
				| "try" | "typeof"
				| "var" | "void"
				| "while" | "with"
		);
		if reserved {
			true
		} else if strict_mode {
			matches!(
				identifier,
				"implements"
					| "interface" | "let"
					| "package" | "private"
					| "protected" | "public"
					| "static"
			)
		} else {
			false
		}
	}

	// TODO move
	pub(crate) fn next_empty_occurance(on: &str) -> usize {
		let mut chars = on.char_indices();
		let is_text = chars.next().is_some_and(|(_, chr)| chr.is_alphabetic());
		for (idx, chr) in chars {
			let should_break = chr.is_whitespace()
				|| (is_text && !chr.is_alphanumeric())
				|| (!is_text && chr.is_alphabetic());
			if should_break {
				return idx;
			}
		}
		0
	}

	/// TODO this could be set to collect, rather than breaking (<https://github.com/kaleidawave/ezno/issues/203>)
	pub(crate) fn assert_type_annotations(
		reader: &super::Lexer,
		position: crate::Span,
	) -> crate::ParseResult<()> {
		if reader.get_options().type_annotations.type_annotations() {
			Ok(())
		} else {
			Err(crate::ParseError::new(crate::ParseErrors::TypeAnnotationUsed, position))
		}
	}

	pub(crate) fn next_item<'a>(reader: &super::Lexer<'a>) -> (&'a str, crate::Span) {
		let current = reader.get_current();
		let until_empty = self::next_empty_occurance(current);
		let position = reader.get_start().with_length(until_empty);
		let found = &current[..until_empty];
		(found, position)
	}

	pub(crate) fn expected_one_of_items(
		reader: &super::Lexer,
		expected: &'static [&'static str],
	) -> crate::ParseError {
		let current = reader.get_current();
		let found = &current[..self::next_empty_occurance(current)];
		let position = reader.get_start().with_length(found.len());
		let reason = crate::ParseErrors::ExpectedOneOfItems { expected, found };
		crate::ParseError::new(reason, position)
	}

	// pub(crate) fn get_not_identifier_length(reader: &super::Lexer) -> Option<usize> {
	// 	let on = reader.get_current();
	// 	for (idx, c) in on.char_indices() {
	// 		if c == '#' || crate::lexer::utilities::is_identifier_continutation(c) {
	// 			return None;
	// 		} else if !c.is_whitespace() {
	// 			let after = &on[idx..];
	// 			return if after.starts_with("//") || after.starts_with("/*") {
	// 				None
	// 			} else {
	// 				Some(idx)
	// 			};
	// 		}
	// 	}

	// 	// Else nothing exists
	// 	Some(0)
	// }
}
