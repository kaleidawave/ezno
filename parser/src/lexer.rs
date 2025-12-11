use crate::{
	Span,
	errors::{ParseError, ParseErrors},
	marker::Marker,
	options::ParseOptions,
};

// TODO state for "use strict" etc?
// TODO hold Keywords map, markers, syntax errors etc
#[derive(Default)]
pub struct ParsingState {
	last_new_lines: u32,
	markers: Vec<Span>,
}

pub struct Lexer<'a> {
	pub(crate) head: u32,
	script: &'a str,
	offset: u32,

	options: ParseOptions,
	state: ParsingState,
}

#[allow(clippy::manual_find)]
impl<'a> Lexer<'a> {
	// (crate)
	#[must_use]
	pub fn new(script: &'a str, offset: u32, options: ParseOptions) -> Self {
		if script.len() > u32::MAX as usize {
			todo!()
			// return Err((LexingErrors::CannotLoadLargeFile(script.len()), source_map::Nullable::NULL));
		}

		let state = ParsingState::default();
		let head = 0;
		Lexer { head, script, offset, options, state }
	}

	#[must_use]
	pub fn get_options(&self) -> &ParseOptions {
		&self.options
	}

	pub fn new_partial_point_marker<T>(&mut self, span: Span) -> Marker<T> {
		let idx = self.state.markers.len() as u8;
		self.state.markers.push(span);
		Marker(idx, std::marker::PhantomData)
	}

	/// Just used for specific things, not all annotations
	#[must_use]
	pub fn parse_type_annotations(&self) -> bool {
		self.options.type_annotations.type_annotations()
	}

	// TODO want to remove where public
	#[must_use]
	pub(crate) fn get_current(&self) -> &'a str {
		&self.script[self.head as usize..]
	}

	#[must_use]
	pub fn source_size(&self) -> u32 {
		self.script.len() as u32
	}

	#[must_use]
	pub fn is_finished(&self) -> bool {
		self.head >= self.source_size()
	}

	#[must_use]
	pub fn left_to_parse(&self) -> u32 {
		self.source_size().saturating_sub(self.head)
	}

	#[must_use]
	pub fn get_some_current(&self) -> (&'a str, usize) {
		(
			&self.script
				[self.head as usize..std::cmp::min(self.script.len(), self.head as usize + 20)],
			self.head as usize,
		)
	}

	#[must_use]
	pub fn last_was_from_new_line(&self) -> u32 {
		self.state.last_new_lines
	}

	pub fn skip(&mut self) {
		let current = self.get_current();
		if current.starts_with(char::is_whitespace) {
			let start = self.head;
			self.state.last_new_lines = 0;

			for (idx, chr) in current.char_indices() {
				if !chr.is_whitespace() {
					self.head = start + idx as u32;
					return;
				}
				if let '\n' = chr {
					self.state.last_new_lines += 1;
				}
			}

			// Else if
			self.head += current.len() as u32;
		}
	}

	pub fn skip_including_comments(&mut self) {
		// TODO
		self.skip();
	}

	pub fn is_keyword(&mut self, keyword: &str) -> bool {
		self.skip();
		let current = self.get_current();
		let length = keyword.len();
		current.starts_with(keyword)
			&& current[length..]
				.chars()
				.next()
				.is_none_or(|chr| !utilities::is_valid_identifier(chr))
	}

	pub fn is_keyword_advance(&mut self, keyword: &str) -> bool {
		self.skip();
		let current = self.get_current();
		let length = keyword.len();
		if current.starts_with(keyword)
			&& current[length..]
				.chars()
				.next()
				.is_none_or(|chr| !utilities::is_valid_identifier(chr))
		{
			self.state.last_new_lines = 0;
			self.head += length as u32;
			true
		} else {
			false
		}
	}

	// Does not advance
	#[must_use]
	pub fn is_one_of_keywords<'b>(&self, keywords: &'static [&'b str]) -> Option<&'b str> {
		let current = self.get_current();
		for item in keywords {
			if current.starts_with(item)
				&& current[item.len()..]
					.chars()
					.next()
					.is_none_or(|chr| !utilities::is_valid_identifier(chr))
			{
				return Some(item);
			}
		}
		None
	}

	pub fn is_one_of_keywords_advance<'b>(
		&mut self,
		keywords: &'static [&'b str],
	) -> Option<&'b str> {
		let current = self.get_current();
		for item in keywords {
			if current.starts_with(item)
				&& current[item.len()..]
					.chars()
					.next()
					.is_none_or(|chr| !utilities::is_valid_identifier(chr))
			{
				self.head += item.len() as u32;
				return Some(item);
			}
		}
		None
	}

	pub fn expect_start(&mut self, chr: char) -> Result<source_map::Start, ParseError> {
		self.skip();
		let current = self.get_current();
		if current.starts_with(chr) {
			let start = source_map::Start(self.offset + self.head);
			self.head += chr.len_utf8() as u32;
			Ok(start)
		} else {
			let position = self.get_start().with_length(chr.len_utf8());
			let reason = ParseErrors::UnexpectedCharacter {
				expected: &[chr],
				found: current.chars().next(),
			};
			Err(ParseError::new(reason, position))
		}
	}

	pub fn expect(&mut self, chr: char) -> Result<source_map::End, ParseError> {
		self.skip();
		let current = self.get_current();
		if current.starts_with(chr) {
			self.head += chr.len_utf8() as u32;
			Ok(source_map::End(self.offset + self.head))
		} else {
			let position = self.get_start().with_length(chr.len_utf8());
			let reason = ParseErrors::UnexpectedCharacter {
				expected: &[chr],
				found: current.chars().next(),
			};
			Err(ParseError::new(reason, position))
		}
	}

	pub fn expect_operator(&mut self, expected: &'static str) -> Result<(), ParseError> {
		self.skip();
		let current = self.get_current();
		if current.starts_with(expected) {
			self.head += expected.len() as u32;
			Ok(())
		} else {
			let (found, position) = utilities::next_item(self);
			let reason = ParseErrors::ExpectedOperator { expected, found };
			Err(ParseError::new(reason, position))
		}
	}

	pub fn expect_keyword(
		&mut self,
		expected: &'static str,
	) -> Result<source_map::Start, ParseError> {
		self.skip();
		let current = self.get_current();
		if current.starts_with(expected) {
			let start = source_map::Start(self.offset + self.head);
			self.head += expected.len() as u32;
			Ok(start)
		} else {
			let (found, position) = utilities::next_item(self);
			let reason = ParseErrors::ExpectedKeyword { expected, found };
			Err(ParseError::new(reason, position))
		}
	}

	pub fn is_no_advance(&mut self, chr: char) -> Result<(), ()> {
		if self.get_current().starts_with(chr) { Ok(()) } else { Err(()) }
	}

	#[must_use]
	pub fn is_one_of<'b>(&self, items: &[&'b str]) -> Option<&'b str> {
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
	pub fn is_one_of_operators<'b>(&self, operators: &'static [&'b str]) -> Option<&'b str> {
		let current = self.get_current();
		for item in operators {
			if current.starts_with(item) {
				return Some(item);
			}
		}
		None
	}

	#[must_use]
	pub fn starts_with(&self, chr: char) -> bool {
		self.get_current().starts_with(chr)
	}

	#[must_use]
	pub fn starts_with_slice(&self, slice: &str) -> bool {
		self.get_current().starts_with(slice)
	}

	/// Can't do `-` and `+` because they are valid expression prefixed
	/// TODO `.` if not number etc.
	#[must_use]
	#[allow(clippy::match_like_matches_macro)]
	pub fn starts_with_expression_delimiter(&self) -> bool {
		let current = self.get_current().trim_start();
		if let Some('=' | ',' | ':' | '?' | ']' | ')' | '}' | ';') | None = current.chars().next() {
			true
		} else {
			false
		}
	}

	#[must_use]
	pub fn starts_with_statement_or_declaration_on_new_line(&self) -> bool {
		let current = self.get_current();
		if self.state.last_new_lines > 0 {
			// `class` and `function` are actual expressions...
			let statement_or_declaration_prefixes =
				&["const", "let", "function", "class", "if", "for", "while"];
			for prefix in statement_or_declaration_prefixes {
				// Starts with prefix and is not other identifer
				let not_identifer = current.starts_with(prefix)
					&& !current[prefix.len()..].starts_with(utilities::is_valid_identifier);
				if not_identifer {
					return true;
				}
			}
			false
		} else {
			false
		}
	}

	pub fn is_operator(&mut self, operator: &str) -> bool {
		self.skip();
		self.starts_with_slice(operator)
	}

	pub fn is_operator_advance(&mut self, operator: &str) -> bool {
		self.skip();
		let current = self.get_current();
		let matches = current.starts_with(operator);
		if matches {
			self.state.last_new_lines = 0;
			self.head += operator.len() as u32;
		}
		matches
	}

	#[must_use]
	pub fn get_start(&self) -> source_map::Start {
		source_map::Start(self.offset + self.head)
	}

	#[must_use]
	pub fn get_end(&self) -> source_map::End {
		source_map::End(self.offset + self.head)
	}

	pub fn advance(&mut self, count: u32) {
		self.state.last_new_lines = 0;
		self.head += count;
	}

	pub fn parse_identifier(
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

		self.skip();
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
		for (idx, matched) in current.match_indices(|c: char| !valid_continue_character(c)) {
			if idx < last {
				continue;
			}
			if matched == "\\" {
				if let Some(after) = &current[idx + 1..].strip_prefix('u') {
					if let Ok((chr, width)) = crate::strings::parse_unicode_escape_sequence(after) {
						value.to_mut().push(chr);
						last = idx + 2 + width;
					} else {
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
				value += &current[last..idx];
				last = idx;
				break;
			}
		}
		if last == 0 {
			value += current;
			last = current.len();
		}
		self.advance(last as u32);
		if check_reserved && !crate::lexer::utilities::is_valid_variable_identifier(&value) {
			Err(ParseError::new(ParseErrors::ReservedIdentifier, start.with_length(value.len())))
		} else {
			Ok(value)
		}
	}

	// Will append the length on `until`
	pub fn parse_until(&mut self, until: &str) -> Result<&'a str, ()> {
		let current = self.get_current();
		for (idx, _) in current.char_indices() {
			if current[idx..].starts_with(until) {
				self.head += (idx + until.len()) as u32;
				// TODO temp fix
				if let "\n" = until {
					self.head -= 1;
				}
				return Ok(&current[..idx]);
			}
		}

		// Fix for at the end stuff
		if let "\n" = until {
			self.head += current.len() as u32;
			Ok(current)
		} else {
			Err(())
		}
	}

	// For comments etc
	pub fn parse_until_no_advance(&mut self, until: &str) -> Result<&'a str, ()> {
		let current = self.get_current();
		for (idx, _) in current.char_indices() {
			if current[idx..].starts_with(until) {
				self.head += idx as u32;
				return Ok(&current[..idx]);
			}
		}
		Err(())
	}

	// For JSX attributes and content. Also returns which one of `possibles` matched
	pub fn parse_until_one_of_advance(
		&mut self,
		possibles: &[&'static str],
	) -> Result<(&'a str, &'static str), ()> {
		let current = self.get_current();
		for (i, _) in current.char_indices() {
			if let Some(until) = possibles.iter().find(|s| current[i..].starts_with(**s)) {
				self.head += (i + until.len()) as u32;
				return Ok((&current[..i], until));
			}
		}
		Err(())
	}

	/// Similar to `parse_until_one_of_advance`. Does not add the matched lenght to head
	pub fn parse_until_one_of_no_advance(
		&mut self,
		possibles: &[&'static str],
	) -> Result<(&'a str, &'static str), ()> {
		self.state.last_new_lines = 0;
		let current = self.get_current();
		for (i, chr) in current.char_indices() {
			if let Some(until) = possibles.iter().find(|s| current[i..].starts_with(**s)) {
				self.head += i as u32;
				let content = &current[..i];
				// self.state.last_new_lines =
				//    content.chars().filter(|char| matches!(char, '\n')).count() as u32;
				return Ok((content, until));
			}
			if let '\n' = chr {
				self.state.last_new_lines += 1;
			}
		}
		Err(())
	}

	#[must_use]
	pub fn starts_with_number(&self) -> bool {
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
	pub fn parse_number_literal(
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
	pub fn starts_with_string_delimeter(&self) -> bool {
		self.starts_with('"') || self.starts_with('\'')
	}

	#[allow(clippy::single_match_else)]
	pub fn parse_string_literal(
		&mut self,
	) -> Result<(std::borrow::Cow<'a, str>, crate::strings::Quoting, u32), ParseError> {
		let value = self.get_current();
		let result = crate::strings::parse_string(value);
		match result {
			Ok(crate::strings::ParseStringOutput {
				value,
				quoting,
				source_length,
				unknown_escapes: _,
			}) => {
				// TODO add unknown escapes to warnings (or errors on strict mode)
				self.advance(source_length);
				Ok((value, quoting, source_length))
			}
			Err(_) => {
				// TODO ...
				let span = self.get_start().with_length(1);
				Err(ParseError::new(ParseErrors::InvalidStringLiteral, span))
			}
		}
	}

	/// Returns content and flags. Flags can be empty
	pub fn parse_regex_literal(&mut self) -> Result<(&'a str, &'a str), ParseError> {
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

		let mut escaped = false;
		let mut in_set = false;
		self.skip();
		let current = self.get_current();
		let mut chars = current.char_indices();
		debug_assert!(chars.next().is_some_and(|(_idx, chr)| chr == '/'));
		let start = self.get_start();

		let mut regex_content = 1;
		let mut found_end_slash = false;

		for (idx, chr) in chars.by_ref() {
			match chr {
				'/' if !escaped && !in_set => {
					regex_content = idx;
					found_end_slash = true;
					break;
				}
				'\\' if !escaped => {
					escaped = true;
				}
				'[' => {
					in_set = true;
				}
				']' if in_set => {
					in_set = false;
				}
				'\n' => {
					return Err(ParseError::new(
						ParseErrors::InvalidRegularExpression,
						start.with_length(idx),
					));
				}
				_ => {
					escaped = false;
				}
			}
		}

		if !found_end_slash {
			return Err(ParseError::new(
				ParseErrors::InvalidRegularExpression,
				start.with_length(current.len()),
			));
		}

		let regex = &current[1..regex_content];
		self.head += 2 + regex.len() as u32;
		let regex_end = regex_content + '/'.len_utf8();

		let first_non_char = chars
			.find_map(|(idx, chr)| (!chr.is_alphabetic()).then_some(idx))
			.unwrap_or(current.len());

		let regex_flags = &current[regex_end..first_non_char];

		let invalid_flag = regex_flags.contains(|chr: char| !valid_regexp_flag(chr));
		if invalid_flag {
			Err(ParseError::new(
				ParseErrors::InvalidRegexFlag,
				self.get_start().with_length(regex_flags.len()),
			))
		} else {
			self.head += regex_flags.len() as u32;
			Ok((regex, regex_flags))
		}
	}

	/// Expects that `//` or `/*` has been parsed
	pub fn parse_comment_literal(&mut self, is_multiline: bool) -> Result<&'a str, ParseError> {
		if is_multiline {
			self.parse_until("*/").map_err(|()| {
				// TODO might be a problem
				let position = self.get_start().with_length(self.get_current().len());
				ParseError::new(ParseErrors::UnexpectedEnd, position)
			})
		} else {
			Ok(self.parse_until("\n").expect("Always should have found end of line or file"))
		}
	}

	/// Note scans after multiple comments
	#[must_use]
	pub fn after_comment_literals(&self) -> &str {
		let mut current = self.get_current().trim_start();
		loop {
			if current.starts_with("//") {
				current = current[current.find('\n').unwrap_or(current.len())..].trim_start();
			} else if current.starts_with("/*") {
				current = current[current.find("*/").unwrap_or(current.len())..].trim_start();
			} else {
				return current;
			}
		}
	}

	// TODO also can exit if there is `=` or `:` and = 0 in some examples
	#[must_use]
	pub fn after_brackets(&self) -> &'a str {
		use crate::Quoting;

		enum State {
			None,
			Comment,
			StringLiteral { escaped: bool, quoting: crate::Quoting },
			// TemplateLiteral { escaped: bool },
			// RegexLiteral { escaped: bool },
			MultilineComment,
		}

		let current = self.get_current();

		let mut bracket_count: u32 = 0;
		let mut open_chevrons = 0u64;
		let mut state = State::None;

		// TODO account for string literals and comments
		// TODO account for utf16
		for (idx, chr) in current.char_indices() {
			match state {
				State::None => {
					if let '(' | '{' | '[' | '<' = chr {
						open_chevrons <<= 1;
						open_chevrons |= u64::from(chr == '<');
						bracket_count += 1;
						// dbg!(chr, bracket_count);
					} else if let ')' | '}' | ']' | '>' = chr {
						// TODO WIP
						let last_was_open_chevron = (open_chevrons & 1) != 0;
						if let '>' = chr {
							if !last_was_open_chevron {
								continue;
							}
							// ...
						} else if last_was_open_chevron {
							// Extra removal
							open_chevrons >>= 1;
							bracket_count = bracket_count.saturating_sub(1);
						}

						open_chevrons >>= 1;
						bracket_count = bracket_count.saturating_sub(1);
						// dbg!(chr, bracket_count, last_was_open_chevron);
						if bracket_count == 0 {
							return current[(idx + 1)..].trim_start();
						}
					} else if let '"' = chr {
						state = State::StringLiteral { escaped: false, quoting: Quoting::Double };
					} else if let '\'' = chr {
						state = State::StringLiteral { escaped: false, quoting: Quoting::Single };
					} else if let '/' = chr {
						if current[idx..].starts_with("/*") {
							state = State::MultilineComment;
						} else if current[idx..].starts_with("//") {
							state = State::Comment;
						}
					}
				}
				State::StringLiteral { ref mut escaped, quoting } => {
					if *escaped {
						*escaped = false;
						continue;
					}
					if let '\\' = chr {
						*escaped = true;
					} else if let (Quoting::Double, '"') | (Quoting::Single, '\'') = (quoting, chr)
					{
						state = State::None;
					}
				}
				State::Comment => {
					if let '\n' = chr {
						state = State::None;
					}
				}
				State::MultilineComment => {
					if current[idx..].starts_with("*/") {
						state = State::None;
					}
				}
			}
		}

		// Return empty slice
		Default::default()
	}

	#[must_use]
	pub fn after_identifier(&self) -> &'a str {
		self.after_identifier_offset(0)
	}

	#[must_use]
	pub fn after_identifier_offset(&self, offset: usize) -> &'a str {
		let current = &self.get_current().trim_start()[offset..];

		if let Some(idx) = current.find(|chr: char| !(chr.is_alphanumeric() || chr == '_')) {
			current[idx..].trim_start()
		} else {
			// Return empty slice
			Default::default()
		}
	}

	// TODO WIP. for for loops
	#[must_use]
	pub fn after_variable_start(&self) -> &'a str {
		let mut current = self.get_current().trim_start();
		if current.starts_with("const") {
			current = current["const".len()..].trim_start();
		} else if current.starts_with("let") {
			current = current["let".len()..].trim_start();
		} else if current.starts_with("var") {
			current = current["var".len()..].trim_start();
		} else if current.starts_with("using") {
			current = current["using".len()..].trim_start();
		}

		if current.starts_with('{') || current.starts_with('[') {
			let mut paren_count: u32 = 0;
			// TODO account for string literals and comments
			for (idx, chr) in current.as_bytes().iter().enumerate() {
				if let b'(' | b'{' | b'[' | b'<' = chr {
					paren_count += 1;
				} else if let b')' | b'}' | b']' | b'>' = chr {
					paren_count = paren_count.saturating_sub(1);
					if paren_count == 0 {
						return current[(idx + 1)..].trim_start();
					}
				}
			}
		} else {
			// let mut paren_count: u32 = 0;
			let mut chars = current.as_bytes().iter().enumerate();
			for (_, chr) in chars.by_ref() {
				if !chr.is_ascii_whitespace() {
					break;
				}
			}
			for (idx, chr) in chars {
				if !chr.is_ascii_alphanumeric() {
					return current[idx..].trim_start();
				}
			}
		}
		// Return empty slice
		Default::default()
	}

	/// Part of [ASI](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#automatic_semicolon_insertion)
	pub fn expect_semi_colon(&mut self) -> Result<(), ParseError> {
		// TODO order
		let semi_colon_like = self.starts_with_slice("//")
			|| self.is_operator_advance(";")
			|| self.last_was_from_new_line() > 0
			|| self.is_operator("}")
			// TODO what about spaces
			|| self.starts_with_slice("\n")
			|| self.is_finished();

		if semi_colon_like {
			Ok(())
		} else {
			let (found, position) = utilities::next_item(self);
			let error = ParseErrors::ExpectedOperator { expected: ";", found };
			Err(ParseError::new(error, position))
		}
	}

	pub fn is_semi_colon(&mut self) -> bool {
		self.skip();
		self.starts_with('}')
			|| self.starts_with(';')
			|| self.last_was_from_new_line() > 0
			|| self.is_finished()
	}
}

pub(crate) mod utilities {
	pub fn is_arrow_function(
		reader: &mut super::Lexer,
	) -> (bool, Option<crate::types::TypeAnnotation>) {
		let after_brackets = trim_whitespace_not_newlines(reader.after_brackets());
		if after_brackets.starts_with("=>") {
			(true, None)
		} else if reader.options.type_annotations.type_annotations()
			&& after_brackets.starts_with(':')
		{
			// TODO WIP implementation
			let save_point = reader.head;
			let after = reader.left_to_parse() - after_brackets.len() as u32;
			reader.head += after as u32 + 1;
			// TODO: I hate this!!
			// Can double allocate for expressions build up bad information
			let annotation = crate::types::TypeAnnotation::from_reader_with_precedence(
				reader,
				crate::types::type_annotations::TypeOperatorKind::ReturnType,
			);
			let starts_with_arrow = reader.starts_with_slice("=>");
			reader.head = save_point;
			if let (true, Ok(annotation)) = (starts_with_arrow, annotation) {
				(true, Some(annotation))
			} else {
				(false, None)
			}
		} else {
			(false, None)
		}
	}

	pub fn is_valid_identifier(chr: char) -> bool {
		// TODO `\\` for unicode identifiers
		chr.is_alphanumeric() || chr == '_' || chr == '$' || chr == '\\'
	}

	pub fn is_identifier_continutation(chr: char) -> bool {
		// TODO `\\` for unicode identifiers
		unicode_id_start::is_id_continue(chr) || chr == '$' || chr == '\\'
	}

	pub fn is_reserved_word(identifier: &str) -> bool {
		matches!(
			identifier,
			"enum"
				| "implements"
				| "interface"
				| "let" | "package"
				| "private" | "protected"
				| "public" | "static"
		)
	}

	pub fn is_valid_variable_identifier(identifier: &str) -> bool {
		let is_invalid = matches!(
			identifier,
			"const"
				| "var" | "if"
				| "else" | "for"
				| "while" | "do"
				| "switch" | "class"
				| "function" | "new"
				| "super" | "case"
				| "return" | "continue"
				| "break" | "import"
				| "export" | "default"
				| "in" | "typeof"
				| "instanceof"
				| "void" | "delete"
				| "debugger" | "try"
				| "catch" | "finally"
				| "throw" | "extends"
		);

		!is_invalid
	}

	// TODO move
	pub fn next_empty_occurance(on: &str) -> usize {
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

	pub fn trim_whitespace_not_newlines(on: &str) -> &str {
		let chars = on.char_indices();
		let mut idx = 0;
		for (at, chr) in chars {
			idx = at;
			if !chr.is_whitespace() || chr == '\n' {
				break;
			}
		}
		&on[idx..]
	}

	fn is_keyword(on: &str, word: &str) -> bool {
		if let Some((lhs, rhs)) = on.split_at_checked(word.len()) {
			lhs == word && !rhs.starts_with(is_identifier_continutation)
		} else {
			false
		}
	}

	pub fn is_function_header(slice: &str) -> bool {
		let slice = slice.trim_start();
		// TODO
		let extras = true;
		is_keyword(slice, "async")
			|| is_keyword(slice, "function")
			// TODO WIP
			|| extras && slice.starts_with("generator ")
			|| extras && slice.starts_with("worker ")
			|| extras && slice.starts_with("serve ")
	}

	/// TODO this could be set to collect, rather than breaking (<https://github.com/kaleidawave/ezno/issues/203>)
	pub fn assert_type_annotations(
		reader: &super::Lexer,
		position: crate::Span,
	) -> crate::ParseResult<()> {
		if reader.get_options().type_annotations.type_annotations() {
			Ok(())
		} else {
			Err(crate::ParseError::new(crate::ParseErrors::TypeAnnotationUsed, position))
		}
	}

	pub fn next_item<'a>(reader: &super::Lexer<'a>) -> (&'a str, crate::Span) {
		let current = reader.get_current();
		let until_empty = self::next_empty_occurance(current);
		let position = reader.get_start().with_length(until_empty);
		let found = &current[..until_empty];
		(found, position)
	}

	pub fn expected_one_of_items(
		reader: &super::Lexer,
		expected: &'static [&'static str],
	) -> crate::ParseError {
		let current = reader.get_current();
		let found = &current[..self::next_empty_occurance(current)];
		let position = reader.get_start().with_length(found.len());
		let reason = crate::ParseErrors::ExpectedOneOfItems { expected, found };
		crate::ParseError::new(reason, position)
	}

	pub fn get_not_identifier_length(reader: &super::Lexer) -> Option<usize> {
		let on = reader.get_current();
		for (idx, c) in on.char_indices() {
			if c == '#' || crate::lexer::utilities::is_valid_identifier(c) {
				return None;
			} else if !c.is_whitespace() {
				let after = &on[idx..];
				return if after.starts_with("//") || after.starts_with("/*") {
					None
				} else {
					Some(idx)
				};
			}
		}

		// Else nothing exists
		Some(0)
	}
}
