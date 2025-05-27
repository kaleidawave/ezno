use crate::{
	errors::{ParseError, ParseErrors},
	marker::Marker,
	options::ParseOptions,
	Span,
};

// pub(super) enum NumberLiteralType {
// 	BinaryLiteral,
// 	/// strict mode done at the parse level
// 	OctalLiteral,
// 	HexadecimalLiteral,
// 	/// Base 10
// 	Decimal {
// 		/// has decimal point
// 		fractional: bool,
// 	},
// 	BigInt,
// 	Exponent,
// }

// impl Default for NumberLiteralType {
// 	fn default() -> Self {
// 		Self::Decimal { fractional: false }
// 	}
// }

// TODO state for "use strict" etc?
// TODO hold Keywords map, markers, syntax errors etc
#[derive(Default)]
pub struct ParsingState {
	last_new_lines: u32,
	markers: Vec<Span>,
}

pub struct Lexer<'a> {
	// last: u32,
	pub(crate) head: u32,
	script: &'a str,

	options: ParseOptions,
	state: ParsingState,
}

#[allow(clippy::manual_find)]
impl<'a> Lexer<'a> {
	// (crate)
	#[must_use]
	pub fn new(script: &'a str, _offset: Option<u32>, options: ParseOptions) -> Self {
		if script.len() > u32::MAX as usize {
			todo!()
			// return Err((LexingErrors::CannotLoadLargeFile(script.len()), source_map::Nullable::NULL));
		}
		// TODO offset.unwrap_or_default(),
		let state = ParsingState::default();
		Lexer { options, state, script, head: 0 }
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
		self.options.type_annotations
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

	#[must_use]
	pub fn last_was_from_new_line_consume(&mut self) -> u32 {
		let last_new_lines = self.state.last_new_lines;
		self.state.last_new_lines = 0;
		last_new_lines
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
		let current = self.get_current();
		let start = self.head;
		self.state.last_new_lines = 0;

		let mut comment = false;
		let mut multiline_comment = false;

		for (idx, chr) in current.char_indices() {
			if comment {
				if let '\n' = chr {
					comment = false;
				} else {
					continue;
				}
			}
			if multiline_comment {
				if current[idx.saturating_sub(2)..].starts_with("*/") {
					multiline_comment = false;
				} else {
					continue;
				}
			}

			if current[idx..].starts_with("//") {
				comment = true;
			} else if current[idx..].starts_with("/*") {
				multiline_comment = true;
			} else if !chr.is_whitespace() {
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
			let start = source_map::Start(self.head);
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
			Ok(source_map::End(self.head))
		} else {
			let position = self.get_start().with_length(chr.len_utf8());
			let reason = ParseErrors::UnexpectedCharacter {
				expected: &[chr],
				found: current.chars().next(),
			};
			Err(ParseError::new(reason, position))
		}
	}

	pub fn expect_operator(&mut self, operator: &'static str) -> Result<(), ParseError> {
		self.skip();
		let current = self.get_current();
		if current.starts_with(operator) {
			self.head += operator.len() as u32;
			Ok(())
		} else {
			let trailing = utilities::next_empty_occurance(current);
			let position = self.get_start().with_length(trailing);
			let found = &current[..trailing];
			let reason = ParseErrors::ExpectedOperator { expected: operator, found };
			Err(ParseError::new(reason, position))
			// let position = self.get_start().with_length(chr.len_utf8());
			// let reason = ParseErrors::UnexpectedCharacter {
			// 	expected: &[chr],
			// 	found: current.chars().next().unwrap(),
			// };
			// Err(ParseError::new(reason, position))
		}
	}

	pub fn expect_keyword(&mut self, str: &'static str) -> Result<source_map::Start, ParseError> {
		self.skip();
		let current = self.get_current();
		if current.starts_with(str) {
			let start = source_map::Start(self.head);
			self.head += str.len() as u32;
			Ok(start)
		} else {
			let found = &current[..utilities::next_empty_occurance(current)];
			let position = self.get_start().with_length(found.len());
			let reason = ParseErrors::ExpectedKeyword { expected: str, found };
			Err(ParseError::new(reason, position))
		}
	}

	pub fn is_no_advance(&mut self, chr: char) -> Result<(), ()> {
		if self.get_current().starts_with(chr) {
			Ok(())
		} else {
			Err(())
		}
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
	pub fn starts_with_expression_delimiter(&self) -> bool {
		let current = self.get_current();
		IntoIterator::into_iter(["=", ",", ":", "?", "]", ")", "}", ";"])
			.any(|expression_delimiter| current.starts_with(expression_delimiter))
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
	pub fn is_finished(&self) -> bool {
		self.get_current().trim_start().is_empty()
	}

	#[must_use]
	pub fn get_start(&self) -> source_map::Start {
		source_map::Start(self.head)
	}

	#[must_use]
	pub fn get_end(&self) -> source_map::End {
		source_map::End(self.head)
	}

	pub fn advance(&mut self, count: u32) {
		self.state.last_new_lines = 0;
		self.head += count;
	}

	pub fn parse_identifier(
		&mut self,
		location: &'static str,
		check_reserved: bool,
	) -> Result<&'a str, ParseError> {
		enum State {
			Standard,
			StartOfUnicode,
			UnicodeEscape(u8),
			UnicodeBracedEscape { first_bracket: bool },
		}

		self.skip();
		let current = self.get_current();
		let start = self.get_start();
		let mut iter = current.char_indices();
		let mut state = State::Standard;
		if let Some((_, chr)) = iter.next() {
			if let '\\' = chr {
				state = State::StartOfUnicode;
			} else {
				// Note `is_alphabetic` here
				let first_is_valid = chr.is_alphabetic() || chr == '_' || chr == '$';
				if !first_is_valid {
					return Err(ParseError::new(
						ParseErrors::ExpectedIdentifier { location },
						start.with_length(chr.len_utf8()),
					));
				}
			}
		} else {
			return Err(ParseError::new(
				ParseErrors::ExpectedIdentifier { location },
				start.with_length(0),
			));
		}

		for (idx, chr) in iter {
			match state {
				State::UnicodeEscape(steps) => {
					if !chr.is_ascii_hexdigit() {
						return Err(ParseError::new(
							ParseErrors::InvalidUnicodeCodePointInIdentifier,
							start.with_length(idx + chr.len_utf8()),
						));
					}
					if steps == 1 {
						state = State::Standard;
					} else {
						state = State::UnicodeEscape(steps - 1);
					}
				}
				State::UnicodeBracedEscape { ref mut first_bracket } => {
					if *first_bracket {
						if chr == '}' {
							state = State::Standard;
						} else if !chr.is_ascii_hexdigit() {
							return Err(ParseError::new(
								ParseErrors::InvalidUnicodeCodePointInIdentifier,
								start.with_length(idx + chr.len_utf8()),
							));
						}
					} else if chr == '{' {
						*first_bracket = true;
					} else {
						return Err(ParseError::new(
							ParseErrors::InvalidUnicodeCodePointInIdentifier,
							start.with_length(idx + chr.len_utf8()),
						));
					}
				}
				State::StartOfUnicode => {
					if let 'u' = chr {
						let next_char = current[(idx + 1)..].chars().next();
						state = if let Some('{') = next_char {
							State::UnicodeBracedEscape { first_bracket: false }
						} else if let Some('0'..='9' | 'A'..='F' | 'a'..='f') = next_char {
							State::UnicodeEscape(4)
						} else {
							return Err(ParseError::new(
								ParseErrors::InvalidUnicodeCodePointInIdentifier,
								start.with_length(idx + chr.len_utf8()),
							));
						};
					} else {
						return Err(ParseError::new(
							ParseErrors::InvalidUnicodeCodePointInIdentifier,
							start.with_length(idx + chr.len_utf8()),
						));
					}
				}
				State::Standard => {
					if let '\\' = chr {
						state = State::StartOfUnicode;
					} else {
						// Note `is_alphanumeric` here
						let is_valid = chr.is_alphanumeric() || chr == '_' || chr == '$';
						if !is_valid {
							let value = &current[..idx];
							let is_invalid = check_reserved
								&& !crate::lexer::utilities::is_valid_variable_identifier(value);
							let result = if is_invalid {
								Err(ParseError::new(
									ParseErrors::ReservedIdentifier,
									start.with_length(value.len()),
								))
							} else {
								self.head += idx as u32;
								Ok(value)
							};
							return result;
						}
					}
				}
			}
		}

		if !matches!(state, State::Standard) {
			return Err(ParseError::new(
				ParseErrors::InvalidUnicodeCodePointInIdentifier,
				start.with_length(current.len()),
			));
		}

		// If left over
		let is_invalid =
			check_reserved && !crate::lexer::utilities::is_valid_variable_identifier(current);
		if is_invalid {
			Err(ParseError::new(ParseErrors::ReservedIdentifier, start.with_length(current.len())))
		} else {
			self.head += current.len() as u32;
			Ok(current)
		}
	}

	// Will append the length on `until`
	pub(crate) fn parse_until(&mut self, until: &str) -> Result<&'a str, ()> {
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

	// For JSX attributes and content. Also returns which one of `possibles` matched
	pub(crate) fn parse_until_one_of(
		&mut self,
		possibles: &[&'static str],
	) -> Result<(&'a str, &'static str), ()> {
		let current = self.get_current();
		for (idx, chr) in current.char_indices() {
			if let Some(until) = possibles.iter().find(|s| current[idx..].starts_with(**s)) {
				self.head += idx as u32;
				return Ok((&current[..idx], until));
			}
			if let '\n' = chr {
				self.state.last_new_lines += 1;
			}
		}
		Err(())
	}

	#[must_use]
	pub fn starts_with_string_delimeter(&self) -> bool {
		self.starts_with('"') || self.starts_with('\'')
	}

	pub fn parse_string_literal(&mut self) -> Result<(&'a str, crate::Quoted), ParseError> {
		let current = self.get_current();
		let mut chars = current.char_indices();
		let quoted = match chars.next() {
			Some((_, '"')) => crate::Quoted::Double,
			Some((_, '\'')) => crate::Quoted::Single,
			_ => {
				let found = &current[..crate::lexer::utilities::next_empty_occurance(current)];
				return Err(ParseError::new(
					ParseErrors::ExpectedOneOfItems { expected: &["\"", "'"], found },
					self.get_start().with_length(1),
				));
			}
		};
		let mut escaped = false;
		for (idx, chr) in chars {
			if escaped {
				escaped = false;
				continue;
			} else if let '\\' = chr {
				escaped = true;
				continue;
			}

			if let (crate::Quoted::Double, '"') | (crate::Quoted::Single, '\'') = (quoted, chr) {
				// TODO double check
				let content = &current[1..idx];
				self.head += idx as u32 + 1;
				return Ok((content, quoted));
			}

			if let '\n' = chr {
				return Err(ParseError::new(
					ParseErrors::NoNewLinesInString,
					self.get_start().with_length(idx),
				));
			}
		}
		Err(ParseError::new(
			ParseErrors::UnexpectedEnd,
			self.get_start().with_length(self.get_current().len()),
		))
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

	// TODO errors + some parts are weird
	pub fn parse_number_literal(
		&mut self,
	) -> Result<(crate::number::NumberRepresentation, u32), ParseError> {
		use std::str::FromStr;

		enum NumberLiteralType {
			BinaryLiteral,
			/// strict mode done at the parse level
			OctalLiteral,
			HexadecimalLiteral,
			/// Base 10
			Decimal {
				/// has decimal point
				fractional: bool,
			},
			Exponent,
		}

		let current = self.get_current();
		let mut chars = current.char_indices();

		let mut state = match chars.next().map(|(_idx, chr)| chr) {
			Some('0') if current.as_bytes().get(1).is_some_and(|b| (b'0'..=b'7').contains(b)) => {
				// TODO strict mode should be done in the parser stage (as that is where context is)
				NumberLiteralType::OctalLiteral
			}
			Some('0'..='9') => NumberLiteralType::Decimal { fractional: false },
			Some('.') => NumberLiteralType::Decimal { fractional: true },
			Some(_) | None => {
				return Err(ParseError::new(
					ParseErrors::InvalidNumberLiteral,
					self.get_start().with_length(1),
				))
			}
		};

		for (idx, chr) in chars {
			match chr {
				'n' => {
					let num_slice = current[..idx].to_owned();
					let number = crate::number::NumberRepresentation::BigInt(
						crate::number::NumberSign::Positive,
						num_slice,
					);
					let length = (idx + 'n'.len_utf16()) as u32;
					self.head += length;
					return Ok((number, length));
				}
				// For binary/hexadecimal/octal literals
				'b' | 'B' | 'x' | 'X' | 'o' | 'O' if idx == 1 => {
					if current.starts_with('0') {
						state = match chr {
							'b' | 'B' => NumberLiteralType::BinaryLiteral,
							'o' | 'O' => NumberLiteralType::OctalLiteral,
							'x' | 'X' => NumberLiteralType::HexadecimalLiteral,
							_ => unreachable!(),
						}
					} else {
						// LexingErrors::NumberLiteralBaseSpecifierMustPrecededWithZero
						return Err(ParseError::new(
							ParseErrors::InvalidNumberLiteral,
							self.get_start().with_length(idx),
						));
					}
				}
				'0'..='9' | 'a'..='f' | 'A'..='F' => match state {
					NumberLiteralType::BinaryLiteral => {
						if !matches!(chr, '0' | '1') {
							// (LexingErrors::InvalidNumeralItemBecauseOfLiteralKind)
							return Err(ParseError::new(
								ParseErrors::InvalidNumberLiteral,
								self.get_start().with_length(idx),
							));
						}
					}
					NumberLiteralType::OctalLiteral => {
						if !matches!(chr, '0'..='7') {
							// (LexingErrors::InvalidNumeralItemBecauseOfLiteralKind)
							return Err(ParseError::new(
								ParseErrors::InvalidNumberLiteral,
								self.get_start().with_length(idx),
							));
						}
					}
					// Handling for 'e' & 'E'
					NumberLiteralType::Decimal { .. } => {
						if matches!(chr, 'e' | 'E') && !current[..idx].ends_with('_') {
							state = NumberLiteralType::Exponent;
						} else if !chr.is_ascii_digit() {
							// (LexingErrors::InvalidNumeralItemBecauseOfLiteralKind)
							return Err(ParseError::new(
								ParseErrors::InvalidNumberLiteral,
								self.get_start().with_length(idx),
							));
						}
					}
					NumberLiteralType::Exponent => {
						if !chr.is_ascii_digit() {
							// (LexingErrors::InvalidNumeralItemBecauseOfLiteralKind)
							return Err(ParseError::new(
								ParseErrors::InvalidNumberLiteral,
								self.get_start().with_length(idx),
							));
						}
					}
					// all above allowed
					NumberLiteralType::HexadecimalLiteral => {}
				},
				'.' => {
					if let NumberLiteralType::Decimal { ref mut fractional } = state {
						// Return if already fractional. This is valid syntax: `1..toString()`
						if *fractional {
							let num_slice = &current[..idx];
							let number = crate::number::NumberRepresentation::from_str(num_slice);
							let number = number.unwrap();
							let length = idx as u32;
							self.head += length;
							return Ok((number, length));
						}

						if current[..idx].ends_with(['_']) {
							// (LexingErrors::InvalidUnderscore)
							return Err(ParseError::new(
								ParseErrors::InvalidNumberLiteral,
								self.get_start().with_length(idx),
							));
						}

						*fractional = true;
					} else {
						// (LexingErrors::NumberLiteralCannotHaveDecimalPoint);
						return Err(ParseError::new(
							ParseErrors::InvalidNumberLiteral,
							self.get_start().with_length(idx),
						));
					}
				}
				'_' => {
					let invalid = match &state {
						NumberLiteralType::BinaryLiteral |
						NumberLiteralType::OctalLiteral |
						// Second `(idx - start) < 1` is for octal with prefix 0
						NumberLiteralType::HexadecimalLiteral => {
							if idx == 2 {
								current[..idx].ends_with(['b', 'B', 'x', 'X', 'o', 'O'])
							} else {
								false
							}
						},
						NumberLiteralType::Decimal { .. } => current[..idx].ends_with('.') || &current[..idx] == "0",
						NumberLiteralType::Exponent => current[..idx].ends_with(['e', 'E']),
					};
					if invalid {
						// (LexingErrors::InvalidUnderscore);
						return Err(ParseError::new(
							ParseErrors::InvalidNumberLiteral,
							self.get_start().with_length(idx),
						));
					}
				}
				// `10e-5` is a valid literal
				'-' if matches!(state, NumberLiteralType::Exponent if current[..idx].ends_with(['e', 'E'])) =>
					{}
				_chr => {
					let num_slice = &current[..idx];
					let length = idx;
					return match crate::number::NumberRepresentation::from_str(num_slice) {
						Ok(number) => {
							self.head += length as u32;
							Ok((number, length as u32))
						}
						Err(_) => Err(ParseError::new(
							ParseErrors::InvalidNumberLiteral,
							self.get_start().with_length(length),
						)),
					};
				}
			}
		}

		// Fix if don't find end
		let length = current.len();
		match crate::number::NumberRepresentation::from_str(current) {
			Ok(number) => {
				self.head += length as u32;
				Ok((number, length as u32))
			}
			Err(_) => Err(ParseError::new(
				ParseErrors::InvalidNumberLiteral,
				self.get_start().with_length(length),
			)),
		}
	}

	/// Returns content and flags. Flags can be empty
	pub fn parse_regex_literal(&mut self) -> Result<(&'a str, &'a str), ParseError> {
		let mut escaped = false;
		let mut in_set = false;
		self.skip();
		let current = self.get_current();
		let mut chars = current.char_indices();
		assert!(chars.next().is_some_and(|(_idx, chr)| chr == '/'));
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

		// TODO under cfg
		let v_flag = true;
		let invalid_flag = regex_flags.chars().any(|chr| {
			!(matches!(chr, 'd' | 'g' | 'i' | 'm' | 's' | 'u' | 'y')
				|| matches!(chr, 'v' if v_flag))
		});
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
				current =
					current[current.find("*/").map_or(current.len(), |idx| idx + 2)..].trim_start();
			} else {
				return current;
			}
		}
	}

	// TODO also can exit if there is `=` or `:` and = 0 in some examples
	#[must_use]
	pub fn after_brackets(&self) -> &'a str {
		utilities::after_brackets(self.get_current())
	}

	#[must_use]
	pub fn after_identifier(&self) -> &'a str {
		let current = self.get_current();

		let mut chars = current.as_bytes().iter().enumerate();
		for (idx, chr) in chars.by_ref() {
			if !chr.is_ascii_whitespace() {
				// test here as iteration consumed
				if chr.is_ascii_alphanumeric() {
					break;
				}

				return current[idx..].trim_start();
			}
		}

		for (idx, chr) in chars {
			if !chr.is_ascii_alphanumeric() {
				return current[idx..].trim_start();
			}
		}

		// Return empty slice
		Default::default()
	}

	// TODO WIP
	#[must_use]
	pub fn after_variable_start(&self) -> &'a str {
		let current = self.get_current().trim_start();
		let current = current
			.strip_prefix("const")
			.or_else(|| current.strip_prefix("let"))
			.or_else(|| current.strip_prefix("var"))
			.unwrap_or(current);

		let current = current.trim_start();
		if current.starts_with('{') || current.starts_with('[') {
			let mut paren_count: u32 = 0;
			// TODO account for string literals, regex and comments
			// For arrow
			let mut last_was_equal = false;
			for (idx, chr) in current.as_bytes().iter().enumerate() {
				if let b'(' | b'{' | b'[' | b'<' = chr {
					paren_count += 1;
				} else if let b')' | b'}' | b']' | b'>' = chr {
					if let (true, b'>') = (last_was_equal, chr) {
						last_was_equal = false;
						continue;
					}
					paren_count = paren_count.saturating_sub(1);
					if paren_count == 0 {
						return current[(idx + 1)..].trim_start();
					}
				}
				last_was_equal = matches!(chr, b'=');
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
			let current = self.get_current();
			dbg!(current);
			let until_empty = crate::lexer::utilities::next_empty_occurance(current);
			let position = self.get_start().with_length(until_empty);
			let error =
				ParseErrors::ExpectedOperator { expected: ";", found: &current[..until_empty] };
			Err(ParseError::new(error, position))
		}
	}

	pub fn is_semi_colon(&mut self) -> bool {
		self.skip();
		self.starts_with('}')
			|| self.starts_with(';')
			|| self.last_was_from_new_line() > 0
			|| self.get_current().is_empty()
	}

	pub fn is_arrow_function(&mut self) -> (bool, Option<crate::types::TypeAnnotation>) {
		let after_brackets = self.after_brackets();
		let after_brackets = utilities::trim_whitespace_not_newlines(after_brackets);
		if after_brackets.starts_with("=>") {
			(true, None)
		} else if self.options.type_annotations && after_brackets.starts_with(':') {
			// TODO WIP implementation
			let save_point = self.head;
			let after = self.get_current().len() - after_brackets.len();
			self.head += after as u32 + 1;
			// TODO: I hate this!!
			// Can double allocate for expressions build up bad information
			let annotation = crate::types::TypeAnnotation::from_reader_with_precedence(
				self,
				crate::types::type_annotations::TypeOperatorKind::ReturnType,
			);
			let starts_with_arrow = self.starts_with_slice("=>");
			self.head = save_point;
			if let (true, Ok(annotation)) = (starts_with_arrow, annotation) {
				(true, Some(annotation))
			} else {
				(false, None)
			}
		} else {
			(false, None)
		}
	}
}

pub(crate) mod utilities {
	pub fn is_valid_identifier(chr: char) -> bool {
		// TODO `\\` for unicode identifiers
		chr.is_alphanumeric() || chr == '_' || chr == '$' || chr == '\\'
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

	pub fn is_function_header(slice: &str) -> bool {
		let slice = slice.trim_start();
		// TODO
		let extras = true;
		slice.starts_with("async ")
			|| {
				slice.starts_with("function")
					&& !slice["function".len()..].chars().next().is_some_and(is_valid_identifier)
			} || (extras && {
			// TODO + after is "function"
			slice.starts_with("generator ")
				|| slice.starts_with("worker ")
				|| slice.starts_with("server ")
				|| slice.starts_with("test ")
		})
	}

	/// TODO this could be set to collect, rather than breaking (<https://github.com/kaleidawave/ezno/issues/203>)
	pub fn assert_type_annotations(
		reader: &super::Lexer,
		position: crate::Span,
	) -> crate::ParseResult<()> {
		if reader.get_options().type_annotations {
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

	pub fn after_brackets(current: &str) -> &str {
		use crate::Quoted;

		enum State {
			None,
			Comment { multiline: bool },
			StringLiteral { escaped: bool, quoted: Quoted },
			// TemplateLiteral { escaped: bool },
			// RegexLiteral { escaped: bool },
		}

		// let mut template_literal_depth = 0;

		let mut bracket_count: u32 = 0;
		let mut open_chevron_positions = 0u64;
		let mut state = State::None;

		// TODO account for string literals and comments
		// TODO account for utf16
		for (idx, chr) in current.char_indices() {
			match state {
				State::None => {
					match chr {
						'(' | '{' | '[' | '<' => {
							open_chevron_positions <<= 1;
							open_chevron_positions |= u64::from(chr == '<');
							bracket_count += 1;
						}
						')' | '}' | ']' | '>' => {
							let last_was_open_chevron = (open_chevron_positions & 1) != 0;
							open_chevron_positions >>= 1;

							// This happens when `(x = 1 < 2) => 4`
							if last_was_open_chevron && chr != '>' {
								open_chevron_positions >>= 1;
								bracket_count = bracket_count.saturating_sub(1);
							}
							// This happens when `(x = 1 > 2) => 4`
							if !last_was_open_chevron && chr == '>' {
								continue;
							}

							bracket_count = bracket_count.saturating_sub(1);
							if bracket_count == 0 {
								return current[(idx + chr.len_utf8())..].trim_start();
							}
						}
						'"' => {
							state = State::StringLiteral { escaped: false, quoted: Quoted::Double };
						}
						'\'' => {
							state = State::StringLiteral { escaped: false, quoted: Quoted::Single };
						}
						'/' => {
							if current[idx..].starts_with("/*") {
								state = State::Comment { multiline: true };
							} else if current[idx..].starts_with("//") {
								state = State::Comment { multiline: false };
							}
						}
						_ => {}
					}
				}
				State::Comment { multiline: false } => {
					if let '\n' = chr {
						state = State::None;
					}
				}
				State::Comment { multiline: true } => {
					if current[idx..].starts_with("*/") {
						state = State::None;
					}
				}
				State::StringLiteral { ref mut escaped, quoted } => {
					if *escaped {
						*escaped = false;
						continue;
					}
					if let '\\' = chr {
						*escaped = true;
					} else if let (Quoted::Double, '"') | (Quoted::Single, '\'') = (quoted, chr) {
						state = State::None;
					}
				}
			}
		}

		// Return empty slice
		Default::default()
	}
}
