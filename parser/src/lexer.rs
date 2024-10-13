#![allow(clippy::as_conversions, clippy::cast_possible_truncation)]
#![allow(unused)]

use crate::{
	errors::{ParseError, ParseErrors},
	marker::Marker,
	options::ParseOptions,
	Comments, Quoted, Span,
};

pub(super) enum NumberLiteralType {
	BinaryLiteral,
	/// strict mode done at the parse level
	OctalLiteral,
	HexadecimalLiteral,
	/// Base 10
	Decimal {
		/// has decimal point
		fractional: bool,
	},
	BigInt,
	Exponent,
}

impl Default for NumberLiteralType {
	fn default() -> Self {
		Self::Decimal { fractional: false }
	}
}

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

// TODO helpers for number, regular expression, maybe JSX maybe not.
impl<'a> Lexer<'a> {
	// (crate)
	pub fn new(script: &'a str, offset: Option<u32>, options: ParseOptions) -> Self {
		if script.len() > u32::MAX as usize {
			todo!()
			// return Err((LexingErrors::CannotLoadLargeFile(script.len()), source_map::Nullable::NULL));
		}
		// TODO offset.unwrap_or_default(),
		let state = ParsingState::default();
		Lexer { options, state, script, head: 0 }
	}

	pub fn get_options(&self) -> &ParseOptions {
		&self.options
	}

	pub fn new_partial_point_marker<T>(&mut self, span: Span) -> Marker<T> {
		let idx = self.state.markers.len() as u8;
		self.state.markers.push(span);
		Marker(idx, std::marker::PhantomData::default())
	}

	/// Just used for specific things, not all annotations
	pub fn parse_type_annotations(&self) -> bool {
		self.options.type_annotations
	}

	pub fn get_current(&self) -> &'a str {
		&self.script[self.head as usize..]
	}

	pub fn get_some_current(&self) -> (&'a str, usize) {
		(
			&self.script
				[self.head as usize..std::cmp::min(self.script.len(), self.head as usize + 20)],
			self.head as usize,
		)
	}

	// TODO temp
	fn get_surrounding(&self) -> (&'a str, &'a str) {
		const WIDTH: usize = 14;
		let head = self.head as usize;
		let start = head.saturating_sub(WIDTH);
		let end = std::cmp::min(head + WIDTH, self.script.len());
		(&self.script[start..head], &self.script[head..end])
	}

	pub fn last_was_from_new_line(&self) -> u32 {
		self.state.last_new_lines
	}

	pub fn skip(&mut self) {
		let mut count = 0;
		let mut new_lines = 0;
		for (idx, chr) in self.get_current().char_indices() {
			if !chr.is_whitespace() {
				break;
			}
			if let '\n' = chr {
				new_lines += 1;
			}
			count += 1;
		}
		if count > 0 {
			self.state.last_new_lines = new_lines;
			self.head += count;
		}
	}

	pub fn is_keyword(&mut self, keyword: &str) -> bool {
		self.skip();
		let current = self.get_current();
		let length = keyword.len();
		current.starts_with(keyword)
			&& current[length..]
				.chars()
				.next()
				.map_or(true, |chr| !utilities::is_valid_identifier(chr))
	}

	pub fn is_keyword_advance(&mut self, keyword: &str) -> bool {
		self.skip();
		let current = self.get_current();
		let length = keyword.len();
		if current.starts_with(keyword)
			&& current[length..]
				.chars()
				.next()
				.map_or(true, |chr| !utilities::is_valid_identifier(chr))
		{
			self.head += length as u32;
			true
		} else {
			false
		}
	}

	// Does not advance
	pub fn is_one_of_keywords<'b>(&self, keywords: &'static [&'b str]) -> Option<&'b str> {
		let current = self.get_current();
		for item in keywords {
			if current.starts_with(item)
				&& current[item.len()..].chars().next().map_or(true, |chr| !chr.is_alphanumeric())
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
				&& current[item.len()..].chars().next().map_or(true, |chr| !chr.is_alphanumeric())
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
	pub fn is_one_of_operators<'b>(&self, operators: &'static [&'b str]) -> Option<&'b str> {
		let current = self.get_current();
		for item in operators {
			if current.starts_with(item) {
				return Some(item);
			}
		}
		None
	}

	pub fn starts_with(&self, chr: char) -> bool {
		self.get_current().starts_with(chr)
	}

	pub fn starts_with_str(&self, str: &str) -> bool {
		self.get_current().starts_with(str)
	}

	/// Can't do `-` and `+` because they are valid expression prefixed
	/// TODO `.` if not number etc.
	pub fn starts_with_expression_delimter(&self) -> bool {
		let current = self.get_current();
		IntoIterator::into_iter(["=", ",", ":", "?", "]", ")", "}", ";"])
			.any(|expression_delimiter| current.starts_with(expression_delimiter))
	}

	pub fn starts_with_statement_or_declaration_on_new_line(&self) -> bool {
		let current = self.get_current();
		IntoIterator::into_iter(["const", "let", "function", "class", "if", "for", "while"]).any(
			|stmt_or_dec_prefix| {
				current.starts_with(stmt_or_dec_prefix)
					&& current[stmt_or_dec_prefix.len()..]
						.chars()
						.next()
						.is_some_and(|next| !utilities::is_valid_identifier(next))
			},
		)
	}

	pub fn is_operator(&mut self, operator: &str) -> bool {
		self.skip();
		self.starts_with_str(operator)
	}

	pub fn is_operator_advance(&mut self, operator: &str) -> bool {
		self.skip();
		let current = self.get_current();
		let matches = current.starts_with(operator);
		if matches {
			self.head += operator.len() as u32;
		}
		matches
	}

	pub fn is_finished(&self) -> bool {
		self.get_current().is_empty()
	}

	pub fn get_start(&self) -> source_map::Start {
		source_map::Start(self.head)
	}

	pub fn get_end(&self) -> source_map::End {
		source_map::End(self.head)
	}

	pub fn advance(&mut self, count: u32) {
		self.head += count;
	}

	pub fn parse_identifier(&mut self, location: &'static str) -> Result<&'a str, ParseError> {
		self.skip();
		let current = self.get_current();
		let start = self.get_start();
		let mut iter = current.char_indices();
		if let Some((_, chr)) = iter.next() {
			let first_is_valid = chr.is_alphabetic() || chr == '_' || chr == '$';
			if !first_is_valid {
				let current = self.get_current();
				return Err(ParseError::new(
					ParseErrors::ExpectedIdentifier { location },
					start.with_length(chr.len_utf8()),
				));
			}
		} else {
			return Err(ParseError::new(
				ParseErrors::ExpectedIdentifier { location },
				start.with_length(0),
			));
		}

		for (idx, chr) in iter {
			// Note `is_alphanumeric` here
			let is_valid = chr.is_alphanumeric() || chr == '_' || chr == '$';
			if !is_valid {
				let value = &current[..idx];
				self.head += idx as u32;
				// TODO check value isn't `const` etc
				return Ok(value);
			}
		}

		self.head += current.len() as u32;

		Ok(current)
	}

	// For comments
	// WIP
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

	// For JSX
	pub fn parse_until_one_of(
		&mut self,
		possibles: &[&'static str],
	) -> Result<(&'a str, &'static str), ()> {
		let current = self.get_current();
		for i in 0.. {
			if let Some(until) = possibles.into_iter().find(|s| current[i..].starts_with(**s)) {
				self.head += (i + until.len()) as u32;
				return Ok((&current[..i], until));
			}
		}
		Err(())
	}

	pub fn parse_until_one_of_no_advance(
		&mut self,
		possibles: &[&'static str],
	) -> Result<(&'a str, &'static str), ()> {
		let current = self.get_current();
		for i in 0.. {
			if let Some(until) = possibles.into_iter().find(|s| current[i..].starts_with(**s)) {
				self.head += i as u32;
				let content = &current[..i];
				self.state.last_new_lines =
					content.chars().filter(|char| matches!(char, '\n')).count() as u32;
				return Ok((content, until));
			}
		}
		Err(())
	}

	// TODO proper error type
	pub fn parse_string_literal(&mut self) -> Result<(&'a str, crate::Quoted), ()> {
		let current = self.get_current();
		let mut chars = current.char_indices();
		let quoted = match chars.next() {
			Some((_, '"')) => crate::Quoted::Double,
			Some((_, '\'')) => crate::Quoted::Single,
			_ => return Err(()),
		};
		let mut escaped = false;
		for (idx, chr) in chars {
			if let '\\' = chr {
				escaped = true;
				continue;
			} else if escaped {
				escaped = false;
				continue;
			}

			if let (crate::Quoted::Double, '"') | (crate::Quoted::Single, '\'') = (quoted, chr) {
				// TODO double check
				let content = &current[1..idx];
				self.head += idx as u32 + 1;
				return Ok((content, quoted));
			}

			if let '\n' = chr {
				return Err(());
			}
		}
		Err(())
	}

	pub fn starts_with_number(&self) -> bool {
		self.get_current()
			.as_bytes()
			.first()
			.is_some_and(|b| (b'0'..=b'9').contains(&b) || *b == b'.')
	}

	pub fn starts_with_string_delimeter(&self) -> bool {
		self.starts_with('"') || self.starts_with('\'')
	}

	// TODO errors + some parts are weird
	pub fn parse_number_literal(
		&mut self,
	) -> Result<(crate::number::NumberRepresentation, u32), ()> {
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
			BigInt,
			Exponent,
		}

		let current = self.get_current();
		let mut chars = current.char_indices();

		let mut state = match chars.next().map(|(idx, chr)| chr) {
			Some('0') if current.as_bytes().get(1).is_some_and(|b| (b'0'..=b'7').contains(&b)) => {
				// TODO strict mode should be done in the parser stage (as that is where context is)
				NumberLiteralType::OctalLiteral
			}
			Some('0'..='9') => NumberLiteralType::Decimal { fractional: false },
			Some('.') => NumberLiteralType::Decimal { fractional: true },
			Some(_) | None => return Err(()),
		};

		for (idx, chr) in chars {
			match chr {
				_ if matches!(state, NumberLiteralType::BigInt) => {
					todo!("big int")
					// if is_number_delimiter(chr) {
					// 	// Content already checked
					// 	push_token!(TSXToken::NumberLiteral(script[start..idx].to_owned()));
					// 	set_state!(LexingState::None);
					// } else {
					// 	return_err!(LexingErrors::UnexpectedEndToNumberLiteral)
					// }
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
						return Err(());
					}
				}
				'0'..='9' | 'a'..='f' | 'A'..='F' => match state {
					NumberLiteralType::BinaryLiteral => {
						if !matches!(chr, '0' | '1') {
							// (LexingErrors::InvalidNumeralItemBecauseOfLiteralKind)
							return Err(());
						}
					}
					NumberLiteralType::OctalLiteral => {
						if !matches!(chr, '0'..='7') {
							// (LexingErrors::InvalidNumeralItemBecauseOfLiteralKind)
							return Err(());
						}
					}
					// Handling for 'e' & 'E'
					NumberLiteralType::Decimal { ref fractional } => {
						if matches!(chr, 'e' | 'E')
							&& !(*fractional || current[..idx].ends_with('_'))
						{
							state = NumberLiteralType::Exponent;
						} else if !chr.is_ascii_digit() {
							// (LexingErrors::InvalidNumeralItemBecauseOfLiteralKind)
							return Err(());
						}
					}
					NumberLiteralType::Exponent => {
						if !chr.is_ascii_digit() {
							// (LexingErrors::InvalidNumeralItemBecauseOfLiteralKind)
							return Err(());
						}
					}
					// all above allowed
					NumberLiteralType::HexadecimalLiteral => {}
					NumberLiteralType::BigInt => unreachable!(),
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
						} else if current[..idx].ends_with(['_']) {
							// (LexingErrors::InvalidUnderscore)
							return Err(());
						} else {
							*fractional = true;
						}
					} else {
						// (LexingErrors::NumberLiteralCannotHaveDecimalPoint);
						return Err(());
					}
				}
				'_' => {
					let invalid = match &state {
						NumberLiteralType::BinaryLiteral |
						NumberLiteralType::OctalLiteral |
						// Second `(idx - start) < 1` is for octal with prefix 0
						NumberLiteralType::HexadecimalLiteral => {
							todo!("hex undescore")
							// if start + 2 == idx {
							// 	current[..idx].ends_with(['b', 'B', 'x', 'X', 'o' , 'O'])
							// } else {
							// 	false
							// }
						},
						NumberLiteralType::Decimal { .. } => current[..idx].ends_with('.') || &current[..idx] == "0",
						NumberLiteralType::Exponent => current[..idx].ends_with(['e', 'E']),
						NumberLiteralType::BigInt => false
					};
					if invalid {
						// (LexingErrors::InvalidUnderscore);
						return Err(());
					}
				}
				'n' if matches!(state, NumberLiteralType::Decimal { fractional: false }) => {
					state = NumberLiteralType::BigInt;
				}
				// `10e-5` is a valid literal
				'-' if matches!(state, NumberLiteralType::Exponent if current[..idx].ends_with(['e', 'E'])) =>
					{}
				chr => {
					let num_slice = &current[..idx];
					let number = crate::number::NumberRepresentation::from_str(num_slice);
					let number = number.unwrap();
					let length = idx as u32;
					self.head += length;
					return Ok((number, length));
					// if is_number_delimiter(chr) {
					// 	// Note not = as don't want to include chr

					// 	if num_slice.trim_end() == "."
					// 		|| num_slice.ends_with(['x', 'X', 'o', 'O', '_', '-'])
					// 		|| (!matches!(state, NumberLiteralType::HexadecimalLiteral)
					// 			&& num_slice.ends_with(['e', 'E', 'b', 'B']))
					// 	{
					// 		// (LexingErrors::UnexpectedEndToNumberLiteral)
					// 		return Err(())
					// 	}
					// } else {
					// 	// (LexingErrors::UnexpectedEndToNumberLiteral)
					// 	return Err(())
					// }
				}
			}
		}

		let number = crate::number::NumberRepresentation::from_str(current).expect("bad number");
		let length = current.len() as u32;
		self.head += length;
		return Ok((number, length));
	}

	pub fn parse_regex_literal(&mut self) -> Result<(&'a str, Option<&'a str>, usize), ()> {
		let mut escaped = false;
		let mut after_last_slash = false;
		let mut in_set = false;
		let current = self.get_current();
		let mut chars = current.char_indices();

		assert!(chars.next().is_some_and(|(idx, chr)| chr == '/'));

		let mut regex_content = 1;

		for (idx, chr) in chars.by_ref() {
			match chr {
				'/' if !escaped && !in_set => {
					regex_content = idx;
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
					todo!("new line in regex")
				}
				_ => {
					escaped = false;
				}
			}
		}

		let regex = &current[1..regex_content];
		let regex_end = regex_content + '/'.len_utf8();

		let mut flag_content = regex_end;

		for (idx, chr) in chars {
			// TODO if flags.contains(|chr| !matches!(chr, 'd' | 'g' | 'i' | 'm' | 's' | 'u' | 'y'))
			if !chr.is_alphabetic() {
				flag_content = idx;
				break;
			}
		}

		let regex_flag = &current[regex_end..flag_content];

		self.head += flag_content as u32;

		Ok((regex, (!regex_flag.is_empty()).then_some(regex_flag), flag_content))
	}

	// TODO also can exit if there is `=` or `:` and = 0 in some examples
	pub fn after_brackets(&self) -> &'a str {
		let current = self.get_current();
		let mut bracket_count: u32 = 0;
		let mut open_chevrons = 0u64;
		// TODO account for string literals and comments
		// TODO account for utf16
		for (idx, chr) in current.as_bytes().into_iter().enumerate() {
			if let b'(' | b'{' | b'[' | b'<' = chr {
				open_chevrons |= (*chr == b'<') as u64;
				open_chevrons = open_chevrons << 1;
				bracket_count += 1;
			} else if let b')' | b'}' | b']' | b'>' = chr {
				// TODO WIP
				open_chevrons = open_chevrons >> 1;
				let last_was_open_chevron = (open_chevrons & 1) != 0;
				if last_was_open_chevron {
					if let b')' | b'}' | b']' = chr {
						// Extra removal
						open_chevrons = open_chevrons >> 1;
						bracket_count.saturating_sub(1);
					}
				} else if let b'>' = chr {
					continue;
				}

				bracket_count = bracket_count.saturating_sub(1);
				if bracket_count == 0 {
					// dbg!(&current[..idx]);
					return &current[(idx + 1)..].trim_start();
				}
			}
		}

		// Return empty slice
		Default::default()
	}

	pub fn after_identifier(&self) -> &'a str {
		let current = self.get_current();
		let mut paren_count: u32 = 0;

		let mut chars = current.as_bytes().into_iter().enumerate();
		for (idx, chr) in chars.by_ref() {
			if !chr.is_ascii_whitespace() {
				// test here as iteration consumed
				if !chr.is_ascii_alphanumeric() {
					return &current[idx..].trim_start();
				} else {
					break;
				}
			}
		}

		for (idx, chr) in chars {
			if !chr.is_ascii_alphanumeric() {
				return &current[idx..].trim_start();
			}
		}

		// Return empty slice
		Default::default()
	}

	// TODO WIP
	pub fn after_variable_start(&self) -> &'a str {
		let mut current = self.get_current().trim_start();
		if current.starts_with("const") {
			current = &current["const".len()..].trim_start();
		} else if current.starts_with("let") {
			current = &current["let".len()..].trim_start();
		} else if current.starts_with("var") {
			current = &current["var".len()..].trim_start();
		}

		if current.starts_with("{") || current.starts_with("[") {
			let mut paren_count: u32 = 0;
			// TODO account for string literals and comments
			for (idx, chr) in current.as_bytes().into_iter().enumerate() {
				if let b'(' | b'{' | b'[' | b'<' = chr {
					paren_count += 1;
				} else if let b')' | b'}' | b']' | b'>' = chr {
					paren_count = paren_count.saturating_sub(1);
					if paren_count == 0 {
						return &current[(idx + 1)..].trim_start();
					}
				}
			}
		} else {
			let mut paren_count: u32 = 0;
			let mut chars = current.as_bytes().into_iter().enumerate();
			for (_, chr) in chars.by_ref() {
				if !chr.is_ascii_whitespace() {
					break;
				}
			}
			for (idx, chr) in chars {
				if !chr.is_ascii_alphanumeric() {
					return &current[idx..].trim_start();
				}
			}
		}
		// Return empty slice
		Default::default()
	}

	/// Part of [ASI](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#automatic_semicolon_insertion)
	///
	/// TODO Also returns the line difference
	pub fn expect_semi_colon(&mut self) -> Result<(), ParseError> {
		let last = self.state.last_new_lines;
		// TODO order
		let semi_colon_like = self.starts_with_str("//")
			|| self.last_was_from_new_line() > 0
			|| self.is_operator("}")
			|| self.is_operator_advance(";")
			// TODO what about spaces
			|| self.starts_with_str("\n")
			|| self.is_finished();

		if semi_colon_like {
			Ok(())
		} else {
			let current = self.get_current();
			let until_empty = crate::lexer::utilities::next_empty_occurance(current);
			let position = self.get_start().with_length(until_empty);
			let error =
				ParseErrors::ExpectedOperator { expected: ";", found: &current[..until_empty] };
			Err(ParseError::new(error, position))
		}
	}

	pub fn is_semi_colon(&mut self) -> bool {
		self.skip();
		self.starts_with('}') || self.starts_with(';') || self.last_was_from_new_line() > 0
	}

	pub fn is_arrow_function(&mut self) -> (bool, Option<crate::types::TypeAnnotation>) {
		let current = self.get_current();
		let mut paren_count: u32 = 0;
		// TODO account for string literals and comments
		let mut after: u32 = 0;
		for (idx, chr) in current.as_bytes().into_iter().enumerate() {
			if let b'(' = chr {
				paren_count += 1;
			} else if let b')' = chr {
				paren_count = paren_count.saturating_sub(1);
				if paren_count == 0 {
					after = (idx + ")".len()) as u32;
					break;
				}
			}
		}

		// TODO no new lines
		let after_brackets = &current[after as usize..].trim_start();
		if after_brackets.starts_with("=>") {
			(true, None)
		} else if self.options.type_annotations && after_brackets.starts_with(":") {
			// TODO WIP implementation
			let save_point = self.head;
			let mut reader = self;
			reader.head += after + 1;
			// I hate this!!. Can double allocate for expressions
			let annotation = crate::types::TypeAnnotation::from_reader_with_precedence(
				reader,
				crate::types::type_annotations::TypeOperatorKind::ReturnType,
			);
			let starts_with_arrow = reader.starts_with_str("=>");
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
}

pub(crate) mod utilities {
	pub fn is_valid_identifier(chr: char) -> bool {
		chr.is_alphanumeric() || chr == '_' || chr == '$'
	}

	// TODO move
	pub fn next_empty_occurance(str: &str) -> usize {
		let mut chars = str.char_indices();
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

	/// TODO lots more, also
	pub fn is_function_header(str: &str) -> bool {
		str.starts_with("async ") || {
			str.starts_with("function")
				&& !is_valid_identifier(str["function".len()..].chars().next().expect("TODO"))
		}
	}

	/// TODO this could be set to collect, rather than breaking (https://github.com/kaleidawave/ezno/issues/203)
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

	pub fn expected_one_of_keywords(
		reader: &super::Lexer,
		expected: &'static [&'static str],
	) -> crate::ParseError {
		let current = reader.get_current();
		let found = &current[..self::next_empty_occurance(current)];
		let position = reader.get_start().with_length(found.len());
		let reason = crate::ParseErrors::ExpectedOneOfKeywords { expected, found };
		crate::ParseError::new(reason, position)
	}
}
