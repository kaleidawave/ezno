//! Contains lexing logic for all the whole of JS + TypeScript type annotations + JSX + other syntax
//!
//! Uses [`TSXToken`]s for data, uses [Span] for location data. Uses [`tokenizer_lib`] for logic.

#![allow(clippy::as_conversions, clippy::cast_possible_truncation)]
#![allow(unused)]

use super::{Span, TSXToken};
use crate::{
	errors::LexingErrors, html_tag_contains_literal_content, html_tag_is_self_closing, Comments,
	Quoted,
};
use tokenizer_lib::{sized_tokens::TokenStart, Token, TokenSender};

use derive_finite_automaton::{
	FiniteAutomata, FiniteAutomataConstructor, GetAutomataStateForValue, GetNextResult,
};

#[allow(clippy::struct_excessive_bools)]
pub struct LexerOptions {
	/// Whether to append tokens when lexing. If false will just ignore
	pub comments: Comments,
	/// Whether to parse JSX. TypeScript's `<number> 2` breaks the lexer so this can be disabled to allow
	/// for that syntax
	pub lex_jsx: bool,
	/// TODO temp
	pub allow_unsupported_characters_in_jsx_attribute_keys: bool,
	pub allow_expressions_in_jsx: bool,
	pub top_level_html: bool,
}

impl Default for LexerOptions {
	fn default() -> Self {
		Self {
			comments: Comments::All,
			lex_jsx: true,
			allow_unsupported_characters_in_jsx_attribute_keys: true,
			allow_expressions_in_jsx: true,
			top_level_html: false,
		}
	}
}

// mod lexer_state {
pub(super) enum JSXAttributeValueDelimiter {
	None,
	SingleQuote,
	DoubleQuote,
}

pub(super) enum JSXTagNameDirection {
	Opening,
	Closing,
}

pub(super) enum JSXLexingState {
	/// Only for top level html
	ExpectingOpenChevron,
	TagName {
		direction: JSXTagNameDirection,
		lexed_start: bool,
	},
	/// For lexing the close chevron after the slash in self closing tags
	SelfClosingTagClose,
	AttributeKey,
	AttributeEqual,
	AttributeValue(JSXAttributeValueDelimiter),
	Comment,
	Content,
	/// For script and style tags
	LiteralContent {
		last_char_was_open_chevron: bool,
	},
}

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

// /// Current parsing state of the lexer.
// pub(super) enum LexingState {
// 	String {
// 		double_quoted: bool,
// 		escaped: bool,
// 	},
// 	TemplateLiteral {
// 		interpolation_depth: u16,
// 		last_char_was_dollar: bool,
// 		escaped: bool,
// 	},
// 	JSXLiteral {
// 		state: JSXLexingState,
// 		interpolation_depth: u16,
// 		tag_depth: u16,
// 		/// `true` for `script` and `style` tags
// 		/// TODO currently isn't handled at all
// 		no_inner_tags_or_expressions: bool,
// 		is_self_closing_tag: bool,
// 	},
// 	RegexLiteral {
// 		escaped: bool,
// 		/// aka on flags
// 		after_last_slash: bool,
// 		/// Forward slash while in `[...]` is allowed
// 		in_set: bool,
// 	},
// }

// TODO WIP
// const DEFAULT_JSX_LEXING_STATE: LexingState = LexingState::JSXLiteral {
// 	interpolation_depth: 0,
// 	tag_depth: 0,
// 	state: JSXLexingState::ExpectingOpenChevron,
// 	no_inner_tags_or_expressions: false,
// 	is_self_closing_tag: false,
// };
// const FIRST_CHEVRON_JSX_LEXING_STATE: LexingState = LexingState::JSXLiteral {
// 	interpolation_depth: 0,
// 	tag_depth: 0,
// 	state: JSXLexingState::TagName { direction: JSXTagNameDirection::Opening, lexed_start: false },
// 	no_inner_tags_or_expressions: false,
// 	is_self_closing_tag: false,
// };
// // }

// TODO state for "use strict" etc?
// TODO hold Keywords map, markers, syntax errors etc
pub struct Lexer<'a> {
	options: LexerOptions,
	// last: u32,
	head: u32,
	on: &'a str,
	// state: LexingState,
	// state_stack: Vec<LexingState>,
}

// TODO helpers for number, regular expression, maybe JSX maybe not.
impl<'a> Lexer<'a> {
	// (crate)
	pub fn new(script: &'a str, offset: Option<u32>, options: LexerOptions) -> Self {
		if script.len() > u32::MAX as usize {
			todo!()
			// return Err((LexingErrors::CannotLoadLargeFile(script.len()), source_map::Nullable::NULL));
		}
		Lexer {
			options,
			// last: offset.unwrap_or_default(),
			head: 0, // TODO offset.unwrap_or_default(),
			on: script,
		}
	}

	pub fn get_current(&self) -> &'a str {
		&self.on[self.head as usize..]
	}

	pub fn skip(&mut self) {
		self.head += self.get_current().chars().take_while(|c| c.is_whitespace()).count() as u32;
	}

	pub fn skip_and_count_new_lines(&mut self) -> u32 {
		let mut count = 0;
		for (idx, chr) in self.get_current().char_indices() {
			if chr == '\n' {
				count += 1;
			} else if !chr.is_whitespace() {
				self.head += idx as u32;
				return count;
			}
		}
		count
	}

	pub fn is_keyword(&mut self, keyword: &str) -> bool {
		let current = self.get_current();
		let length = keyword.len();
		current.starts_with(keyword)
			&& current[length..].chars().next().map_or(true, |chr| !chr.is_alphanumeric())
	}

	pub fn is_keyword_advance(&mut self, keyword: &str) -> bool {
		let current = self.get_current();
		let length = keyword.len();
		if current.starts_with(keyword)
			&& current[length..].chars().next().map_or(true, |chr| !chr.is_alphanumeric())
		{
			self.head += length as u32;
			true
		} else {
			false
		}
	}

	// Does not advance
	pub fn is_one_of_keyword<'b>(&self, keywords: &'static [&'b str]) -> Option<&'b str> {
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

	pub fn is_one_of_keyword_advance<'b>(
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

	pub fn expect(&mut self, chr: char) -> Result<(), crate::ParseError> {
		self.skip();
		let current = self.get_current();
		if current.starts_with(chr) {
			self.head += chr.len_utf8() as u32;
			Ok(())
		} else {
			let position = self.get_start().with_length(chr.len_utf8());
			let reason = crate::ParseErrors::UnexpectedCharacter {
				expected: &[chr],
				found: current.chars().next().unwrap(),
			};
			Err(crate::ParseError::new(reason, position))
		}
	}

	pub fn expect_keyword(
		&mut self,
		str: &'static str,
	) -> Result<source_map::Start, crate::ParseError> {
		self.skip();
		let current = self.get_current();
		if current.starts_with(str) {
			let start = source_map::Start(self.head);
			self.head += str.len() as u32;
			Ok(start)
		} else {
			// TODO move
			fn next_empty_occurance(str: &str) -> usize {
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

			let found = &current[..next_empty_occurance(current)];
			let position = self.get_start().with_length(found.len());
			let reason = crate::ParseErrors::ExpectedKeyword { expected: str, found };
			Err(crate::ParseError::new(reason, position))
		}
	}

	pub fn expect_and_get_after(
		&mut self,
		chr: char,
	) -> Result<source_map::End, crate::ParseError> {
		self.expect(chr).map(|()| source_map::End(self.head))
	}

	pub fn is_no_advance(&mut self, chr: char) -> Result<(), ()> {
		if self.get_current().starts_with(chr) {
			Ok(())
		} else {
			Err(())
		}
	}

	pub fn starts_with(&mut self, chr: char) -> bool {
		self.get_current().starts_with(chr)
	}

	pub fn starts_with_str(&mut self, str: &str) -> bool {
		self.get_current().starts_with(str)
	}

	pub fn starts_with_str_advance(&mut self, str: &str) -> bool {
		let result = self.get_current().starts_with(str);
		if result {
			self.head += str.len() as u32;
		}
		result
	}

	pub fn is_and_advance(&mut self, chr: char) -> bool {
		if self.get_current().starts_with(chr) {
			self.head += chr.len_utf8() as u32;
			true
		} else {
			false
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

	pub fn is_operator_advance<'b>(&mut self, operator: &str) -> bool {
		let current = self.get_current();
		let matches = current.starts_with(operator);
		if matches {
			self.head += operator.len() as u32;
		}
		matches
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

	pub fn parse_identifier(&mut self) -> Result<&'a str, ()> {
		self.skip();
		let current = self.get_current();
		let mut iter = current.char_indices();
		if iter.next().is_some_and(|(_, chr)| ('0'..='9').contains(&chr)) {
			return Err(());
		}
		for (idx, chr) in iter {
			if !chr.is_alphanumeric() {
				let value = &current[..idx];
				self.head += idx as u32;
				return Ok(value);
			}
		}
		Err(())
	}

	// TODO proper error type
	pub fn parse_string_literal(&mut self) -> Result<(String, crate::Quoted), ()> {
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
				let content = current[1..idx].to_owned();
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

	// TODO errors + some parts are weird
	pub fn parse_number_literal(
		&mut self,
	) -> Result<(crate::number::NumberRepresentation, u32), ()> {
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
					todo!()
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
						if current[..idx].ends_with(['_']) {
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
							todo!()
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
					use std::str::FromStr;

					let num_slice = &current[..idx];
					let number = crate::number::NumberRepresentation::from_str(num_slice);
					let number = number.unwrap();
					self.head += idx as u32;
					return Ok((number, idx as u32));
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
		Err(())
	}

	pub fn parse_regex_literal(&mut self) -> Result<(String, Option<String>, usize), ()> {
		todo!();
		// let flags = if let Some(Token(TSXToken::RegexFlagLiteral(flags), start)) =
		// 	flag_token
		// {
		// 	if flags.contains(|chr| !matches!(chr, 'd' | 'g' | 'i' | 'm' | 's' | 'u' | 'y'))
		// 	{
		// 		return Err(ParseError::new(
		// 			ParseErrors::InvalidRegexFlag,
		// 			start.with_length(flags.len()),
		// 		));
		// 	}
		// 	position = position.union(start.get_end_after(flags.len()));
		// 	Some(flags)
		// } else {
		// 	None
		// };
	}

	// TODO also can exit if there is `=` or `:` and = 0 in some examples
	pub fn after_brackets(&self) -> &'a str {
		let current = self.get_current();
		let mut paren_count = 0;
		for (idx, chr) in current.as_bytes().into_iter().enumerate() {
			if let b'(' | b'{' | b'[' = chr {
				paren_count += 1;
			} else if let b')' | b'}' | b']' = chr {
				if paren_count > 0 {
					paren_count -= 1;
				} else {
					return &current[idx..];
				}
			}
		}

		// Return empty slice
		Default::default()
	}
}
