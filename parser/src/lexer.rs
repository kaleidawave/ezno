//! Contains lexing logic for all the whole of JS + TypeScript type annotations + JSX + other syntax
//!
//! Uses [`TSXToken`]s for data, uses [Span] for location data. Uses [`tokenizer_lib`] for logic.

#![allow(clippy::as_conversions, clippy::cast_possible_truncation)]

use super::{Span, TSXToken};
use crate::{
	errors::LexingErrors, html_tag_contains_literal_content, html_tag_is_self_closing, Comments,
	Quoted,
};
use tokenizer_lib::{sized_tokens::TokenStart, Token, TokenSender};

use derive_finite_automaton::{
	FiniteAutomata, FiniteAutomataConstructor, GetAutomataStateForValue, GetNextResult,
};

mod html {}

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

fn is_number_delimiter(chr: char) -> bool {
	matches!(
		chr,
		' ' | ','
			| '\n' | '\r'
			| ';' | '+' | '-'
			| '*' | '/' | '&'
			| '|' | '!' | '^'
			| '(' | '{' | '['
			| ')' | '}' | ']'
			| '%' | '=' | ':'
			| '<' | '>' | '?'
			| '"' | '\'' | '`'
			| '#'
	)
}

/// *Tokenizes* script appending Tokens to `sender` using [TokenSender::push]
/// `offset` represents the start of the source if script is contained in some larger buffer
///
/// Returns () if successful, if runs into lexing error will short-circuit
///
/// **MARKERS HAVE TO BE IN FORWARD ORDER**
#[doc(hidden)]
pub fn lex_script(
	script: &str,
	sender: &mut impl TokenSender<TSXToken, crate::TokenStart>,
	options: &LexerOptions,
	offset: Option<u32>,
) -> Result<(), (LexingErrors, Span)> {
	#[derive(PartialEq, Debug)]
	enum JSXAttributeValueDelimiter {
		None,
		SingleQuote,
		DoubleQuote,
	}

	#[derive(PartialEq, Debug, Eq)]
	enum JSXTagNameDirection {
		Opening,
		Closing,
	}

	#[derive(PartialEq, Debug)]
	enum JSXLexingState {
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

	#[derive(PartialEq, Debug)]
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

	impl Default for NumberLiteralType {
		fn default() -> Self {
			Self::Decimal { fractional: false }
		}
	}

	/// Current parsing state of the lexer.
	#[derive(PartialEq, Debug)]
	enum LexingState {
		None,
		Identifier,
		Symbol(GetAutomataStateForValue<TSXToken>),
		// Literals:
		Number(NumberLiteralType),
		String {
			double_quoted: bool,
			escaped: bool,
		},
		TemplateLiteral {
			interpolation_depth: u16,
			last_char_was_dollar: bool,
			escaped: bool,
		},
		JSXLiteral {
			state: JSXLexingState,
			interpolation_depth: u16,
			tag_depth: u16,
			/// `true` for `script` and `style` tags
			/// TODO currently isn't handled at all
			no_inner_tags_or_expressions: bool,
			is_self_closing_tag: bool,
		},
		SingleLineComment,
		MultiLineComment {
			last_char_was_star: bool,
		},
		RegexLiteral {
			escaped: bool,
			/// aka on flags
			after_last_slash: bool,
			/// Forward slash while in `[...]` is allowed
			in_set: bool,
		},
	}

	// TODO WIP
	const DEFAULT_JSX_LEXING_STATE: LexingState = LexingState::JSXLiteral {
		interpolation_depth: 0,
		tag_depth: 0,
		state: JSXLexingState::ExpectingOpenChevron,
		no_inner_tags_or_expressions: false,
		is_self_closing_tag: false,
	};
	const FIRST_CHEVRON_JSX_LEXING_STATE: LexingState = LexingState::JSXLiteral {
		interpolation_depth: 0,
		tag_depth: 0,
		state: JSXLexingState::TagName {
			direction: JSXTagNameDirection::Opening,
			lexed_start: false,
		},
		no_inner_tags_or_expressions: false,
		is_self_closing_tag: false,
	};

	if script.len() > u32::MAX as usize {
		return Err((LexingErrors::CannotLoadLargeFile(script.len()), source_map::Nullable::NULL));
	}

	let mut state: LexingState =
		if options.top_level_html { DEFAULT_JSX_LEXING_STATE } else { LexingState::None };

	// Used to go back to previous state if was in template literal or JSX literal
	let mut state_stack: Vec<LexingState> = Vec::new();

	// Used to index the slice (thus no offset)
	let mut start: usize = 0;
	let offset = offset.unwrap_or_default();

	// This is a sneaky technique for regex and JSX literals. It seems to be almost impossible to determine
	// whether the forward slash in: `/ x` should be a division symbol token or the start of regex literal. It is import
	// to discern whether it is regex or division at this point as regex literal needs to be parsed as a literal rather
	// than a sequence of tokens. Similarly for JSX is a < a less than comparison or the start of a tag. This variable
	// should be set to true if the last pushed token was `=`, `return` etc and set to else set to false.
	// TODO this doesn't work see #165
	let mut expect_expression = true;

	macro_rules! return_err {
		($err:expr) => {{
			sender.push(Token(TSXToken::EOS, TokenStart::new(script.len() as u32)));
			return Err((
				$err,
				Span {
					start: start as u32 + offset,
					// TODO + 1
					end: start as u32 + offset,
					source: (),
				},
			));
		}};
	}

	let mut characters = script.char_indices();
	if script.starts_with("#!") {
		for (idx, c) in characters.by_ref() {
			if c == '\n' {
				sender.push(Token(
					TSXToken::HashBangComment(script[2..idx].to_owned()),
					TokenStart::new(0),
				));
				break;
			}
		}
	}

	if options.top_level_html && script.starts_with("<!DOCTYPE html>") {
		for (_idx, c) in characters.by_ref() {
			if c == '>' {
				sender.push(Token(TSXToken::DocTypeHTML, TokenStart::new(0)));
				break;
			}
		}
	}

	for (idx, chr) in characters {
		// dbg!(chr, &state);

		// Sets current parser state and updates start track
		macro_rules! set_state {
			($s:expr) => {{
				start = idx;
				state = $s;
				expect_expression = false;
			}};

			($s:expr, EXPECT_EXPRESSION: $v:expr) => {{
				start = idx;
				state = $s;
				expect_expression = $v;
			}};
		}

		// Pushes a new token
		macro_rules! push_token {
			($t:expr $(,)?) => {{
				let res = sender.push(Token($t, TokenStart::new(start as u32 + offset)));
				if !res {
					return Ok(());
				}
			}};
		}

		match state {
			LexingState::Number(ref mut literal_type) => {
				match chr {
					_ if matches!(literal_type, NumberLiteralType::BigInt) => {
						if is_number_delimiter(chr) {
							// Content already checked
							push_token!(TSXToken::NumberLiteral(script[start..idx].to_owned()));
							set_state!(LexingState::None);
						} else {
							return_err!(LexingErrors::UnexpectedEndToNumberLiteral)
						}
					}
					// For binary/hexadecimal/octal literals
					'b' | 'B' | 'x' | 'X' | 'o' | 'O' if start + 1 == idx => {
						if script[start..].starts_with('0') {
							*literal_type = match chr {
								'b' | 'B' => NumberLiteralType::BinaryLiteral,
								'o' | 'O' => NumberLiteralType::OctalLiteral,
								'x' | 'X' => NumberLiteralType::HexadecimalLiteral,
								_ => unreachable!(),
							}
						} else {
							return_err!(
								LexingErrors::NumberLiteralBaseSpecifierMustPrecededWithZero
							);
						}
					}
					'0'..='9' | 'a'..='f' | 'A'..='F' => match literal_type {
						NumberLiteralType::BinaryLiteral => {
							if !matches!(chr, '0' | '1') {
								return_err!(LexingErrors::InvalidNumeralItemBecauseOfLiteralKind)
							}
						}
						NumberLiteralType::OctalLiteral => {
							if !matches!(chr, '0'..='7') {
								return_err!(LexingErrors::InvalidNumeralItemBecauseOfLiteralKind)
							}
						}
						// Handling for 'e' & 'E'
						NumberLiteralType::Decimal { fractional } => {
							if matches!(chr, 'e' | 'E')
								&& !(*fractional || script[..idx].ends_with('_'))
							{
								*literal_type = NumberLiteralType::Exponent;
							} else if !chr.is_ascii_digit() {
								return_err!(LexingErrors::InvalidNumeralItemBecauseOfLiteralKind)
							}
						}
						NumberLiteralType::Exponent => {
							if !chr.is_ascii_digit() {
								return_err!(LexingErrors::InvalidNumeralItemBecauseOfLiteralKind)
							}
						}
						// all above allowed
						NumberLiteralType::HexadecimalLiteral => {}
						NumberLiteralType::BigInt => unreachable!(),
					},
					'.' => {
						if let NumberLiteralType::Decimal { fractional } = literal_type {
							if script[..idx].ends_with(['_']) {
								return_err!(LexingErrors::InvalidUnderscore)
							} else if *fractional {
								// Catch for spread token `...`
								if start + 1 == idx {
									let automaton = TSXToken::new_automaton();
									let derive_finite_automaton::GetNextResult::NewState(
										dot_state_one,
									) = automaton.get_next('.')
									else {
										unreachable!()
									};
									let derive_finite_automaton::GetNextResult::NewState(
										dot_state_two,
									) = dot_state_one.get_next('.')
									else {
										unreachable!()
									};
									state = LexingState::Symbol(dot_state_two);
								} else {
									return_err!(LexingErrors::SecondDecimalPoint);
								}
							} else {
								*fractional = true;
							}
						} else {
							return_err!(LexingErrors::NumberLiteralCannotHaveDecimalPoint);
						}
					}
					'_' => {
						let invalid = match literal_type {
							NumberLiteralType::BinaryLiteral |
							NumberLiteralType::OctalLiteral |
							// Second `(idx - start) < 1` is for octal with prefix 0
							NumberLiteralType::HexadecimalLiteral => {
								if start + 2 == idx {
									script[..idx].ends_with(['b', 'B', 'x', 'X', 'o' , 'O'])
								} else {
									false
								}
							},
							NumberLiteralType::Decimal { .. } => script[..idx].ends_with('.') || &script[start..idx] == "0",
							NumberLiteralType::Exponent => script[..idx].ends_with(['e', 'E']),
							NumberLiteralType::BigInt => false
						};
						if invalid {
							return_err!(LexingErrors::InvalidUnderscore);
						}
					}
					'n' if matches!(
						literal_type,
						NumberLiteralType::Decimal { fractional: false }
					) =>
					{
						*literal_type = NumberLiteralType::BigInt;
					}
					// `10e-5` is a valid literal
					'-' if matches!(literal_type, NumberLiteralType::Exponent if script[..idx].ends_with(['e', 'E'])) =>
						{}
					chr => {
						if is_number_delimiter(chr) {
							// Note not = as don't want to include chr
							let num_slice = &script[start..idx];
							if num_slice.trim_end() == "."
								|| num_slice.ends_with(['x', 'X', 'o', 'O', '_', '-'])
								|| (!matches!(literal_type, NumberLiteralType::HexadecimalLiteral)
									&& num_slice.ends_with(['e', 'E', 'b', 'B']))
							{
								return_err!(LexingErrors::UnexpectedEndToNumberLiteral)
							}
							push_token!(TSXToken::NumberLiteral(num_slice.to_owned()));
							set_state!(LexingState::None);
						} else {
							return_err!(LexingErrors::UnexpectedEndToNumberLiteral)
						}
					}
				}
			}
			LexingState::Symbol(symbol_state) => {
				// TODO if number and state == first dot then do number parsing (should be
				// done when derive finite automaton gets pattern support)
				match symbol_state.get_next(chr) {
					GetNextResult::Result { result, ate_character } => {
						// Handle comments
						match result {
							TSXToken::Comment(_) => {
								state = LexingState::SingleLineComment;
								continue;
							}
							TSXToken::MultiLineComment(_) => {
								state = LexingState::MultiLineComment { last_char_was_star: false };
								continue;
							}
							_ => {}
						}
						state = LexingState::None;
						expect_expression = result.is_expression_prefix();
						if ate_character {
							push_token!(result);
							start = idx + chr.len_utf8();
							continue;
						}

						push_token!(result);
						start = idx;
					}
					GetNextResult::NewState(new_state) => {
						state = LexingState::Symbol(new_state);
					}
					GetNextResult::InvalidCharacter(err) => {
						return_err!(LexingErrors::UnexpectedCharacter(err));
					}
				}
			}
			LexingState::Identifier => match chr {
				'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | '$' => {}
				_ => {
					let token = TSXToken::from_slice(&script[start..idx]);
					let is_expression_prefix = token.is_expression_prefix();
					push_token!(token);
					set_state!(LexingState::None, EXPECT_EXPRESSION: is_expression_prefix);
				}
			},
			LexingState::String { ref mut double_quoted, ref mut escaped } => match chr {
				'\n' => {
					return_err!(LexingErrors::NewLineInStringLiteral);
				}
				'\'' if !*double_quoted && !*escaped => {
					push_token!(TSXToken::StringLiteral(
						script[(start + 1)..idx].to_owned(),
						Quoted::Single
					));
					state = LexingState::None;
					start = idx + 1;
					expect_expression = false;
					continue;
				}
				'"' if *double_quoted && !*escaped => {
					push_token!(TSXToken::StringLiteral(
						script[(start + 1)..idx].to_owned(),
						Quoted::Double
					));
					state = LexingState::None;
					start = idx + 1;
					expect_expression = false;
					continue;
				}
				'\\' if !*escaped => {
					*escaped = true;
				}
				_ => {
					*escaped = false;
				}
			},
			LexingState::SingleLineComment => {
				if let '\n' = chr {
					let content = &script[(start + 2)..idx];
					if options.comments.should_add_comment(content) {
						push_token!(TSXToken::Comment(content.trim_end().to_owned()));
					}
					set_state!(LexingState::None);
					continue;
				}
			}
			LexingState::MultiLineComment { ref mut last_char_was_star } => match chr {
				'/' if *last_char_was_star => {
					let content = &script[(start + 2)..(idx - 1)];
					if options.comments.should_add_comment(content) {
						push_token!(TSXToken::MultiLineComment(content.to_owned()));
					}
					set_state!(LexingState::None);
					continue;
				}
				chr => {
					*last_char_was_star = chr == '*';
				}
			},
			LexingState::RegexLiteral {
				ref mut escaped,
				ref mut after_last_slash,
				ref mut in_set,
			} => {
				if *after_last_slash {
					if !chr.is_alphabetic() {
						if start != idx {
							push_token!(TSXToken::RegexFlagLiteral(script[start..idx].to_owned()));
						}
						set_state!(LexingState::None);
					}
				} else {
					match chr {
						'/' if start + 1 == idx => {
							state = LexingState::SingleLineComment;
							continue;
						}
						'*' if start + 1 == idx => {
							state = LexingState::MultiLineComment { last_char_was_star: false };
							continue;
						}
						'/' if !*escaped && !*in_set => {
							push_token!(TSXToken::RegexLiteral(
								script[(start + 1)..idx].to_owned()
							));
							*after_last_slash = true;
							start = idx + 1;
						}
						'\\' if !*escaped => {
							*escaped = true;
						}
						'[' => {
							*in_set = true;
						}
						']' if *in_set => {
							*in_set = false;
						}
						'\n' => {
							return_err!(LexingErrors::ExpectedEndToRegexLiteral);
						}
						_ => {
							*escaped = false;
						}
					}
				}
			}
			LexingState::TemplateLiteral {
				ref mut last_char_was_dollar,
				ref mut interpolation_depth,
				ref mut escaped,
			} => match chr {
				'$' if !*escaped => *last_char_was_dollar = true,
				'{' if *last_char_was_dollar => {
					if idx > start + 1 {
						push_token!(TSXToken::TemplateLiteralChunk(
							script[start..(idx - 1)].to_owned()
						));
					}
					start = idx - 1;
					push_token!(TSXToken::TemplateLiteralExpressionStart);
					*interpolation_depth += 1;
					*last_char_was_dollar = false;
					state_stack.push(state);

					start = idx + 1;
					state = LexingState::None;
					expect_expression = true;
					continue;
				}
				'`' if !*escaped => {
					if idx > start {
						push_token!(TSXToken::TemplateLiteralChunk(script[start..idx].to_owned()));
					}
					start = idx;
					push_token!(TSXToken::TemplateLiteralEnd);
					start = idx + 1;
					state = LexingState::None;
					expect_expression = false;
					continue;
				}
				'\\' => {
					*last_char_was_dollar = false;
					*escaped = true;
				}
				_ => {
					*last_char_was_dollar = false;
					*escaped = false;
				}
			},
			LexingState::JSXLiteral {
				ref mut interpolation_depth,
				ref mut tag_depth,
				ref mut no_inner_tags_or_expressions,
				ref mut is_self_closing_tag,
				state: ref mut jsx_state,
			} => {
				match jsx_state {
					JSXLexingState::ExpectingOpenChevron => {
						if chr == '<' {
							set_state!(FIRST_CHEVRON_JSX_LEXING_STATE);
						} else if !chr.is_whitespace() {
							dbg!(chr);
							return_err!(LexingErrors::ExpectedOpenChevron);
						}
					}
					JSXLexingState::TagName { ref mut direction, ref mut lexed_start } => match chr
					{
						// Closing tag
						'>' if *direction == JSXTagNameDirection::Closing => {
							*tag_depth = match tag_depth.checked_sub(1) {
								Some(value) => value,
								None => {
									return_err!(LexingErrors::UnbalancedJSXClosingTags);
								}
							};
							if *lexed_start {
								push_token!(TSXToken::JSXClosingTagName(
									script[start..idx].trim().to_owned()
								));
							} else {
								push_token!(TSXToken::JSXFragmentEnd);
							}
							// If JSX literal range has ended
							if *tag_depth == 0 {
								set_state!(LexingState::None);
								continue;
							}

							start = idx + 1;
							*jsx_state = JSXLexingState::Content;
						}
						// Fragment start
						'>' if !*lexed_start => {
							push_token!(TSXToken::JSXFragmentStart);
							*jsx_state = JSXLexingState::Content;
							start = idx + 1;
							*tag_depth += 1;
							continue;
						}
						// Tag name characters:
						'A'..='Z' | 'a'..='z' | '0'..='9' => {
							// Add the opening tag here as know it is not closing
							if !*lexed_start {
								match direction {
									JSXTagNameDirection::Opening => {
										push_token!(TSXToken::JSXOpeningTagStart);
										start += 1;
									}
									JSXTagNameDirection::Closing => {
										push_token!(TSXToken::JSXClosingTagStart);
										start += 2;
									}
								}
								*lexed_start = true;
							}
						}
						'-' => {
							if start + 1 == idx {
								// TODO this is really the position rather the character
								return_err!(LexingErrors::InvalidCharacterInJSXTag('-'))
							}
						}
						// Runs if closing tag </div>
						'/' if start + 1 == idx => {
							*direction = JSXTagNameDirection::Closing;
						}
						// HTML comments!!!
						'!' if start + 1 == idx => {
							*jsx_state = JSXLexingState::Comment;
						}
						// Non-tag name character
						chr => {
							if *direction == JSXTagNameDirection::Closing {
								return_err!(LexingErrors::ExpectedJSXEndTag);
							}
							let tag_name = script[start..idx].trim();
							*is_self_closing_tag = html_tag_is_self_closing(tag_name);
							*no_inner_tags_or_expressions =
								html_tag_contains_literal_content(tag_name);
							push_token!(TSXToken::JSXTagName(tag_name.to_owned()));
							start = idx;
							*tag_depth += 1;
							match chr {
								'/' if *is_self_closing_tag => {
									*jsx_state = JSXLexingState::SelfClosingTagClose;
								}
								'>' => {
									push_token!(TSXToken::JSXOpeningTagEnd);
									start = idx + 1;
									*jsx_state = if *no_inner_tags_or_expressions {
										JSXLexingState::LiteralContent {
											last_char_was_open_chevron: false,
										}
									} else {
										JSXLexingState::Content
									};
									continue;
								}
								chr if chr.is_whitespace() => {
									*jsx_state = JSXLexingState::AttributeKey;
								}
								chr => {
									return_err!(LexingErrors::InvalidCharacterInJSXTag(chr));
								}
							}
							start = idx + chr.len_utf8();
						}
					},
					JSXLexingState::SelfClosingTagClose => {
						if chr == '>' {
							*tag_depth = match tag_depth.checked_sub(1) {
								Some(value) => value,
								None => {
									return_err!(LexingErrors::UnbalancedJSXClosingTags);
								}
							};
							push_token!(TSXToken::JSXSelfClosingTag);
							start = idx + 1;
							// If JSX literal range has ended
							if *tag_depth == 0 {
								set_state!(LexingState::None);
							} else {
								*jsx_state = JSXLexingState::Content;
							}
							continue;
						}
						return_err!(LexingErrors::ExpectedClosingChevronAtEndOfSelfClosingTag);
					}
					JSXLexingState::AttributeKey => match chr {
						'=' => {
							if start >= idx {
								return_err!(LexingErrors::EmptyAttributeName);
							}
							let key_slice = script[start..idx].trim();
							if !key_slice.is_empty() {
								push_token!(TSXToken::JSXAttributeKey(key_slice.to_owned()));
							}
							start = idx;
							push_token!(TSXToken::JSXAttributeAssign);
							*jsx_state = JSXLexingState::AttributeEqual;
							start = idx + 1;
						}
						'{' => {
							push_token!(TSXToken::JSXExpressionStart);
							*interpolation_depth += 1;
							state_stack.push(state);
							set_state!(LexingState::None, EXPECT_EXPRESSION: true);
							continue;
						}
						'/' => {
							*jsx_state = JSXLexingState::SelfClosingTagClose;
						}
						'>' => {
							// Accounts for <div hidden>
							if start < idx {
								push_token!(TSXToken::JSXAttributeKey(
									script[start..idx].to_owned()
								));
							}
							if *is_self_closing_tag {
								*tag_depth = match tag_depth.checked_sub(1) {
									Some(value) => value,
									None => {
										return_err!(LexingErrors::UnbalancedJSXClosingTags);
									}
								};
								push_token!(TSXToken::JSXSelfClosingTag);
								start = idx + 1;
								// If JSX literal range has ended
								if *tag_depth == 0 {
									set_state!(LexingState::None);
								} else {
									*jsx_state = JSXLexingState::Content;
									*is_self_closing_tag = false;
								}
							} else {
								push_token!(TSXToken::JSXOpeningTagEnd);
								start = idx + 1;
								*jsx_state = if *no_inner_tags_or_expressions {
									JSXLexingState::LiteralContent {
										last_char_was_open_chevron: false,
									}
								} else {
									JSXLexingState::Content
								};
							}
							continue;
						}
						chr if chr.is_whitespace() => {
							if start < idx {
								push_token!(TSXToken::JSXAttributeKey(
									script[start..idx].to_owned()
								));
							}
							start = idx + chr.len_utf8();
						}
						chr => {
							let character_allowed = chr.is_alphanumeric()
								|| chr == '-' || (options
								.allow_unsupported_characters_in_jsx_attribute_keys
								&& matches!(
									chr,
									'@' | ':' | '.' | '[' | ']' | '+' | '$' | '*' | '%'
								));
							if !character_allowed {
								return_err!(LexingErrors::InvalidCharacterInAttributeKey(chr));
							}
						}
					},
					JSXLexingState::AttributeEqual => {
						let delimiter = match chr {
							'{' if options.allow_expressions_in_jsx => {
								push_token!(TSXToken::JSXExpressionStart);
								*interpolation_depth += 1;
								*jsx_state = JSXLexingState::AttributeKey;
								state_stack.push(state);
								set_state!(LexingState::None, EXPECT_EXPRESSION: true);
								continue;
							}
							'"' => JSXAttributeValueDelimiter::DoubleQuote,
							'\'' => JSXAttributeValueDelimiter::SingleQuote,
							'>' => {
								return_err!(LexingErrors::EmptyAttributeName);
							}
							_ => JSXAttributeValueDelimiter::None,
						};
						*jsx_state = JSXLexingState::AttributeValue(delimiter);
					}
					JSXLexingState::AttributeValue(delimiter) => match (delimiter, chr) {
						(JSXAttributeValueDelimiter::DoubleQuote, '"')
						| (JSXAttributeValueDelimiter::SingleQuote, '\'') => {
							push_token!(TSXToken::JSXAttributeValue(
								script[(start + 1)..idx].to_owned()
							));
							*jsx_state = JSXLexingState::AttributeKey;
							start = idx + 1;
							continue;
						}
						(JSXAttributeValueDelimiter::None, ' ') => {
							push_token!(TSXToken::JSXAttributeValue(script[start..idx].to_owned()));
							*jsx_state = JSXLexingState::AttributeKey;
							start = idx;
						}
						(JSXAttributeValueDelimiter::None, '>') => {
							push_token!(TSXToken::JSXAttributeValue(script[start..idx].to_owned()));
							if *is_self_closing_tag {
								*tag_depth = match tag_depth.checked_sub(1) {
									Some(value) => value,
									None => {
										return_err!(LexingErrors::UnbalancedJSXClosingTags);
									}
								};
								push_token!(TSXToken::JSXSelfClosingTag);
								start = idx + 1;
								// If JSX literal range has ended
								if *tag_depth == 0 {
									set_state!(LexingState::None);
								} else {
									*jsx_state = JSXLexingState::Content;
									*is_self_closing_tag = false;
								}
							} else {
								push_token!(TSXToken::JSXOpeningTagEnd);
								start = idx + 1;
								*jsx_state = if *no_inner_tags_or_expressions {
									JSXLexingState::LiteralContent {
										last_char_was_open_chevron: false,
									}
								} else {
									JSXLexingState::Content
								};
							}
							continue;
						}
						_ => {}
					},
					JSXLexingState::Content => {
						match chr {
							'<' => {
								let content_slice = &script[start..idx];
								if !content_slice.trim().is_empty() {
									push_token!(TSXToken::JSXContent(content_slice.to_owned()));
								}
								*jsx_state = JSXLexingState::TagName {
									direction: JSXTagNameDirection::Opening,
									lexed_start: false,
								};
								start = idx;
							}
							'{' if options.allow_expressions_in_jsx => {
								let content_slice = &script[start..idx];
								if !content_slice.trim().is_empty() {
									push_token!(TSXToken::JSXContent(content_slice.to_owned()));
								}
								push_token!(TSXToken::JSXExpressionStart);
								*interpolation_depth += 1;
								state_stack.push(state);
								set_state!(LexingState::None, EXPECT_EXPRESSION: true);
								continue;
							}
							'\n' => {
								let source = script[start..idx].trim();
								if !source.is_empty() {
									push_token!(TSXToken::JSXContent(source.to_owned()));
									start = idx;
								}
								push_token!(TSXToken::JSXContentLineBreak);
								start = idx + 1;
							}
							// Any content
							_ => {}
						}
					}
					JSXLexingState::LiteralContent { ref mut last_char_was_open_chevron } => {
						match chr {
							'<' => {
								*last_char_was_open_chevron = true;
							}
							'/' if *last_char_was_open_chevron => {
								let end = idx - '<'.len_utf8();
								let source = script[start..end].trim();
								if !source.is_empty() {
									push_token!(TSXToken::JSXContent(source.to_owned()));
								}
								start = end;
								push_token!(TSXToken::JSXClosingTagStart);
								start = idx + '/'.len_utf8();
								*jsx_state = JSXLexingState::TagName {
									direction: JSXTagNameDirection::Closing,
									lexed_start: true,
								};
								*no_inner_tags_or_expressions = false;
							}
							_ => {
								*last_char_was_open_chevron = false;
							}
						}
					}
					// TODO this will allow for <!--> as a valid comment
					JSXLexingState::Comment => {
						if idx - start < 4 {
							if chr != '-' {
								return_err!(LexingErrors::ExpectedDashInComment);
							}
						} else if chr == '>' && script[..idx].ends_with("--") {
							push_token!(TSXToken::JSXComment(
								script[(start + 4)..(idx - 2)].to_owned()
							));
							start = idx + 1;
							if *tag_depth == 0 {
								set_state!(if options.top_level_html {
									DEFAULT_JSX_LEXING_STATE
								} else {
									LexingState::None
								});
							} else {
								*jsx_state = JSXLexingState::Content;
							}
							continue;
						}
					}
				}
			}
			LexingState::None => {}
		}

		// This is done later as state may have been set to none by the matching
		if state == LexingState::None {
			match chr {
				'0' if matches!(script.as_bytes().get(idx + 1), Some(b'0'..=b'7')) => {
					// strict mode should be done in the parser stage (as that is where context is)
					set_state!(LexingState::Number(NumberLiteralType::OctalLiteral));
				}
				'0'..='9' => set_state!(LexingState::Number(Default::default())),
				'"' => set_state!(LexingState::String { double_quoted: true, escaped: false }),
				'\'' => set_state!(LexingState::String { double_quoted: false, escaped: false }),
				'_' | '$' => {
					set_state!(LexingState::Identifier);
				}
				chr if chr.is_alphabetic() => {
					set_state!(LexingState::Identifier);
				}
				chr if chr.is_whitespace() => {
					continue;
				}
				chr => {
					// Handles lexing in nested contexts, e.g. JSX and template literals
					match (chr, state_stack.last_mut()) {
						(
							'}',
							Some(LexingState::TemplateLiteral {
								ref mut interpolation_depth, ..
							}),
						) => {
							*interpolation_depth -= 1;
							if *interpolation_depth == 0 {
								push_token!(TSXToken::TemplateLiteralExpressionEnd);
								start = idx + '}'.len_utf8();
								state = state_stack.pop().unwrap();
								continue;
							}
						}
						(
							'}',
							Some(LexingState::JSXLiteral { ref mut interpolation_depth, .. }),
						) => {
							*interpolation_depth -= 1;
							if *interpolation_depth == 0 {
								push_token!(TSXToken::JSXExpressionEnd);
								start = idx + '}'.len_utf8();
								state = state_stack.pop().unwrap();
								continue;
							}
						}
						(
							'{',
							Some(
								LexingState::JSXLiteral { ref mut interpolation_depth, .. }
								| LexingState::TemplateLiteral {
									ref mut interpolation_depth, ..
								},
							),
						) => {
							// Handle for if '{' are in the interpolation
							*interpolation_depth += 1;
						}
						(_, _) => {}
					}

					start = idx;

					// Handle regex, JSX literals and template literals
					match (expect_expression, chr) {
						(_, '`') => {
							push_token!(TSXToken::TemplateLiteralStart);
							start = idx + 1;
							state = LexingState::TemplateLiteral {
								interpolation_depth: 0,
								last_char_was_dollar: false,
								escaped: false,
							};
						}
						(true, '<') if options.lex_jsx => {
							set_state!(FIRST_CHEVRON_JSX_LEXING_STATE);
						}
						(true, '/') => {
							state = LexingState::RegexLiteral {
								escaped: false,
								after_last_slash: false,
								in_set: false,
							};
						}
						(true, '.') => {
							state = LexingState::Number(NumberLiteralType::Decimal {
								fractional: true,
							});
						}
						(_, _) => {
							// Else try do a symbol
							let automaton = TSXToken::new_automaton();
							match automaton.get_next(chr) {
								GetNextResult::Result {
									result,
									ate_character: _, // Should always be true
								} => {
									expect_expression = result.is_expression_prefix();
									push_token!(result);
								}
								GetNextResult::NewState(new_state) => {
									state = LexingState::Symbol(new_state);
								}
								GetNextResult::InvalidCharacter(err) => {
									return_err!(LexingErrors::UnexpectedCharacter(err));
								}
							}
						}
					}
				}
			}
		}
	}

	// If source ends while there is still a parsing state
	match state {
		LexingState::Number(literal_type) => {
			// Just `.` or ends with combination token
			if script[start..].trim_end() == "."
				|| script.ends_with(['x', 'X', 'o', 'O', '_', '-'])
				|| (!matches!(literal_type, NumberLiteralType::HexadecimalLiteral)
					&& script.ends_with(['e', 'E', 'b', 'B']))
			{
				return_err!(LexingErrors::UnexpectedEndToNumberLiteral)
			}
			sender.push(Token(
				TSXToken::NumberLiteral(script[start..].to_owned()),
				TokenStart::new(start as u32 + offset),
			));
		}
		LexingState::Identifier => {
			sender.push(Token(
				TSXToken::from_slice(&script[start..]),
				TokenStart::new(start as u32 + offset),
			));
		}
		LexingState::Symbol(symbol_state) => {
			// Uses 0 as char to prevent continued matches, this is okay as long as
			// there is no 0 char in the finite automata
			match symbol_state.get_next(0 as char) {
				GetNextResult::Result {
					result,
					ate_character: _, // Should always be true
				} => {
					sender.push(Token(result, TokenStart::new(start as u32 + offset)));
				}
				GetNextResult::NewState(_new_state) => unreachable!(),
				GetNextResult::InvalidCharacter(err) => {
					return_err!(LexingErrors::UnexpectedCharacter(err));
				}
			}
		}
		LexingState::SingleLineComment => {
			let content = &script[(start + 2)..];
			if options.comments.should_add_comment(content) {
				sender.push(Token(
					TSXToken::Comment(content.trim_end().to_owned()),
					TokenStart::new(start as u32 + offset),
				));
			}
		}
		LexingState::MultiLineComment { .. } => {
			return_err!(LexingErrors::ExpectedEndToMultilineComment);
		}
		LexingState::String { .. } => {
			return_err!(LexingErrors::ExpectedEndToStringLiteral);
		}
		// This is okay as the state is not cleared until it finds flags.
		LexingState::RegexLiteral { after_last_slash, .. } => {
			if after_last_slash {
				sender.push(Token(
					TSXToken::RegexFlagLiteral(script[start..].to_owned()),
					TokenStart::new(start as u32 + offset),
				));
				sender.push(Token(TSXToken::EOS, TokenStart::new(script.len() as u32)));
			} else {
				sender.push(Token(TSXToken::EOS, TokenStart::new(script.len() as u32)));
				return_err!(LexingErrors::ExpectedEndToRegexLiteral);
			}
		}
		LexingState::JSXLiteral { state, .. } => {
			if !matches!(state, JSXLexingState::ExpectingOpenChevron) {
				return_err!(LexingErrors::ExpectedEndToJSXLiteral);
			}
		}
		LexingState::TemplateLiteral { .. } => {
			return_err!(LexingErrors::ExpectedEndToTemplateLiteral);
		}
		LexingState::None => {}
	}

	sender.push(Token(TSXToken::EOS, TokenStart::new(script.len() as u32)));

	Ok(())
}
