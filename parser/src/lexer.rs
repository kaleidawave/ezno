//! Contains lexing logic for all the whole of JS + TypeScript type annotations + JSX + other syntax
//!
//! Uses [TSXToken]s for data, uses [Span] for location data. Uses [tokenizer_lib] for logic.

use super::{ParseError, SourceId, Span, TSXToken};
use crate::{
	cursor::EmptyCursorId, errors::LexingErrors, html_tag_contains_literal_content,
	html_tag_is_self_closing,
};
use tokenizer_lib::{Token, TokenSender};

use derive_finite_automaton::{
	FiniteAutomata, FiniteAutomataConstructor, GetAutomataStateForValue, GetNextResult,
};

pub struct LexSettings {
	/// Whether to append tokens when lexing. If false will just ignore
	pub include_comments: bool,
	/// Whether to parse JSX. TypeScripts `<number> 2` breaks the lexer so this can be disabled to allow
	/// for that syntax
	pub lex_jsx: bool,
	/// TODO temp
	pub allow_unsupported_characters_in_jsx_attribute_keys: bool,
}

impl Default for LexSettings {
	fn default() -> Self {
		Self {
			include_comments: true,
			lex_jsx: true,
			allow_unsupported_characters_in_jsx_attribute_keys: true,
		}
	}
}

/// *Tokenizes* script appending Tokens to `sender` using [TokenSender::push]
/// `offset` represents the start of the source if script is contained in some larger buffer
///
/// Returns () if successful, if runs into lexing error will short-circuit and return a [ParseError]
///
/// **CURSORS HAVE TO BE IN FORWARD ORDER**
#[doc(hidden)]
pub fn lex_source(
	script: &str,
	sender: &mut impl TokenSender<TSXToken, Span>,
	settings: &LexSettings,
	source: Option<SourceId>,
	offset: Option<usize>,
	mut cursors: Vec<(usize, EmptyCursorId)>,
) -> Result<(), ParseError> {
	let source = source.unwrap_or(SourceId::NULL);

	cursors.reverse();

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
		TagName {
			direction: JSXTagNameDirection,
			lexed_start: bool,
		},
		/// For lexing the close chevron after the slash in self closing tags
		SelfClosingTagClose,
		AttributeKey,
		AttributeEqual,
		AttributeValue(JSXAttributeValueDelimiter),
		Comment(JSXCommentState),
		Content,
		/// For script and style tags
		LiteralContent {
			last_char_was_open_chevron: bool,
		},
	}

	#[derive(PartialEq, Debug)]
	enum JSXCommentState {
		None,
		FirstDash,
		SecondDash,
	}

	#[derive(PartialEq, Debug)]
	enum NumberLiteralType {
		BinaryLiteral,
		OctalLiteral,
		HexadecimalLiteral,
		/// Base 10
		Decimal {
			/// has decimal point
			fractional: bool,
		},
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
		Number {
			literal_type: NumberLiteralType,
			/// For binary, hex, etc `0b0121`
			last_character_zero: bool,
			/// Past and `e` or `E`
			past_exponential: bool,
			last_was_underscore: bool,
		},
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
		Comment,
		MultiLineComment {
			last_char_was_star: bool,
		},
		RegexLiteral {
			escaped: bool,
			/// aka on flags
			after_last_slash: bool,
		},
	}

	let mut state: LexingState = LexingState::None;

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
	let mut expect_expression = true;

	/// Returns a span at the current end position. Used for throwing errors
	/// TODO not sure about this, maybe shouldn't return span
	macro_rules! current_position {
		() => {
			Span { start: start as u32 + offset as u32, end: start as u32 + offset as u32, source }
		};
	}

	macro_rules! return_err {
		($err:expr) => {{
			sender.push(Token(TSXToken::EOS, current_position!()));
			return Err(ParseError::new($err, current_position!()));
		}};
	}

	for (idx, chr) in script.char_indices() {
		// TODO somewhat temp
		if matches!(cursors.last(), Some((x, _)) if *x < idx) {
			cursors.pop();
		}

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
		// TODO these need to be tidied up
		macro_rules! push_token {
			($t:expr $(,)?) => {{
				let res = sender.push(Token(
					$t,
					Span {
						start: (start + offset) as u32,
						end: (idx + offset + chr.len_utf8()) as u32,
						source,
					},
				));
				if !res {
					return Ok(());
				}
			}};

			(EXCLUDING_LAST_CHAR, $t:expr $(,)?) => {{
				let res = sender.push(Token(
					$t,
					Span { start: (start + offset) as u32, end: (idx + offset) as u32, source },
				));
				if !res {
					return Ok(());
				}
			}};

			(EXCLUDING_SLICE $slice:expr, $t:expr $(,)?) => {{
				let res = sender.push(Token(
					$t,
					Span {
						start: (start + offset) as u32,
						end: (idx + offset - $slice.len_utf8()) as u32,
						source,
					},
				));
				if !res {
					return Ok(());
				}
			}};

			(AFTER_LAST_CHAR, $t:expr $(,)?) => {{
				let res = sender.push(Token(
					$t,
					Span {
						start: (idx + offset) as u32,
						end: (idx + offset + chr.len_utf8()) as u32,
						source,
					},
				));
				if !res {
					return Ok(());
				}
			}};
		}

		match state {
			LexingState::Number {
				ref mut literal_type,
				ref mut last_character_zero,
				ref mut past_exponential,
				ref mut last_was_underscore,
			} => {
				match chr {
					'0' => {
						*last_character_zero = true;
						*last_was_underscore = false;
					}
					'1'..='9' => {
						*last_character_zero = false;
						*last_was_underscore = false;
					}
					'.' => {
						if let NumberLiteralType::Decimal { fractional } = literal_type {
							if *fractional {
								return_err!(LexingErrors::SecondDecimalPoint);
							}
							*fractional = true;
							*last_character_zero = false;
						} else {
							return_err!(LexingErrors::NumberLiteralCannotHaveDecimalPoint);
						}
					}
					'a'..='f' | 'A'..='F'
						if matches!(literal_type, NumberLiteralType::HexadecimalLiteral) =>
					{
						*last_character_zero = false;
					}
					// For binary/hexadecimal/octal literals
					'b' | 'x' | 'o' | 'B' | 'X' | 'O' => {
						if start + 1 != idx {
							return_err!(
								LexingErrors::NumberLiteralBaseSpecifierMustBeSecondCharacter
							);
						} else if !*last_character_zero {
							return_err!(
								LexingErrors::NumberLiteralBaseSpecifierMustPrecededWithZero
							);
						} else {
							*literal_type = match chr.to_ascii_lowercase() {
								'b' => NumberLiteralType::BinaryLiteral,
								'0' => NumberLiteralType::OctalLiteral,
								'x' => NumberLiteralType::HexadecimalLiteral,
								_ => unreachable!(),
							}
						}
					}
					'e' | 'E' => {
						if *past_exponential {
							todo!()
						}
						*past_exponential = true;
					}
					'_' => {
						if *last_was_underscore {
							todo!()
						}
						*last_was_underscore = true;
					}
					_ => {
						push_token!(
							EXCLUDING_LAST_CHAR,
							TSXToken::NumberLiteral(script[start..idx].to_owned())
						);
						set_state!(LexingState::None);
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
								state = LexingState::Comment;
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
						} else {
							push_token!(EXCLUDING_LAST_CHAR, result);
							start = idx;
						}
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
					push_token!(EXCLUDING_LAST_CHAR, token);
					set_state!(LexingState::None, EXPECT_EXPRESSION: is_expression_prefix);
				}
			},
			LexingState::String { ref mut double_quoted, ref mut escaped } => match chr {
				'\n' => {
					return_err!(LexingErrors::NewLineInStringLiteral);
				}
				'\'' if !*double_quoted && !*escaped => {
					push_token!(TSXToken::SingleQuotedStringLiteral(
						script[(start + 1)..idx].to_owned()
					));
					state = LexingState::None;
					start = idx + 1;
					expect_expression = false;
					continue;
				}
				'"' if *double_quoted && !*escaped => {
					push_token!(TSXToken::DoubleQuotedStringLiteral(
						script[(start + 1)..idx].to_owned()
					));
					state = LexingState::None;
					start = idx + 1;
					expect_expression = false;
					continue;
				}
				'\\' => {
					*escaped = true;
				}
				_ => {
					*escaped = false;
				}
			},
			LexingState::Comment => {
				if let '\n' = chr {
					if settings.include_comments {
						push_token!(
							EXCLUDING_LAST_CHAR,
							TSXToken::Comment(script[(start + 2)..idx].trim_end().to_owned()),
						);
					}
					set_state!(LexingState::None);
					continue;
				}
			}
			LexingState::MultiLineComment { ref mut last_char_was_star } => match chr {
				'/' if *last_char_was_star => {
					if settings.include_comments {
						push_token!(TSXToken::MultiLineComment(
							script[(start + 2)..(idx - 1)].to_owned()
						));
					}
					set_state!(LexingState::None);
					continue;
				}
				chr => {
					*last_char_was_star = chr == '*';
				}
			},
			LexingState::RegexLiteral { ref mut escaped, ref mut after_last_slash } => {
				if *after_last_slash {
					if !matches!(chr, 'd' | 'g' | 'i' | 'm' | 's' | 'u' | 'y') {
						if start != idx {
							push_token!(TSXToken::RegexFlagLiteral(script[start..idx].to_owned()));
						}
						set_state!(LexingState::None);
					}
				} else {
					match chr {
						'/' if start + 1 == idx => {
							state = LexingState::Comment;
							continue;
						}
						'*' if start + 1 == idx => {
							state = LexingState::MultiLineComment { last_char_was_star: false };
							continue;
						}
						'/' if !*escaped => {
							push_token!(TSXToken::RegexLiteral(
								script[(start + 1)..idx].to_owned()
							));
							*after_last_slash = true;
							start = idx + 1;
						}
						chr => {
							*escaped = chr == '\\';
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
						// TODO using push token
						sender.push(Token(
							TSXToken::TemplateLiteralChunk(script[start..(idx - 1)].to_owned()),
							Span {
								start: (start + offset) as u32,
								end: (idx + offset) as u32,
								source,
							},
						));
					}
					push_token!(AFTER_LAST_CHAR, TSXToken::TemplateLiteralExpressionStart);
					*interpolation_depth += 1;
					state_stack.push(state);

					start = idx + 1;
					state = LexingState::None;
					continue;
				}
				'`' if !*escaped => {
					if idx > start + 1 {
						push_token!(
							EXCLUDING_LAST_CHAR,
							TSXToken::TemplateLiteralChunk(script[start..idx].to_owned())
						);
					}
					push_token!(TSXToken::TemplateLiteralEnd);
					start = idx + 1;
					state = LexingState::None;
					continue;
				}
				'\\' => {
					*escaped = true;
				}
				_ => {
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
							} else {
								start = idx + 1;
								*jsx_state = JSXLexingState::Content;
							}
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
										push_token!(
											EXCLUDING_LAST_CHAR,
											TSXToken::JSXOpeningTagStart
										);
										start += 1;
									}
									JSXTagNameDirection::Closing => {
										push_token!(
											EXCLUDING_LAST_CHAR,
											TSXToken::JSXClosingTagStart
										);
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
						// Comment
						'!' if start + 1 == idx => {
							*jsx_state = JSXLexingState::Comment(JSXCommentState::None);
						}
						// Non tag name character
						chr => {
							if *direction == JSXTagNameDirection::Closing {
								return_err!(LexingErrors::ExpectedJSXEndTag);
							}
							let tag_name = script[start..idx].trim();
							*is_self_closing_tag = html_tag_is_self_closing(&tag_name);
							push_token!(
								EXCLUDING_LAST_CHAR,
								TSXToken::JSXTagName(tag_name.to_owned())
							);
							*tag_depth += 1;
							match chr {
								'>' => {
									if *is_self_closing_tag {
										*tag_depth -= 1;
										push_token!(AFTER_LAST_CHAR, TSXToken::JSXSelfClosingTag);
										start = idx + 1;
										// If JSX literal range has ended
										if *tag_depth == 0 {
											set_state!(LexingState::None);
										} else {
											*jsx_state = JSXLexingState::Content;
											*is_self_closing_tag = false;
										}
									} else {
										*no_inner_tags_or_expressions =
											html_tag_contains_literal_content(&script[start..idx]);
										push_token!(AFTER_LAST_CHAR, TSXToken::JSXOpeningTagEnd);
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
							push_token!(AFTER_LAST_CHAR, TSXToken::JSXSelfClosingTag);
							start = idx + 1;
							// If JSX literal range has ended
							if *tag_depth == 0 {
								set_state!(LexingState::None);
							} else {
								*jsx_state = JSXLexingState::Content;
							}
							continue;
						} else {
							return_err!(LexingErrors::ExpectedClosingAngleAtEndOfSelfClosingTag);
						}
					}
					JSXLexingState::AttributeKey => match chr {
						'=' => {
							if start >= idx {
								return_err!(LexingErrors::EmptyAttributeName);
							}
							let key_slice = script[start..idx].trim();
							if !key_slice.is_empty() {
								push_token!(
									EXCLUDING_LAST_CHAR,
									TSXToken::JSXAttributeKey(key_slice.to_owned())
								);
							}
							push_token!(AFTER_LAST_CHAR, TSXToken::JSXAttributeAssign);
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
								push_token!(
									EXCLUDING_LAST_CHAR,
									TSXToken::JSXAttributeKey(script[start..idx].to_owned())
								);
							}
							if *is_self_closing_tag {
								*tag_depth = match tag_depth.checked_sub(1) {
									Some(value) => value,
									None => {
										return_err!(LexingErrors::UnbalancedJSXClosingTags);
									}
								};
								push_token!(AFTER_LAST_CHAR, TSXToken::JSXSelfClosingTag);
								start = idx + 1;
								// If JSX literal range has ended
								if *tag_depth == 0 {
									set_state!(LexingState::None);
								} else {
									*jsx_state = JSXLexingState::Content;
									*is_self_closing_tag = false;
								}
							} else {
								push_token!(AFTER_LAST_CHAR, TSXToken::JSXOpeningTagEnd);
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
								push_token!(
									EXCLUDING_LAST_CHAR,
									TSXToken::JSXAttributeKey(script[start..idx].to_owned())
								);
							}
							start = idx + chr.len_utf8();
						}
						chr => {
							let character_allowed = chr.is_alphanumeric()
								|| chr == '-' || (settings
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
							'{' => {
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
							push_token!(
								EXCLUDING_LAST_CHAR,
								TSXToken::JSXAttributeValue(script[start..idx].to_owned())
							);
							*jsx_state = JSXLexingState::AttributeKey;
							start = idx;
						}
						(JSXAttributeValueDelimiter::None, '>') => {
							push_token!(
								EXCLUDING_LAST_CHAR,
								TSXToken::JSXAttributeValue(script[start..idx].to_owned())
							);
							if *is_self_closing_tag {
								*tag_depth = match tag_depth.checked_sub(1) {
									Some(value) => value,
									None => {
										return_err!(LexingErrors::UnbalancedJSXClosingTags);
									}
								};
								push_token!(AFTER_LAST_CHAR, TSXToken::JSXSelfClosingTag);
								start = idx + 1;
								// If JSX literal range has ended
								if *tag_depth == 0 {
									set_state!(LexingState::None);
								} else {
									*jsx_state = JSXLexingState::Content;
									*is_self_closing_tag = false;
								}
							} else {
								push_token!(AFTER_LAST_CHAR, TSXToken::JSXOpeningTagEnd);
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
									push_token!(
										EXCLUDING_LAST_CHAR,
										TSXToken::JSXContent(content_slice.to_owned())
									);
								}
								*jsx_state = JSXLexingState::TagName {
									direction: JSXTagNameDirection::Opening,
									lexed_start: false,
								};
								start = idx;
							}
							'{' => {
								let content_slice = &script[start..idx];
								if !content_slice.trim().is_empty() {
									push_token!(
										EXCLUDING_LAST_CHAR,
										TSXToken::JSXContent(content_slice.to_owned())
									);
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
									push_token!(
										EXCLUDING_LAST_CHAR,
										TSXToken::JSXContent(source.to_owned())
									);
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
									push_token!(
										EXCLUDING_SLICE '<',
										TSXToken::JSXContent(source.to_owned())
									);
								}
								start = end;
								push_token!(EXCLUDING_LAST_CHAR, TSXToken::JSXClosingTagStart);
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
					JSXLexingState::Comment(ref mut comment_state) => match (&comment_state, chr) {
						(JSXCommentState::None, '-') => {
							*comment_state = JSXCommentState::FirstDash;
						}
						(JSXCommentState::FirstDash, '-') => {
							*comment_state = JSXCommentState::SecondDash;
						}
						(JSXCommentState::SecondDash, '>') => {
							push_token!(TSXToken::JSXComment(
								script[(start + 4)..(idx - 2)].to_owned()
							));
							if *tag_depth == 0 {
								set_state!(LexingState::None);
								continue;
							}
						}
						_ => {
							*comment_state = JSXCommentState::None;
						}
					},
				}
			}
			LexingState::None => {}
		}

		// This is done later as state may have been set to none by the matching
		if state == LexingState::None {
			if matches!(cursors.last(), Some((cursor_idx, _)) if *cursor_idx == idx) {
				sender.push(Token(TSXToken::Cursor(cursors.pop().unwrap().1), current_position!()));
			}
			match chr {
				'0' => set_state!(LexingState::Number {
					literal_type: Default::default(),
					last_character_zero: true,
					last_was_underscore: false,
					past_exponential: false
				}),
				'1'..='9' => set_state!(LexingState::Number {
					literal_type: Default::default(),
					last_character_zero: false,
					last_was_underscore: false,
					past_exponential: false
				}),
				'"' => set_state!(LexingState::String { double_quoted: true, escaped: false }),
				'\'' => set_state!(LexingState::String { double_quoted: false, escaped: false }),
				'_' | '$' => {
					set_state!(LexingState::Identifier)
				}
				chr if chr.is_alphabetic() => {
					set_state!(LexingState::Identifier)
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
								start += 1;
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
							continue;
						}
						(true, '<') if settings.lex_jsx => {
							set_state!(LexingState::JSXLiteral {
								interpolation_depth: 0,
								tag_depth: 0,
								state: JSXLexingState::TagName {
									direction: JSXTagNameDirection::Opening,
									lexed_start: false,
								},
								no_inner_tags_or_expressions: false,
								is_self_closing_tag: false,
							});
							continue;
						}
						(true, '/') => {
							state = LexingState::RegexLiteral {
								escaped: false,
								after_last_slash: false,
							};
							continue;
						}
						(_, _) => {}
					}

					// Else try do a symbol
					// TODO catch `.4` number literals. needs update to automaton
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

	let end_of_source = (script.len() + offset) as u32;

	// If source ends while there is still a parsing state
	match state {
		LexingState::Number { .. } => {
			sender.push(Token(
				TSXToken::NumberLiteral(script[start..].to_owned()),
				Span { start: (start + offset) as u32, end: end_of_source, source },
			));
		}
		LexingState::Identifier => {
			sender.push(Token(
				TSXToken::from_slice(&script[start..]),
				Span { start: (start + offset) as u32, end: end_of_source, source },
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
					sender.push(Token(
						result,
						Span { start: (start + offset) as u32, end: end_of_source, source },
					));
				}
				GetNextResult::NewState(_new_state) => unreachable!(),
				GetNextResult::InvalidCharacter(err) => {
					return_err!(LexingErrors::UnexpectedCharacter(err));
				}
			}
		}
		LexingState::String { .. } => {
			return_err!(LexingErrors::ExpectedEndToStringLiteral);
		}
		LexingState::Comment => {
			sender.push(Token(
				TSXToken::Comment(script[(start + 2)..].trim_end().to_owned()),
				Span { start: (start + offset) as u32, end: end_of_source, source },
			));
		}
		LexingState::MultiLineComment { .. } => {
			sender.push(Token(TSXToken::EOS, current_position!()));
			return Err(ParseError::new(
				LexingErrors::ExpectedEndToMultilineComment,
				current_position!(),
			));
		}
		LexingState::RegexLiteral { .. } => {
			return_err!(LexingErrors::ExpectedEndToRegexLiteral);
		}
		LexingState::JSXLiteral { .. } => {
			return_err!(LexingErrors::ExpectedEndToJSXLiteral);
		}
		LexingState::TemplateLiteral { .. } => {
			return_err!(LexingErrors::ExpectedEndToTemplateLiteral);
		}
		LexingState::None => {}
	}

	if matches!(cursors.last(), Some((idx, _)) if *idx == script.len()) {
		sender.push(Token(TSXToken::Cursor(cursors.pop().unwrap().1), current_position!()));
	}

	sender.push(Token(TSXToken::EOS, current_position!()));

	Ok(())
}
