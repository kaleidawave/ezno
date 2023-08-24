/// Contains lexing and parser errors
use std::fmt::{self, Display};

use crate::TSXToken;
use source_map::Span;
use tokenizer_lib::Token;

#[allow(missing_docs)]
pub enum ParseErrors<'a> {
	UnexpectedToken { expected: &'a [TSXToken], found: TSXToken },
	UnexpectedSymbol(derive_finite_automaton::InvalidCharacter),
	ClosingTagDoesNotMatch { expected: &'a str, found: &'a str },
	ExpectedStringLiteral { found: TSXToken },
	TypeArgumentsNotValidOnReference,
	UnmatchedBrackets,
	FunctionParameterOptionalAndDefaultValue,
	ExpectedIdent { found: TSXToken, at_location: &'a str },
	ParameterCannotHaveDefaultValueHere,
	InvalidLHSAssignment,
	LexingFailed,
	ExpectedCatchOrFinally,
}

#[allow(missing_docs)]
pub enum LexingErrors {
	SecondDecimalPoint,
	NumberLiteralCannotHaveDecimalPoint,
	NumberLiteralBaseSpecifierMustBeSecondCharacter,
	NumberLiteralBaseSpecifierMustPrecededWithZero,
	InvalidCharacterInJSXTag(char),
	UnbalancedJSXClosingTags,
	ExpectedClosingAngleAtEndOfSelfClosingTag,
	InvalidCharacterInAttributeKey(char),
	UnexpectedCharacter(derive_finite_automaton::InvalidCharacter),
	EmptyAttributeName,
	ExpectedJSXEndTag,
	NewLineInStringLiteral,
	ExpectedEndToMultilineComment,
	ExpectedEndToStringLiteral,
	ExpectedEndToRegexLiteral,
	ExpectedEndToJSXLiteral,
	ExpectedEndToTemplateLiteral,
	TwoExponents,
	TwoUnderscores,
	TrailingUnderscore,
}

impl Display for LexingErrors {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			LexingErrors::SecondDecimalPoint => {
				f.write_str("Second decimal point found in number literal")
			}
			LexingErrors::NumberLiteralCannotHaveDecimalPoint => {
				f.write_str("Number literal with specified base cannot have decimal point")
			}
			LexingErrors::NumberLiteralBaseSpecifierMustBeSecondCharacter => {
				f.write_str("Number literal base character must be second character in literal")
			}
			LexingErrors::NumberLiteralBaseSpecifierMustPrecededWithZero => {
				f.write_str("Number literal base character must be proceeded with a zero")
			}
			LexingErrors::InvalidCharacterInJSXTag(chr) => {
				write!(f, "Invalid character {chr:?} in JSX tag")
			}
			LexingErrors::ExpectedClosingAngleAtEndOfSelfClosingTag => {
				f.write_str("Expected closing angle at end of self closing JSX tag")
			}
			LexingErrors::InvalidCharacterInAttributeKey(chr) => {
				write!(f, "Invalid character {:?} in JSX attribute name", chr)
			}
			LexingErrors::EmptyAttributeName => f.write_str("Empty JSX attribute name"),
			LexingErrors::ExpectedJSXEndTag => f.write_str("Expected JSX end tag"),
			LexingErrors::NewLineInStringLiteral => {
				f.write_str("String literals cannot contain new lines")
			}
			LexingErrors::ExpectedEndToMultilineComment => {
				f.write_str("Unclosed multiline comment")
			}
			LexingErrors::ExpectedEndToStringLiteral => f.write_str("Unclosed string literal"),
			LexingErrors::ExpectedEndToRegexLiteral => f.write_str("Unclosed regex literal"),
			LexingErrors::ExpectedEndToJSXLiteral => f.write_str("Unclosed JSX literal"),
			LexingErrors::ExpectedEndToTemplateLiteral => f.write_str("Unclosed template literal"),
			LexingErrors::UnexpectedCharacter(err) => Display::fmt(err, f),
			LexingErrors::UnbalancedJSXClosingTags => f.write_str("Too many closing JSX tags"),
			LexingErrors::TwoExponents => f.write_str("Two e in number literal"),
			LexingErrors::TwoUnderscores => {
				f.write_str("Only one underscore is allowed as numeric separator")
			}
			LexingErrors::TrailingUnderscore => {
				f.write_str("Number literal cannot end with numeric separator")
			}
		}
	}
}

impl<'a> Display for ParseErrors<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ParseErrors::UnexpectedToken { expected, found } => {
				f.write_str("Expected ")?;
				match expected {
					[] => unreachable!("no expected tokens given"),
					[a] => f.write_fmt(format_args!("{a:?}")),
					[a, b] => f.write_fmt(format_args!("{a:?} or {b:?}")),
					[head @ .., end] => f.write_fmt(format_args!(
						"{} or {:?}",
						head.iter()
							.map(|chr| format!("{chr:?}"))
							.reduce(|mut a, b| {
								a.push_str(", ");
								a.push_str(&b);
								a
							})
							.unwrap(),
						end
					)),
				}?;
				write!(f, " found {found:?}")
			}
			ParseErrors::UnexpectedSymbol(invalid_character) => Display::fmt(invalid_character, f),
			ParseErrors::ClosingTagDoesNotMatch { expected, found } => {
				write!(f, "Expected </{expected}> found </{found}>")
			}
			ParseErrors::ExpectedStringLiteral { found } => {
				write!(f, "Expected string literal, found {found:?}")
			}
			ParseErrors::TypeArgumentsNotValidOnReference => {
				write!(f, "Type arguments not valid on reference",)
			}
			ParseErrors::UnmatchedBrackets => f.write_str("Unmatched brackets"),
			ParseErrors::FunctionParameterOptionalAndDefaultValue => {
				f.write_str("Function parameter cannot be optional *and* have default expression")
			}
			ParseErrors::ExpectedIdent { found, at_location } => {
				write!(f, "Expected identifier at {at_location}, found {found:?}")
			}
			ParseErrors::ParameterCannotHaveDefaultValueHere => {
				f.write_str("Function parameter cannot be have default value here")
			}
			ParseErrors::InvalidLHSAssignment => f.write_str("Invalid syntax on LHS of assignment"),
			ParseErrors::ExpectedCatchOrFinally => {
				f.write_str("Expected 'catch' or 'finally' to follow 'try'")
			}
			ParseErrors::LexingFailed => {
				// unreachable!("This should never be written"),
				f.write_str("Lexing issue")
			}
		}
	}
}

// For TokenReader::expect_next
impl From<Option<(TSXToken, Token<TSXToken, Span>)>> for ParseError {
	fn from(opt: Option<(TSXToken, Token<TSXToken, Span>)>) -> Self {
		if let Some((expected_type, Token(token, invalid_token_position))) = opt {
			Self::new(
				ParseErrors::UnexpectedToken { expected: &[expected_type], found: token },
				invalid_token_position,
			)
		} else {
			parse_lexing_error()
		}
	}
}

// For TokenReader::next which only
pub(crate) fn parse_lexing_error() -> ParseError {
	ParseError::new(ParseErrors::LexingFailed, Span::NULL_SPAN)
}

pub trait ParserErrorReason: Display {}

impl<'a> ParserErrorReason for ParseErrors<'a> {}
impl ParserErrorReason for LexingErrors {}

/// A error for not parsing
#[derive(Debug)]
pub struct ParseError {
	pub reason: String,
	pub position: Span,
}

impl ParseError {
	pub fn new(reason: impl ParserErrorReason, position: Span) -> Self {
		Self { reason: reason.to_string(), position }
	}
}

pub type ParseResult<T> = Result<T, ParseError>;
