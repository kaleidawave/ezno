use source_map::Span;
/// Contains lexing and parser errors
use std::fmt::{self, Display};

pub trait ParserErrorReason: Display {}

impl ParserErrorReason for ParseErrors<'_> {}

/// A error for not parsing
#[derive(Debug)]
pub struct ParseError {
	pub reason: String,
	pub position: Span,
}

impl ParseError {
	#[allow(clippy::needless_pass_by_value)]
	pub fn new(reason: impl ParserErrorReason, position: Span) -> Self {
		Self { reason: reason.to_string(), position }
	}
}

impl std::error::Error for ParseError {}
impl std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_fmt(format_args!("ParseError: {} @ byte indices {:?}", self.reason, self.position))
	}
}

pub type ParseResult<T> = Result<T, ParseError>;

/// TODO documentation + combine some of these
#[allow(missing_docs)]
pub enum ParseErrors<'a> {
	UnexpectedCharacter {
		expected: &'a [char],
		found: Option<char>,
	},
	ExpectedKeyword {
		expected: &'static str,
		found: &'a str,
	},
	// Keywords and/or operators
	ExpectedOneOfItems {
		expected: &'static [&'static str],
		found: &'a str,
	},
	ExpectedOperator {
		expected: &'static str,
		found: &'a str,
	},
	ClosingTagDoesNotMatch {
		tag_name: &'a str,
		closing_tag_name: &'a str,
	},
	ExpectedStringLiteral {
		found: &'a str,
	},
	TypeArgumentsNotValidOnReference,
	ExpectedEndOfSource {
		found: &'a str,
	},
	InvalidUnicodeCodePointInIdentifier,
	FunctionParameterOptionalAndDefaultValue,
	ParameterCannotHaveDefaultValueHere,
	InvalidLHSAssignment,
	LexingFailed,
	ExpectedCatchOrFinally,
	InvalidDeclareItem(&'static str),
	DestructuringRequiresValue,
	CannotAccessObjectLiteralDirectly,
	TrailingCommaNotAllowedHere,
	InvalidNumberLiteral,
	ReservedIdentifier,
	AwaitRequiresForOf,
	CannotUseLeadingParameterHere,
	ExpectedIdentifier {
		location: &'static str,
	},
	ExpectedNumberLiteral,
	NonStandardSyntaxUsedWithoutEnabled,
	ExpectedRule,
	ExpectedJSXAttribute,
	InvalidRegexFlag,
	ExpectedDeclaration,
	CannotHaveRegularMemberAfterSpread,
	InvalidLHSOfIs,
	NoNewLinesInString,
	InvalidRegularExpression,
	/// For strings, regular expressions, multiline comments.
	/// TODO specify by field
	UnexpectedEnd,
	/// TODO this could be set to collect, rather than breaking (<https://github.com/kaleidawave/ezno/issues/203>)
	TypeAnnotationUsed,
	/// TODO this could be set to collect, rather than breaking (<https://github.com/kaleidawave/ezno/issues/203>)
	TaggedTemplateCannotBeUsedWithOptionalChain,
	ExpectedExpression,
	DuplicateParameterName,
}

impl Display for ParseErrors<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ParseErrors::UnexpectedCharacter { expected, found } => {
				f.write_str("Expected ")?;
				utilities::format_list(f, expected)?;
				if let Some(found) = found {
					write!(f, " found {found}")
				} else {
					write!(f, " found end of source")
				}
			}
			ParseErrors::ExpectedOperator { expected, found } => {
				write!(f, "Expected {expected} found {found}")
			}
			ParseErrors::ExpectedOneOfItems { expected, found } => {
				f.write_str("Expected ")?;
				utilities::format_list(f, expected)?;
				write!(f, " found {found}")
			}
			ParseErrors::ExpectedKeyword { expected, found } => {
				write!(f, "Expected {expected:?}, found {found:?}")
			}
			ParseErrors::NoNewLinesInString => {
				write!(f, "Cannot use new lines in string")
			}
			ParseErrors::ExpectedJSXAttribute => {
				write!(f, "Invalid JSX attribute")
			}
			ParseErrors::InvalidRegularExpression => {
				write!(f, "Invalid regular expression")
			}
			ParseErrors::InvalidUnicodeCodePointInIdentifier => {
				write!(f, "invalid unicode code point in identifier")
			}
			ParseErrors::UnexpectedEnd => {
				write!(f, "Unexpected end")
			}
			ParseErrors::ClosingTagDoesNotMatch { tag_name: expected, closing_tag_name: found } => {
				write!(f, "Closing tag does not match, expected </{expected}> found </{found}>")
			}
			ParseErrors::NonStandardSyntaxUsedWithoutEnabled => {
				write!(f, "Cannot use this syntax without flag enabled")
			}
			ParseErrors::ExpectedStringLiteral { found } => {
				write!(f, "Expected string literal, found {found:?}")
			}
			ParseErrors::TypeArgumentsNotValidOnReference => {
				write!(f, "Type arguments not valid on reference")
			}
			ParseErrors::TaggedTemplateCannotBeUsedWithOptionalChain => {
				write!(f, "Tagged template cannot be used with optional chain")
			}
			ParseErrors::ExpectedEndOfSource { found } => {
				let found = &found[..std::cmp::min(found.len(), 10)];
				write!(f, "Expected end of source, found {found}")
			}
			ParseErrors::FunctionParameterOptionalAndDefaultValue => {
				write!(f, "Function parameter cannot be optional *and* have default expression")
			}
			ParseErrors::ParameterCannotHaveDefaultValueHere => {
				write!(f, "Function parameter cannot be have default value here")
			}
			ParseErrors::InvalidLHSAssignment => write!(f, "Invalid syntax on LHS of assignment"),
			ParseErrors::ExpectedCatchOrFinally => {
				write!(f, "Expected 'catch' or 'finally' to follow 'try'")
			}
			ParseErrors::LexingFailed => {
				// unreachable!("This should never be written"),
				write!(f, "Lexing issue")
			}
			ParseErrors::InvalidDeclareItem(item) => {
				write!(f, "Declare item '{item}' must be in .d.ts file")
			}
			ParseErrors::DestructuringRequiresValue => {
				write!(f, "RHS of destructured declaration requires expression")
			}
			ParseErrors::CannotAccessObjectLiteralDirectly => {
				write!(f, "Cannot get property on object literal directly")
			}
			ParseErrors::TrailingCommaNotAllowedHere => {
				write!(f, "Trailing comma not allowed here")
			}
			ParseErrors::InvalidNumberLiteral => {
				write!(f, "Invalid number literal")
			}
			ParseErrors::ReservedIdentifier => {
				write!(f, "Found reserved identifier")
			}
			ParseErrors::AwaitRequiresForOf => {
				write!(f, "Can only use await on for (.. of ..)")
			}
			ParseErrors::CannotUseLeadingParameterHere => {
				write!(f, "Cannot write this constraint in this kind of function")
			}
			ParseErrors::ExpectedIdentifier { location } => {
				if *location == "variable identifier" {
					write!(f, "Expected variable identifier")
				} else {
					write!(f, "Expected variable identifier at {location}")
				}
			}
			ParseErrors::ExpectedNumberLiteral => {
				write!(f, "Expected number literal")
			}
			ParseErrors::ExpectedRule => {
				write!(f, "'-' must be followed by a readonly rule")
			}
			ParseErrors::InvalidRegexFlag => {
				write!(f, "Regexp flags must be 'd', 'g', 'i', 'm', 's', 'u' or 'y'")
			}
			ParseErrors::ExpectedDeclaration => {
				write!(f, "Expected identifier after variable declaration keyword")
			}
			ParseErrors::CannotHaveRegularMemberAfterSpread => {
				write!(f, "Cannot have regular member after spread")
			}
			ParseErrors::InvalidLHSOfIs => {
				write!(f, "LHS must be variable reference")
			}
			ParseErrors::TypeAnnotationUsed => {
				write!(f, "Cannot use type annotations in non-ts file")
			}
			ParseErrors::ExpectedExpression => {
				write!(f, "Expected start of expression")
			}
			ParseErrors::DuplicateParameterName => {
				write!(f, "Duplicate parameter name")
			}
		}
	}
}

mod utilities {
	pub fn format_list<T: std::fmt::Display>(
		f: &mut std::fmt::Formatter<'_>,
		items: &[T],
	) -> std::fmt::Result {
		match items {
			[] => panic!("no items given"),
			[a] => f.write_fmt(format_args!("{a}")),
			[a, b] => f.write_fmt(format_args!("{a} or {b}")),
			[head @ .., end] => {
				let start = head
					.iter()
					.map(ToString::to_string)
					.reduce(|mut acc, char| {
						acc.push_str(", ");
						acc.push_str(&char);
						acc
					})
					.unwrap();
				f.write_fmt(format_args!("{start} or {end}"))
			}
		}
	}
}
