use source_map::Span;
/// Contains lexing and parser errors
use std::fmt::{self, Display};

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
	ExpectedOneOfKeywords {
		expected: &'static [&'static str],
		found: &'a str,
	},
	ExpectedOperator {
		expected: &'static str,
		found: &'a str,
	},
	ExpectedOneOfOperators {
		expected: &'static [&'static str],
		found: &'a str,
	},
	ClosingTagDoesNotMatch {
		expected: &'a str,
		found: &'a str,
	},
	ExpectedStringLiteral {
		found: &'a str,
	},
	TypeArgumentsNotValidOnReference,
	ExpectedEndOfSource {
		found: &'a str,
	},
	FunctionParameterOptionalAndDefaultValue,
	ExpectedIdent {
		found: &'a str,
		at_location: &'a str,
	},
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
	ExpectRule,
	InvalidRegexFlag,
	ExpectedDeclaration,
	CannotHaveRegularMemberAfterSpread,
	InvalidLHSOfIs,
	/// TODO this could be set to collect, rather than breaking (https://github.com/kaleidawave/ezno/issues/203)
	TypeAnnotationUsed,
}

impl<'a> Display for ParseErrors<'a> {
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
			ParseErrors::ExpectedOneOfKeywords { expected, found }
			| ParseErrors::ExpectedOneOfOperators { expected, found } => {
				f.write_str("Expected ")?;
				utilities::format_list(f, expected)?;
				write!(f, " found {found}")
			}
			ParseErrors::ExpectedKeyword { expected, found } => {
				write!(f, "Expected {expected:?}, found {found:?}")
			}
			// ParseErrors::UnexpectedSymbol(invalid_character) => Display::fmt(invalid_character, f),
			ParseErrors::ClosingTagDoesNotMatch { expected, found } => {
				write!(f, "Closing tag does not match, expected </{expected}> found </{found}>")
			}
			ParseErrors::NonStandardSyntaxUsedWithoutEnabled => {
				write!(f, "Cannot use this syntax without flag enabled")
			}
			ParseErrors::ExpectedStringLiteral { found } => {
				write!(f, "Expected string literal, found {found:?}")
			}
			ParseErrors::TypeArgumentsNotValidOnReference => {
				write!(f, "Type arguments not valid on reference",)
			}
			ParseErrors::ExpectedEndOfSource { found } => {
				let found = &found[..std::cmp::min(found.len(), 10)];
				f.write_str("Expected end of source, found {found}")
			}
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
				write!(f, "Expected variable identifier at {location}")
			}
			ParseErrors::ExpectedNumberLiteral => {
				write!(f, "Expected number literal")
			}
			ParseErrors::ExpectRule => {
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

pub trait ParserErrorReason: Display {}

impl<'a> ParserErrorReason for ParseErrors<'a> {}

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
