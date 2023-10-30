//! Contains the declaration for [TSXToken] which are pieces of syntax. Also
//! - How tokens are made from consecutive characters
//! - Keywords

use derive_finite_automaton::FiniteAutomataConstructor;
use derive_partial_eq_extras::PartialEqExtras;
use enum_variant_type::EnumVariantType;
use enum_variants_strings::EnumVariantsStrings;
use source_map::Span;
use tokenizer_lib::{sized_tokens::TokenStart, Token};

use crate::ParseError;

/// All JS Tokens with extensions including TypeScript, JSX and more
#[derive(Debug, FiniteAutomataConstructor, PartialEqExtras)]
#[automaton_mappings(
    "{" => TSXToken::OpenBrace,
    "}" => TSXToken::CloseBrace,
    "[" => TSXToken::OpenBracket,
    "]" => TSXToken::CloseBracket,
    "(" => TSXToken::OpenParentheses,
    ")" => TSXToken::CloseParentheses,
    ":" => TSXToken::Colon,
    "@" => TSXToken::At,
    "," => TSXToken::Comma,
    ";" => TSXToken::SemiColon,
    "#" => TSXToken::HashTag,
    "+" => TSXToken::Add,
    "+=" => TSXToken::AddAssign,
    "++" => TSXToken::Increment,
    "-" => TSXToken::Subtract,
    "-=" => TSXToken::SubtractAssign,
    "--" => TSXToken::Decrement,
    "*" => TSXToken::Multiply,
    "*=" => TSXToken::MultiplyAssign,
    "**" => TSXToken::Exponent,
    "**=" => TSXToken::ExponentAssign,
    "/" => TSXToken::Divide,
    "//" => TSXToken::Comment(String::new()),
    "/=" => TSXToken::DivideAssign,
    "/*" => TSXToken::MultiLineComment(String::new()),
    "%" => TSXToken::Modulo,
    "%=" => TSXToken::ModuloAssign,
    "=" => TSXToken::Assign,
    "==" => TSXToken::Equal,
    "===" => TSXToken::StrictEqual,
    "=>" => TSXToken::Arrow,
    "!" => TSXToken::LogicalNot,
    "!=" => TSXToken::NotEqual,
    "!==" => TSXToken::StrictNotEqual,
    "&" => TSXToken::BitwiseAnd,
    "&=" => TSXToken::BitwiseAndAssign,
    "&&" => TSXToken::LogicalAnd,
    "&&=" => TSXToken::LogicalAndAssign,
    "|" => TSXToken::BitwiseOr,
    "|=" => TSXToken::BitwiseOrAssign,
    "||" => TSXToken::LogicalOr,
    "||=" => TSXToken::LogicalOrAssign,
    "~" => TSXToken::BitwiseNot,
    "^" => TSXToken::BitwiseXOr,
    "^=" => TSXToken::BitwiseXorAssign,
    "?" => TSXToken::QuestionMark,
    "?:" => TSXToken::OptionalMember,
    "?." => TSXToken::OptionalChain,
    "-?:" => TSXToken::NonOptionalMember,
    "??" => TSXToken::NullishCoalescing,
    "??=" => TSXToken::NullishCoalescingAssign,
    "!" => TSXToken::LogicalNot,
    "!=" => TSXToken::NotEqual,
    "!==" => TSXToken::StrictNotEqual,
    "<" => TSXToken::OpenChevron,
    ">" => TSXToken::CloseChevron,
    "<=" => TSXToken::LessThanEqual,
    ">=" => TSXToken::GreaterThanEqual,
    "<<" => TSXToken::BitwiseShiftLeft,
    "<<=" => TSXToken::BitwiseShiftLeftAssign,
    ">>" => TSXToken::BitwiseShiftRight,
    ">>=" => TSXToken::BitwiseShiftRightAssign,
    ">>>" => TSXToken::BitwiseShiftRightUnsigned,
    ">>>=" => TSXToken::BitwiseShiftRightUnsignedAssign,
    "." => TSXToken::Dot,
    "..." => TSXToken::Spread,
)]
#[cfg_attr(feature = "extras", automaton_mappings(
    ">!" => TSXToken::InvertAssign,
    "/%" => TSXToken::DividesOperator,
    "<@>" => TSXToken::ComposeOperator,
    "|>" => TSXToken::PipeOperator,
))]
#[rustfmt::skip]
pub enum TSXToken {
    IdentLiteral(String),
    Keyword(TSXKeyword),
    NumberLiteral(String), 
    SingleQuotedStringLiteral(String), DoubleQuotedStringLiteral(String),
    MultiLineComment(String), Comment(String),
    RegexLiteral(String), RegexFlagLiteral(String),
    TemplateLiteralStart, TemplateLiteralChunk(String), TemplateLiteralEnd,
    TemplateLiteralExpressionStart, TemplateLiteralExpressionEnd,
    Comma, SemiColon, Colon, Dot, 
    /// @
    At,
    Spread, Assign, 
    /// `=>`
    Arrow,
    /// `(` 
    OpenParentheses, 
    /// `)` 
    CloseParentheses, 
    /// `{` 
    OpenBrace, 
    /// `}` 
    CloseBrace, 
    /// `[` 
    OpenBracket, 
    /// `]` 
    CloseBracket, 
    /// `<` 
    OpenChevron, 
    /// `>` 
    CloseChevron,
    Add, Subtract, Multiply, Divide,
    QuestionMark, Exponent, Modulo,
    AddAssign, SubtractAssign, MultiplyAssign, DivideAssign, ExponentAssign, ModuloAssign,
    Increment, Decrement,
    BitwiseShiftLeft, BitwiseShiftRight, BitwiseShiftRightUnsigned,
    BitwiseShiftLeftAssign, BitwiseShiftRightAssign, BitwiseShiftRightUnsignedAssign,
    BitwiseOr, BitwiseXOr, BitwiseAnd, BitwiseNot,
    BitwiseOrAssign, BitwiseAndAssign, BitwiseXorAssign,
    LogicalOr, LogicalAnd, LogicalNot,
    LogicalOrAssign, LogicalAndAssign,
    Equal, NotEqual, StrictEqual, StrictNotEqual,
    GreaterThanEqual, LessThanEqual,
    OptionalChain, NullishCoalescing, NullishCoalescingAssign,
    /// `?:` 
    OptionalMember, 
    /// '!:` 
    NonOptionalMember, 
    /// For scripts thing
    HashTag,
    // JSX Tokens. Some like JSXComment are non standard
    JSXOpeningTagStart, JSXTagName(String), JSXOpeningTagEnd, 
    JSXClosingTagStart, 
    /// This also covers the end of a token, thus no TSXToken::JSXClosingTagEnd
    JSXClosingTagName(String), 
    /// />
    JSXSelfClosingTag, 
    JSXAttributeKey(String), JSXAttributeAssign, JSXAttributeValue(String),
    JSXContent(String), JSXContentLineBreak,
    /// The start and end of expressions either as a node or a attribute
    JSXExpressionStart, JSXExpressionEnd,
    // <> and </>
    JSXFragmentStart, JSXFragmentEnd,
    /// <!-- -->
    JSXComment(String),

    // Non standard
    #[cfg(feature = "extras")]
    DividesOperator,
    #[cfg(feature = "extras")]
    InvertAssign,
    #[cfg(feature = "extras")]
    ComposeOperator,
    #[cfg(feature = "extras")]
    PipeOperator,

    /// Special cursor marker
    Cursor(crate::EmptyCursorId),

    EOS
}

impl tokenizer_lib::TokenTrait for TSXToken {
	fn is_skippable(&self) -> bool {
		matches!(self, TSXToken::Cursor(_))
	}
}

impl tokenizer_lib::sized_tokens::SizedToken for TSXToken {
	fn length(&self) -> u32 {
		match self {
			TSXToken::Keyword(kw) => kw.to_str().len() as u32,

			TSXToken::JSXClosingTagName(lit)
			| TSXToken::TemplateLiteralChunk(lit)
			| TSXToken::JSXAttributeKey(lit)
			| TSXToken::JSXAttributeValue(lit)
			| TSXToken::JSXContent(lit)
			| TSXToken::JSXComment(lit)
			| TSXToken::JSXTagName(lit)
			| TSXToken::IdentLiteral(lit)
			| TSXToken::NumberLiteral(lit)
			| TSXToken::RegexFlagLiteral(lit) => lit.len() as u32,

			TSXToken::MultiLineComment(comment) => comment.len() as u32 + 4,
			TSXToken::SingleQuotedStringLiteral(comment)
			| TSXToken::DoubleQuotedStringLiteral(comment)
			| TSXToken::Comment(comment) => comment.len() as u32 + 2,
			TSXToken::RegexLiteral(regex) => regex.len() as u32 + 2,

			TSXToken::Comma
			| TSXToken::SemiColon
			| TSXToken::Colon
			| TSXToken::At
			| TSXToken::Assign
			| TSXToken::OpenParentheses
			| TSXToken::CloseParentheses
			| TSXToken::OpenBrace
			| TSXToken::CloseBrace
			| TSXToken::OpenBracket
			| TSXToken::CloseBracket
			| TSXToken::OpenChevron
			| TSXToken::CloseChevron
			| TSXToken::Add
			| TSXToken::Subtract
			| TSXToken::Multiply
			| TSXToken::Divide
			| TSXToken::Modulo
			| TSXToken::QuestionMark
			| TSXToken::BitwiseOr
			| TSXToken::BitwiseXOr
			| TSXToken::BitwiseAnd
			| TSXToken::BitwiseNot
			| TSXToken::HashTag
			| TSXToken::Dot
			| TSXToken::TemplateLiteralStart
			| TSXToken::TemplateLiteralEnd
			| TSXToken::TemplateLiteralExpressionEnd
			| TSXToken::JSXOpeningTagStart
			| TSXToken::JSXOpeningTagEnd
			| TSXToken::JSXExpressionStart
			| TSXToken::JSXExpressionEnd
			| TSXToken::JSXAttributeAssign => 1,

			TSXToken::AddAssign
			| TSXToken::SubtractAssign
			| TSXToken::MultiplyAssign
			| TSXToken::DivideAssign
			| TSXToken::ModuloAssign
			| TSXToken::Exponent
			| TSXToken::ExponentAssign
			| TSXToken::Increment
			| TSXToken::Decrement
			| TSXToken::Equal
			| TSXToken::GreaterThanEqual
			| TSXToken::LessThanEqual
			| TSXToken::OptionalChain
			| TSXToken::NullishCoalescing
			| TSXToken::OptionalMember
			| TSXToken::NonOptionalMember
			| TSXToken::BitwiseOrAssign
			| TSXToken::BitwiseAndAssign
			| TSXToken::BitwiseXorAssign
			| TSXToken::LogicalOr
			| TSXToken::LogicalAnd
			| TSXToken::LogicalNot
			| TSXToken::Arrow
			| TSXToken::TemplateLiteralExpressionStart
			| TSXToken::JSXFragmentStart => 2,

			TSXToken::BitwiseShiftLeft
			| TSXToken::BitwiseShiftRight
			| TSXToken::Spread
			| TSXToken::StrictEqual
			| TSXToken::NullishCoalescingAssign
			| TSXToken::LogicalOrAssign
			| TSXToken::LogicalAndAssign
			| TSXToken::NotEqual
			| TSXToken::BitwiseShiftLeftAssign
			| TSXToken::BitwiseShiftRightAssign
			| TSXToken::StrictNotEqual
			| TSXToken::BitwiseShiftRightUnsigned
			| TSXToken::JSXFragmentEnd => 3,

			TSXToken::BitwiseShiftRightUnsignedAssign => 4,

			// TODO
			TSXToken::JSXClosingTagStart
			| TSXToken::JSXSelfClosingTag
			| TSXToken::JSXContentLineBreak
			| TSXToken::EOS
			| TSXToken::Cursor(_) => 0,

			#[cfg(feature = "extras")]
			TSXToken::InvertAssign | TSXToken::DividesOperator | TSXToken::PipeOperator => 2,
			#[cfg(feature = "extras")]
			TSXToken::ComposeOperator => 3,
		}
	}
}

impl Eq for TSXToken {}

pub trait TSXKeywordNode: Into<TSXKeyword> + Copy + Default {}

#[derive(Debug, PartialEq, Eq, EnumVariantsStrings, EnumVariantType)]
#[enum_variants_strings_transform(transform = "lower_case")]
#[evt(module = "tsx_keywords", implement_marker_traits(TSXKeywordNode), derive(Clone, Copy, PartialEq, Eq, Debug, Default))]
#[rustfmt::skip]
pub enum TSXKeyword {
    Const, Var, Let,
    If, Else, For, While, Do, Switch,
    Class, Function, Constructor,
    New, This, Super,
    Case, Yield, Return, Continue, Break,
    Import, Export, Default, From,
    In, Of,
    TypeOf, InstanceOf, Void, Delete, 
    Debugger,
    Try, Catch, Finally, Throw,
    Async, Await,
    Static,
    Get, Set,
    Extends, 
    Null, 
    True, False,
    // TS special keywords
    Abstract, Implements,
    // TS structure keyword
    Enum, Interface, Type,
    // TS publicity attributes
    Private, Public, Protected,
    // TS Keywords
    As, Declare, Readonly, Infer, Is, Satisfies, Namespace, KeyOf,
    // Extra function modifiers
    #[cfg(feature = "extras")] Server, #[cfg(feature = "extras")] Module,
    // Type declaration changes
    #[cfg(feature = "extras")] Nominal, #[cfg(feature = "extras")] Performs,

    #[cfg(feature = "extras")]
    /// https://github.com/tc39/proposal-generator-arrow-functions#introduce-new-generator-keyword-for-both-function-and-arrow-function
    Generator,
}

impl std::fmt::Display for TSXKeyword {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		std::fmt::Debug::fmt(&self, f)
	}
}

impl std::fmt::Display for TSXToken {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TSXToken::Keyword(kw) => std::fmt::Debug::fmt(kw, f),
			TSXToken::NumberLiteral(num) => std::fmt::Display::fmt(num, f),
			TSXToken::IdentLiteral(value) => std::fmt::Display::fmt(value, f),
			_ => std::fmt::Debug::fmt(&self, f),
		}
	}
}

impl TSXToken {
	pub fn is_comment(&self) -> bool {
		matches!(self, TSXToken::Comment(_) | TSXToken::MultiLineComment(_))
	}

	pub fn is_string_literal(&self) -> bool {
		matches!(
			self,
			TSXToken::SingleQuotedStringLiteral(_) | TSXToken::DoubleQuotedStringLiteral(_)
		)
	}

	/// Used for lexing regular expression and JSX literals differently
	pub fn is_expression_prefix(&self) -> bool {
		matches!(
			self,
			TSXToken::Keyword(TSXKeyword::Return)
                | TSXToken::Assign
                | TSXToken::Arrow
                | TSXToken::OpenParentheses
                | TSXToken::OpenBrace
                | TSXToken::JSXExpressionStart
                | TSXToken::QuestionMark
                | TSXToken::Colon
                // This is for match bindings
                | TSXToken::At
		)
	}

	/// Returns a keyword token else an identifier literal
	pub fn from_slice(slice: &str) -> Self {
		match TSXKeyword::from_str(slice) {
			Ok(keyword_token) => TSXToken::Keyword(keyword_token),
			Err(_) => TSXToken::IdentLiteral(slice.to_owned()),
		}
	}
}

/// Some tokens can be used as names for variables, methods (eg 'get' in <Map>.get()). This function
/// takes a [Token] and returns its name as a [String] and the location as a [Span]. Will throw [ParseError] if
/// cannot convert token to string
pub(crate) fn token_as_identifier(
	token: Token<TSXToken, TokenStart>,
	at_location: &str,
) -> Result<(String, Span), ParseError> {
	let position = token.get_span();
	let name = match token.0 {
		TSXToken::IdentLiteral(value) => value,
		TSXToken::Keyword(keyword) => EnumVariantsStrings::to_str(&keyword).to_owned(),
		token_type => {
			return Err(ParseError::new(
				crate::ParseErrors::ExpectedIdent { found: token_type, at_location },
				position,
			));
		}
	};
	Ok((name, position))
}
