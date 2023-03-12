//! Contains the declaration for [TSXToken] which are pieces of syntax. Also
//! - How tokens are made from consecutive characters
//! - Keywords

use derive_finite_automaton::FiniteAutomataConstructor;
use derive_partial_eq_extras::PartialEqExtras;
use enum_variant_type::EnumVariantType;
use enum_variants_strings::EnumVariantsStrings;
use source_map::Span;
use tokenizer_lib::Token;

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
    ">>>=" => TSXToken::UnsignedBitwiseShiftRightAssign,
    "." => TSXToken::Dot,
    "..." => TSXToken::Spread,
    // Special ones: TODO unify
    "×" => TSXToken::Multiply,
    "×=" => TSXToken::MultiplyAssign,
    "¡" => TSXToken::InvertAssign,
    ">!" => TSXToken::InvertAssign,
    "∣" => TSXToken::DividesOperator,
    "/%" => TSXToken::DividesOperator,
    "∘" => TSXToken::ComposeOperator,
    "<@>" => TSXToken::ComposeOperator,
    "|>" => TSXToken::PipeOperator,
)]
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
    BitwiseShiftLeftAssign, BitwiseShiftRightAssign, UnsignedBitwiseShiftRightAssign,
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
    // Extra blocks
    #[cfg(feature = "extras")] Server, #[cfg(feature = "extras")] Module,
    // Type changes
    #[cfg(feature = "extras")] Nominal, #[cfg(feature = "extras")] Performs,

    #[cfg(feature = "extras")]
    /// https://github.com/tc39/proposal-generator-arrow-functions#introduce-new-generator-keyword-for-both-function-and-arrow-function
    Generator,
}

impl TSXToken {
	pub fn is_comment(&self) -> bool {
		matches!(self, TSXToken::Comment(_) | TSXToken::MultiLineComment(_))
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
	token: Token<TSXToken, Span>,
	at_location: &str,
) -> Result<(String, Span), ParseError> {
	let Token(token_type, position) = token;
	let name = match token_type {
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
