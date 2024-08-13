//! Contains the declaration for [`TSXToken`] which are pieces of syntax. Also
//! - How tokens are made from consecutive characters
//! - Keywords

use derive_finite_automaton::FiniteAutomataConstructor;
use derive_partial_eq_extras::PartialEqExtras;
use enum_variants_strings::EnumVariantsStrings;
use source_map::Span;
use tokenizer_lib::{sized_tokens::TokenStart, Token};

use crate::{ParseError, Quoted};

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
    "?.(" => TSXToken::OptionalCall,
    "?.[" => TSXToken::OptionalIndex,
    "-?:" => TSXToken::NonOptionalMember,
    "??" => TSXToken::NullishCoalescing,
    "??=" => TSXToken::NullishCoalescingAssign,
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
    "<@>" => TSXToken::ComposeOperator,
    "|>" => TSXToken::PipeOperator,
))]
#[rustfmt::skip]
pub enum TSXToken {
    Identifier(String),
    Keyword(TSXKeyword),
    NumberLiteral(String),
    StringLiteral(String, Quoted),
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
    OptionalChain, OptionalCall, OptionalIndex, NullishCoalescing, NullishCoalescingAssign,
    /// `?:`
    OptionalMember,
    /// `!:`
    NonOptionalMember,
    /// For scripts thing
    HashTag,
    // JSX Tokens. Some like JSXComment are non standard
    JSXOpeningTagStart, JSXTagName(String), JSXOpeningTagEnd,
    JSXClosingTagStart,
    /// This also covers the end of a token, thus no `TSXToken::JSXClosingTagEnd`
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
    InvertAssign,
    #[cfg(feature = "extras")]
    ComposeOperator,
    #[cfg(feature = "extras")]
    PipeOperator,

    EOS
}

impl tokenizer_lib::TokenTrait for TSXToken {
	fn is_skippable(&self) -> bool {
		// TODO is this correct?
		self.is_comment()
	}
}

impl tokenizer_lib::sized_tokens::SizedToken for TSXToken {
	#[allow(clippy::cast_possible_truncation)]
	fn length(&self) -> u32 {
		match self {
			TSXToken::Keyword(kw) => kw.length(),

			TSXToken::JSXClosingTagName(lit)
			| TSXToken::TemplateLiteralChunk(lit)
			| TSXToken::JSXAttributeKey(lit)
			| TSXToken::JSXAttributeValue(lit)
			| TSXToken::JSXContent(lit)
			| TSXToken::JSXTagName(lit)
			| TSXToken::Identifier(lit)
			| TSXToken::NumberLiteral(lit)
			| TSXToken::RegexFlagLiteral(lit) => lit.len() as u32,

			TSXToken::JSXComment(comment) => comment.len() as u32 + 7,
			TSXToken::MultiLineComment(comment) => comment.len() as u32 + 4,
			TSXToken::StringLiteral(comment, _) | TSXToken::Comment(comment) => {
				comment.len() as u32 + 2
			}
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
			| TSXToken::JSXAttributeAssign
			| TSXToken::JSXClosingTagStart
			| TSXToken::JSXContentLineBreak => 1,

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
			| TSXToken::BitwiseShiftLeft
			| TSXToken::BitwiseShiftRight
			| TSXToken::TemplateLiteralExpressionStart
			| TSXToken::JSXFragmentStart
			| TSXToken::JSXSelfClosingTag => 2,

			TSXToken::Spread
			| TSXToken::StrictEqual
			| TSXToken::OptionalCall
			| TSXToken::OptionalIndex
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

			// Marker nodes with no length
			TSXToken::EOS => 0,

			#[cfg(feature = "extras")]
			TSXToken::InvertAssign | TSXToken::PipeOperator => 2,
			#[cfg(feature = "extras")]
			TSXToken::ComposeOperator => 3,
		}
	}
}

impl Eq for TSXToken {}

#[derive(Debug, PartialEq, EnumVariantsStrings, Clone, Copy)]
#[enum_variants_strings_transform(transform = "lower_case")]
#[rustfmt::skip]
pub enum TSXKeyword {
    Const, Var, Let,
    If, Else, For, While, Do, Switch,
    Class, Function, Constructor,
    New, This, Super,
    Case, Yield, Return, Continue, Break,
    Import, Export, Default, From, With,
    In, Of,
    TypeOf, InstanceOf, Void, Delete, 
	/// For [import assertions](https://v8.dev/features/import-assertions) lol
	Assert,
	/// For [assertion function type annotations](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-7.html#assertion-functions) lol
	Asserts,
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
    As, Readonly, Satisfies, Declare, Namespace,
	// TS & Ezno
	Is,
	Infer, KeyOf, Unique, Symbol,
	// TODO unsure
	#[cfg(feature = "extras")] Module,
    // Extra function modifiers
    #[cfg(feature = "extras")] Server, #[cfg(feature = "extras")] Worker,
    // Type declaration changes
    #[cfg(feature = "extras")] Nominal, #[cfg(feature = "extras")] Performs,

    #[cfg(feature = "extras")]
    /// <https://github.com/tc39/proposal-generator-arrow-functions#introduce-new-generator-keyword-for-both-function-and-arrow-function>
    Generator,

    #[cfg(feature = "extras")]
    Deferred
}

impl TSXKeyword {
	#[cfg(feature = "extras")]
	pub(crate) fn is_special_function_header(self) -> bool {
		matches!(self, TSXKeyword::Worker | TSXKeyword::Server | TSXKeyword::Generator)
	}

	#[cfg(not(feature = "extras"))]
	pub(crate) fn is_special_function_header(&self) -> bool {
		false
	}

	pub(crate) fn is_in_function_header(self) -> bool {
		matches!(self, TSXKeyword::Function | TSXKeyword::Async)
	}

	#[allow(clippy::cast_possible_truncation)]
	pub(crate) fn length(self) -> u32 {
		self.to_str().len() as u32
	}

	#[rustfmt::skip]
	pub(crate) fn is_invalid_identifier(self) -> bool {
		matches!(
			self,
			TSXKeyword::Const | TSXKeyword::Var | TSXKeyword::If | TSXKeyword::Else | TSXKeyword::For
				| TSXKeyword::While | TSXKeyword::Do | TSXKeyword::Switch | TSXKeyword::Class | TSXKeyword::Function
				| TSXKeyword::New | TSXKeyword::Super | TSXKeyword::Case | TSXKeyword::Return | TSXKeyword::Continue
				| TSXKeyword::Break | TSXKeyword::Import | TSXKeyword::Export | TSXKeyword::Default | TSXKeyword::In
				| TSXKeyword::TypeOf | TSXKeyword::InstanceOf | TSXKeyword::Void | TSXKeyword::Delete
				| TSXKeyword::Debugger | TSXKeyword::Try | TSXKeyword::Catch | TSXKeyword::Finally | TSXKeyword::Throw
				| TSXKeyword::Extends | TSXKeyword::Enum
		)
	}
}

impl TSXToken {
	#[must_use]
	pub fn is_identifier_or_ident(&self) -> bool {
		matches!(self, TSXToken::Identifier(_) | TSXToken::Keyword(_))
	}

	#[must_use]
	pub fn is_comment(&self) -> bool {
		matches!(self, TSXToken::Comment(_) | TSXToken::MultiLineComment(_))
	}

	/// Returns `(*content*, *is_multiline*, *position*)`
	pub fn try_into_comment(
		token: Token<TSXToken, TokenStart>,
	) -> Result<(String, bool, Span), Token<TSXToken, TokenStart>> {
		if let Token(TSXToken::MultiLineComment(c), d) = token {
			let len = c.len();
			Ok((c, true, d.with_length(len + 4)))
		} else if let Token(TSXToken::Comment(c), d) = token {
			let len = c.len();
			Ok((c, false, d.with_length(len + 2)))
		} else {
			Err(token)
		}
	}

	/// Used for lexing regular expression and JSX literals differently
	#[must_use]
	pub fn is_expression_prefix(&self) -> bool {
		matches!(
			self,
			TSXToken::Keyword(TSXKeyword::Return | TSXKeyword::Case | TSXKeyword::Yield | TSXKeyword::Throw | TSXKeyword::TypeOf | TSXKeyword::In | TSXKeyword::Of | TSXKeyword::Await | TSXKeyword::Do)
				| TSXToken::Arrow
				// for `const x = 2; /something/g`
				| TSXToken::SemiColon
				| TSXToken::OpenParentheses
				| TSXToken::OpenBrace
				| TSXToken::OpenBracket
				| TSXToken::JSXExpressionStart
				| TSXToken::QuestionMark
				| TSXToken::Colon
				| TSXToken::LogicalNot
				| TSXToken::LogicalAnd
				| TSXToken::LogicalOr
				| TSXToken::BitwiseNot
				| TSXToken::BitwiseAnd
				| TSXToken::BitwiseOr
				| TSXToken::Multiply
				| TSXToken::Add
				| TSXToken::Subtract
				| TSXToken::Divide
		) || self.is_assignment()
	}

	/// For trailing expression comments
	#[must_use]
	pub fn is_expression_postfix(&self) -> bool {
		matches!(
			self,
			TSXToken::MultiLineComment(..)
				| TSXToken::LogicalAnd
				| TSXToken::LogicalOr
				| TSXToken::Multiply
				| TSXToken::Add | TSXToken::Subtract
				| TSXToken::Divide
		) || self.is_assignment()
	}

	/// Returns a keyword token else an identifier literal
	#[must_use]
	pub fn from_slice(slice: &str) -> Self {
		match TSXKeyword::from_str(slice) {
			Ok(keyword_token) => TSXToken::Keyword(keyword_token),
			Err(_) => TSXToken::Identifier(slice.to_owned()),
		}
	}

	pub(crate) fn is_symbol(&self) -> bool {
		!matches!(self, TSXToken::Keyword(_) | TSXToken::Identifier(..))
	}

	pub(crate) fn is_statement_or_declaration_start(&self) -> bool {
		matches!(
			self,
			TSXToken::Keyword(
				TSXKeyword::Function
					| TSXKeyword::If | TSXKeyword::For
					| TSXKeyword::While | TSXKeyword::Const
					| TSXKeyword::Let | TSXKeyword::Break
					| TSXKeyword::Import | TSXKeyword::Export
			)
		)
	}

	pub(crate) fn is_expression_delimiter(&self) -> bool {
		matches!(
			self,
			TSXToken::Comma
				| TSXToken::SemiColon
				| TSXToken::Colon
				| TSXToken::LogicalOr
				| TSXToken::LogicalAnd
				| TSXToken::CloseBrace
				| TSXToken::CloseParentheses
				| TSXToken::CloseBracket
		) || self.is_assignment()
	}

	pub(crate) fn is_assignment(&self) -> bool {
		matches!(
			self,
			TSXToken::Assign
				| TSXToken::MultiplyAssign
				| TSXToken::AddAssign
				| TSXToken::SubtractAssign
				| TSXToken::DivideAssign
				| TSXToken::ModuloAssign
				| TSXToken::BitwiseOrAssign
				| TSXToken::BitwiseAndAssign
				| TSXToken::LogicalOrAssign
				| TSXToken::LogicalAndAssign
		)
	}
}

/// Some tokens can be used as names for variables, methods (eg 'get' in `Map.get()`). This function
/// takes a [Token] and returns its name as a [String] and the location as a [Span]. Will throw [`ParseError`] if
/// cannot convert token to string
pub(crate) fn token_as_identifier(
	token: Token<TSXToken, TokenStart>,
	at_location: &str,
) -> crate::ParseResult<(String, Span)> {
	let position = token.get_span();
	let name = match token.0 {
		TSXToken::Identifier(value) => value,
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
