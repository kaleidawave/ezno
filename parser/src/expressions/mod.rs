use crate::{
	declarations::ClassDeclaration,
	errors::parse_lexing_error,
	functions::GeneralFunctionBase,
	operators::{
		AssociativityDirection, BinaryAssignmentOperator, UnaryPostfixAssignmentOperator,
		UnaryPrefixAssignmentOperator, ASSIGNMENT_PRECEDENCE, AS_PRECEDENCE,
		FUNCTION_CALL_PRECEDENCE,
	},
	parse_bracketed, throw_unexpected_token_with_token, to_string_bracketed,
	type_annotations::generic_arguments_from_reader_sub_open_angle,
	CursorId, ExpressionPosition, FunctionHeader, Keyword, NumberRepresentation, ParseResult,
	Quoted, TSXKeyword,
};

use self::{
	assignments::{LHSOfAssignment, VariableOrPropertyAccess},
	object_literal::ObjectLiteral,
};

use super::{
	operators::{
		BinaryOperator, Operator, UnaryOperator, COMMA_PRECEDENCE, CONDITIONAL_TERNARY_PRECEDENCE,
		CONSTRUCTOR_PRECEDENCE, CONSTRUCTOR_WITHOUT_PARENTHESIS_PRECEDENCE, INDEX_PRECEDENCE,
		MEMBER_ACCESS_PRECEDENCE, PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE,
	},
	tokens::token_as_identifier,
	ASTNode, Block, FunctionBase, JSXRoot, ParseError, ParseOptions, Span, TSXToken, Token,
	TokenReader, TypeAnnotation,
};

#[cfg(feature = "extras")]
use crate::extensions::is_expression::{is_expression_from_reader_sub_is_keyword, IsExpression};

use crate::tsx_keywords::{self, As, Satisfies};
use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use tokenizer_lib::sized_tokens::{TokenEnd, TokenReaderWithTokenEnds, TokenStart};
use visitable_derive::Visitable;

pub mod arrow_function;
pub mod assignments;
pub mod object_literal;
pub mod template_literal;
pub use arrow_function::{ArrowFunction, ExpressionOrBlock};

pub use template_literal::{TemplateLiteral, TemplateLiteralPart};

pub type ExpressionFunctionBase = GeneralFunctionBase<ExpressionPosition>;
pub type ExpressionFunction = FunctionBase<ExpressionFunctionBase>;

use std::convert::{TryFrom, TryInto};

/// Expression structures
///
/// Comma is implemented as a [BinaryOperator]
#[derive(PartialEqExtras, Debug, Clone, Visitable, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[partial_eq_ignore_types(Span)]
#[visit_self]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum Expression {
	// Literals:
	NumberLiteral(NumberRepresentation, Span),
	StringLiteral(String, #[partial_eq_ignore] Quoted, Span),
	BooleanLiteral(bool, Span),
	RegexLiteral {
		pattern: String,
		flags: Option<String>,
		position: Span,
	},
	ArrayLiteral(Vec<SpreadExpression>, Span),
	ObjectLiteral(ObjectLiteral),
	TemplateLiteral(TemplateLiteral),
	ParenthesizedExpression(Box<MultipleExpression>, Span),
	// Regular operations:
	BinaryOperation {
		lhs: Box<Expression>,
		operator: BinaryOperator,
		rhs: Box<Expression>,
		position: Span,
	},
	SpecialOperators(SpecialOperators, Span),
	UnaryOperation {
		operator: UnaryOperator,
		operand: Box<Expression>,
		position: Span,
	},
	// Assignment operations
	Assignment {
		lhs: LHSOfAssignment,
		rhs: Box<Expression>,
		position: Span,
	},
	/// Modified assignment cannot have destructured thingies
	BinaryAssignmentOperation {
		lhs: VariableOrPropertyAccess,
		operator: BinaryAssignmentOperator,
		rhs: Box<Expression>,
		position: Span,
	},
	UnaryPrefixAssignmentOperation {
		operator: UnaryPrefixAssignmentOperator,
		operand: VariableOrPropertyAccess,
		position: Span,
	},
	UnaryPostfixAssignmentOperation {
		operand: VariableOrPropertyAccess,
		operator: UnaryPostfixAssignmentOperator,
		position: Span,
	},
	/// e.g `x` or `(...).hi`
	VariableReference(String, Span),
	ThisReference(Span),
	SuperExpression(SuperReference, Span),
	/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target
	NewTarget(Span),
	DynamicImport {
		path: Box<Expression>,
		position: Span,
	},
	PropertyAccess {
		parent: Box<Expression>,
		property: PropertyReference,
		is_optional: bool,
		position: Span,
	},
	/// e.g `...[4]`
	Index {
		indexee: Box<Expression>,
		indexer: Box<MultipleExpression>,
		is_optional: bool,
		position: Span,
	},
	// Function calls
	FunctionCall {
		function: Box<Expression>,
		type_arguments: Option<Vec<TypeAnnotation>>,
		arguments: Vec<SpreadExpression>,
		is_optional: bool,
		position: Span,
	},
	ConstructorCall {
		constructor: Box<Expression>,
		type_arguments: Option<Vec<TypeAnnotation>>,
		arguments: Option<Vec<SpreadExpression>>,
		position: Span,
	},
	/// e.g `... ? ... ? ...`
	ConditionalTernaryExpression {
		condition: Box<Expression>,
		truthy_result: Box<Expression>,
		falsy_result: Box<Expression>,
		position: Span,
	},
	// Functions
	ArrowFunction(ArrowFunction),
	ExpressionFunction(ExpressionFunction),
	/// Yes classes can exist in expr position :?
	ClassExpression(ClassDeclaration<ExpressionPosition>),
	Null(Span),
	// Comments
	PrefixComment(String, Box<Expression>, Span),
	PostfixComment(Box<Expression>, String, Span),
	/// Allowed in trailing functions and JSX for some reason
	Comment(String, Span),
	/// TODO under cfg
	/// A start of a JSXNode
	JSXRoot(JSXRoot),
	/// Not to be confused with binary operator `is`
	#[cfg(feature = "extras")]
	IsExpression(IsExpression),
	#[cfg_attr(feature = "self-rust-tokenize", self_tokenize_field(cursor_id))]
	Cursor {
		#[visit_skip_field]
		cursor_id: CursorId<Expression>,
		position: Span,
	},
}

impl Eq for Expression {}

#[derive(PartialEq, Eq, Debug, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum PropertyReference {
	Standard(String),
	#[cfg_attr(feature = "self-rust-tokenize", self_tokenize_field(0))]
	Cursor(CursorId<PropertyReference>),
}

impl ASTNode for Expression {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Expression> {
		Self::from_reader_with_precedence(reader, state, settings, COMMA_PRECEDENCE)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		self.to_string_using_precedence(buf, settings, depth, COMMA_PRECEDENCE)
	}

	fn get_position(&self) -> &Span {
		get_field_by_type::GetFieldByType::get(self)
	}
}

impl Expression {
	pub(self) fn from_reader_with_precedence(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
		return_precedence: u8,
	) -> ParseResult<Self> {
		let first_expression = match reader.next().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::Cursor(cursor_id), position) => {
				return Ok(Expression::Cursor {
					cursor_id: cursor_id.into_cursor(),
					position: Span { start: position.0, end: position.0, source: () },
				});
			}
			Token(TSXToken::SingleQuotedStringLiteral(content), start) => {
				let position = start.with_length(content.len() + 2);
				Expression::StringLiteral(content, Quoted::Single, position)
			}
			Token(TSXToken::DoubleQuotedStringLiteral(content), start) => {
				let position = start.with_length(content.len() + 2);
				Expression::StringLiteral(content, Quoted::Double, position)
			}
			Token(TSXToken::NumberLiteral(value), start) => {
				let position = start.with_length(value.len());
				let res = value.parse::<NumberRepresentation>();
				match res {
					Ok(number) => Expression::NumberLiteral(number, position),
					Err(_) => unreachable!("Could not parse {value}"),
				}
			}
			Token(TSXToken::RegexLiteral(pattern), start) => {
				let mut position = start.with_length(pattern.len());
				let flag_token =
					reader.conditional_next(|t| matches!(t, TSXToken::RegexFlagLiteral(..)));
				let flags =
					if let Some(Token(TSXToken::RegexFlagLiteral(flags), start)) = flag_token {
						position = position.union(start.get_end_after(flags.len()));
						Some(flags)
					} else {
						None
					};
				Expression::RegexLiteral { pattern, flags, position }
			}
			t @ Token(TSXToken::Keyword(TSXKeyword::True), _) => {
				Expression::BooleanLiteral(true, t.get_span())
			}
			t @ Token(TSXToken::Keyword(TSXKeyword::False), _) => {
				Expression::BooleanLiteral(false, t.get_span())
			}
			t @ Token(TSXToken::Keyword(TSXKeyword::This), _) => {
				Expression::ThisReference(t.get_span())
			}
			t @ Token(TSXToken::Keyword(TSXKeyword::Super), _) => {
				let _super_position = t.get_span();
				let token = reader.next().unwrap();
				let position = token.get_span();

				let (reference, end) = match token {
					Token(TSXToken::Dot, _) => {
						let (property, property_pos) =
							token_as_identifier(reader.next().unwrap(), "super property")?;
						(
							SuperReference::PropertyAccess { property },
							TokenEnd::new(property_pos.end),
						)
					}
					Token(TSXToken::OpenParentheses, _) => {
						let (arguments, end_pos) = parse_bracketed(
							reader,
							state,
							settings,
							None,
							TSXToken::CloseParentheses,
						)?;
						(SuperReference::Call { arguments }, end_pos)
					}
					Token(TSXToken::OpenBracket, _) => {
						let indexer = Expression::from_reader(reader, state, settings)?;
						let end = reader.expect_next(TSXToken::CloseBracket)?;
						(
							SuperReference::Index { indexer: Box::new(indexer) },
							TokenEnd::new(end.0 + 1),
						)
					}
					token => {
						return throw_unexpected_token_with_token(
							token,
							&[TSXToken::Dot, TSXToken::OpenParentheses, TSXToken::OpenBracket],
						);
					}
				};
				Expression::SuperExpression(reference, position.union(end))
			}
			t @ Token(TSXToken::Keyword(TSXKeyword::Null), _) => Expression::Null(t.get_span()),
			t @ Token(TSXToken::Keyword(TSXKeyword::Class), _) => {
				let keyword = Keyword(tsx_keywords::Class, t.get_span());
				let class_declaration = ClassDeclaration::from_reader_sub_class_keyword(
					reader, state, settings, keyword,
				)?;
				Expression::ClassExpression(class_declaration)
			}
			t @ Token(TSXToken::OpenBracket, start) => {
				let mut bracket_depth = 1;
				let after_bracket = reader.scan(|token, _| match token {
					TSXToken::OpenBracket => {
						bracket_depth += 1;
						false
					}
					TSXToken::CloseBracket => {
						bracket_depth -= 1;
						bracket_depth == 0
					}
					_ => false,
				});
				if let Some(Token(token_type, _)) = after_bracket {
					if let TSXToken::Assign = token_type {
						let (members, end_span) =
							parse_bracketed(reader, state, settings, None, TSXToken::CloseBracket)?;
						reader.next();
						let rhs = Expression::from_reader_with_precedence(
							reader,
							state,
							settings,
							ASSIGNMENT_PRECEDENCE,
						)?;
						let array_position = start.union(end_span);
						Expression::Assignment {
							position: array_position.union(rhs.get_position()),
							lhs: LHSOfAssignment::ArrayDestructuring(members, array_position),
							rhs: Box::new(rhs),
						}
					} else {
						let (items, end) =
							parse_bracketed(reader, state, settings, None, TSXToken::CloseBracket)?;
						Expression::ArrayLiteral(items, start.union(end))
					}
				} else {
					return Err(ParseError::new(
						crate::ParseErrors::UnmatchedBrackets,
						t.get_span(),
					));
				}
			}
			t @ Token(TSXToken::OpenBrace, start) => {
				let mut brace_depth = 1;
				let after_brace = reader.scan(|token, _| match token {
					TSXToken::OpenBrace => {
						brace_depth += 1;
						false
					}
					TSXToken::CloseBrace => {
						brace_depth -= 1;
						brace_depth == 0
					}
					_ => false,
				});
				if let Some(Token(token_type, _)) = after_brace {
					if let TSXToken::Assign = token_type {
						let (members, end) =
							parse_bracketed(reader, state, settings, None, TSXToken::CloseBrace)?;
						reader.next();
						let rhs = Expression::from_reader_with_precedence(
							reader,
							state,
							settings,
							ASSIGNMENT_PRECEDENCE,
						)?;
						let object_position = start.union(end);
						Expression::Assignment {
							position: object_position.union(rhs.get_position()),
							lhs: LHSOfAssignment::ObjectDestructuring(members, object_position),
							rhs: Box::new(rhs),
						}
					} else {
						let object_literal = ObjectLiteral::from_reader_sub_open_curly(
							reader, state, settings, start,
						)?;
						Expression::ObjectLiteral(object_literal)
					}
				} else {
					return Err(ParseError::new(
						crate::ParseErrors::UnmatchedBrackets,
						t.get_span(),
					));
				}
			}
			t @ Token(TSXToken::OpenParentheses, start) => {
				let mut parentheses_depth = 1;
				let next = reader.scan(|token, _| match token {
					TSXToken::OpenParentheses => {
						parentheses_depth += 1;
						false
					}
					TSXToken::CloseParentheses => {
						parentheses_depth -= 1;
						parentheses_depth == 0
					}
					_ => false,
				});
				if let Some(Token(token_type, _)) = next {
					if let TSXToken::Arrow = token_type {
						let arrow_function = ArrowFunction::from_reader_sub_open_paren(
							reader, state, settings, None, start,
						)?;
						Expression::ArrowFunction(arrow_function)
					} else {
						let parenthesize_expression =
							MultipleExpression::from_reader(reader, state, settings)?;
						let end = reader.expect_next_get_end(TSXToken::CloseParentheses)?;
						Expression::ParenthesizedExpression(
							Box::new(parenthesize_expression),
							start.union(end),
						)
					}
				} else {
					return Err(ParseError::new(
						crate::ParseErrors::UnmatchedBrackets,
						t.get_span(),
					));
				}
			}
			t @ Token(TSXToken::Keyword(TSXKeyword::New), start) => {
				if let Some(Token(TSXToken::Dot, _)) = reader.peek() {
					// TODO assert not lonely, else syntax error
					reader.expect_next(TSXToken::Dot)?;
					reader.expect_next(TSXToken::IdentLiteral("target".into()))?;
					Expression::NewTarget(t.get_span())
				} else {
					// Pass as a function call and then adds the conversion
					let constructor_expression = Self::from_reader_with_precedence(
						reader,
						state,
						settings,
						FUNCTION_CALL_PRECEDENCE,
					)?;
					let position = start.union(constructor_expression.get_position());

					let (type_arguments, end) = if reader
						.conditional_next(|token| *token == TSXToken::OpenChevron)
						.is_some()
					{
						let (generic_arguments, end_pos) =
							parse_bracketed(reader, state, settings, None, TSXToken::CloseChevron)?;
						(Some(generic_arguments), end_pos)
					} else {
						(None, TokenEnd::new(position.end))
					};

					let (arguments, end) = if reader
						.conditional_next(|token| *token == TSXToken::OpenChevron)
						.is_some()
					{
						parse_bracketed(reader, state, settings, None, TSXToken::CloseParentheses)
							.map(|(args, end)| (Some(args), end))?
					} else {
						// TODO are type arguments not allowed...?
						(None, end)
					};

					Expression::ConstructorCall {
						constructor: constructor_expression.into(),
						type_arguments,
						arguments,
						position: start.union(end),
					}
				}
			}
			// Yes single line comment can occur here if the line splits
			// ```
			// [
			//     // Hi
			//     2
			// ]
			// ```
			Token(TSXToken::MultiLineComment(comment) | TSXToken::Comment(comment), start) => {
				if let Some(Token(TSXToken::CloseParentheses | TSXToken::JSXExpressionEnd, _)) =
					reader.peek()
				{
					let with_length = start.with_length(comment.len() + 2);
					return Ok(Expression::Comment(comment, with_length));
				}
				let expression =
					Self::from_reader_with_precedence(reader, state, settings, return_precedence)?;
				let position = start.union(expression.get_position());
				Expression::PrefixComment(comment, Box::new(expression), position)
			}
			Token(tok @ TSXToken::JSXOpeningTagStart | tok @ TSXToken::JSXFragmentStart, span) => {
				let var_name = matches!(tok, TSXToken::JSXFragmentStart);
				let root = JSXRoot::from_reader_sub_start(reader, state, settings, var_name, span)?;
				Expression::JSXRoot(root)
			}
			Token(TSXToken::TemplateLiteralStart, start) => {
				let template_literal = TemplateLiteral::from_reader_sub_start_with_tag(
					reader, state, settings, None, start,
				)?;
				Expression::TemplateLiteral(template_literal)
			}
			t @ Token(TSXToken::Keyword(TSXKeyword::Function), _) => {
				let span = t.get_span();
				let header = FunctionHeader::VirginFunctionHeader {
					async_keyword: None,
					function_keyword: Keyword::new(span.clone()),
					// TODO
					generator_star_token_position: None,
					position: span,
				};
				let name = if let Some(Token(TSXToken::OpenParentheses, _)) = reader.peek() {
					None
				} else {
					let (token, span) =
						token_as_identifier(reader.next().unwrap(), "function name")?;
					Some(crate::VariableIdentifier::Standard(token, span))
				};
				let function: ExpressionFunction = FunctionBase::from_reader_with_header_and_name(
					reader, state, settings, header, name,
				)?;
				Expression::ExpressionFunction(function)
			}
			// TODO this should be extracted to a function that allows it to also work for leading `generator`
			t @ Token(TSXToken::Keyword(TSXKeyword::Async), _) => {
				let async_keyword = Some(Keyword::new(t.get_span()));
				let header = crate::functions::function_header_from_reader_with_async_keyword(
					reader,
					async_keyword,
				)?;
				let name = if let Some(Token(TSXToken::OpenParentheses, _)) = reader.peek() {
					None
				} else {
					let (token, span) =
						token_as_identifier(reader.next().unwrap(), "function name")?;
					Some(crate::VariableIdentifier::Standard(token, span))
				};
				Expression::ExpressionFunction(FunctionBase::from_reader_with_header_and_name(
					reader, state, settings, header, name,
				)?)
			}
			#[cfg(feature = "extras")]
			t @ Token(TSXToken::Keyword(TSXKeyword::Generator), _) => {
				let get_span = t.get_span();
				let function_keyword = Keyword::from_reader(reader)?;
				let position = get_span.union(function_keyword.get_position());
				let header = FunctionHeader::ChadFunctionHeader {
					async_keyword: None,
					generator_keyword: Some(Keyword::new(get_span)),
					function_keyword,
					position,
				};
				let name = if let Some(Token(TSXToken::OpenParentheses, _)) = reader.peek() {
					None
				} else {
					let (token, span) =
						token_as_identifier(reader.next().unwrap(), "function name")?;
					Some(crate::VariableIdentifier::Standard(token, span))
				};
				Expression::ExpressionFunction(FunctionBase::from_reader_with_header_and_name(
					reader, state, settings, header, name,
				)?)
			}
			#[cfg(feature = "extras")]
			t @ Token(TSXToken::Keyword(TSXKeyword::Is), start) if settings.is_expressions => {
				// Maintains compatibility here
				let mut parentheses_depth = 0;
				let next = reader.scan(|token, _| match token {
					TSXToken::OpenParentheses => {
						parentheses_depth += 1;
						false
					}
					TSXToken::CloseParentheses => {
						parentheses_depth -= 1;
						parentheses_depth == 0
					}
					_ => false,
				});
				if let Some(Token(token_type, _)) = next {
					if let TSXToken::OpenBrace = token_type {
						let is_expression_from_reader_sub_is_keyword =
							is_expression_from_reader_sub_is_keyword(
								reader,
								state,
								settings,
								Keyword::new(t.get_span()),
							);

						is_expression_from_reader_sub_is_keyword.map(Expression::IsExpression)?
					} else {
						Expression::VariableReference("is".to_owned(), start.with_length(2))
					}
				} else {
					return Err(ParseError::new(
						crate::ParseErrors::UnmatchedBrackets,
						t.get_span(),
					));
				}
			}
			token => {
				if let Ok(unary_operator) = UnaryOperator::try_from(&token.0) {
					let precedence = unary_operator.precedence();
					let operand =
						Self::from_reader_with_precedence(reader, state, settings, precedence)?;
					let position = token.get_span().union(operand.get_position());
					Expression::UnaryOperation {
						operand: Box::new(operand),
						operator: unary_operator,
						position,
					}
				} else if let Ok(unary_prefix_operator) =
					UnaryPrefixAssignmentOperator::try_from(&token.0)
				{
					// TODO is precedence needed...?
					let op_precedence = unary_prefix_operator.precedence();
					let operand = VariableOrPropertyAccess::from_reader_with_precedence(
						reader,
						state,
						settings,
						op_precedence,
					)?;
					let position = token.get_span().union(operand.get_position());
					Expression::UnaryPrefixAssignmentOperation {
						operand,
						operator: unary_prefix_operator,
						position,
					}
				} else {
					let (name, position) = token_as_identifier(token, "variable reference")?;
					if let Some(Token(TSXToken::Arrow, _)) = reader.peek() {
						let function = ArrowFunction::from_reader_with_first_parameter(
							reader,
							state,
							settings,
							(name, position),
						)?;
						Expression::ArrowFunction(function)
					} else {
						Expression::VariableReference(name, position)
					}
				}
			}
		};
		Self::from_reader_sub_first_expression(
			reader,
			state,
			settings,
			return_precedence,
			first_expression,
		)
	}

	pub(crate) fn from_reader_sub_first_expression(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
		parent_precedence: u8,
		first_expression: Expression,
	) -> ParseResult<Self> {
		let mut top = first_expression;
		loop {
			let Token(peeked_token, _peeked_pos) = &reader.peek().unwrap();

			match peeked_token {
				TSXToken::Comma => {
					return Ok(top);
				}
				TSXToken::QuestionMark => {
					if AssociativityDirection::RightToLeft
						.should_return(parent_precedence, CONDITIONAL_TERNARY_PRECEDENCE)
					{
						return Ok(top);
					}
					reader.next();
					let lhs = Self::from_reader(reader, state, settings)?;
					reader.expect_next(TSXToken::Colon)?;
					let rhs = Self::from_reader(reader, state, settings)?;
					top = Expression::ConditionalTernaryExpression {
						position: top.get_position().union(rhs.get_position()),
						condition: Box::new(top),
						truthy_result: Box::new(lhs),
						falsy_result: Box::new(rhs),
					};
				}
				TSXToken::OpenParentheses => {
					if AssociativityDirection::LeftToRight
						.should_return(parent_precedence, FUNCTION_CALL_PRECEDENCE)
					{
						return Ok(top);
					}
					reader.next();
					let (arguments, end) =
						parse_bracketed(reader, state, settings, None, TSXToken::CloseParentheses)?;
					let position = top.get_position().union(end);
					top = Expression::FunctionCall {
						function: Box::new(top),
						type_arguments: None,
						arguments,
						position,
						is_optional: false,
					};
				}
				TSXToken::OpenBracket => {
					if AssociativityDirection::LeftToRight
						.should_return(parent_precedence, INDEX_PRECEDENCE)
					{
						return Ok(top);
					}
					reader.next();
					let indexer = MultipleExpression::from_reader(reader, state, settings)?;
					let end = reader.expect_next_get_end(TSXToken::CloseBracket)?;
					let position = top.get_position().union(end);
					top = Expression::Index {
						position,
						indexee: Box::new(top),
						indexer: Box::new(indexer),
						is_optional: false,
					};
				}
				TSXToken::TemplateLiteralStart => {
					// TODO I think this is true
					if AssociativityDirection::LeftToRight
						.should_return(parent_precedence, FUNCTION_CALL_PRECEDENCE)
					{
						return Ok(top);
					}
					reader.next();
					// TODO should check adjacency
					let start = TokenStart::new(top.get_position().start);
					let tag = Some(Box::new(top));
					let template_lit = TemplateLiteral::from_reader_sub_start_with_tag(
						reader, state, settings, tag, start,
					);
					top = template_lit.map(Expression::TemplateLiteral)?;
				}
				TSXToken::OptionalChain => {
					if AssociativityDirection::LeftToRight
						.should_return(parent_precedence, MEMBER_ACCESS_PRECEDENCE)
					{
						return Ok(top);
					}
					let _ = reader.next().unwrap();
					let token = reader.next().ok_or_else(parse_lexing_error)?;
					top = match token {
						Token(TSXToken::OpenBracket, _start) => {
							let indexer = MultipleExpression::from_reader(reader, state, settings)?;
							let end = reader.expect_next_get_end(TSXToken::CloseBracket)?;
							let position = top.get_position().union(end);
							Expression::Index {
								position,
								indexee: Box::new(top),
								indexer: Box::new(indexer),
								is_optional: false,
							}
						}
						Token(TSXToken::OpenParentheses, _start) => {
							let (arguments, end) = parse_bracketed(
								reader,
								state,
								settings,
								Some(TSXToken::CloseParentheses),
								TSXToken::CloseParentheses,
							)?;
							let position = top.get_position().union(end);
							Expression::FunctionCall {
								function: Box::new(top),
								type_arguments: None,
								arguments,
								position,
								is_optional: true,
							}
						}
						token => {
							let (property, position) =
								if let Token(TSXToken::Cursor(cursor_id), start) = token {
									(
										PropertyReference::Cursor(cursor_id.into_cursor()),
										start.with_length(1),
									)
								} else {
									let (property, position) =
										token_as_identifier(token, "variable reference")?;
									(PropertyReference::Standard(property), position)
								};
							let position = top.get_position().union(&position);
							Expression::PropertyAccess {
								parent: Box::new(top),
								property,
								position,
								is_optional: true,
							}
						}
					}
				}
				TSXToken::Dot => {
					if AssociativityDirection::LeftToRight
						.should_return(parent_precedence, MEMBER_ACCESS_PRECEDENCE)
					{
						return Ok(top);
					}
					let _ = reader.next().unwrap();
					let token = reader.next().ok_or_else(parse_lexing_error)?;
					let (property, position) = if let Token(TSXToken::Cursor(cursor_id), start) =
						token
					{
						(PropertyReference::Cursor(cursor_id.into_cursor()), start.with_length(1))
					} else {
						let (property, position) =
							token_as_identifier(token, "variable reference")?;
						(PropertyReference::Standard(property), position)
					};
					let position = top.get_position().union(&position);
					top = Expression::PropertyAccess {
						parent: Box::new(top),
						property,
						position,
						is_optional: false,
					};
				}
				TSXToken::Assign => {
					if AssociativityDirection::RightToLeft
						.should_return(parent_precedence, ASSIGNMENT_PRECEDENCE)
					{
						return Ok(top);
					}
					reader.next();
					let new_rhs = Self::from_reader_with_precedence(
						reader,
						state,
						settings,
						parent_precedence,
					)?;
					top = Expression::Assignment {
						position: top.get_position().union(new_rhs.get_position()),
						lhs: LHSOfAssignment::VariableOrPropertyAccess(top.try_into()?),
						rhs: Box::new(new_rhs),
					};
				}
				TSXToken::MultiLineComment(_) | TSXToken::Comment(_) => {
					let token = reader.next().unwrap();
					let position = token.get_span();
					let Token(TSXToken::MultiLineComment(comment) | TSXToken::Comment(comment), _) =
						token
					else {
						unreachable!()
					};
					top = Expression::PostfixComment(Box::new(top), comment, position);
				}
				TSXToken::Keyword(TSXKeyword::As | TSXKeyword::Satisfies | TSXKeyword::Is) => {
					if AssociativityDirection::LeftToRight
						.should_return(parent_precedence, AS_PRECEDENCE)
					{
						return Ok(top);
					}

					if cfg!(not(feature = "extras"))
						&& matches!(peeked_token, TSXToken::Keyword(TSXKeyword::Is))
					{
						return Ok(top);
					}

					let token = reader.next().unwrap();
					let reference = TypeAnnotation::from_reader(reader, state, settings)?;
					let position = top.get_position().union(reference.get_position());

					let keyword_span = token.get_span();

					let special_operators = match token.0 {
						TSXToken::Keyword(TSXKeyword::As) => SpecialOperators::AsExpression {
							value: top.into(),
							as_keyword: Keyword::new(keyword_span),
							type_annotation: Box::new(reference),
						},
						TSXToken::Keyword(TSXKeyword::Satisfies) => {
							SpecialOperators::SatisfiesExpression {
								value: top.into(),
								satisfies_keyword: Keyword::new(keyword_span),
								type_annotation: Box::new(reference),
							}
						}
						#[cfg(feature = "extras")]
						TSXToken::Keyword(TSXKeyword::Is) => SpecialOperators::IsExpression {
							value: top.into(),
							is_keyword: Keyword::new(keyword_span),
							type_annotation: Box::new(reference),
						},
						_ => unreachable!(),
					};
					top = Self::SpecialOperators(special_operators, position);
				}
				token => {
					let token = if *token == TSXToken::OpenChevron {
						if is_generic_arguments(reader) {
							let _ = reader.next();
							let (type_arguments, _) = generic_arguments_from_reader_sub_open_angle(
								reader, state, settings, false,
							)?;
							let (arguments, end) = parse_bracketed(
								reader,
								state,
								settings,
								Some(TSXToken::OpenParentheses),
								TSXToken::CloseParentheses,
							)?;
							top = Expression::FunctionCall {
								position: top.get_position().union(end),
								function: Box::new(top),
								type_arguments: Some(type_arguments),
								arguments,
								is_optional: false,
							};
							continue;
						} else {
							// TODO
							&reader.peek().unwrap().0
						}
					} else {
						token
					};

					if let Ok(operator) = UnaryPostfixAssignmentOperator::try_from(token) {
						if operator
							.associativity_direction()
							.should_return(parent_precedence, operator.precedence())
						{
							return Ok(top);
						}
						let token = reader.next().unwrap();

						// Increment and decrement are the only two postfix operations
						let position = top.get_position().union(token.get_end());
						top = Expression::UnaryPostfixAssignmentOperation {
							operand: top.try_into()?,
							operator,
							position,
						};
					} else if let Ok(operator) = BinaryOperator::try_from(token) {
						if operator
							.associativity_direction()
							.should_return(parent_precedence, operator.precedence())
						{
							return Ok(top);
						}
						let _ = reader.next().unwrap();
						// Note precedence is already handled
						let rhs = Self::from_reader_with_precedence(
							reader,
							state,
							settings,
							operator.precedence(),
						)?;

						top = Expression::BinaryOperation {
							position: top.get_position().union(rhs.get_position()),
							lhs: Box::new(top),
							operator,
							rhs: Box::new(rhs),
						};
					} else if let Ok(operator) = BinaryAssignmentOperator::try_from(token) {
						if operator
							.associativity_direction()
							.should_return(parent_precedence, operator.precedence())
						{
							return Ok(top);
						}
						let _ = reader.next().unwrap();
						let new_rhs = Self::from_reader_with_precedence(
							reader,
							state,
							settings,
							operator.precedence(),
						)?;
						top = Expression::BinaryAssignmentOperation {
							position: top.get_position().union(new_rhs.get_position()),
							lhs: top.try_into()?,
							operator,
							rhs: Box::new(new_rhs),
						};
					} else {
						return Ok(top);
					}
				}
			}
		}
	}

	pub fn get_precedence(&self) -> u8 {
		match self {
            Self::NumberLiteral(..)
            | Self::BooleanLiteral(..)
            | Self::StringLiteral(..)
            | Self::RegexLiteral { .. }
            | Self::ArrayLiteral(..)
            | Self::TemplateLiteral(..)
            | Self::ParenthesizedExpression(..)
            | Self::JSXRoot(..)
            | Self::ArrowFunction(..)
            | Self::ExpressionFunction(..)
            | Self::Null(..)
            | Self::ObjectLiteral(..)
            | Self::VariableReference(..)
            | Self::ThisReference(..)
            | Self::SuperExpression(..)
            | Self::NewTarget(..)
            | Self::ClassExpression(..)
            // TODO not sure about this one...?
            | Self::DynamicImport { .. }
            | Self::Cursor { .. } => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE, // TODO think this is true <-
            // TODO not sure about this one...?
			#[cfg(feature = "extras")]
            Self::IsExpression(..) => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE,
            Self::BinaryOperation { operator, .. } => operator.precedence(),
            Self::UnaryOperation{ operator, .. } => operator.precedence(),
            Self::Assignment { .. } => ASSIGNMENT_PRECEDENCE,
            Self::BinaryAssignmentOperation { operator, .. } => operator.precedence(),
            Self::UnaryPrefixAssignmentOperation{ operator, .. } => operator.precedence(),
            Self::UnaryPostfixAssignmentOperation{ operator, .. } => operator.precedence(),
            Self::PropertyAccess { .. } => MEMBER_ACCESS_PRECEDENCE,
            Self::FunctionCall { .. } => FUNCTION_CALL_PRECEDENCE,
            Self::ConstructorCall { arguments: Some(_), .. } => CONSTRUCTOR_PRECEDENCE,
            Self::ConstructorCall { arguments: None, .. } => {
                CONSTRUCTOR_WITHOUT_PARENTHESIS_PRECEDENCE
            }
            Self::Index { .. } => INDEX_PRECEDENCE,
            Self::ConditionalTernaryExpression { .. } => CONDITIONAL_TERNARY_PRECEDENCE,
            Self::PrefixComment(_, expression, _) | Self::PostfixComment(expression, _, _) => {
                expression.get_precedence()
            }
            Self::Comment(_, _) => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE, // TODO not sure about this
            Self::SpecialOperators(op, _) => match op {
				 // TODO not sure about this
                SpecialOperators::AsExpression { .. } => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE,
                SpecialOperators::SatisfiesExpression { .. } => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE,
				#[cfg(feature = "extras")]
                SpecialOperators::IsExpression { .. } => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE,
            },
        }
	}

	pub(crate) fn to_string_using_precedence<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
		_parent_precedence: u8,
	) {
		match self {
			Self::Cursor { .. } => {
				if !settings.expect_cursors {
					panic!();
				}
			}
			Self::NumberLiteral(num, _) => buf.push_str(&num.to_string()),
			Self::StringLiteral(string, quoted, _) => {
				buf.push(quoted.as_char());
				buf.push_str(string);
				buf.push(quoted.as_char());
			}
			Self::BooleanLiteral(expression, _) => {
				buf.push_str(if *expression { "true" } else { "false" });
			}
			Self::RegexLiteral { pattern, flags, .. } => {
				buf.push('/');
				buf.push_str(pattern);
				buf.push('/');
				if let Some(flags) = flags {
					buf.push_str(flags);
				}
			}
			Self::BinaryOperation { lhs, operator, rhs, .. } => {
				let op_precedence = operator.precedence();
				lhs.to_string_using_precedence(buf, settings, depth, op_precedence);
				settings.add_gap(buf);
				buf.push_str(operator.to_str());
				settings.add_gap(buf);
				rhs.to_string_using_precedence(buf, settings, depth, op_precedence);
			}
			Self::SpecialOperators(special, _) => match special {
				SpecialOperators::AsExpression { value, type_annotation, .. }
				| SpecialOperators::SatisfiesExpression { value, type_annotation, .. } => {
					value.to_string_from_buffer(buf, settings, depth);
					if settings.include_types {
						buf.push_str(match special {
							SpecialOperators::AsExpression { .. } => " as ",
							SpecialOperators::SatisfiesExpression { .. } => " satisfies ",
							#[cfg(feature = "extras")]
							_ => unreachable!(),
						});
						type_annotation.to_string_from_buffer(buf, settings, depth);
					}
				}
				#[cfg(feature = "extras")]
				SpecialOperators::IsExpression { value, type_annotation, .. } => {
					value.to_string_from_buffer(buf, settings, depth);
					type_annotation.to_string_from_buffer(buf, settings, depth);
				}
			},
			Self::UnaryOperation { operand, operator, .. } => {
				buf.push_str(operator.to_str());
				operand.to_string_from_buffer(buf, settings, depth);
			}
			Self::Assignment { lhs, rhs, .. } => {
				lhs.to_string_from_buffer(buf, settings, depth);
				buf.push_str(if settings.pretty { " = " } else { "=" });
				rhs.to_string_from_buffer(buf, settings, depth)
			}
			Self::BinaryAssignmentOperation { lhs, operator, rhs, .. } => {
				lhs.to_string_from_buffer(buf, settings, depth);
				settings.add_gap(buf);
				buf.push_str(operator.to_str());
				settings.add_gap(buf);
				rhs.to_string_from_buffer(buf, settings, depth)
			}
			Self::UnaryPrefixAssignmentOperation { operand, operator, .. } => {
				buf.push_str(operator.to_str());
				operand.to_string_from_buffer(buf, settings, depth);
			}
			Self::UnaryPostfixAssignmentOperation { operand, operator, .. } => {
				operand.to_string_from_buffer(buf, settings, depth);
				buf.push_str(operator.to_str());
			}
			Self::VariableReference(name, _position) => {
				buf.push_str(name);
			}
			Self::ThisReference(..) => {
				buf.push_str("this");
			}
			Self::NewTarget(..) => {
				buf.push_str("new.target");
			}
			Self::DynamicImport { path, .. } => {
				buf.push_str("import(");
				path.to_string_from_buffer(buf, settings, depth);
				buf.push(')');
			}
			Self::PropertyAccess { parent, property, is_optional, position: _, .. } => {
				parent.to_string_from_buffer(buf, settings, depth);
				if *is_optional {
					buf.push('?');
				}
				buf.push('.');
				if let PropertyReference::Standard(property) = property {
					buf.push_str(property);
				} else if !settings.expect_cursors {
					panic!("found cursor");
				}
			}
			Self::ParenthesizedExpression(expr, _) => {
				// TODO more expressions could be considered fro parenthesis elision
				if let MultipleExpression::Single(inner @ Expression::VariableReference(..)) =
					&**expr
				{
					inner.to_string_from_buffer(buf, settings, depth)
				} else {
					buf.push('(');
					expr.to_string_from_buffer(buf, settings, depth);
					buf.push(')');
				}
			}
			Self::Index { indexee: expression, indexer, .. } => {
				expression.to_string_from_buffer(buf, settings, depth);
				buf.push('[');
				indexer.to_string_from_buffer(buf, settings, depth);
				buf.push(']');
			}
			Self::FunctionCall { function, type_arguments, arguments, .. } => {
				// TODO is this okay?
				if let Some(ExpressionOrBlock::Expression(expression)) = self.is_iife() {
					expression.to_string_from_buffer(buf, settings, depth);
					return;
				}
				let is_raw_function = matches!(
					&**function,
					Expression::ArrowFunction(..) | Expression::ExpressionFunction(..)
				);
				if is_raw_function {
					buf.push('(');
				}
				function.to_string_from_buffer(buf, settings, depth);
				if is_raw_function {
					buf.push(')');
				}
				if let (true, Some(type_arguments)) = (settings.include_types, type_arguments) {
					to_string_bracketed(type_arguments, ('<', '>'), buf, settings, depth);
				}
				to_string_bracketed(arguments, ('(', ')'), buf, settings, depth);
			}
			Self::ConstructorCall { constructor, type_arguments, arguments, .. } => {
				buf.push_str("new ");
				constructor.to_string_from_buffer(buf, settings, depth);
				if let (true, Some(type_arguments)) = (settings.include_types, type_arguments) {
					to_string_bracketed(type_arguments, ('<', '>'), buf, settings, depth);
				}
				if let Some(arguments) = arguments {
					// Constructor calls can drop arguments if none
					if !arguments.is_empty() {
						to_string_bracketed(arguments, ('(', ')'), buf, settings, depth);
					}
				}
			}
			Self::ArrayLiteral(values, _) => {
				to_string_bracketed(values, ('[', ']'), buf, settings, depth);
			}
			Self::JSXRoot(root) => root.to_string_from_buffer(buf, settings, depth),
			Self::ObjectLiteral(object_literal) => {
				object_literal.to_string_from_buffer(buf, settings, depth)
			}
			Self::ArrowFunction(arrow_function) => {
				arrow_function.to_string_from_buffer(buf, settings, depth)
			}
			Self::ExpressionFunction(function) => {
				function.to_string_from_buffer(buf, settings, depth);
			}
			Self::ClassExpression(class) => class.to_string_from_buffer(buf, settings, depth),
			Self::PrefixComment(comment, expression, _) => {
				if settings.should_add_comment() {
					buf.push_str("/*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/ ");
				}
				expression.to_string_from_buffer(buf, settings, depth);
			}
			Self::PostfixComment(expression, comment, _) => {
				expression.to_string_from_buffer(buf, settings, depth);
				if settings.should_add_comment() {
					buf.push_str(" /*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/");
				}
			}
			Self::Comment(comment, _) => {
				if settings.should_add_comment() {
					buf.push_str("/*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/");
				}
			}
			Self::TemplateLiteral(template_literal) => {
				template_literal.to_string_from_buffer(buf, settings, depth)
			}
			Self::ConditionalTernaryExpression {
				condition, truthy_result, falsy_result, ..
			} => {
				condition.to_string_using_precedence(
					buf,
					settings,
					depth,
					CONDITIONAL_TERNARY_PRECEDENCE,
				);
				buf.push_str(if settings.pretty { " ? " } else { "?" });
				truthy_result.to_string_using_precedence(
					buf,
					settings,
					depth,
					CONDITIONAL_TERNARY_PRECEDENCE,
				);
				buf.push_str(if settings.pretty { " : " } else { ":" });
				falsy_result.to_string_using_precedence(
					buf,
					settings,
					depth,
					CONDITIONAL_TERNARY_PRECEDENCE,
				);
			}
			Self::Null(..) => buf.push_str("null"),
			#[cfg(feature = "extras")]
			Self::IsExpression(is_expr) => is_expr.to_string_from_buffer(buf, settings, depth),
			Self::SuperExpression(super_expr, _) => {
				buf.push_str("super");
				match super_expr {
					SuperReference::Call { arguments } => {
						to_string_bracketed(arguments, ('(', ')'), buf, settings, depth);
					}
					SuperReference::PropertyAccess { property } => {
						buf.push('.');
						buf.push_str(property);
					}
					SuperReference::Index { indexer: index } => {
						buf.push('[');
						index.to_string_from_buffer(buf, settings, depth);
						buf.push(']');
					}
				}
			}
		}
	}
}

/// Represents expressions that can be `,`
#[derive(Debug, Clone, PartialEq, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize), serde(untagged))]
pub enum MultipleExpression {
	Multiple { lhs: Box<MultipleExpression>, rhs: Expression, position: Span },
	Single(Expression),
}

impl MultipleExpression {
	pub fn is_iife(&self) -> Option<&ExpressionOrBlock> {
		if let MultipleExpression::Single(inner) = self {
			inner.is_iife()
		} else {
			None
		}
	}
}

impl ASTNode for MultipleExpression {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let first = Expression::from_reader(reader, state, settings)?;
		if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
			let mut top: MultipleExpression = first.into();
			while let Some(Token(TSXToken::Comma, _)) = reader.peek() {
				reader.next();
				let rhs = Expression::from_reader(reader, state, settings)?;
				let position = top.get_position().union(rhs.get_position());
				top = MultipleExpression::Multiple { lhs: Box::new(top), rhs, position };
			}
			Ok(top)
		} else {
			Ok(MultipleExpression::Single(first))
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			MultipleExpression::Multiple { lhs, rhs, position: _ } => {
				lhs.to_string_from_buffer(buf, settings, depth);
				buf.push(',');
				rhs.to_string_from_buffer(buf, settings, depth);
			}
			MultipleExpression::Single(rhs) => {
				rhs.to_string_from_buffer(buf, settings, depth);
			}
		}
	}

	fn get_position(&self) -> &Span {
		match self {
			MultipleExpression::Multiple { position, .. } => position,
			MultipleExpression::Single(expr) => expr.get_position(),
		}
	}
}

impl From<Expression> for MultipleExpression {
	fn from(expr: Expression) -> Self {
		MultipleExpression::Single(expr)
	}
}

/// Determines whether '<' is a comparison or start of generic arguments
fn is_generic_arguments(reader: &mut impl TokenReader<TSXToken, crate::TokenStart>) -> bool {
	if !matches!(reader.peek(), Some(Token(TSXToken::OpenChevron, _))) {
		return false;
	}

	// Keep a eye on brackets. e.g. for: `if (x<4) {}` should break after the 4
	let (mut generic_depth, mut bracket_depth) = (0, 0);
	let mut final_generic_position = None::<TokenEnd>;

	let next_token = reader.scan(|token, position| {
		// Early break if logical operators
		if matches!(
			token,
			TSXToken::StrictEqual
				| TSXToken::StrictNotEqual
				| TSXToken::LogicalAnd
				| TSXToken::LogicalOr
				| TSXToken::SemiColon
				| TSXToken::QuestionMark
		) {
			true
		} else {
			match token {
				TSXToken::OpenChevron => generic_depth += 1,
				TSXToken::CloseChevron => generic_depth -= 1,
				TSXToken::BitwiseShiftRight => generic_depth -= 2,
				TSXToken::BitwiseShiftRightUnsigned => generic_depth -= 3,
				TSXToken::OpenParentheses => bracket_depth += 1,
				TSXToken::CloseParentheses => bracket_depth -= 1,
				_ => {}
			}
			if generic_depth == 0 {
				final_generic_position = Some(TokenEnd::new(
					position.0 + tokenizer_lib::sized_tokens::SizedToken::length(token),
				));
				true
			} else {
				bracket_depth < 0
			}
		}
	});
	if let (Some(last_position), Some(Token(TSXToken::OpenParentheses, open_paren_start))) =
		(final_generic_position, next_token)
	{
		last_position.is_adjacent_to(*open_paren_start)
	} else {
		false
	}
}

/// Binary operations whose RHS are types rather than [Expression]s
#[derive(PartialEqExtras, Debug, Clone, Visitable)]
#[partial_eq_ignore_types(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum SpecialOperators {
	/// TS Only
	AsExpression {
		value: Box<Expression>,
		as_keyword: Keyword<As>,
		type_annotation: Box<TypeAnnotation>,
	},
	/// TS Only
	SatisfiesExpression {
		value: Box<Expression>,
		satisfies_keyword: Keyword<Satisfies>,
		type_annotation: Box<TypeAnnotation>,
	},
	#[cfg(feature = "extras")]
	IsExpression {
		value: Box<Expression>,
		is_keyword: Keyword<tsx_keywords::Is>,
		type_annotation: Box<TypeAnnotation>,
	},
}

/// A either spread expression or not
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum SpreadExpression {
	Spread(Expression, Span),
	NonSpread(Expression),
	Empty,
}

impl ASTNode for SpreadExpression {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let peek = &reader.peek().ok_or_else(parse_lexing_error)?.0;
		match peek {
			TSXToken::Spread => {
				let start_pos = reader.next().unwrap().1;
				let expression = Expression::from_reader(reader, state, settings)?;
				let position = start_pos.union(expression.get_position());
				Ok(Self::Spread(expression, position))
			}
			TSXToken::Comma => Ok(Self::Empty),
			_ => Ok(Self::NonSpread(Expression::from_reader(reader, state, settings)?)),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			SpreadExpression::Spread(expression, _) => {
				buf.push_str("...");
				expression.to_string_from_buffer(buf, settings, depth);
			}
			SpreadExpression::NonSpread(expression) => {
				expression.to_string_from_buffer(buf, settings, depth);
			}
			SpreadExpression::Empty => {}
		}
	}

	fn get_position(&self) -> &Span {
		match self {
			SpreadExpression::Spread(_, pos) => pos,
			SpreadExpression::NonSpread(expr) => expr.get_position(),
			SpreadExpression::Empty => todo!(),
		}
	}
}

impl SpreadExpression {
	/// Only for walking
	fn _get_inner_expression_ref(&self) -> &Expression {
		match self {
			SpreadExpression::Spread(expression, _) | SpreadExpression::NonSpread(expression) => {
				expression
			}
			_ => panic!(),
		}
	}
}

impl From<Expression> for SpreadExpression {
	fn from(value: Expression) -> Self {
		SpreadExpression::NonSpread(value)
	}
}

// Utils for Expression
impl Expression {
	/// IIFE = immediate invoked function execution
	pub fn build_iife(block: Block) -> Self {
		let position = block.get_position().clone();
		Expression::FunctionCall {
			function: Expression::ParenthesizedExpression(
				Box::new(
					Expression::ArrowFunction(ArrowFunction {
						// TODO maybe async
						header: None,
						name: (),
						parameters: crate::FunctionParameters {
							parameters: Default::default(),
							rest_parameter: Default::default(),
							position: position.clone(),
						},
						return_type: None,
						type_parameters: None,
						position: position.clone(),
						body: ExpressionOrBlock::Block(block),
					})
					.into(),
				),
				position.clone(),
			)
			.into(),
			type_arguments: None,
			arguments: Vec::new(),
			is_optional: false,
			position,
		}
	}

	pub fn is_iife(&self) -> Option<&ExpressionOrBlock> {
		if let Expression::FunctionCall { arguments, function, .. } = self {
			if let (true, Expression::ParenthesizedExpression(expression, _)) =
				(arguments.is_empty(), &**function)
			{
				if let MultipleExpression::Single(Expression::ArrowFunction(function)) =
					&**expression
				{
					return Some(&function.body);
				}
			}
		}
		None
	}

	/// Recurses to find first non parenthesized expression
	pub fn get_non_parenthesized(&self) -> &Self {
		if let Expression::ParenthesizedExpression(inner_multiple_expr, _) = self {
			if let MultipleExpression::Single(expr) = &**inner_multiple_expr {
				expr.get_non_parenthesized()
			} else {
				// TODO could return a variant here...
				self
			}
		} else if let Expression::PrefixComment(_, expr, ..)
		| Expression::PostfixComment(expr, ..) = self
		{
			expr.get_non_parenthesized()
		} else {
			self
		}
	}

	/// For prettier printing
	///
	/// TODO temp
	pub fn is_small(&self) -> bool {
		match self {
			Self::NumberLiteral(..) | Self::BooleanLiteral(..) | Self::VariableReference(..) => {
				true
			}
			Self::StringLiteral(value, ..) => value.len() < 8,
			_ => false,
		}
	}
}

/// "super" cannot be used alone
#[derive(PartialEqExtras, Debug, Clone, Visitable)]
#[partial_eq_ignore_types(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum SuperReference {
	Call { arguments: Vec<SpreadExpression> },
	PropertyAccess { property: String },
	Index { indexer: Box<Expression> },
}

#[cfg(test)]
mod tests {
	use super::{ASTNode, Expression, Expression::*, MultipleExpression};
	use crate::{
		assert_matches_ast, operators::BinaryOperator, span, NumberRepresentation, Quoted, SourceId,
	};

	#[test]
	fn literal() {
		assert_matches_ast!(
			"'string'",
			StringLiteral(Deref @ "string", Quoted::Single, span!(0, 8))
		);
		assert_matches_ast!(
			"\"string\"",
			StringLiteral(Deref @ "string", Quoted::Double, span!(0, 8))
		);
		// TODO different method
		// assert_matches_ast!("45", NumberLiteral(NumberStructure::Number(45.0), span!(0, 2)));
		// assert_matches_ast!("45.63", NumberLiteral(NumberStructure::Number(45.63), span!(0, 5)));
		assert_matches_ast!("true", BooleanLiteral(true, span!(0, 4)));
	}

	#[test]
	fn parenthesized_expression() {
		// Can't match 45 here
		assert_matches_ast!(
			"(45)",
			ParenthesizedExpression(
				Deref @ MultipleExpression::Single(NumberLiteral(
					NumberRepresentation::Number { .. },
					span!(1, 3),
				)),
				span!(0, 4),
			)
		);
	}

	#[test]
	fn is_iife() {
		let expr = Expression::from_string(
			"(() => 2)()".to_owned(),
			Default::default(),
			SourceId::NULL,
			Default::default(),
		)
		.unwrap();
		assert!(expr.is_iife().is_some())
	}

	#[test]
	fn multiple_expression() {
		assert_matches_ast!(
			"(45,2)",
			ParenthesizedExpression(
				Deref @ MultipleExpression::Multiple {
					lhs:
						Deref @ MultipleExpression::Single(NumberLiteral(
							NumberRepresentation::Number { .. },
							span!(1, 3),
						)),
					rhs: NumberLiteral(NumberRepresentation::Number { .. }, span!(4, 5)),
					position: _,
				},
				span!(0, 6),
			)
		);
	}

	#[test]
	fn binary_expressions() {
		assert_matches_ast!("2 + 3", BinaryOperation {
			lhs: Deref @ NumberLiteral(NumberRepresentation::Number { .. }, span!(0, 1)),
			operator: BinaryOperator::Add,
			rhs: Deref @ NumberLiteral(NumberRepresentation::Number { .. }, span!(4, 5)),
			position: _
		});
		assert_matches_ast!("xt === 3", BinaryOperation {
			lhs: Deref @ VariableReference(..),
			operator: BinaryOperator::StrictEqual,
			rhs: Deref @ NumberLiteral(NumberRepresentation::Number { .. }, span!(7, 8)),
			position: _
		});
		assert_matches_ast!("x << 3", BinaryOperation {
			lhs: Deref @ VariableReference(..),
			operator: BinaryOperator::BitwiseShiftLeft,
			rhs: Deref @ NumberLiteral(NumberRepresentation::Number { .. }, span!(5, 6)),
			position: _
		});
		assert_matches_ast!("x >> 3", BinaryOperation {
			lhs: Deref @ VariableReference(..),
			operator: BinaryOperator::BitwiseShiftRight,
			rhs: Deref @ NumberLiteral(NumberRepresentation::Number { .. }, span!(5, 6)),
			position: _
		});
		assert_matches_ast!("x >>> 3", BinaryOperation {
			lhs: Deref @ VariableReference(..),
			operator: BinaryOperator::BitwiseShiftRightUnsigned,
			rhs: Deref @ NumberLiteral(NumberRepresentation::Number { .. }, span!(6, 7)),
			position: _
		});
	}
}
