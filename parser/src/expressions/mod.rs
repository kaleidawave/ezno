use crate::{
	errors::parse_lexing_error,
	extensions::is_expression::{is_expression_from_reader_sub_is_keyword, IsExpression},
	extractor::{ExtractedFunction, ExtractedFunctions, GetFunction},
	functions::GeneralFunctionBase,
	operators::{
		AssociativityDirection, BinaryAssignmentOperator, UnaryPostfixAssignmentOperator,
		UnaryPrefixAssignmentOperator, ASSIGNMENT_PRECEDENCE, AS_PRECEDENCE,
		FUNCTION_CALL_PRECEDENCE, OPTIONAL_CHAINING_PRECEDENCE,
	},
	parse_bracketed,
	statements::ClassDeclaration,
	to_string_bracketed,
	type_references::generic_arguments_from_reader_sub_open_angle,
	CursorId, ExpressionPosition, FunctionHeader, FunctionId, Keyword, NumberStructure,
	ParseResult, Quoted, TSXKeyword,
};

use self::{
	arrow_function::ArrowFunctionBase,
	assignments::{LHSOfAssignment, VariableOrPropertyAccess},
	object_literal::ObjectLiteral,
};

use super::{
	operators::{
		BinaryOperator, Operator, UnaryOperator, COMMA_PRECEDENCE, CONSTRUCTOR_PRECEDENCE,
		CONSTRUCTOR_WITHOUT_PARENTHESIS_PRECEDENCE, INDEX_PRECEDENCE, MEMBER_ACCESS_PRECEDENCE,
		PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE, TERNARY_PRECEDENCE,
	},
	tokens::token_as_identifier,
	ASTNode, Block, FunctionBase, JSXRoot, ParseError, ParseSettings, Span, TSXToken, Token,
	TokenReader, TypeReference,
};

use crate::tsx_keywords::{self, As, Generator, Is, Satisfies};
use derive_debug_extras::DebugExtras;
use derive_partial_eq_extras::PartialEqExtras;
use visitable_derive::Visitable;

pub mod arrow_function;
pub mod assignments;
pub mod object_literal;
pub mod template_literal;
pub use arrow_function::{ArrowFunction, ExpressionOrBlock};

pub use template_literal::{TemplateLiteral, TemplateLiteralPart};

pub type ExpressionFunctionBase = GeneralFunctionBase<ExpressionPosition>;
pub type ExpressionFunction = FunctionBase<ExpressionFunctionBase>;

use std::{
	borrow::Cow,
	convert::{TryFrom, TryInto},
	sync::atomic::{AtomicU16, Ordering},
};

static EXPRESSION_ID_COUNTER: AtomicU16 = AtomicU16::new(1);

/// Id given to AST that declares a type
/// Used for keeping track of expressions between passes
#[derive(PartialEq, Eq, Clone, Copy, DebugExtras, Hash)]
// #[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct ExpressionId(u16);

impl ExpressionId {
	pub fn new() -> Self {
		Self(EXPRESSION_ID_COUNTER.fetch_add(1, Ordering::SeqCst))
	}

	/// **Use with care**
	pub const NULL: Self = Self(0);

	/// TODO temp
	pub fn unwrap_counter(&self) -> u16 {
		self.0
	}
}

#[cfg(feature = "self-rust-tokenize")]
impl self_rust_tokenize::SelfRustTokenize for ExpressionId {
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		token_stream.extend(self_rust_tokenize::quote!(ExpressionId::NULL));
	}
}

/// Expression structures
///
/// Comma is implemented as a [BinaryOperator]
#[derive(PartialEqExtras, Debug, Clone, Visitable)]
#[partial_eq_ignore_types(Span, ExpressionId)]
#[visit_self]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum Expression {
	// Literals:
	NumberLiteral(NumberStructure, Span, ExpressionId),
	StringLiteral(String, #[partial_eq_ignore] Quoted, Span, ExpressionId),
	BooleanLiteral(bool, Span, ExpressionId),
	RegexLiteral {
		pattern: String,
		flags: Option<String>,
		position: Span,
		id: ExpressionId,
	},
	ArrayLiteral(Vec<SpreadExpression>, Span, ExpressionId),
	ObjectLiteral(ObjectLiteral),
	TemplateLiteral(TemplateLiteral),
	ParenthesizedExpression(Box<MultipleExpression>, Span, ExpressionId),
	// Regular operations:
	BinaryOperation {
		lhs: Box<Expression>,
		operator: BinaryOperator,
		rhs: Box<Expression>,
		id: ExpressionId,
	},
	SpecialOperators(SpecialOperators, Span, ExpressionId),
	UnaryOperation {
		operator: UnaryOperator,
		operand: Box<Expression>,
		position: Span,
		id: ExpressionId,
	},
	// Assignment operations
	Assignment {
		lhs: LHSOfAssignment,
		rhs: Box<Expression>,
		id: ExpressionId,
	},
	/// Modified assignment cannot have destructured thingies
	BinaryAssignmentOperation {
		lhs: VariableOrPropertyAccess,
		operator: BinaryAssignmentOperator,
		rhs: Box<Expression>,
		id: ExpressionId,
	},
	UnaryPrefixAssignmentOperation {
		operator: UnaryPrefixAssignmentOperator,
		operand: VariableOrPropertyAccess,
		position: Span,
		id: ExpressionId,
	},
	UnaryPostfixAssignmentOperation {
		operand: VariableOrPropertyAccess,
		operator: UnaryPostfixAssignmentOperator,
		position: Span,
		id: ExpressionId,
	},
	/// e.g `x` or `(...).hi`
	VariableReference(String, Span, ExpressionId),
	ThisReference(Span, ExpressionId),
	SuperExpression(SuperReference, Span, ExpressionId),
	/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target
	NewTarget(Span, ExpressionId),
	DynamicImport {
		path: Box<Expression>,
		position: Span,
		expression_id: ExpressionId,
	},
	PropertyAccess {
		parent: Box<Expression>,
		property: PropertyReference,
		position: Span,
		expression_id: ExpressionId,
		is_optional: bool,
	},
	/// e.g `...[4]`
	Index {
		indexee: Box<Expression>,
		indexer: Box<MultipleExpression>,
		position: Span,
		expression_id: ExpressionId,
	},
	// Function calls
	FunctionCall {
		function: Box<Expression>,
		type_arguments: Option<Vec<TypeReference>>,
		arguments: Vec<SpreadExpression>,
		position: Span,
		expression_id: ExpressionId,
	},
	ConstructorCall {
		constructor: Box<Expression>,
		type_arguments: Option<Vec<TypeReference>>,
		arguments: Option<Vec<SpreadExpression>>,
		position: Span,
		expression_id: ExpressionId,
	},
	/// e.g `... ? ... ? ...`
	TernaryExpression {
		condition: Box<Expression>,
		truthy_result: Box<Expression>,
		falsy_result: Box<Expression>,
		id: ExpressionId,
	},
	/// e.g `throw ...`
	/// JS treats these as statements
	Throw(Box<Expression>, Span, ExpressionId),
	// Functions
	ArrowFunction(ArrowFunction),
	ExpressionFunction(ExpressionFunction),
	ExtractedArrowFunction(ExtractedFunction<ArrowFunctionBase>),
	ExtractedExpressionFunction(ExtractedFunction<ExpressionFunctionBase>),
	/// Yes classes can exist in expr position :?
	ClassExpression(ClassDeclaration<ExpressionPosition>, ExpressionId),
	Null(Span, ExpressionId),
	// Comments
	PrefixComment(String, Box<Expression>, Span, ExpressionId),
	PostfixComment(Box<Expression>, String, Span, ExpressionId),
	/// Allowed in trailing functions and JSX for some reason
	Comment(String, Span),
	/// TODO under cfg
	/// A start of a JSXNode
	JSXRoot(JSXRoot),
	/// Not to be confused with binary operator `is`
	IsExpression(IsExpression),
	/// TODO under cfg
	#[self_tokenize_field(cursor_id)]
	Cursor {
		#[visit_skip_field]
		cursor_id: CursorId<Expression>,
		position: Span,
		expression_id: ExpressionId,
	},
}

impl Eq for Expression {}

#[derive(PartialEq, Eq, Debug, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum PropertyReference {
	Standard(String),
	#[self_tokenize_field(0)]
	Cursor(CursorId<PropertyReference>),
}

impl ASTNode for Expression {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Expression> {
		Self::from_reader_with_precedence(reader, state, settings, COMMA_PRECEDENCE)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		self.to_string_using_precedence(buf, settings, depth, COMMA_PRECEDENCE)
	}

	fn get_position(&self) -> Cow<Span> {
		match self {
			Self::BinaryOperation { lhs, rhs, .. } => {
				Cow::Owned(lhs.get_position().union(&rhs.get_position()))
			}
			Self::Assignment { lhs, rhs, .. } => {
				Cow::Owned(lhs.get_position().union(&rhs.get_position()))
			}
			Self::TernaryExpression { condition, falsy_result, .. } => {
				Cow::Owned(condition.get_position().union(&falsy_result.get_position()))
			}
			Self::BinaryAssignmentOperation { lhs, rhs, .. } => {
				Cow::Owned(lhs.get_position().union(&rhs.get_position()))
			}
			Self::NumberLiteral(_, pos, _)
			| Self::StringLiteral(_, _, pos, _)
			| Self::BooleanLiteral(_, pos, _)
			| Self::ArrayLiteral(_, pos, _)
			| Self::ParenthesizedExpression(_, pos, _)
			| Self::SpecialOperators(_, pos, _)
			| Self::UnaryOperation { position: pos, .. }
			| Self::UnaryPrefixAssignmentOperation { position: pos, .. }
			| Self::UnaryPostfixAssignmentOperation { position: pos, .. }
			| Self::VariableReference(_, pos, _)
			| Self::Index { position: pos, .. }
			| Self::Null(pos, _)
			| Self::PrefixComment(_, _, pos, _)
			| Self::PostfixComment(_, _, pos, _)
			| Self::Comment(_, pos)
			| Self::FunctionCall { position: pos, .. }
			| Self::PropertyAccess { position: pos, .. }
			| Self::ThisReference(pos, _)
			| Self::NewTarget(pos, _)
			| Self::SuperExpression(_, pos, _)
			| Self::DynamicImport { position: pos, .. }
			| Self::Throw(_, pos, _)
			| Self::ConstructorCall { position: pos, .. }
			| Self::RegexLiteral { position: pos, .. }
			| Self::Cursor { position: pos, .. } => Cow::Borrowed(pos),
			Self::JSXRoot(root) => root.get_position(),
			Self::ObjectLiteral(object_literal) => object_literal.get_position(),
			Self::TemplateLiteral(template_literal) => template_literal.get_position(),
			Self::ClassExpression(class_expression, _) => class_expression.get_position(),
			Self::IsExpression(is) => is.get_position(),
			Self::ArrowFunction(arrow_function) => arrow_function.get_position(),
			Self::ExpressionFunction(function) => function.get_position(),
			Self::ExtractedArrowFunction(ext) => ext.get_position(),
			Self::ExtractedExpressionFunction(ext) => ext.get_position(),
		}
	}
}

impl Expression {
	pub(self) fn from_reader_with_precedence(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
		return_precedence: u8,
	) -> ParseResult<Self> {
		let first_expression = match reader.next().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::Cursor(cursor_id), position) => {
				return Ok(Expression::Cursor {
					cursor_id: cursor_id.into_cursor(),
					position,
					expression_id: ExpressionId::new(),
				});
			}
			Token(TSXToken::SingleQuotedStringLiteral(expression), position) => {
				Expression::StringLiteral(expression, Quoted::Single, position, ExpressionId::new())
			}
			Token(TSXToken::DoubleQuotedStringLiteral(expression), position) => {
				Expression::StringLiteral(expression, Quoted::Double, position, ExpressionId::new())
			}
			Token(TSXToken::NumberLiteral(value), position) => Expression::NumberLiteral(
				value.parse::<NumberStructure>().unwrap(),
				position,
				ExpressionId::new(),
			),
			Token(TSXToken::RegexLiteral(pattern), mut position) => {
				let flag_token =
					reader.conditional_next(|t| matches!(t, TSXToken::RegexFlagLiteral(..)));
				let flags = if let Some(Token(TSXToken::RegexFlagLiteral(flags), flags_position)) =
					flag_token
				{
					position = position.union(&flags_position);
					Some(flags)
				} else {
					None
				};
				Expression::RegexLiteral { pattern, flags, position, id: ExpressionId::new() }
			}
			Token(TSXToken::Keyword(TSXKeyword::True), position) => {
				Expression::BooleanLiteral(true, position, ExpressionId::new())
			}
			Token(TSXToken::Keyword(TSXKeyword::False), position) => {
				Expression::BooleanLiteral(false, position, ExpressionId::new())
			}
			Token(TSXToken::Keyword(TSXKeyword::This), position) => {
				Expression::ThisReference(position, ExpressionId::new())
			}
			Token(TSXToken::Keyword(TSXKeyword::Super), position) => {
				let Token(token, modifier_pos) = reader.next().unwrap();
				let (reference, end) = match token {
					TSXToken::Dot => {
						let (property, property_pos) =
							token_as_identifier(reader.next().unwrap(), "super property")?;
						(SuperReference::PropertyAccess { property }, property_pos)
					}
					TSXToken::OpenParentheses => {
						let (arguments, end_pos) = parse_bracketed(
							reader,
							state,
							settings,
							None,
							TSXToken::CloseParentheses,
						)?;
						(SuperReference::Call { arguments }, end_pos)
					}
					TSXToken::OpenBracket => {
						let indexer = Expression::from_reader(reader, state, settings)?;
						let close_bracket_pos = reader.expect_next(TSXToken::CloseBracket)?;
						(SuperReference::Index { indexer: Box::new(indexer) }, close_bracket_pos)
					}
					token => {
						return Err(ParseError::new(
							crate::ParseErrors::UnexpectedToken {
								expected: &[
									TSXToken::Dot,
									TSXToken::OpenParentheses,
									TSXToken::OpenBracket,
								],
								found: token,
							},
							modifier_pos,
						));
					}
				};
				Expression::SuperExpression(reference, position.union(&end), ExpressionId::new())
			}
			Token(TSXToken::Keyword(TSXKeyword::Null), position) => {
				Expression::Null(position, ExpressionId::new())
			}
			Token(TSXToken::Keyword(TSXKeyword::Class), position) => {
				let keyword = Keyword(tsx_keywords::Class, position);
				let class_declaration = ClassDeclaration::from_reader_sub_class_keyword(
					reader, state, settings, keyword,
				)?;
				Expression::ClassExpression(class_declaration, ExpressionId::new())
			}
			Token(TSXToken::OpenBracket, start_pos) => {
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
						Expression::Assignment {
							lhs: LHSOfAssignment::ArrayDestructuring(
								members,
								start_pos.union(&end_span),
								ExpressionId::new(),
							),
							rhs: Box::new(rhs),
							id: ExpressionId::new(),
						}
					} else {
						let (items, end_pos) =
							parse_bracketed(reader, state, settings, None, TSXToken::CloseBracket)?;
						Expression::ArrayLiteral(
							items,
							start_pos.union(&end_pos),
							ExpressionId::new(),
						)
					}
				} else {
					return Err(ParseError::new(crate::ParseErrors::UnmatchedBrackets, start_pos));
				}
			}
			Token(TSXToken::OpenBrace, start_pos) => {
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
						let (members, end_span) =
							parse_bracketed(reader, state, settings, None, TSXToken::CloseBrace)?;
						reader.next();
						let rhs = Expression::from_reader_with_precedence(
							reader,
							state,
							settings,
							ASSIGNMENT_PRECEDENCE,
						)?;
						Expression::Assignment {
							lhs: LHSOfAssignment::ObjectDestructuring(
								members,
								start_pos.union(&end_span),
								ExpressionId::new(),
							),
							rhs: Box::new(rhs),
							id: ExpressionId::new(),
						}
					} else {
						let object_literal = ObjectLiteral::from_reader_sub_open_curly(
							reader, state, settings, start_pos,
						)?;
						Expression::ObjectLiteral(object_literal)
					}
				} else {
					return Err(ParseError::new(crate::ParseErrors::UnmatchedBrackets, start_pos));
				}
			}
			Token(TSXToken::OpenParentheses, open_paren_span) => {
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
							reader,
							state,
							settings,
							None,
							open_paren_span,
						)?;
						let id = state.function_extractor.new_extracted_function(arrow_function);
						Expression::ExtractedArrowFunction(id)
					} else {
						let parenthesize_expression =
							MultipleExpression::from_reader(reader, state, settings)?;
						let close_paren_span = reader.expect_next(TSXToken::CloseParentheses)?;
						Expression::ParenthesizedExpression(
							Box::new(parenthesize_expression),
							open_paren_span.union(&close_paren_span),
							ExpressionId::new(),
						)
					}
				} else {
					return Err(ParseError::new(
						crate::ParseErrors::UnmatchedBrackets,
						open_paren_span,
					));
				}
			}
			Token(TSXToken::Keyword(TSXKeyword::New), new_pos) => {
				if let Some(Token(TSXToken::Dot, _)) = reader.peek() {
					// TODO assert not lonely, else syntax error
					reader.expect_next(TSXToken::Dot)?;
					reader.expect_next(TSXToken::IdentLiteral("target".into()))?;
					Expression::NewTarget(new_pos, ExpressionId::new())
				} else {
					// Pass as a function call and then adds the conversion
					let constructor_expression = Self::from_reader_with_precedence(
						reader,
						state,
						settings,
						FUNCTION_CALL_PRECEDENCE,
					)?;
					let position = new_pos.union(&constructor_expression.get_position());
					let (type_arguments, end_pos) = if reader
						.conditional_next(|token| *token == TSXToken::OpenChevron)
						.is_some()
					{
						let (generic_arguments, end_pos) =
							parse_bracketed(reader, state, settings, None, TSXToken::CloseChevron)?;
						(Some(generic_arguments), end_pos)
					} else {
						(None, position)
					};
					let (arguments, end_pos) = if reader
						.conditional_next(|token| *token == TSXToken::OpenChevron)
						.is_some()
					{
						parse_bracketed(reader, state, settings, None, TSXToken::CloseParentheses)
							.map(|(args, end_span)| (Some(args), end_span))?
					} else {
						// TODO are type arguments not allowed...?
						(None, end_pos)
					};
					Expression::ConstructorCall {
						constructor: constructor_expression.into(),
						type_arguments,
						arguments,
						position: new_pos.union(&end_pos),
						expression_id: ExpressionId::new(),
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
			Token(TSXToken::MultiLineComment(comment) | TSXToken::Comment(comment), start_pos) => {
				if let Some(Token(TSXToken::CloseParentheses | TSXToken::JSXExpressionEnd, _)) =
					reader.peek()
				{
					return Ok(Expression::Comment(comment, start_pos));
				}
				let expression =
					Self::from_reader_with_precedence(reader, state, settings, return_precedence)?;
				let position = start_pos.union(&expression.get_position());
				Expression::PrefixComment(
					comment,
					Box::new(expression),
					position,
					ExpressionId::new(),
				)
			}
			Token(tok @ TSXToken::JSXOpeningTagStart | tok @ TSXToken::JSXFragmentStart, span) => {
				let var_name = matches!(tok, TSXToken::JSXFragmentStart);
				let root = JSXRoot::from_reader_sub_start(reader, state, settings, var_name, span)?;
				Expression::JSXRoot(root)
			}
			Token(TSXToken::TemplateLiteralStart, start_pos) => {
				return TemplateLiteral::from_reader_sub_start_with_tag(
					reader, state, settings, None, start_pos,
				)
				.map(Expression::TemplateLiteral);
			}
			Token(TSXToken::Keyword(TSXKeyword::Throw), start_pos) => {
				let expression = Expression::from_reader(reader, state, settings)?;
				let position = start_pos.union(&expression.get_position());
				return Ok(Expression::Throw(expression.into(), position, ExpressionId::new()));
			}
			Token(TSXToken::Keyword(TSXKeyword::Function), span) => {
				let header = FunctionHeader::VirginFunctionHeader {
					async_keyword: None,
					function_keyword: Keyword::new(span),
					// TODO
					generator_star_token_pos: None,
				};
				let name = if let Some(Token(TSXToken::OpenParentheses, _)) = reader.peek() {
					None
				} else {
					let (token, span) =
						token_as_identifier(reader.next().unwrap(), "function name")?;
					Some(crate::VariableIdentifier::Standard(token, crate::VariableId::new(), span))
				};
				let expression_function: ExpressionFunction =
					FunctionBase::from_reader_with_header_and_name(
						reader, state, settings, header, name,
					)?;
				let id = state.function_extractor.new_extracted_function(expression_function);
				Expression::ExtractedExpressionFunction(id)
			}
			// TODO this should be extracted to a function that allows it to also work for leading `generator`
			Token(TSXToken::Keyword(TSXKeyword::Async), async_span) => {
				let async_keyword = Some(Keyword::new(async_span));
				let generator_keyword: Option<Keyword<Generator>> = settings
					.generator_keyword
					.then(|| {
						reader
							.conditional_next(|tok| {
								*tok == TSXToken::Keyword(TSXKeyword::Generator)
							})
							.map(|Token(_, span)| Keyword::new(span))
					})
					.flatten();

				match reader.next().ok_or_else(parse_lexing_error)? {
					Token(TSXToken::Keyword(TSXKeyword::Function), function_span) => {
						let header = if generator_keyword.is_some() {
							FunctionHeader::ChadFunctionHeader {
								async_keyword,
								generator_keyword,
								function_keyword: Keyword::new(function_span),
							}
						} else {
							let generator_star_token_pos = reader
								.conditional_next(|tok| *tok == TSXToken::Multiply)
								.map(|Token(_, pos)| pos);

							FunctionHeader::VirginFunctionHeader {
								async_keyword,
								function_keyword: Keyword::new(function_span),
								generator_star_token_pos,
							}
						};
						let name = if let Some(Token(TSXToken::OpenParentheses, _)) = reader.peek()
						{
							None
						} else {
							let (token, span) =
								token_as_identifier(reader.next().unwrap(), "function name")?;
							Some(crate::VariableIdentifier::Standard(
								token,
								crate::VariableId::new(),
								span,
							))
						};
						let expression_function: ExpressionFunction =
							FunctionBase::from_reader_with_header_and_name(
								reader, state, settings, header, name,
							)?;
						let id =
							state.function_extractor.new_extracted_function(expression_function);

						Expression::ExtractedExpressionFunction(id)
					}
					Token(TSXToken::OpenParentheses, start_pos) => {
						assert!(generator_keyword.is_none(), "TODO");
						let arrow_function = ArrowFunction::from_reader_sub_open_paren(
							reader,
							state,
							settings,
							async_keyword,
							start_pos,
						)?;
						let id = state.function_extractor.new_extracted_function(arrow_function);
						Expression::ExtractedArrowFunction(id)
					}
					Token(token, position) => {
						return Err(ParseError::new(
							crate::ParseErrors::UnexpectedToken {
								expected: &[
									TSXToken::Keyword(TSXKeyword::Function),
									TSXToken::OpenParentheses,
								],
								found: token,
							},
							position,
						))
					}
				}
			}
			#[cfg(feature = "extras")]
			Token(TSXToken::Keyword(TSXKeyword::Is), span) => is_expression_from_reader_sub_is_keyword(
				reader,
				state,
				settings,
				Keyword::new(span),
			)
			.map(Expression::IsExpression)?,
			token => {
				if let Ok(unary_operator) = UnaryOperator::try_from(&token.0) {
					let op_precedence = unary_operator.precedence();
					let operand =
						Self::from_reader_with_precedence(reader, state, settings, op_precedence)?;
					let position = token.1.union(&operand.get_position());
					Expression::UnaryOperation {
						operand: Box::new(operand),
						operator: unary_operator,
						position,
						id: ExpressionId::new(),
					}
				} else if let Ok(unary_prefix_operator) =
					UnaryPrefixAssignmentOperator::try_from(&token.0)
				{
					// TODO is precedence needed...?
					// let op_precedence = unary_prefix_operator.precedence();
					let operand = VariableOrPropertyAccess::from_reader(reader, state, settings)?;
					let position = token.1.union(&operand.get_position());
					Expression::UnaryPrefixAssignmentOperation {
						operand,
						operator: unary_prefix_operator,
						position,
						id: ExpressionId::new(),
					}
				} else {
					let (name, position) = token_as_identifier(token, "variable reference")?;
					if let Some(Token(TSXToken::Arrow, _)) = reader.peek() {
						let arrow_function = ArrowFunction::from_reader_with_first_parameter(
							reader,
							state,
							settings,
							(name, position),
						)?;
						let id = state.function_extractor.new_extracted_function(arrow_function);
						Expression::ExtractedArrowFunction(id)
					} else {
						Expression::VariableReference(name, position, ExpressionId::new())
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
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
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
						.should_return(parent_precedence, TERNARY_PRECEDENCE)
					{
						return Ok(top);
					}
					reader.next();
					let lhs = Self::from_reader(reader, state, settings)?;
					reader.expect_next(TSXToken::Colon)?;
					let rhs = Self::from_reader(reader, state, settings)?;
					top = Expression::TernaryExpression {
						condition: Box::new(top),
						truthy_result: Box::new(lhs),
						falsy_result: Box::new(rhs),
						id: ExpressionId::new(),
					};
				}
				TSXToken::OpenParentheses => {
					if AssociativityDirection::LeftToRight
						.should_return(parent_precedence, FUNCTION_CALL_PRECEDENCE)
					{
						return Ok(top);
					}
					reader.next();
					let (arguments, end_pos) =
						parse_bracketed(reader, state, settings, None, TSXToken::CloseParentheses)?;
					let position = top.get_position().union(&end_pos);
					top = Expression::FunctionCall {
						function: Box::new(top),
						type_arguments: None,
						arguments,
						position,
						expression_id: ExpressionId::new(),
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
					let end_position = reader.expect_next(TSXToken::CloseBracket)?;
					let position = top.get_position().union(&end_position);
					top = Expression::Index {
						position,
						indexee: Box::new(top),
						indexer: Box::new(indexer),
						expression_id: ExpressionId::new(),
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
					let start_pos = top.get_position().into_owned();
					let tag = Some(Box::new(top));
					let template_lit = TemplateLiteral::from_reader_sub_start_with_tag(
						reader, state, settings, tag, start_pos,
					);
					top = template_lit.map(Expression::TemplateLiteral)?;
				}
				dot @ TSXToken::Dot | dot @ TSXToken::OptionalChain => {
					let is_optional = matches!(dot, TSXToken::OptionalChain);
					if AssociativityDirection::LeftToRight.should_return(
						parent_precedence,
						if is_optional {
							OPTIONAL_CHAINING_PRECEDENCE
						} else {
							MEMBER_ACCESS_PRECEDENCE
						},
					) {
						return Ok(top);
					}
					let _ = reader.next().unwrap();
					let token = reader.next().ok_or_else(parse_lexing_error)?;
					let (property, position) =
						if let Token(TSXToken::Cursor(cursor_id), position) = token {
							(PropertyReference::Cursor(cursor_id.into_cursor()), position)
						} else {
							let (property, pos) = token_as_identifier(token, "variable reference")?;
							(PropertyReference::Standard(property), pos)
						};
					let position = top.get_position().union(&position);
					top = Expression::PropertyAccess {
						parent: Box::new(top),
						property,
						position,
						expression_id: ExpressionId::new(),
						is_optional,
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
						lhs: LHSOfAssignment::VariableOrPropertyAccess(top.try_into()?),
						rhs: Box::new(new_rhs),
						id: ExpressionId::new(),
					};
				}
				TSXToken::MultiLineComment(_) | TSXToken::Comment(_) => {
					let Token(
						TSXToken::MultiLineComment(comment) | TSXToken::Comment(comment),
						position,
					) = reader.next().unwrap() else { unreachable!() } ;
					top = Expression::PostfixComment(
						Box::new(top),
						comment,
						position,
						ExpressionId::new(),
					);
				}
				TSXToken::Keyword(TSXKeyword::As | TSXKeyword::Satisfies | TSXKeyword::Is) => {
					if AssociativityDirection::LeftToRight
						.should_return(parent_precedence, AS_PRECEDENCE)
					{
						return Ok(top);
					}

					let Token(token, keyword_pos) = reader.next().unwrap();
					let reference = TypeReference::from_reader(reader, state, settings)?;
					let position = top.get_position().union(&reference.get_position());

					let special_operators = match token {
						TSXToken::Keyword(TSXKeyword::As) => SpecialOperators::AsExpression {
							value: top.into(),
							as_keyword: Keyword::new(keyword_pos),
							type_annotation: Box::new(reference),
						},
						TSXToken::Keyword(TSXKeyword::Is) => SpecialOperators::IsExpression {
							value: top.into(),
							is_keyword: Keyword::new(keyword_pos),
							type_annotation: Box::new(reference),
						},
						TSXToken::Keyword(TSXKeyword::Satisfies) => {
							SpecialOperators::SatisfiesExpression {
								value: top.into(),
								satisfies_keyword: Keyword::new(keyword_pos),
								type_annotation: Box::new(reference),
							}
						}
						_ => unreachable!(),
					};
					top = Self::SpecialOperators(special_operators, position, ExpressionId::new());
				}
				token => {
					let token = if *token == TSXToken::OpenChevron {
						if is_generic_arguments(reader) {
							let _ = reader.next();
							let (type_arguments, _) = generic_arguments_from_reader_sub_open_angle(
								reader, state, settings, false,
							)?;
							let (arguments, end_pos) = parse_bracketed(
								reader,
								state,
								settings,
								Some(TSXToken::OpenParentheses),
								TSXToken::CloseParentheses,
							)?;
							top = Expression::FunctionCall {
								position: top.get_position().union(&end_pos),
								function: Box::new(top),
								type_arguments: Some(type_arguments),
								arguments,
								expression_id: ExpressionId::new(),
							};
							continue;
						} else {
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
						let Token(_, pos) = reader.next().unwrap();

						// Increment and decrement are the only two postfix operations
						let position = top.get_position().union(&pos);
						top = Expression::UnaryPostfixAssignmentOperation {
							operand: top.try_into()?,
							operator,
							position,
							id: ExpressionId::new(),
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
							parent_precedence,
						)?;

						top = Expression::BinaryOperation {
							lhs: Box::new(top),
							operator,
							rhs: Box::new(rhs),
							id: ExpressionId::new(),
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
							parent_precedence,
						)?;
						top = Expression::BinaryAssignmentOperation {
							lhs: top.try_into()?,
							operator,
							rhs: Box::new(new_rhs),
							id: ExpressionId::new(),
						};
					} else {
						// debug_assert!(
						// 	matches!(
						// 		token,
						// 		TSXToken::EOS
						// 			| TSXToken::CloseParentheses | TSXToken::CloseBracket
						// 			| TSXToken::CloseBrace,
						// 			| TSXToken::Colon,
						// 	),
						// 	"expected a closing expression, found {:?}",
						// 	token
						// );
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
            | Self::ExtractedArrowFunction(..)
            | Self::ExtractedExpressionFunction(..)
            | Self::Null(..)
            | Self::ObjectLiteral(..)
            | Self::Throw(..)
            | Self::VariableReference(..)
            | Self::ThisReference(..)
            | Self::SuperExpression(..)
            | Self::NewTarget(..)
            | Self::ClassExpression(..)
            // TODO not sure about this one...?
            | Self::IsExpression(..)
            // TODO not sure about this one...?
            | Self::DynamicImport { .. }
            | Self::Cursor { .. } => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE, // TODO think this is true <-
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
            Self::TernaryExpression { .. } => TERNARY_PRECEDENCE,
            Self::PrefixComment(_, expression, _, _) | Self::PostfixComment(expression, _, _, _) => {
                expression.get_precedence()
            }
            Self::Comment(_, _) => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE, // TODO not sure about this
            Self::SpecialOperators(op, _, _) => match op {
				 // TODO not sure about this
                SpecialOperators::AsExpression { .. } => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE,
                SpecialOperators::IsExpression { .. } => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE,
                SpecialOperators::SatisfiesExpression { .. } => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE,
            },
        }
	}

	pub(crate) fn to_string_using_precedence<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
		_parent_precedence: u8,
	) {
		match self {
			Self::Cursor { .. } => {
				if !settings.0.expect_cursors {
					panic!();
				}
			}
			Self::NumberLiteral(num, _, _) => buf.push_str(&num.to_string()),
			Self::StringLiteral(string, quoted, _, _) => {
				buf.push(quoted.as_char());
				buf.push_str(string);
				buf.push(quoted.as_char());
			}
			Self::BooleanLiteral(expression, _, _) => {
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
				settings.0.add_gap(buf);
				buf.push_str(operator.to_str());
				settings.0.add_gap(buf);
				rhs.to_string_using_precedence(buf, settings, depth, op_precedence);
			}
			Self::SpecialOperators(special, _, _) => match special {
				SpecialOperators::AsExpression { value, type_annotation, .. }
				| SpecialOperators::IsExpression { value, type_annotation, .. }
				| SpecialOperators::SatisfiesExpression { value, type_annotation, .. } => {
					value.to_string_from_buffer(buf, settings, depth);
					// TODO is
					if settings.0.include_types {
						buf.push_str(match special {
							SpecialOperators::AsExpression { .. } => " as ",
							SpecialOperators::IsExpression { .. } => " is ",
							SpecialOperators::SatisfiesExpression { .. } => " satisfies ",
						});
						type_annotation.to_string_from_buffer(buf, settings, depth);
					}
				}
			},
			Self::UnaryOperation { operand, operator, .. } => {
				buf.push_str(operator.to_str());
				operand.to_string_from_buffer(buf, settings, depth);
			}
			Self::Assignment { lhs, rhs, .. } => {
				lhs.to_string_from_buffer(buf, settings, depth);
				buf.push_str(if settings.0.pretty { " = " } else { "=" });
				rhs.to_string_from_buffer(buf, settings, depth)
			}
			Self::BinaryAssignmentOperation { lhs, operator, rhs, .. } => {
				lhs.to_string_from_buffer(buf, settings, depth);
				settings.0.add_gap(buf);
				buf.push_str(operator.to_str());
				settings.0.add_gap(buf);
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
			Self::VariableReference(name, _position, _) => {
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
				} else if !settings.0.expect_cursors {
					panic!("found cursor");
				}
			}
			Self::ParenthesizedExpression(expr, _, _) => {
				buf.push('(');
				expr.to_string_from_buffer(buf, settings, depth);
				buf.push(')');
			}
			Self::Index { indexee: expression, indexer, .. } => {
				expression.to_string_from_buffer(buf, settings, depth);
				buf.push('[');
				indexer.to_string_from_buffer(buf, settings, depth);
				buf.push(']');
			}
			Self::FunctionCall { function, type_arguments, arguments, .. } => {
				if let Some(ExpressionOrBlock::Expression(expression)) = self.is_iife(&settings.1) {
					expression.to_string_from_buffer(buf, settings, depth);
					return;
				}
				function.to_string_from_buffer(buf, settings, depth);
				if let (true, Some(type_arguments)) = (settings.0.include_types, type_arguments) {
					to_string_bracketed(type_arguments, ('<', '>'), buf, settings, depth);
				}
				to_string_bracketed(arguments, ('(', ')'), buf, settings, depth);
			}
			Self::ConstructorCall { constructor, type_arguments, arguments, .. } => {
				buf.push_str("new ");
				constructor.to_string_from_buffer(buf, settings, depth);
				if let (true, Some(type_arguments)) = (settings.0.include_types, type_arguments) {
					to_string_bracketed(type_arguments, ('<', '>'), buf, settings, depth);
				}
				if let Some(arguments) = arguments {
					// Constructor calls can drop arguments if none
					if !arguments.is_empty() {
						to_string_bracketed(arguments, ('(', ')'), buf, settings, depth);
					}
				}
			}
			Self::ArrayLiteral(values, _, _) => {
				to_string_bracketed(values, ('[', ']'), buf, settings, depth);
			}
			Self::JSXRoot(root) => root.to_string_from_buffer(buf, settings, depth),
			Self::Throw(thrown_expression, _, _) => {
				buf.push_str("throw ");
				thrown_expression.to_string_from_buffer(buf, settings, depth);
			}
			Self::ObjectLiteral(object_literal) => {
				object_literal.to_string_from_buffer(buf, settings, depth)
			}
			Self::ArrowFunction(arrow_function) => {
				arrow_function.to_string_from_buffer(buf, settings, depth)
			}
			Self::ExpressionFunction(function) => {
				function.to_string_from_buffer(buf, settings, depth);
			}
			Self::ExtractedArrowFunction(function) => {
				if let Some(func) =
					GetFunction::<ArrowFunctionBase>::get_function_ref(&settings.1, function.0)
				{
					func.to_string_from_buffer(buf, settings, depth);
				} else {
					// TODO not the same, but oh well
					buf.push_str("null");
				}
			}
			Self::ExtractedExpressionFunction(function) => {
				if let Some(func) =
					GetFunction::<ExpressionFunctionBase>::get_function_ref(&settings.1, function.0)
				{
					func.to_string_from_buffer(buf, settings, depth);
				} else {
					// TODO not the same, but oh well
					buf.push_str("null");
				}
			}
			Self::ClassExpression(class, _) => class.to_string_from_buffer(buf, settings, depth),
			Self::PrefixComment(comment, expression, _, _) => {
				if settings.0.should_add_comment() {
					buf.push_str("/*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/ ");
				}
				expression.to_string_from_buffer(buf, settings, depth);
			}
			Self::PostfixComment(expression, comment, _, _) => {
				expression.to_string_from_buffer(buf, settings, depth);
				if settings.0.should_add_comment() {
					buf.push_str(" /*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/");
				}
			}
			Self::Comment(comment, _) => {
				if settings.0.should_add_comment() {
					buf.push_str("/*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/");
				}
			}
			Self::TemplateLiteral(template_literal) => {
				template_literal.to_string_from_buffer(buf, settings, depth)
			}
			Self::TernaryExpression { condition, truthy_result, falsy_result, .. } => {
				condition.to_string_using_precedence(buf, settings, depth, TERNARY_PRECEDENCE);
				buf.push_str(if settings.0.pretty { " ? " } else { "?" });
				truthy_result.to_string_using_precedence(buf, settings, depth, TERNARY_PRECEDENCE);
				buf.push_str(if settings.0.pretty { " : " } else { ":" });
				falsy_result.to_string_using_precedence(buf, settings, depth, TERNARY_PRECEDENCE);
			}
			Self::Null(..) => buf.push_str("null"),
			Self::IsExpression(is_expr) => is_expr.to_string_from_buffer(buf, settings, depth),
			Self::SuperExpression(super_expr, _, _) => {
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
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct MultipleExpression {
	pub lhs: Option<Box<MultipleExpression>>,
	pub rhs: Expression,
}

impl MultipleExpression {
	pub fn is_iife<'a>(&self, functions: &'a ExtractedFunctions) -> Option<&'a ExpressionOrBlock> {
		if self.lhs.is_none() {
			self.rhs.is_iife(functions)
		} else {
			None
		}
	}
}

impl ASTNode for MultipleExpression {
	fn get_position(&self) -> Cow<Span> {
		if let Some(ref lhs) = self.lhs {
			Cow::Owned(lhs.get_position().union(&self.rhs.get_position()))
		} else {
			self.rhs.get_position()
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let first = Expression::from_reader(reader, state, settings)?;
		let mut top: MultipleExpression = first.into();
		while let Some(Token(TSXToken::Comma, _)) = reader.peek() {
			reader.next();
			let rhs = Expression::from_reader(reader, state, settings)?;
			top = MultipleExpression { lhs: Some(Box::new(top)), rhs };
		}
		Ok(top)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		if let Some(ref lhs) = self.lhs {
			buf.push(',');
			lhs.to_string_from_buffer(buf, settings, depth);
		}
		self.rhs.to_string_from_buffer(buf, settings, depth);
	}
}

impl From<Expression> for MultipleExpression {
	fn from(expr: Expression) -> Self {
		Self { lhs: None, rhs: expr }
	}
}

/// Determines whether '<' is a comparison or start of generic arguments
fn is_generic_arguments(reader: &mut impl TokenReader<TSXToken, Span>) -> bool {
	if !matches!(reader.peek(), Some(Token(TSXToken::OpenChevron, _))) {
		return false;
	}

	// Keep a eye on brackets. e.g. for: `if (x<4) {}` should break after the 4
	let (mut generic_depth, mut bracket_depth) = (0, 0);
	let mut final_generic_position = None::<Span>;

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
				final_generic_position = Some(position.clone());
				true
			} else {
				bracket_depth < 0
			}
		}
	});
	if let (Some(last_position), Some(Token(TSXToken::OpenParentheses, position))) =
		(final_generic_position, next_token)
	{
		last_position.is_adjacent_to(position)
	} else {
		false
	}
}

/// Binary operations whose RHS are types rather than [Expression]s
#[derive(PartialEqExtras, Debug, Clone, Visitable)]
#[partial_eq_ignore_types(Span, ExpressionId)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum SpecialOperators {
	/// TS Only
	AsExpression {
		value: Box<Expression>,
		as_keyword: Keyword<As>,
		type_annotation: Box<TypeReference>,
	},
	/// TS Only
	SatisfiesExpression {
		value: Box<Expression>,
		satisfies_keyword: Keyword<Satisfies>,
		type_annotation: Box<TypeReference>,
	},
	#[cfg(feature = "extras")]
	IsExpression {
		value: Box<Expression>,
		is_keyword: Keyword<Is>,
		type_annotation: Box<TypeReference>,
	},
}

/// A either spread expression or not
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum SpreadExpression {
	Spread(Expression, Span),
	NonSpread(Expression),
	Empty,
}

impl ASTNode for SpreadExpression {
	fn get_position(&self) -> Cow<Span> {
		match self {
			SpreadExpression::Spread(_, pos) => Cow::Borrowed(pos),
			SpreadExpression::NonSpread(ast) => ast.get_position(),
			SpreadExpression::Empty => todo!(),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let peek = &reader.peek().ok_or_else(parse_lexing_error)?.0;
		match peek {
			TSXToken::Spread => {
				let start_pos = reader.next().unwrap().1;
				let expression = Expression::from_reader(reader, state, settings)?;
				let position = start_pos.union(&expression.get_position());
				Ok(Self::Spread(expression, position))
			}
			TSXToken::Comma => {
				todo!("EMpty?");
			}
			_ => Ok(Self::NonSpread(Expression::from_reader(reader, state, settings)?)),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
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
}

impl SpreadExpression {
	/// TODO Remover for get_inner_expression
	pub fn get_expression_id(&self) -> Option<ExpressionId> {
		match self {
			SpreadExpression::Spread(expression, _) | SpreadExpression::NonSpread(expression) => {
				expression.get_expression_id()
			}
			_ => panic!(),
		}
	}

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
	pub fn get_expression_id(&self) -> Option<ExpressionId> {
		match self {
			Self::NumberLiteral(.., id)
			| Self::StringLiteral(.., id)
			| Self::BooleanLiteral(.., id)
			| Self::RegexLiteral { id, .. }
			| Self::ArrayLiteral(_, _, id)
			| Self::ParenthesizedExpression(_, _, id)
			| Self::BinaryOperation { id, .. }
			| Self::Assignment { id, .. }
			| Self::BinaryAssignmentOperation { id, .. }
			| Self::UnaryOperation { id, .. }
			| Self::UnaryPrefixAssignmentOperation { id, .. }
			| Self::UnaryPostfixAssignmentOperation { id, .. }
			| Self::VariableReference(_, _, id)
			| Self::FunctionCall { expression_id: id, .. }
			| Self::ConstructorCall { expression_id: id, .. }
			| Self::Index { expression_id: id, .. }
			| Self::TernaryExpression { id, .. }
			| Self::Null(_, id)
			| Self::SuperExpression(_, _, id)
			| Self::ThisReference(_, id)
			| Self::NewTarget(_, id)
			| Self::TemplateLiteral(TemplateLiteral { expression_id: id, .. })
			| Self::Throw(_, _, id)
			| Self::DynamicImport { expression_id: id, .. }
			| Self::ClassExpression(_, id)
			| Self::PrefixComment(_, _, _, id)
			| Self::PostfixComment(_, _, _, id)
			| Self::Cursor { expression_id: id, .. }
			| Self::SpecialOperators(_, _, id)
			| Self::PropertyAccess { expression_id: id, .. } => Some(*id),
			Self::JSXRoot(root) => Some(root.get_expression_id()),
			Self::ObjectLiteral(object_literal) => Some(object_literal.get_expression_id()),
			Self::IsExpression(is_expr) => Some(is_expr.expression_id),
			Self::Comment(..) => None,
			Self::ExpressionFunction(..) | Self::ArrowFunction(..) => None,
			Self::ExtractedArrowFunction(..) => None,
			Self::ExtractedExpressionFunction(..) => None,
		}
	}

	/// IIFE = immediate invoked function execution
	pub fn build_iife(block: Block) -> Self {
		let position = block.get_position().into_owned();
		Expression::FunctionCall {
			function: Expression::ParenthesizedExpression(
				Box::new(
					Expression::ArrowFunction(ArrowFunction {
						// TODO maybe async
						header: None,
						name: (),
						parameters: crate::FunctionParameters {
							parameters: Default::default(),
							optional_parameters: Default::default(),
							rest_parameter: Default::default(),
							position: position.clone(),
						},
						return_type: None,
						type_parameters: None,
						body: ExpressionOrBlock::Block(block),
						function_id: FunctionId::new(),
					})
					.into(),
				),
				position.clone(),
				ExpressionId::new(),
			)
			.into(),
			type_arguments: None,
			arguments: Vec::new(),
			position,
			expression_id: ExpressionId::new(),
		}
	}

	pub fn is_iife<'a>(&self, functions: &'a ExtractedFunctions) -> Option<&'a ExpressionOrBlock> {
		if let Expression::FunctionCall { arguments, function, .. } = self {
			if let (true, Expression::ParenthesizedExpression(expression, _, _)) =
				(arguments.is_empty(), &**function)
			{
				if let MultipleExpression {
					lhs: None,
					rhs: Expression::ExtractedArrowFunction(arrow_function),
				} = &**expression
				{
					return Some(&functions.get_function_ref(arrow_function.0).unwrap().body);
				}
			}
		}
		None
	}

	/// Recurses to find first non parenthesized expression
	pub fn get_non_parenthesized(&self) -> &Self {
		if let Expression::ParenthesizedExpression(inner_multiple_expr, _, _) = self {
			if inner_multiple_expr.lhs.is_none() {
				inner_multiple_expr.rhs.get_non_parenthesized()
			} else {
				// TODO could return something here...
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
#[partial_eq_ignore_types(Span, ExpressionId)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum SuperReference {
	Call { arguments: Vec<SpreadExpression> },
	PropertyAccess { property: String },
	Index { indexer: Box<Expression> },
}

#[cfg(test)]
mod tests {
	use super::{ASTNode, Expression, Expression::*, MultipleExpression};
	use crate::{
		assert_matches_ast, operators::BinaryOperator, span, NumberStructure, ParseOutput, Quoted,
		SourceId,
	};

	#[test]
	fn literal() {
		assert_matches_ast!(
			"'string'",
			StringLiteral(Deref @ "string", Quoted::Single, span!(0, 8), _)
		);
		assert_matches_ast!(
			"\"string\"",
			StringLiteral(Deref @ "string", Quoted::Double, span!(0, 8), _)
		);
		// TODO different method
		// assert_matches_ast!("45", NumberLiteral(NumberStructure::Number(45.0), span!(0, 2), _));
		// assert_matches_ast!("45.63", NumberLiteral(NumberStructure::Number(45.63), span!(0, 5), _));
		assert_matches_ast!("true", BooleanLiteral(true, span!(0, 4), _));
	}

	#[test]
	fn parenthesized_expression() {
		assert_matches_ast!(
			"(45)",
			ParenthesizedExpression(
				Deref @ MultipleExpression {
					lhs: None,
					rhs: NumberLiteral(NumberStructure::Number(_), span!(1, 3), _),
				},
				span!(0, 4),
				_,
			)
		);
	}

	#[test]
	fn is_iife() {
		let ParseOutput(expr, state) = Expression::from_string(
			"(() => 2)()".to_owned(),
			Default::default(),
			SourceId::NULL,
			Default::default(),
			Default::default(),
		)
		.unwrap();
		assert!(expr.is_iife(&state.function_extractor).is_some())
	}

	#[test]
	fn multiple_expression() {
		assert_matches_ast!(
			"(45,2)",
			ParenthesizedExpression(
				Deref @ MultipleExpression {
					lhs:
						Some(
							Deref @ MultipleExpression {
								lhs: None,
								rhs: NumberLiteral(NumberStructure::Number(_), span!(1, 3), _),
							},
						),
					rhs: NumberLiteral(NumberStructure::Number(_), span!(4, 5), _),
				},
				span!(0, 6),
				_,
			)
		);
	}

	#[test]
	fn binary_expressions() {
		assert_matches_ast!("2 + 3", BinaryOperation {
			lhs: Deref @ NumberLiteral(NumberStructure::Number(_), span!(0, 1), _),
			operator: BinaryOperator::Add,
			rhs: Deref @ NumberLiteral(NumberStructure::Number(_), span!(4, 5), _),
			id: _
		});
		assert_matches_ast!("xt === 3", BinaryOperation {
			lhs: Deref @ VariableReference(..),
			operator: BinaryOperator::StrictEqual,
			rhs: Deref @ NumberLiteral(NumberStructure::Number(_), span!(7, 8), _),
			id: _
		});
		assert_matches_ast!("x << 3", BinaryOperation {
			lhs: Deref @ VariableReference(..),
			operator: BinaryOperator::BitwiseShiftLeft,
			rhs: Deref @ NumberLiteral(NumberStructure::Number(_), span!(5, 6), _),
			id: _
		});
		assert_matches_ast!("x >> 3", BinaryOperation {
			lhs: Deref @ VariableReference(..),
			operator: BinaryOperator::BitwiseShiftRight,
			rhs: Deref @ NumberLiteral(NumberStructure::Number(_), span!(5, 6), _),
			id: _
		});
		assert_matches_ast!("x >>> 3", BinaryOperation {
			lhs: Deref @ VariableReference(..),
			operator: BinaryOperator::BitwiseShiftRightUnsigned,
			rhs: Deref @ NumberLiteral(NumberStructure::Number(_), span!(6, 7), _),
			id: _
		});
	}
}
