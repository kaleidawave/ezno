use crate::{
	are_nodes_over_length, declarations::ClassDeclaration, derive_ASTNode, functions,
	number::NumberRepresentation, parse_bracketed, to_string_bracketed, ExpressionPosition,
	FunctionHeader, ListItem, Marker, ParseErrors, ParseResult, Quoted,
};

// type_annotations::generic_arguments_from_reader

use self::{
	assignments::{LHSOfAssignment, VariableOrPropertyAccess},
	object_literal::ObjectLiteral,
	operators::{
		IncrementOrDecrement, Operator, ARROW_FUNCTION_PRECEDENCE, COMMA_PRECEDENCE,
		CONDITIONAL_TERNARY_PRECEDENCE, CONSTRUCTOR_PRECEDENCE,
		CONSTRUCTOR_WITHOUT_PARENTHESIS_PRECEDENCE, INDEX_PRECEDENCE, MEMBER_ACCESS_PRECEDENCE,
		PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE,
	},
};

use super::{
	ASTNode, Block, FunctionBase, JSXRoot, ParseError, ParseOptions, Span, TypeAnnotation,
};

// is_expression_from_reader_sub_is_keyword

#[cfg(feature = "extras")]
use crate::extensions::is_expression::IsExpression;

use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use source_map::{Nullable, ToString};
use visitable_derive::Visitable;

pub mod arrow_function;
pub mod assignments;
pub mod object_literal;
pub mod operators;
pub mod template_literal;
pub use arrow_function::{ArrowFunction, ExpressionOrBlock};

pub use template_literal::TemplateLiteral;

use operators::{
	AssociativityDirection, BinaryAssignmentOperator, BinaryOperator, UnaryOperator,
	UnaryPostfixAssignmentOperator, UnaryPrefixAssignmentOperator, ASSIGNMENT_PRECEDENCE,
	FUNCTION_CALL_PRECEDENCE, RELATION_PRECEDENCE,
};

pub type ExpressionFunctionBase = functions::GeneralFunctionBase<ExpressionPosition>;
pub type ExpressionFunction = FunctionBase<ExpressionFunctionBase>;

use std::convert::{TryFrom, TryInto};

/// Expression structures
///
/// Comma is implemented as a [`BinaryOperator`]
#[apply(derive_ASTNode)]
#[derive(PartialEqExtras, Debug, Clone, Visitable, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[partial_eq_ignore_types(Span)]
#[visit_self]
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
	ArrayLiteral(Vec<ArrayElement>, Span),
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
	ImportMeta(Span),
	DynamicImport {
		path: Box<Expression>,
		options: Option<Box<Expression>>,
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
		arguments: Vec<FunctionArgument>,
		is_optional: bool,
		position: Span,
	},
	ConstructorCall {
		constructor: Box<Expression>,
		type_arguments: Option<Vec<TypeAnnotation>>,
		arguments: Option<Vec<FunctionArgument>>,
		position: Span,
	},
	/// e.g `... ? ... ? ...`
	ConditionalTernary {
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
	Comment {
		content: String,
		on: Box<Expression>,
		position: Span,
		is_multiline: bool,
		prefix: bool,
	},
	/// A start of a JSXNode
	JSXRoot(JSXRoot),
	/// Not to be confused with binary operator `is`
	#[cfg(feature = "extras")]
	IsExpression(IsExpression),
	#[cfg_attr(feature = "self-rust-tokenize", self_tokenize_field(marker_id))]
	Marker {
		#[visit_skip_field]
		marker_id: Marker<Expression>,
		position: Span,
	},
}

impl Eq for Expression {}

#[derive(PartialEq, Debug, Clone)]
#[apply(derive_ASTNode)]
pub enum PropertyReference {
	Standard {
		property: String,
		is_private: bool,
	},
	#[cfg_attr(feature = "self-rust-tokenize", self_tokenize_field(0))]
	Marker(Marker<PropertyReference>),
}

impl ASTNode for Expression {
	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Expression> {
		Self::from_reader_with_precedence(reader, COMMA_PRECEDENCE)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		self.to_string_using_precedence(
			buf,
			options,
			local,
			ExpressionToStringArgument { on_left: false, parent_precedence: u8::MAX },
		);
	}

	fn get_position(&self) -> Span {
		*GetFieldByType::get(self)
	}
}

impl Expression {
	pub fn from_reader_with_precedence(
		reader: &mut crate::new::Lexer,
		return_precedence: u8,
	) -> ParseResult<Self> {
		// if let (true, Some(Token(peek, at))) = (options.partial_syntax, reader.peek()) {
		// 	let next_is_not_expression_like = peek.is_expression_delimiter()
		// 		|| start.map_or(false, |start| {
		// 			peek.is_statement_or_declaration_start()
		// 				&& state
		// 					.line_starts
		// 					.byte_indexes_on_different_lines(start.0 as usize, at.0 as usize)
		// 		});

		// 	if next_is_not_expression_like {
		// 		let point = start.unwrap_or(*at);
		// 		// take up the whole next part for checker suggestions
		// 		let position = point.union(source_map::End(at.0));
		// 		return Ok(Expression::Marker {
		// 			marker_id: state.new_partial_point_marker(point),
		// 			position,
		// 		});
		// 	}
		// }

		reader.skip();

		let start = reader.get_start();
		let first_expression = {
			if reader.starts_with('"') || reader.starts_with('\'') {
				let (content, quoted) = reader.parse_string_literal().expect("TODO");
				let position = start.with_length(content.len() + 2);
				Expression::StringLiteral(content.to_owned(), quoted, position)
			} else if reader.starts_with_number() {
				let (value, length) = reader.parse_number_literal().expect("TODO");
				let position = start.with_length(length as usize);
				Expression::NumberLiteral(value, position)
			} else if reader.starts_with('/') {
				let (pattern, flags, length) = reader.parse_regex_literal().expect("TODO");
				let position = start.with_length(length as usize);
				Expression::RegexLiteral { pattern, flags, position }
			} else if reader.starts_with('(') {
				reader.advance(1);
				// TODO scan for =>
				let parenthesize_expression = MultipleExpression::from_reader(reader)?;

				reader.skip();
				let end = reader.expect(')')?;
				Expression::ParenthesizedExpression(
					Box::new(parenthesize_expression),
					start.union(end),
				)
			} else if reader.starts_with('[') {
				todo!()
			// let (items, _, end) = parse_bracketed::<ArrayElement>(
			// 	reader,
			// 	state,
			// 	options,
			// 	None,
			// 	TSXToken::CloseBracket,
			// )?;

			// Expression::ArrayLiteral(items, start.union(end))
			} else if reader.starts_with('{') {
				todo!()
			// ObjectLiteral::from_reader(reader, state, options, start)
			// 	.map(Expression::ObjectLiteral)?
			} else if reader.starts_with('(') {
				todo!()
			// let mut parentheses_depth = 1;
			// let is_arrow_function = if let Some(Token(
			// 	TSXToken::Keyword(..)
			// 	| TSXToken::Identifier(..)
			// 	| TSXToken::OpenBrace
			// 	| TSXToken::OpenBracket
			// 	| TSXToken::CloseParentheses
			// 	| TSXToken::Spread,
			// 	_,
			// )) = reader.peek()
			// {
			// 	let next = reader.scan(|token, _| match token {
			// 		TSXToken::OpenParentheses => {
			// 			parentheses_depth += 1;
			// 			false
			// 		}
			// 		TSXToken::CloseParentheses => {
			// 			parentheses_depth -= 1;
			// 			parentheses_depth == 0
			// 		}
			// 		_ => false,
			// 	});

			// 	if let Some(Token(token_type, _)) = next {
			// 		matches!(token_type, TSXToken::Arrow)
			// 	} else {
			// 		return Err(ParseError::new(
			// 			crate::ParseErrors::UnmatchedBrackets,
			// 			t.get_span(),
			// 		));
			// 	}
			// } else {
			// 	false
			// };

			// if is_arrow_function {
			// 	let arrow_function = ArrowFunction::from_reader_sub_open_paren(
			// 		reader, state, options, false, start,
			// 	)?;
			// 	Expression::ArrowFunction(arrow_function)
			// } else {
			// 	let parenthesize_expression =
			// 		MultipleExpression::from_reader(reader, state, options)?;
			// 	let end = reader.expect_next_get_end(TSXToken::CloseParentheses)?;
			// 	Expression::ParenthesizedExpression(
			// 		Box::new(parenthesize_expression),
			// 		start.union(end),
			// 	)
			// }
			}
			// Yes single line comment can occur here if the line splits
			// ```
			// [
			//     // Hi
			//     2
			// ]
			// ```
			else if reader.starts_with_str("//") || reader.starts_with_str("/*") {
				let is_multiline = reader.starts_with_str("/*");
				reader.advance(2);
				let (content, length) = if is_multiline { todo!() } else { todo!() };

				reader.advance(length);

				let expression = Self::from_reader_with_precedence(reader, return_precedence)?;
				let position = start.union(expression.get_position());
				Expression::Comment {
					is_multiline,
					content,
					position,
					on: Box::new(expression),
					prefix: true,
				}
			} else if reader.starts_with('<') {
				// Token(tok @ (TSXToken::JSXOpeningTagStart | TSXToken::JSXFragmentStart), span) => {
				// 	let var_name = matches!(tok, TSXToken::JSXFragmentStart);
				// 	JSXRoot::from_reader_sub_start(reader, state, options, var_name, span)
				// 		.map(Expression::JSXRoot)?
				// }
				todo!()
			} else if reader.starts_with('`') {
				todo!()
			// TemplateLiteral::from_reader(reader, state, options, None, start)
			// 	.map(Expression::TemplateLiteral)?
			}
			// Token(TSXToken::Keyword(kw), start) if function_header_ish(kw, reader) => {
			// 	// TODO not great to recreate token, but that is how Rust works :)
			// 	let token = Token(TSXToken::Keyword(kw), start);
			// 	let (is_async, start, token) =
			// 		if let Token(TSXToken::Keyword(TSXKeyword::Async), start) = token {
			// 			(true, start, reader.next().unwrap())
			// 		} else {
			// 			(false, start, token)
			// 		};

			// 	if is_async
			// 		&& !matches!(token, Token(TSXToken::Keyword(ref kw), _) if kw.is_in_function_header())
			// 	{
			// 		if let Token(TSXToken::OpenParentheses, start) = token {
			// 			let function = ArrowFunction::from_reader_sub_open_paren(
			// 				reader, state, options, is_async, start,
			// 			)?;
			// 			return Ok(Expression::ArrowFunction(function));
			// 		}

			// 		let (name, position) = token_as_identifier(token, "function parameter")?;
			// 		ArrowFunction::from_reader_with_first_parameter(
			// 			reader,
			// 			state,
			// 			options,
			// 			(name, position),
			// 			is_async,
			// 		)
			// 		.map(Expression::ArrowFunction)?
			// 	} else {
			// 		#[cfg(feature = "extras")]
			// 		{
			// 			use crate::functions::FunctionLocationModifier;
			// 			let (generator_keyword, token) =
			// 				if let Token(TSXToken::Keyword(TSXKeyword::Generator), _) = token {
			// 					(Some(token.get_span()), reader.next().unwrap())
			// 				} else {
			// 					(None, token)
			// 				};

			// 			let (location, token) = match token {
			// 				Token(TSXToken::Keyword(TSXKeyword::Server), _) => {
			// 					(Some(FunctionLocationModifier::Server), reader.next().unwrap())
			// 				}
			// 				Token(TSXToken::Keyword(TSXKeyword::Worker), _) => {
			// 					(Some(FunctionLocationModifier::Worker), reader.next().unwrap())
			// 				}
			// 				token => (None, token),
			// 			};

			// 			// Here because `token` (can't use `.expect_next()`)
			// 			let Token(TSXToken::Keyword(TSXKeyword::Function), function_start) = token
			// 			else {
			// 				return throw_unexpected_token_with_token(
			// 					token,
			// 					&[TSXToken::Keyword(TSXKeyword::Function)],
			// 				);
			// 			};

			// 			let function_end =
			// 				function_start.get_end_after(TSXKeyword::Function.length() as usize);

			// 			if generator_keyword.is_some() {
			// 				let position = start.union(function_end);

			// 				let header = FunctionHeader::ChadFunctionHeader {
			// 					is_async,
			// 					is_generator: true,
			// 					location,
			// 					position,
			// 				};

			// 				let name = if let Some(Token(TSXToken::OpenParentheses, _)) =
			// 					reader.peek()
			// 				{
			// 					None
			// 				} else {
			// 					let (token, span) =
			// 						token_as_identifier(reader.next().unwrap(), "function name")?;
			// 					Some(crate::VariableIdentifier::Standard(token, span))
			// 				};

			// 				FunctionBase::from_reader_with_header_and_name(
			// 					reader,
			// 					state,
			// 					options,
			// 					(Some(header.get_position().get_start()), header),
			// 					ExpressionPosition(name),
			// 				)
			// 				.map(Expression::ExpressionFunction)?
			// 			} else {
			// 				let generator_star_token_position = reader
			// 					.conditional_next(|tok| matches!(tok, TSXToken::Multiply))
			// 					.map(|token| token.get_span());

			// 				let end = generator_star_token_position
			// 					.as_ref()
			// 					.map_or(function_end, Span::get_end);

			// 				let header = FunctionHeader::VirginFunctionHeader {
			// 					position: start.union(end),
			// 					is_async,
			// 					location,
			// 					generator_star_token_position,
			// 				};

			// 				let name = if let Some(Token(TSXToken::OpenParentheses, _)) =
			// 					reader.peek()
			// 				{
			// 					None
			// 				} else {
			// 					let (token, span) =
			// 						token_as_identifier(reader.next().unwrap(), "function name")?;
			// 					Some(crate::VariableIdentifier::Standard(token, span))
			// 				};

			// 				FunctionBase::from_reader_with_header_and_name(
			// 					reader,
			// 					state,
			// 					options,
			// 					(Some(header.get_position().get_start()), header),
			// 					ExpressionPosition(name),
			// 				)
			// 				.map(Expression::ExpressionFunction)?
			// 			}
			// 		}

			// 		#[cfg(not(feature = "extras"))]
			// 		{
			// 			let Token(TSXToken::Keyword(TSXKeyword::Function), _) = token else {
			// 				return throw_unexpected_token_with_token(
			// 					token,
			// 					&[TSXToken::Keyword(TSXKeyword::Function)],
			// 				);
			// 			};

			// 			let generator_star_token_position = reader
			// 				.conditional_next(|tok| matches!(tok, TSXToken::Multiply))
			// 				.map(|token| token.get_span());

			// 			let position =
			// 				start.union(generator_star_token_position.as_ref().unwrap_or(
			// 					&start.with_length(TSXKeyword::Function.length() as usize),
			// 				));

			// 			let header = FunctionHeader::VirginFunctionHeader {
			// 				position,
			// 				is_async,
			// 				generator_star_token_position,
			// 			};

			// 			let name = if let Some(Token(TSXToken::OpenParentheses, _)) = reader.peek()
			// 			{
			// 				None
			// 			} else {
			// 				let (token, span) =
			// 					token_as_identifier(reader.next().unwrap(), "function name")?;

			// 				Some(crate::VariableIdentifier::Standard(token, span))
			// 			};
			// 			FunctionBase::from_reader_with_header_and_name(
			// 				reader,
			// 				state,
			// 				options,
			// 				(Some(start), header),
			// 				ExpressionPosition(name),
			// 			)
			// 			.map(Expression::ExpressionFunction)?
			// 		}
			// 	}
			// }

			// #[cfg(feature = "extras")]
			// t @ Token(TSXToken::Keyword(TSXKeyword::Is), start) if options.is_expressions => {
			// 	// Maintains compatibility here
			// 	let mut parentheses_depth = 0;
			// 	let next = reader.scan(|token, _| match token {
			// 		TSXToken::OpenParentheses => {
			// 			parentheses_depth += 1;
			// 			false
			// 		}
			// 		TSXToken::CloseParentheses => {
			// 			parentheses_depth -= 1;
			// 			parentheses_depth == 0
			// 		}
			// 		_ => false,
			// 	});
			// 	if let Some(Token(token_type, _)) = next {
			// 		if let TSXToken::OpenBrace = token_type {
			// 			let is_expression_from_reader_sub_is_keyword =
			// 				is_expression_from_reader_sub_is_keyword(reader, state, options, start);

			// 			is_expression_from_reader_sub_is_keyword.map(Expression::IsExpression)?
			// 		} else {
			// 			Expression::VariableReference("is".to_owned(), start.with_length(2))
			// 		}
			// 	} else {
			// 		return Err(ParseError::new(
			// 			crate::ParseErrors::UnmatchedBrackets,
			// 			t.get_span(),
			// 		));
			// 	}
			// }
			else if reader.starts_with('#') {
				// let identifier = reader.parse_identifier()?;
				todo!();
			// let (property_name, _) = token_as_identifier(
			// 	reader.next().ok_or_else(parse_lexing_error)?,
			// 	"private in expression",
			// )?;
			// let in_pos = state.expect_keyword(reader, TSXKeyword::In)?;
			// let rhs = Expression::from_reader_with_precedence(
			// 	reader,
			// 	state,
			// 	options,
			// 	RELATION_PRECEDENCE,
			// 	Some(in_pos),
			// )?;
			// let position = start.union(rhs.get_position());
			// Self::SpecialOperators(
			// 	SpecialOperators::In {
			// 		lhs: InExpressionLHS::PrivateProperty(property_name),
			// 		rhs: Box::new(rhs),
			// 	},
			// 	position,
			// )
			} else if let Some(op) = reader.is_one_of_operators(&["++", "--"]) {
				let operator = match op {
					"++" => UnaryPrefixAssignmentOperator::IncrementOrDecrement(
						IncrementOrDecrement::Increment,
					),
					"--" => UnaryPrefixAssignmentOperator::IncrementOrDecrement(
						IncrementOrDecrement::Decrement,
					),
					op => unreachable!("{op:?}"),
				};
				let precedence = operator.precedence();
				todo!();
			// let operand = VariableOrPropertyAccess::from_reader_with_precedence(
			// 	lexer,
			// 	precedence,
			// )?;
			// let position = start.union(operand.get_position());
			// Expression::UnaryPrefixAssignmentOperation {
			// 	operand,
			// 	operator,
			// 	position,
			// }
			} else if reader.is_keyword("async") || reader.is_keyword("function") {
				// TODO generator keyword as well
				// TODO arrow functions
				todo!()
			} else if reader.is_keyword("class") {
				ClassDeclaration::from_reader(reader).map(Expression::ClassExpression)?
			} else if let Some(op) = reader.is_one_of_operators(&["+", "-", "~", "!"]) {
				let operator = match op {
					"+" => UnaryOperator::Plus,
					"-" => UnaryOperator::Negation,
					"~" => UnaryOperator::BitwiseNot,
					"!" => UnaryOperator::LogicalNot,
					op => unreachable!("{op:?}"),
				};
				let precedence = operator.precedence();
				todo!();
			// let operand = VariableOrPropertyAccess::from_reader_with_precedence(
			// 	lexer,
			// 	precedence,
			// )?;
			// let position = start.union(operand.get_position());
			// Expression::UnaryOperation {
			// 	operand: Box::new(operand),
			// 	operator,
			// 	position,
			// }
			} else if let Some(keyword) =
				reader.is_one_of_keyword_advance(&["yield", "typeof", "await", "delete", "void"])
			{
				let operator = match keyword {
					"yield" => {
						let is_delegated = reader.is_operator_advance("*");
						if is_delegated {
							UnaryOperator::DelegatedYield
						} else {
							UnaryOperator::Yield
						}
					}
					"typeof" => UnaryOperator::TypeOf,
					"await" => UnaryOperator::Await,
					"delete" => UnaryOperator::Delete,
					"void" => UnaryOperator::Void,
					slice => unreachable!("{slice:?}"),
				};

				let operand =
					Expression::from_reader_with_precedence(reader, operator.precedence())?;
				let position = start.union(operand.get_position());
				Expression::UnaryOperation { operator, operand: Box::new(operand), position }
			} else if let Some(keyword) = reader.is_one_of_keyword_advance(&["true", "false"]) {
				Expression::BooleanLiteral(keyword == "true", start.with_length(keyword.len()))
			} else if reader.is_keyword_advance("this") {
				Expression::ThisReference(start.with_length(4))
			} else if reader.is_keyword_advance("null") {
				Expression::Null(start.with_length(4))
			} else if reader.is_keyword_advance("new") {
				todo!()
			} else if reader.is_keyword_advance("super") {
				todo!()
			} else if reader.is_keyword_advance("import") {
				todo!()
			} else {
				let name = reader.parse_identifier().unwrap();
				let position = start.with_length(name.len());
				// if keyword.is_invalid_identifier() {
				// 	return Err(ParseError::new(
				// 		ParseErrors::ReservedIdentifier,
				// 		token.get_span(),
				// 	));
				// }
				// if options.interpolation_points && name == crate::marker::MARKER {
				// 	let marker_id = state.new_partial_point_marker(position.get_start());
				// 	Expression::Marker { marker_id, position }
				// } else if {
				// TODO if next is arrrow
				// } else {
				Expression::VariableReference(name.to_owned(), position)
				// }
			}
		};

		// Operator precedence == 2, nothing can beat so
		// if let Expression::ArrowFunction(..) = first_expression {
		// 	return Ok(first_expression);
		// }

		Self::from_reader_after_first_expression(reader, return_precedence, first_expression)
	}

	pub fn from_reader_after_first_expression(
		reader: &mut crate::new::Lexer,
		return_precedence: u8,
		first_expression: Expression,
	) -> ParseResult<Self> {
		let mut top = first_expression;
		loop {
			reader.skip();

			// Order is important here
			let postfix_binary_operators = &[
				"**", "+", "-", "*", "+", "-", "*", "/", ">>>", "<<", ">>", "<", ">", "<=", ">=",
				"===", "==", "!==", "!=", "%", "??", "&&", "||", "&", "|", "^", "<@>", "|>",
			];

			// TODO could abstract this as left<->right etc + string options
			if let Some(operator_str) = reader.is_one_of_operators(postfix_binary_operators) {
				let operator = match operator_str {
					"+" => BinaryOperator::Add,
					"-" => BinaryOperator::Subtract,
					"*" => BinaryOperator::Multiply,
					"+" => BinaryOperator::Add,
					"-" => BinaryOperator::Subtract,
					"*" => BinaryOperator::Multiply,
					"/" => BinaryOperator::Divide,
					"**" => BinaryOperator::Exponent,
					"<" => BinaryOperator::LessThan,
					">" => BinaryOperator::GreaterThan,
					"<=" => BinaryOperator::LessThanEqual,
					">=" => BinaryOperator::GreaterThanEqual,
					"==" => BinaryOperator::Equal,
					"===" => BinaryOperator::StrictEqual,
					"!=" => BinaryOperator::NotEqual,
					"!==" => BinaryOperator::StrictNotEqual,
					"%" => BinaryOperator::Modulo,
					"??" => BinaryOperator::NullCoalescing,
					"&&" => BinaryOperator::LogicalAnd,
					"||" => BinaryOperator::LogicalOr,
					"<<" => BinaryOperator::BitwiseShiftLeft,
					">>" => BinaryOperator::BitwiseShiftRight,
					">>>" => BinaryOperator::BitwiseShiftRightUnsigned,
					"&" => BinaryOperator::BitwiseAnd,
					"|" => BinaryOperator::BitwiseOr,
					"^" => BinaryOperator::BitwiseXOr,
					// ∘
					"<@>" => BinaryOperator::Compose,
					"|>" => BinaryOperator::Pipe,
					slice => unreachable!("{slice:?}"),
				};

				if operator
					.associativity_direction()
					.should_return(return_precedence, operator.precedence())
				{
					return Ok(top);
				}

				reader.advance(operator_str.len() as u32);

				// if !options.extra_operators && operator.is_non_standard() {
				// 	return Err(ParseError::new(
				// 		ParseErrors::NonStandardSyntaxUsedWithoutEnabled,
				// 		op_pos.with_length(token.length() as usize),
				// 	));
				// }

				let rhs = Self::from_reader_with_precedence(reader, operator.precedence())?;

				top = Expression::BinaryOperation {
					position: top.get_position().union(rhs.get_position()),
					lhs: Box::new(top),
					operator,
					rhs: Box::new(rhs),
				};
			} else {
				return Ok(top);
			}

			// let Token(peeked_token, _peeked_pos) = &reader.peek().unwrap();

			// 		if reader.is_no_move(',') {
			// 			return Ok(top);
			// 			// TODO need a reader.is_a_or_b for the optional-ness
			// 		} else if reader.is_no_move('(') {
			// 			if AssociativityDirection::LeftToRight
			// 				.should_return(parent_precedence, FUNCTION_CALL_PRECEDENCE)
			// 			{
			// 				return Ok(top);
			// 			}
			// 			let is_optional = reader.is_and_move(')
			// 		} else if reader.is_no_move('?') {
			// 			if AssociativityDirection::RightToLeft
			// 				.should_return(parent_precedence, CONDITIONAL_TERNARY_PRECEDENCE)
			// 			{
			// 				return Ok(top);
			// 			}
			// 			reader.next_character();
			// 			let condition_position = top.get_position();
			// 			let condition = Box::new(top);
			// 			let lhs = Box::new(Self::from_reader(reader, state, options)?);
			// 			reader.expect_next(TSXToken::Colon)?;
			// 			let rhs = Self::from_reader(reader, state, options)?;
			// 			let position = condition_position.union(rhs.get_position());
			// 			top = Expression::ConditionalTernary {
			// 				position,
			// 				condition,
			// 				truthy_result: lhs,
			// 				falsy_result: Box::new(rhs),
			// 			};
			// 		}
			// 			// TSXToken::OpenParentheses | TSXToken::OptionalCall => {

			// 			// 	let next = reader.next().unwrap();
			// 			// 	let is_optional = matches!(next.0, TSXToken::OptionalCall);
			// 			// 	let (arguments, _, end) =
			// 			// 		parse_bracketed(reader, state, options, None, TSXToken::CloseParentheses)?;
			// 			// 	let position = top.get_position().union(end);
			// 			// 	top = Expression::FunctionCall {
			// 			// 		function: Box::new(top),
			// 			// 		type_arguments: None,
			// 			// 		arguments,
			// 			// 		position,
			// 			// 		is_optional,
			// 			// 	};
			// 			// }
			// 			// TSXToken::OpenBracket | TSXToken::OptionalIndex => {
			// 			// 	if AssociativityDirection::LeftToRight
			// 			// 		.should_return(parent_precedence, INDEX_PRECEDENCE)
			// 			// 	{
			// 			// 		return Ok(top);
			// 			// 	}
			// 			// 	let next = reader.next().unwrap();
			// 			// 	let is_optional = matches!(next.0, TSXToken::OptionalIndex);

			// 			// 	let indexer = MultipleExpression::from_reader(reader, state, options)?;
			// 			// 	let end = reader.expect_next_get_end(TSXToken::CloseBracket)?;
			// 			// 	let position = top.get_position().union(end);
			// 			// 	top = Expression::Index {
			// 			// 		position,
			// 			// 		indexee: Box::new(top),
			// 			// 		indexer: Box::new(indexer),
			// 			// 		is_optional,
			// 			// 	};
			// 			// }
			// 			// TSXToken::Dot | TSXToken::OptionalChain => {
			// 			// 	if AssociativityDirection::LeftToRight
			// 			// 		.should_return(parent_precedence, MEMBER_ACCESS_PRECEDENCE)
			// 			// 	{
			// 			// 		return Ok(top);
			// 			// 	}

			// 			// 	let Token(accessor, accessor_position) = reader.next().unwrap();
			// 			// 	// TODO not sure
			// 			// 	if matches!(top, Self::ObjectLiteral(..)) {
			// 			// 		return Err(ParseError::new(
			// 			// 			ParseErrors::CannotAccessObjectLiteralDirectly,
			// 			// 			accessor_position.with_length(1),
			// 			// 		));
			// 			// 	}

			// 			// 	let is_optional = matches!(accessor, TSXToken::OptionalChain);

			// 			// 	let Token(peek, at) = reader.peek().ok_or_else(parse_lexing_error)?;
			// 			// 	let is_next_not_identifier = peek.is_expression_delimiter()
			// 			// 		|| (peek.is_statement_or_declaration_start()
			// 			// 			&& state.line_starts.byte_indexes_on_different_lines(
			// 			// 				accessor_position.0 as usize,
			// 			// 				at.0 as usize,
			// 			// 			));

			// 			// 	let (property, position) = if options.partial_syntax && is_next_not_identifier {
			// 			// 		let marker = state.new_partial_point_marker(accessor_position);
			// 			// 		let position = accessor_position.union(source_map::End(at.0));
			// 			// 		(PropertyReference::Marker(marker), position)
			// 			// 	} else {
			// 			// 		let is_private =
			// 			// 			reader.conditional_next(|t| matches!(t, TSXToken::HashTag)).is_some();
			// 			// 		let (property, position) = token_as_identifier(
			// 			// 			reader.next().ok_or_else(parse_lexing_error)?,
			// 			// 			"variable reference",
			// 			// 		)?;
			// 			// 		(PropertyReference::Standard { property, is_private }, position)
			// 			// 	};
			// 			// 	let position = top.get_position().union(position);
			// 			// 	top = Expression::PropertyAccess {
			// 			// 		parent: Box::new(top),
			// 			// 		property,
			// 			// 		position,
			// 			// 		is_optional,
			// 			// 	};
			// 			// }

			// 			// TODO how should this work under the new lexing system>
			// 			// TSXToken::TemplateLiteralStart => {
			// 			// 	// TODO I think this is true
			// 			// 	if AssociativityDirection::LeftToRight
			// 			// 		.should_return(parent_precedence, FUNCTION_CALL_PRECEDENCE)
			// 			// 	{
			// 			// 		return Ok(top);
			// 			// 	}
			// 			// 	reader.next();
			// 			// 	// TODO should check adjacency
			// 			// 	let start = TokenStart::new(top.get_position().start);
			// 			// 	let tag = Some(Box::new(top));
			// 			// 	let mut template_literal = TemplateLiteral::from_reader_with_tag(
			// 			// 		reader, state, options, tagsubl
			// 			// 	);
			// 			// 	top = template_literal.map(Expression::TemplateLiteral)?;
			// 			// }

			// 			else if reader.is_no_move('=') {
			// 				if AssociativityDirection::RightToLeft
			// 					.should_return(parent_precedence, ASSIGNMENT_PRECEDENCE)
			// 				{
			// 					return Ok(top);
			// 				}

			// 				let position = top.get_position();
			// 				let lhs: LHSOfAssignment = top.try_into()?;

			// 				let Token(_assignment, assignment_pos) = reader.next().unwrap();
			// 				let new_rhs = Self::from_reader_with_precedence(
			// 					reader,
			// 					state,
			// 					options,
			// 					parent_precedence,
			// 					Some(assignment_pos),
			// 				)?;

			// 				let position = position.union(new_rhs.get_position());

			// 				top = Expression::Assignment { position, lhs, rhs: Box::new(new_rhs) };
			// 			}

			// 			else if reader.is_and_move("//") {
			// 				// TODO while not comment
			// 				if reader.peek_n(1).is_some_and(|t| t.0.is_expression_postfix()) {
			// 					let (content, is_multiline, position) =
			// 						TSXToken::try_into_comment(reader.next().unwrap()).unwrap();
			// 					top = Expression::Comment {
			// 						content,
			// 						on: Box::new(top),
			// 						position,
			// 						is_multiline,
			// 						prefix: false,
			// 					};
			// 				} else {
			// 					return Ok(top);
			// 				}
			// 			}

			// 			else if reader.is_and_move("/*") {
			// 				let (content, is_multiline, position) =
			// 					TSXToken::try_into_comment(reader.next().unwrap()).unwrap();
			// 				top = Expression::Comment {
			// 					content,
			// 					on: Box::new(top),
			// 					position,
			// 					is_multiline,
			// 					prefix: false,
			// 				};
			// 			}

			// 			// TODO postfix operator ?
			// 			// TSXToken::Keyword(TSXKeyword::As | TSXKeyword::Satisfies | TSXKeyword::Is)
			// 			// 	if options.type_annotations =>
			// 			// {
			// 			// 	if AssociativityDirection::LeftToRight
			// 			// 		.should_return(parent_precedence, RELATION_PRECEDENCE)
			// 			// 	{
			// 			// 		return Ok(top);
			// 			// 	}

			// 			// 	if (cfg!(not(feature = "extras"))
			// 			// 		&& matches!(peeked_token, TSXToken::Keyword(TSXKeyword::Is)))
			// 			// 		|| (cfg!(not(feature = "full-typescript"))
			// 			// 			&& matches!(peeked_token, TSXToken::Keyword(TSXKeyword::As)))
			// 			// 	{
			// 			// 		return Ok(top);
			// 			// 	}

			// 			// 	let token = reader.next().unwrap();
			// 			// 	let reference = TypeAnnotation::from_reader(reader, state, options)?;
			// 			// 	let position = top.get_position().union(reference.get_position());

			// 			// 	let special_operators = match token.0 {
			// 			// 		#[cfg(feature = "full-typescript")]
			// 			// 		TSXToken::Keyword(TSXKeyword::As) => SpecialOperators::AsCast {
			// 			// 			value: top.into(),
			// 			// 			rhs: match reference {
			// 			// 				// TODO temp :0
			// 			// 				TypeAnnotation::Name(
			// 			// 					crate::type_annotations::TypeName::Name(name),
			// 			// 					span,
			// 			// 				) if name == "const" => TypeOrConst::Const(span),
			// 			// 				reference => TypeOrConst::Type(Box::new(reference)),
			// 			// 			},
			// 			// 		},
			// 			// 		TSXToken::Keyword(TSXKeyword::Satisfies) => SpecialOperators::Satisfies {
			// 			// 			value: top.into(),
			// 			// 			type_annotation: Box::new(reference),
			// 			// 		},
			// 			// 		#[cfg(feature = "extras")]
			// 			// 		TSXToken::Keyword(TSXKeyword::Is) => SpecialOperators::Is {
			// 			// 			value: top.into(),
			// 			// 			type_annotation: Box::new(reference),
			// 			// 		},
			// 			// 		_ => unreachable!(),
			// 			// 	};
			// 			// 	top = Self::SpecialOperators(special_operators, position);
			// 			// }

			// 			// TODO ...? How to feature gate this?
			// 			// #[cfg(feature = "full-typescript")]
			// 			// TSXToken::LogicalNot if options.type_annotations => {
			// 			// 	let Token(_token, not_pos) = reader.next().unwrap();
			// 			// 	let position = top.get_position().union(not_pos.get_end_after(1));
			// 			// 	top = Self::SpecialOperators(
			// 			// 		SpecialOperators::NonNullAssertion(Box::new(top)),
			// 			// 		position,
			// 			// 	);
			// 			// }

			// 			// else if reader.is_next_keyword_no_move("is") {
			// 			// 	if AssociativityDirection::LeftToRight
			// 			// 		.should_return(parent_precedence, RELATION_PRECEDENCE)
			// 			// 	{
			// 			// 		return Ok(top);
			// 			// 	}

			// 			// 	// let Token(_token, in_pos) = reader.next().unwrap();
			// 			// 	let rhs = Expression::from_reader_with_precedence(
			// 			// 		reader,
			// 			// 		state,
			// 			// 		options,
			// 			// 		RELATION_PRECEDENCE,
			// 			// 		Some(in_pos),
			// 			// 	)?;
			// 			// 	let position = top.get_position().union(rhs.get_position());
			// 			// 	top = Self::SpecialOperators(
			// 			// 		SpecialOperators::In {
			// 			// 			lhs: InExpressionLHS::Expression(Box::new(top)),
			// 			// 			rhs: Box::new(rhs),
			// 			// 		},
			// 			// 		position,
			// 			// 	);
			// 			// }

			// 			// else if reader.is_next_keyword_no_move("instanceof") {
			// 			// 	if AssociativityDirection::LeftToRight
			// 			// 		.should_return(parent_precedence, RELATION_PRECEDENCE)
			// 			// 	{
			// 			// 		return Ok(top);
			// 			// 	}

			// 			// 	let Token(_, instance_of_pos) = reader.next().unwrap();
			// 			// 	let rhs = Expression::from_reader_with_precedence(
			// 			// 		reader,
			// 			// 		state,
			// 			// 		options,
			// 			// 		RELATION_PRECEDENCE,
			// 			// 		Some(instance_of_pos),
			// 			// 	)?;
			// 			// 	let position = top.get_position().union(rhs.get_position());
			// 			// 	top = Self::SpecialOperators(
			// 			// 		SpecialOperators::InstanceOf { lhs: Box::new(top), rhs: Box::new(rhs) },
			// 			// 		position,
			// 			// 	);
			// 			// }

			// 			token => {
			// 				// Splitting here side-steps some complaints the borrow checker has with passing
			// 				// a mutable reader here
			// 				let token = if let TSXToken::OpenChevron = token {
			// 					if is_generic_arguments(reader) {
			// 						let _ = reader.next();
			// 						let (type_arguments, _) = generic_arguments_from_reader_sub_open_angle(
			// 							reader, state, options, None,
			// 						)?;
			// 						let (arguments, _, end) = parse_bracketed(
			// 							reader,
			// 							state,
			// 							options,
			// 							Some(TSXToken::OpenParentheses),
			// 							TSXToken::CloseParentheses,
			// 						)?;
			// 						top = Expression::FunctionCall {
			// 							position: top.get_position().union(end),
			// 							function: Box::new(top),
			// 							type_arguments: Some(type_arguments),
			// 							arguments,
			// 							is_optional: false,
			// 						};
			// 						continue;
			// 					}
			// 					&TSXToken::OpenChevron
			// 				} else {
			// 					token
			// 				};

			// 				if let Ok(operator) = UnaryPostfixAssignmentOperator::try_from(token) {
			// 					if operator
			// 						.associativity_direction()
			// 						.should_return(parent_precedence, operator.precedence())
			// 					{
			// 						return Ok(top);
			// 					}
			// 					let token = reader.next().unwrap();

			// 					// Increment and decrement are the only two postfix operations
			// 					let position = top.get_position().union(token.get_end());
			// 					top = Expression::UnaryPostfixAssignmentOperation {
			// 						operand: top.try_into()?,
			// 						operator,
			// 						position,
			// 					};
			// 				} else if let Ok(operator) = BinaryOperator::try_from(token) {
			//
			// 				} else if let Ok(operator) = BinaryAssignmentOperator::try_from(token) {
			// 					if operator
			// 						.associativity_direction()
			// 						.should_return(parent_precedence, operator.precedence())
			// 					{
			// 						return Ok(top);
			// 					}
			// 					let Token(_, op_pos) = reader.next().unwrap();
			// 					let new_rhs = Self::from_reader_with_precedence(
			// 						reader,
			// 						state,
			// 						options,
			// 						operator.precedence(),
			// 						Some(op_pos),
			// 					)?;
			// 					top = Expression::BinaryAssignmentOperation {
			// 						position: top.get_position().union(new_rhs.get_position()),
			// 						lhs: top.try_into()?,
			// 						operator,
			// 						rhs: Box::new(new_rhs),
			// 					};
			// 				} else {
			// 					return Ok(top);
			// 				}
			// 			}
			// 		}
			// 	}
		}

		Ok(top)
	}

	#[must_use]
	pub fn get_precedence(&self) -> u8 {
		// TODO unsure about some of these
		match self {
			Self::NumberLiteral(..)
			| Self::BooleanLiteral(..)
			| Self::StringLiteral(..)
			| Self::RegexLiteral { .. }
			| Self::ArrayLiteral(..)
			| Self::TemplateLiteral(..)
			| Self::ParenthesizedExpression(..)
			| Self::JSXRoot(..)
			| Self::ExpressionFunction(..)
			| Self::Null(..)
			| Self::ObjectLiteral(..)
			| Self::VariableReference(..)
			| Self::ThisReference(..)
			| Self::SuperExpression(..)
			| Self::NewTarget(..)
			| Self::ClassExpression(..)
			| Self::ImportMeta(..)
			| Self::DynamicImport { .. }
			| Self::Marker { .. } => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE,
			Self::BinaryOperation { operator, .. } => operator.precedence(),
			Self::UnaryOperation { operator, .. } => operator.precedence(),
			Self::Assignment { .. } => ASSIGNMENT_PRECEDENCE,
			Self::BinaryAssignmentOperation { operator, .. } => operator.precedence(),
			Self::UnaryPrefixAssignmentOperation { operator, .. } => operator.precedence(),
			Self::UnaryPostfixAssignmentOperation { operator, .. } => operator.precedence(),
			Self::PropertyAccess { .. } => MEMBER_ACCESS_PRECEDENCE,
			Self::FunctionCall { .. } => FUNCTION_CALL_PRECEDENCE,
			Self::ConstructorCall { arguments: Some(_), .. } => CONSTRUCTOR_PRECEDENCE,
			Self::ConstructorCall { arguments: None, .. } => {
				CONSTRUCTOR_WITHOUT_PARENTHESIS_PRECEDENCE
			}
			Self::ArrowFunction(..) => ARROW_FUNCTION_PRECEDENCE,
			Self::Index { .. } => INDEX_PRECEDENCE,
			Self::ConditionalTernary { .. } => CONDITIONAL_TERNARY_PRECEDENCE,
			Self::Comment { ref on, .. } => on.get_precedence(),
			#[cfg(feature = "full-typescript")]
			Self::SpecialOperators(SpecialOperators::NonNullAssertion(..), _) => {
				PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE
			}
			// All these are relational and have the same precedence
			Self::SpecialOperators(..) => RELATION_PRECEDENCE,
			// TODO unsure about this one...?
			#[cfg(feature = "extras")]
			Self::IsExpression(..) => PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE,
		}
	}

	pub(crate) fn to_string_using_precedence<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
		local2: ExpressionToStringArgument,
	) {
		let self_precedence = self.get_precedence();
		// let inverted = local2.parent_precedence < self_precedence;
		// if inverted {
		// 	buf.push('(');
		// }
		match self {
			Self::Marker { .. } => {
				assert!(options.expect_markers, "marker found");
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
				lhs.to_string_using_precedence(
					buf,
					options,
					local,
					local2.with_precedence(self_precedence),
				);
				// TODO not great
				if options.pretty
					|| matches!(
						(operator, &**lhs),
						(
							BinaryOperator::Subtract,
							Expression::UnaryPostfixAssignmentOperation {
								operator: UnaryPostfixAssignmentOperator(
									IncrementOrDecrement::Decrement
								),
								..
							},
						) | (
							BinaryOperator::Add,
							Expression::UnaryPostfixAssignmentOperation {
								operator: UnaryPostfixAssignmentOperator(
									IncrementOrDecrement::Increment
								),
								..
							},
						)
					) {
					buf.push(' ');
				}
				buf.push_str(operator.to_str());
				// TODO not great
				if options.pretty
					|| matches!(
						(operator, &**rhs),
						(
							BinaryOperator::Subtract,
							Expression::UnaryPrefixAssignmentOperation {
								operator: UnaryPrefixAssignmentOperator::IncrementOrDecrement(
									IncrementOrDecrement::Decrement
								),
								..
							} | Expression::UnaryOperation {
								operator: UnaryOperator::Negation,
								..
							},
						) | (
							BinaryOperator::Add,
							Expression::UnaryPrefixAssignmentOperation {
								operator: UnaryPrefixAssignmentOperator::IncrementOrDecrement(
									IncrementOrDecrement::Increment
								),
								..
							} | Expression::UnaryOperation { operator: UnaryOperator::Plus, .. },
						)
					) {
					buf.push(' ');
				}
				rhs.to_string_using_precedence(
					buf,
					options,
					local,
					local2.with_precedence(self_precedence),
				);
			}
			Self::SpecialOperators(special, _) => match special {
				#[cfg(feature = "full-typescript")]
				SpecialOperators::AsCast { value, rhs, .. } => {
					value.to_string_from_buffer(buf, options, local);
					if options.include_type_annotations {
						buf.push_str(" as ");
						match rhs {
							TypeOrConst::Type(type_annotation) => {
								type_annotation.to_string_from_buffer(buf, options, local);
							}
							TypeOrConst::Const(_) => {
								buf.push_str("const");
							}
						}
					}
				}
				SpecialOperators::Satisfies { value, type_annotation, .. } => {
					value.to_string_from_buffer(buf, options, local);
					if options.include_type_annotations {
						buf.push_str(" satisfies ");
						type_annotation.to_string_from_buffer(buf, options, local);
					}
				}
				SpecialOperators::In { lhs, rhs } => {
					match lhs {
						InExpressionLHS::PrivateProperty(property) => {
							buf.push('#');
							buf.push_str(property);
						}
						InExpressionLHS::Expression(lhs) => {
							lhs.to_string_using_precedence(
								buf,
								options,
								local,
								local2.with_precedence(self_precedence),
							);
						}
					}
					// TODO whitespace can be dropped depending on LHS and RHS
					buf.push_str(" in ");
					rhs.to_string_using_precedence(
						buf,
						options,
						local,
						local2.with_precedence(self_precedence),
					);
				}
				SpecialOperators::InstanceOf { lhs, rhs } => {
					lhs.to_string_using_precedence(
						buf,
						options,
						local,
						local2.with_precedence(self_precedence),
					);
					// TODO whitespace can be dropped depending on LHS and RHS
					buf.push_str(" instanceof ");
					rhs.to_string_using_precedence(
						buf,
						options,
						local,
						local2.with_precedence(self_precedence),
					);
				}
				#[cfg(feature = "full-typescript")]
				SpecialOperators::NonNullAssertion(on) => {
					on.to_string_using_precedence(
						buf,
						options,
						local,
						local2.with_precedence(self_precedence),
					);
					if options.include_type_annotations {
						buf.push('!');
					}
				}
				#[cfg(feature = "extras")]
				SpecialOperators::Is { value, type_annotation, .. } => {
					value.to_string_from_buffer(buf, options, local);
					type_annotation.to_string_from_buffer(buf, options, local);
				}
			},
			Self::UnaryOperation { operand, operator, .. } => {
				buf.push_str(operator.to_str());
				// TODO not great
				if let (
					UnaryOperator::Negation,
					Expression::UnaryPrefixAssignmentOperation {
						operator:
							UnaryPrefixAssignmentOperator::IncrementOrDecrement(
								IncrementOrDecrement::Decrement,
							),
						..
					}
					| Expression::UnaryOperation { operator: UnaryOperator::Negation, .. },
				)
				| (
					UnaryOperator::Plus,
					Expression::UnaryPrefixAssignmentOperation {
						operator:
							UnaryPrefixAssignmentOperator::IncrementOrDecrement(
								IncrementOrDecrement::Increment,
							),
						..
					}
					| Expression::UnaryOperation { operator: UnaryOperator::Plus, .. },
				) = (operator, &**operand)
				{
					buf.push(' ');
				}
				let right_argument = local2.with_precedence(self_precedence).on_right();
				operand.to_string_using_precedence(buf, options, local, right_argument);
			}
			Self::Assignment { lhs, rhs, .. } => {
				let require_parenthesis =
					matches!(lhs, LHSOfAssignment::ObjectDestructuring { .. }) && local2.on_left;

				if require_parenthesis {
					buf.push('(');
				}
				lhs.to_string_from_buffer(buf, options, local);
				buf.push_str(if options.pretty { " = " } else { "=" });
				let right_argument = local2.with_precedence(self_precedence).on_right();
				rhs.to_string_using_precedence(buf, options, local, right_argument);
				if require_parenthesis {
					buf.push(')');
				}
			}
			Self::BinaryAssignmentOperation { lhs, operator, rhs, .. } => {
				lhs.to_string_from_buffer(buf, options, local);
				options.push_gap_optionally(buf);
				buf.push_str(operator.to_str());
				options.push_gap_optionally(buf);
				let right_argument = local2.with_precedence(self_precedence).on_right();
				rhs.to_string_using_precedence(buf, options, local, right_argument);
			}
			Self::UnaryPrefixAssignmentOperation { operand, operator, .. } => {
				buf.push_str(operator.to_str());
				operand.to_string_from_buffer(buf, options, local);
			}
			Self::UnaryPostfixAssignmentOperation { operand, operator, .. } => {
				operand.to_string_from_buffer(buf, options, local);
				buf.push_str(operator.to_str());
			}
			Self::VariableReference(name, position) => {
				buf.add_mapping(&position.with_source(local.under));
				buf.push_str(name);
			}
			Self::ThisReference(..) => {
				buf.push_str("this");
			}
			Self::NewTarget(..) => {
				buf.push_str("new.target");
			}
			Self::ImportMeta(..) => {
				buf.push_str("import.meta");
			}
			Self::DynamicImport { path, .. } => {
				buf.push_str("import(");
				path.to_string_from_buffer(buf, options, local);
				buf.push(')');
			}
			Self::PropertyAccess { parent, property, is_optional, position, .. } => {
				if options.enforce_limit_length_limit() && local.should_try_pretty_print {
					chain_to_string_from_buffer(self, buf, options, local);
					return;
				}

				buf.add_mapping(&position.with_source(local.under));

				// TODO number okay, others don't quite get?
				if let Self::NumberLiteral(..) | Self::ObjectLiteral(..) | Self::ArrowFunction(..) =
					parent.get_non_parenthesized()
				{
					buf.push('(');
					parent.to_string_from_buffer(buf, options, local);
					buf.push(')');
				} else {
					parent.to_string_from_buffer(buf, options, local);
				}

				if *is_optional {
					buf.push_str("?.");
				} else {
					buf.push('.');
				}

				match property {
					PropertyReference::Standard { property, is_private } => {
						if *is_private {
							buf.push('#');
						}
						buf.push_str(property);
					}
					PropertyReference::Marker(..) => {
						assert!(options.expect_markers, "found marker");
					}
				}
			}
			Self::ParenthesizedExpression(expr, _) => {
				// TODO more expressions could be considered for parenthesis elision
				if matches!(&**expr, MultipleExpression::Single(inner) if inner.get_precedence() == PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE)
				{
					expr.to_string_on_left(buf, options, local);
				} else {
					buf.push('(');
					expr.to_string_from_buffer(buf, options, local);
					buf.push(')');
				}
			}
			Self::Index { indexee: expression, indexer, is_optional, .. } => {
				expression.to_string_using_precedence(buf, options, local, local2);
				if *is_optional {
					buf.push_str("?.");
				}
				buf.push('[');
				indexer.to_string_from_buffer(buf, options, local);
				buf.push(']');
			}
			Self::FunctionCall { function, type_arguments, arguments, is_optional, .. } => {
				// TODO is this okay?
				if let Some(ExpressionOrBlock::Expression(expression)) = self.is_iife() {
					expression.to_string_from_buffer(buf, options, local);
					return;
				}

				function.to_string_using_precedence(buf, options, local, local2);

				if *is_optional {
					buf.push_str("?.");
				}
				if let (true, Some(type_arguments)) =
					(options.include_type_annotations, type_arguments)
				{
					to_string_bracketed(type_arguments, ('<', '>'), buf, options, local);
				}
				arguments_to_string(arguments, buf, options, local);
			}
			Self::ConstructorCall { constructor, type_arguments, arguments, .. } => {
				buf.push_str("new ");
				constructor.to_string_from_buffer(buf, options, local);
				if let (true, Some(type_arguments)) =
					(options.include_type_annotations, type_arguments)
				{
					to_string_bracketed(type_arguments, ('<', '>'), buf, options, local);
				}
				if let Some(arguments) = arguments {
					// Constructor calls can drop arguments if none
					if !arguments.is_empty() || options.pretty {
						arguments_to_string(arguments, buf, options, local);
					}
				}
			}
			Self::JSXRoot(root) => root.to_string_from_buffer(buf, options, local),
			Self::ArrowFunction(arrow_function) => {
				// `async () => {}` looks like async statement declaration when in declaration
				if local2.on_left && arrow_function.header {
					buf.push('(');
				}
				arrow_function.to_string_from_buffer(buf, options, local);
				if local2.on_left && arrow_function.header {
					buf.push(')');
				}
			}
			Self::ExpressionFunction(function) => {
				if local2.on_left {
					buf.push('(');
				}
				function.to_string_from_buffer(buf, options, local);
				if local2.on_left {
					buf.push(')');
				}
			}
			Self::ArrayLiteral(values, _) => {
				// Fix, see: https://github.com/kaleidawave/ezno/pull/158#issuecomment-2169621017
				if options.pretty && options.enforce_limit_length_limit() {
					const MAX_INLINE_OBJECT_LITERAL: u32 = 40;

					let values_are_all_booleans_or_numbers =
						values.first().and_then(ArrayElement::inner_ref).is_some_and(|e| {
							matches!(
								e,
								Expression::BooleanLiteral(..) | Expression::NumberLiteral(..)
							)
						}) && values.iter().all(|e| {
							e.inner_ref().is_some_and(|e| {
								matches!(
									e,
									Expression::BooleanLiteral(..) | Expression::NumberLiteral(..)
								)
							})
						}) && are_nodes_over_length(
							values.iter(),
							options,
							local,
							Some(MAX_INLINE_OBJECT_LITERAL),
							true,
						);

					if values_are_all_booleans_or_numbers {
						buf.push('[');
						let inner_local = local.next_level();
						buf.push_new_line();
						options.add_indent(inner_local.depth, buf);
						for (at_end, node) in
							iterator_endiate::EndiateIteratorExt::endiate(values.iter())
						{
							if buf.characters_on_current_line() > MAX_INLINE_OBJECT_LITERAL {
								buf.push_new_line();
								options.add_indent(inner_local.depth, buf);
							}
							node.to_string_from_buffer(buf, options, inner_local);
							if !at_end {
								buf.push(',');
								options.push_gap_optionally(buf);
							}
						}
						buf.push_new_line();
						options.add_indent(local.depth, buf);
						buf.push(']');
						return;
					};
				}
				to_string_bracketed(values, ('[', ']'), buf, options, local);
			}
			Self::ObjectLiteral(object_literal) => {
				if local2.on_left {
					buf.push('(');
				}
				to_string_bracketed(&object_literal.members, ('{', '}'), buf, options, local);
				if local2.on_left {
					buf.push(')');
				}
			}
			Self::ClassExpression(class) => {
				if local2.on_left {
					buf.push('(');
				}
				class.to_string_from_buffer(buf, options, local);
				if local2.on_left {
					buf.push(')');
				}
			}
			Self::Comment { content, on, is_multiline, prefix, position: _ } => {
				if *prefix && options.should_add_comment(content) {
					if *is_multiline {
						buf.push_str("/*");
						buf.push_str_contains_new_line(content);
						buf.push_str("*/ ");
					} else {
						buf.push_str("//");
						buf.push_str(content);
						buf.push_new_line();
					}
				}
				on.to_string_using_precedence(buf, options, local, local2);
				if !prefix && options.should_add_comment(content) {
					if *is_multiline {
						buf.push_str("/*");
						buf.push_str_contains_new_line(content);
						buf.push_str("*/ ");
					} else {
						buf.push_str("//");
						buf.push_str(content);
						buf.push_new_line();
					}
				}
			}
			Self::TemplateLiteral(template_literal) => {
				// Doing here because of tag precedence
				if let Some(tag) = &template_literal.tag {
					tag.to_string_using_precedence(buf, options, local, local2);
				}
				buf.push('`');
				for (static_part, dynamic_part) in &template_literal.parts {
					buf.push_str_contains_new_line(static_part.as_str());

					buf.push_str("${");
					dynamic_part.to_string_from_buffer(buf, options, local);
					buf.push('}');
				}
				buf.push_str_contains_new_line(template_literal.last.as_str());
				buf.push('`');
			}
			Self::ConditionalTernary { condition, truthy_result, falsy_result, .. } => {
				let available_space = u32::from(options.max_line_length)
					.saturating_sub(buf.characters_on_current_line());

				let split_lines = crate::are_nodes_over_length(
					[condition, truthy_result, falsy_result].iter().map(AsRef::as_ref),
					options,
					local,
					Some(available_space),
					true,
				);
				condition.to_string_using_precedence(
					buf,
					options,
					local,
					local2.with_precedence(CONDITIONAL_TERNARY_PRECEDENCE),
				);
				if split_lines {
					buf.push_new_line();
					options.add_indent(local.depth + 1, buf);
					buf.push_str("? ");
				} else {
					buf.push_str(if options.pretty { " ? " } else { "?" });
				}
				truthy_result.to_string_using_precedence(
					buf,
					options,
					local,
					local2.with_precedence(CONDITIONAL_TERNARY_PRECEDENCE).on_right(),
				);
				if split_lines {
					buf.push_new_line();
					options.add_indent(local.depth + 1, buf);
					buf.push_str(": ");
				} else {
					buf.push_str(if options.pretty { " : " } else { ":" });
				}
				falsy_result.to_string_using_precedence(
					buf,
					options,
					local,
					local2.with_precedence(CONDITIONAL_TERNARY_PRECEDENCE).on_right(),
				);
			}
			Self::Null(..) => buf.push_str("null"),
			#[cfg(feature = "extras")]
			Self::IsExpression(is_expr) => is_expr.to_string_from_buffer(buf, options, local),
			Self::SuperExpression(super_expr, _) => {
				buf.push_str("super");
				match super_expr {
					SuperReference::Call { arguments } => {
						arguments_to_string(arguments, buf, options, local);
					}
					SuperReference::PropertyAccess { property } => {
						buf.push('.');
						buf.push_str(property);
					}
					SuperReference::Index { indexer: index } => {
						buf.push('[');
						index.to_string_from_buffer(buf, options, local);
						buf.push(']');
					}
				}
			}
		}
		// if inverted {
		// 	buf.push(')');
		// }
	}
}

// fn function_header_ish(
// 	kw: TSXKeyword,
// 	reader: &mut impl TokenReader<TSXToken, TokenStart>,
// ) -> bool {
// 	kw.is_in_function_header()
// 		|| (kw.is_special_function_header()
// 			&& reader.peek().map_or(
// 				false,
// 				|Token(t, _)| matches!(t, TSXToken::Keyword(kw) if kw.is_in_function_header()),
// 			))
// }

#[derive(Clone, Copy)]
pub(crate) struct ExpressionToStringArgument {
	/// On left of statement
	pub on_left: bool,
	pub parent_precedence: u8,
}

impl ExpressionToStringArgument {
	pub fn on_right(self) -> Self {
		Self { on_left: false, parent_precedence: self.parent_precedence }
	}

	pub fn with_precedence(self, precedence: u8) -> Self {
		Self { on_left: self.on_left, parent_precedence: precedence }
	}
}

/// Represents expressions under the comma operator
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum MultipleExpression {
	Multiple { lhs: Box<MultipleExpression>, rhs: Expression, position: Span },
	Single(Expression),
}

impl MultipleExpression {
	#[must_use]
	pub fn is_iife(&self) -> Option<&ExpressionOrBlock> {
		if let MultipleExpression::Single(inner) = self {
			inner.is_iife()
		} else {
			None
		}
	}

	pub(crate) fn to_string_on_left<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			MultipleExpression::Multiple { lhs, rhs, position: _ } => {
				lhs.to_string_on_left(buf, options, local);
				buf.push(',');
				rhs.to_string_from_buffer(buf, options, local);
			}
			MultipleExpression::Single(single) => {
				let local2 =
					ExpressionToStringArgument { on_left: true, parent_precedence: u8::MAX };
				single.to_string_using_precedence(buf, options, local, local2);
			}
		}
	}
}

impl ASTNode for MultipleExpression {
	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let first = Expression::from_reader(reader)?;
		reader.skip();
		if reader.starts_with(',') {
			let mut top: MultipleExpression = first.into();
			while reader.is_operator_advance(",") {
				let rhs = Expression::from_reader(reader)?;
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
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			MultipleExpression::Multiple { lhs, rhs, position: _ } => {
				lhs.to_string_from_buffer(buf, options, local);
				buf.push(',');
				rhs.to_string_from_buffer(buf, options, local);
			}
			MultipleExpression::Single(rhs) => {
				rhs.to_string_from_buffer(buf, options, local);
			}
		}
	}

	fn get_position(&self) -> Span {
		match self {
			MultipleExpression::Multiple { position, .. } => *position,
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
fn is_generic_arguments(reader: &mut crate::new::Lexer) -> bool {
	todo!();
	// if !matches!(reader.peek(), Some(Token(TSXToken::OpenChevron, _))) {
	// 	return false;
	// }

	// // Keep a eye on brackets. e.g. for: `if (x<4) {}` should break after the 4
	// let (mut generic_depth, mut bracket_depth) = (0, 0);
	// let mut final_generic_position = None::<TokenEnd>;

	// let next_token = reader.scan(|token, position| {
	// 	// Early break if logical operators
	// 	if matches!(
	// 		token,
	// 		TSXToken::StrictEqual
	// 			| TSXToken::StrictNotEqual
	// 			| TSXToken::LogicalAnd
	// 			| TSXToken::LogicalOr
	// 			| TSXToken::SemiColon
	// 	) {
	// 		true
	// 	} else {
	// 		match token {
	// 			TSXToken::OpenChevron => generic_depth += 1,
	// 			TSXToken::CloseChevron => generic_depth -= 1,
	// 			TSXToken::BitwiseShiftRight => generic_depth -= 2,
	// 			TSXToken::BitwiseShiftRightUnsigned => generic_depth -= 3,
	// 			TSXToken::OpenParentheses => bracket_depth += 1,
	// 			TSXToken::CloseParentheses => bracket_depth -= 1,
	// 			_ => {}
	// 		}
	// 		if generic_depth == 0 {
	// 			final_generic_position = Some(TokenEnd::new(
	// 				position.0 + tokenizer_lib::sized_tokens::SizedToken::length(token),
	// 			));
	// 			true
	// 		} else {
	// 			bracket_depth < 0
	// 		}
	// 	}
	// });
	// if let (Some(last_position), Some(Token(TSXToken::OpenParentheses, open_paren_start))) =
	// 	(final_generic_position, next_token)
	// {
	// 	last_position.is_adjacent_to(*open_paren_start)
	// } else {
	// 	false
	// }
}

pub(crate) fn arguments_to_string<T: source_map::ToString>(
	nodes: &[FunctionArgument],
	buf: &mut T,
	options: &crate::ToStringOptions,
	local: crate::LocalToStringInformation,
) {
	buf.push('(');
	if nodes.is_empty() {
		buf.push(')');
		return;
	}

	let add_new_lines = are_nodes_over_length(
		nodes.iter(),
		options,
		local,
		Some(u32::from(options.max_line_length).saturating_sub(buf.characters_on_current_line())),
		true,
	);

	if add_new_lines {
		buf.push_new_line();
		options.add_indent(local.depth + 1, buf);
	}
	for (at_end, node) in iterator_endiate::EndiateIteratorExt::endiate(nodes.iter()) {
		// Hack for arrays, this is just easier for generators and ends up in a smaller output
		if let FunctionArgument::Spread(Expression::ArrayLiteral(items, _), _) = node {
			if items.is_empty() {
				continue;
			}
			for (inner_at_end, item) in iterator_endiate::EndiateIteratorExt::endiate(items.iter())
			{
				if item.0.is_none() {
					buf.push_str("undefined");
				} else {
					item.to_string_from_buffer(buf, options, local);
				}
				if !inner_at_end {
					buf.push(',');
					options.push_gap_optionally(buf);
				}
			}
		} else {
			node.to_string_from_buffer(buf, options, local);
		}
		if !at_end {
			buf.push(',');
			if add_new_lines {
				buf.push_new_line();
				options.add_indent(local.depth + 1, buf);
			} else {
				options.push_gap_optionally(buf);
			}
		}
	}
	if add_new_lines {
		buf.push_new_line();
		options.add_indent(local.depth, buf);
	}
	buf.push(')');
}

/// Binary operations whose RHS are types rather than [Expression]s
#[apply(derive_ASTNode)]
#[derive(PartialEqExtras, Debug, Clone, Visitable)]
#[partial_eq_ignore_types(Span)]
pub enum SpecialOperators {
	/// TS Only
	Satisfies {
		value: Box<Expression>,
		type_annotation: Box<TypeAnnotation>,
	},
	In {
		lhs: InExpressionLHS,
		rhs: Box<Expression>,
	},
	InstanceOf {
		lhs: Box<Expression>,
		rhs: Box<Expression>,
	},
	#[cfg(feature = "extras")]
	Is {
		value: Box<Expression>,
		type_annotation: Box<TypeAnnotation>,
	},
	#[cfg(feature = "full-typescript")]
	NonNullAssertion(Box<Expression>),
	#[cfg(feature = "full-typescript")]
	AsCast {
		value: Box<Expression>,
		rhs: TypeOrConst,
	},
}

#[cfg(feature = "full-typescript")]
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable)]
pub enum TypeOrConst {
	Type(Box<TypeAnnotation>),
	Const(Span),
}

#[apply(derive_ASTNode)]
#[derive(PartialEqExtras, Debug, Clone, Visitable)]
#[partial_eq_ignore_types(Span)]
pub enum InExpressionLHS {
	PrivateProperty(String),
	Expression(Box<Expression>),
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable)]
pub enum FunctionArgument {
	Spread(Expression, Span),
	Standard(Expression),
	Comment { content: String, is_multiline: bool, position: Span },
}

impl ListItem for FunctionArgument {
	type LAST = ();
}

impl ASTNode for FunctionArgument {
	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let _existing = r#"let peek = &reader.peek().ok_or_else(parse_lexing_error)?.0;
		match peek {
			TSXToken::Spread => {
				let start_pos = reader.next().unwrap().1;
				let expression = Expression::from_reader(reader, state, options)?;
				let position = start_pos.union(expression.get_position());
				Ok(Self::Spread(expression, position))
			}
			t if t.is_comment() => {
				let (content, is_multiline, position) =
					TSXToken::try_into_comment(reader.next().unwrap()).unwrap();

				// Function arguments, JSX interpolated expressions and array elements don't have to have a value
				if let Some(Token(
					TSXToken::CloseParentheses
					| TSXToken::JSXExpressionEnd
					| TSXToken::CloseBracket
					| TSXToken::Comma,
					_,
				)) = reader.peek()
				{
					return Ok(Self::Comment { content, is_multiline, position });
				}

				let expr = Self::from_reader(reader, state, options)?;
				let position = position.union(expr.get_position());

				Ok(match expr {
					FunctionArgument::Spread(expr, _end) => FunctionArgument::Spread(
						Expression::Comment {
							content,
							on: Box::new(expr),
							position,
							is_multiline,
							prefix: true,
						},
						position,
					),
					FunctionArgument::Standard(expr) => {
						FunctionArgument::Standard(Expression::Comment {
							content,
							on: Box::new(expr),
							position,
							is_multiline,
							prefix: true,
						})
					}
					// TODO
					c @ FunctionArgument::Comment { .. } => c,
				})
			}
			_ => Ok(Self::Standard(Expression::from_reader(reader, state, options)?)),
		}"#;
		todo!();
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			FunctionArgument::Spread(expression, _) => {
				buf.push_str("...");
				expression.to_string_from_buffer(buf, options, local);
			}
			FunctionArgument::Standard(expression) => {
				expression.to_string_from_buffer(buf, options, local);
			}
			FunctionArgument::Comment { content, is_multiline: _is_multiline, position: _ } => {
				if options.should_add_comment(content) {
					buf.push_str("/*");
					buf.push_str(content);
					buf.push_str("*/");
				}
			}
		}
	}

	fn get_position(&self) -> Span {
		match self {
			FunctionArgument::Comment { position, .. } | FunctionArgument::Spread(_, position) => {
				*position
			}
			FunctionArgument::Standard(expr) => expr.get_position(),
		}
	}
}

impl From<Expression> for FunctionArgument {
	fn from(value: Expression) -> Self {
		FunctionArgument::Standard(value)
	}
}

#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub struct ArrayElement(pub Option<FunctionArgument>);

impl ASTNode for ArrayElement {
	fn get_position(&self) -> Span {
		self.0.as_ref().map_or(Span::NULL, ASTNode::get_position)
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let _existing = r#"Ok(Self(Some(FunctionArgument::from_reader(reader, state, options)?)))"#;
		todo!();
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if let Some(ref s) = self.0 {
			s.to_string_from_buffer(buf, options, local);
		}
	}
}

impl ArrayElement {
	/// For utility purposes! Loses spread information
	#[must_use]
	pub fn inner_ref(&self) -> Option<&Expression> {
		if let Some(ref inner) = self.0 {
			match inner {
				FunctionArgument::Spread(expr, _) | FunctionArgument::Standard(expr) => Some(expr),
				FunctionArgument::Comment { .. } => None,
			}
		} else {
			None
		}
	}
}

impl ListItem for ArrayElement {
	const EMPTY: Option<Self> = Some(Self(None));

	type LAST = ();
}

// Utils for Expression
impl Expression {
	/// IIFE = immediate invoked function execution
	#[must_use]
	pub fn build_iife(block: Block) -> Self {
		let position = block.get_position();
		Expression::FunctionCall {
			function: Expression::ParenthesizedExpression(
				Box::new(
					Expression::ArrowFunction(ArrowFunction {
						// TODO maybe async
						header: false,
						name: (),
						parameters: crate::functions::FunctionParameters {
							parameters: Default::default(),
							rest_parameter: Default::default(),
							position,
							leading: (),
						},
						return_type: None,
						type_parameters: None,
						position,
						body: ExpressionOrBlock::Block(block),
					})
					.into(),
				),
				position,
			)
			.into(),
			type_arguments: None,
			arguments: Vec::new(),
			is_optional: false,
			position,
		}
	}

	#[must_use]
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
	#[must_use]
	pub fn get_non_parenthesized(&self) -> &Self {
		if let Expression::ParenthesizedExpression(inner_multiple_expr, _) = self {
			if let MultipleExpression::Single(expr) = &**inner_multiple_expr {
				expr.get_non_parenthesized()
			} else {
				// TODO could return a variant here...
				self
			}
		} else if let Expression::Comment { on, .. } = self {
			on.get_non_parenthesized()
		} else {
			self
		}
	}
}

/// "super" cannot be used alone
#[apply(derive_ASTNode)]
#[derive(PartialEqExtras, Debug, Clone, Visitable)]
#[partial_eq_ignore_types(Span)]
pub enum SuperReference {
	Call { arguments: Vec<FunctionArgument> },
	PropertyAccess { property: String },
	Index { indexer: Box<Expression> },
}

pub(crate) fn chain_to_string_from_buffer<T: source_map::ToString>(
	original: &Expression,
	buf: &mut T,
	options: &crate::ToStringOptions,
	local: crate::LocalToStringInformation,
) {
	let mut chain = Vec::new();

	let split_between_lines = if options.enforce_limit_length_limit() {
		let room =
			u32::from(options.max_line_length).saturating_sub(buf.characters_on_current_line());

		let mut buf = source_map::StringWithOptionalSourceMap {
			source: String::new(),
			source_map: None,
			quit_after: Some(room as usize),
			since_new_line: 0,
		};
		let mut over = false;
		let mut cur = Some(original);
		while let Some(node) = cur {
			chain.push(node);
			// Just measure the link in change (not the parent)
			cur = match node {
				Expression::PropertyAccess { parent, property, .. } => {
					match property {
						PropertyReference::Standard { property, .. } => buf.push_str(property),
						PropertyReference::Marker(_) => {}
					}
					Some(parent)
				}
				Expression::Index { indexer, indexee, .. } => {
					indexer.to_string_from_buffer(&mut buf, options, local);
					Some(indexee)
				}
				Expression::FunctionCall { function, type_arguments, arguments, .. } => {
					if let (true, Some(type_arguments)) =
						(options.include_type_annotations, type_arguments)
					{
						to_string_bracketed(type_arguments, ('<', '>'), &mut buf, options, local);
					}
					arguments_to_string(arguments, &mut buf, options, local);
					Some(function)
				}
				expression => {
					expression.to_string_from_buffer(&mut buf, options, local);
					None
				}
			};

			if buf.should_halt() {
				over = true;
				// Continue to build chain
			}
		}
		over
	} else {
		false
	};

	if split_between_lines && !chain.is_empty() {
		let mut items = chain.into_iter().rev();
		items.next().unwrap().to_string_from_buffer(buf, options, local);

		for item in items {
			// Just measure the link in change (not the parent)
			match item {
				Expression::PropertyAccess { property, is_optional, .. } => {
					buf.push_new_line();
					options.add_indent(local.depth + 1, buf);
					if *is_optional {
						buf.push_str("?.");
					} else {
						buf.push('.');
					}
					match property {
						PropertyReference::Standard { property, is_private } => {
							if *is_private {
								buf.push('#');
							}
							buf.push_str(property);
						}
						PropertyReference::Marker(..) => {
							assert!(options.expect_markers, "found marker");
						}
					}
				}
				Expression::Index { indexer, is_optional, .. } => {
					buf.push_new_line();
					options.add_indent(local.depth + 1, buf);
					if *is_optional {
						buf.push_str("?.");
					}
					buf.push('[');
					indexer.to_string_from_buffer(buf, options, local);
					buf.push(']');
				}
				Expression::FunctionCall { type_arguments, arguments, is_optional, .. } => {
					if *is_optional {
						buf.push_str("?.");
					}
					if let (true, Some(type_arguments)) =
						(options.include_type_annotations, type_arguments)
					{
						to_string_bracketed(type_arguments, ('<', '>'), buf, options, local);
					}
					arguments_to_string(arguments, buf, options, local);
				}
				_ => unreachable!(),
			}
		}
	} else {
		original.to_string_from_buffer(buf, options, local.do_not_pretty_print());
	}
}

#[cfg(test)]
mod tests {
	use super::{ASTNode, BinaryOperator, Expression, Expression::*, MultipleExpression};
	use crate::{
		assert_matches_ast, ast::FunctionArgument, number::NumberRepresentation, span, Quoted,
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
		let expr = Expression::from_string("(() => 2)()".to_owned(), Default::default()).unwrap();
		assert!(expr.is_iife().is_some());
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
	fn spread_function_argument() {
		assert_matches_ast!(
			"console.table(...a)",
			FunctionCall { arguments: Deref @ [FunctionArgument::Spread(VariableReference(..), span!(14, 18))], .. }
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
