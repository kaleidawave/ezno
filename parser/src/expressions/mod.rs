use crate::{
	are_nodes_over_length, bracketed_items_from_reader, bracketed_items_to_string,
	declarations::ClassDeclaration, derive_ASTNode, functions, number::NumberRepresentation,
	types::type_annotations::TypeOperatorKind, ExpressionPosition, FunctionHeader, ListItem,
	Marker, ParseErrors, ParseResult, Quoted,
};

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
	jsx::JSXRoot, ASTNode, Block, FunctionBase, ParseError, ParseOptions, Span, TypeAnnotation,
};

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
		/// Can be `""`
		flags: String,
		position: Span,
	},
	ArrayLiteral(Vec<ArrayElement>, Span),
	ObjectLiteral(ObjectLiteral),
	TemplateLiteral(TemplateLiteral),
	Parenthesised(Box<MultipleExpression>, Span),
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
		is_optional: bool,
		property: PropertyReference,
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
	fn get_position(&self) -> Span {
		*GetFieldByType::get(self)
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Expression> {
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
			ExpressionToStringArgument { on_left: false, return_precedence: u8::MAX },
		);
	}
}

static SPECIAL_OPERATORS: &[&str] = {
	if cfg!(feature = "full-typescript") && cfg!(feature = "extras") {
		&["as", "is", "satisfies"]
	} else if cfg!(feature = "full-typescript") {
		&["as", "satisfies"]
	} else if cfg!(feature = "extras") {
		&["is"]
	} else {
		&[]
	}
};

impl Expression {
	pub fn from_reader_with_precedence(
		reader: &mut crate::Lexer,
		return_precedence: u8,
	) -> ParseResult<Self> {
		if reader.get_options().partial_syntax {
			let start = reader.get_start();
			reader.skip();
			let next_is_not_expression_like = reader.starts_with_expression_delimter()
				|| reader.starts_with_statement_or_declaration_on_new_line();

			if next_is_not_expression_like {
				// take up the whole next part for checker suggestions
				let position = start.union(reader.get_end());
				return Ok(Expression::Marker {
					marker_id: reader.new_partial_point_marker(position),
					position,
				});
			}
		} else {
			reader.skip();
		}

		let start = reader.get_start();
		let first_expression = {
			if reader.starts_with_string_delimeter() {
				let (content, quoted) = reader.parse_string_literal()?;
				let position = start.with_length(content.len() + 2);
				Expression::StringLiteral(content.to_owned(), quoted, position)
			} else if reader.starts_with_number() {
				let (value, length) = reader.parse_number_literal()?;
				let position = start.with_length(length as usize);
				Expression::NumberLiteral(value, position)
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
				let content = reader.parse_comment_literal(is_multiline)?.to_owned();
				let expression = Self::from_reader_with_precedence(reader, return_precedence)?;
				let position = start.union(expression.get_position());
				Expression::Comment {
					is_multiline,
					content,
					position,
					on: Box::new(expression),
					prefix: true,
				}
			} else if reader.starts_with('/') {
				let (pattern, flags) = reader.parse_regex_literal()?;
				let position = start.with_length(2 + pattern.len() + flags.len());
				Expression::RegexLiteral {
					pattern: pattern.to_owned(),
					flags: flags.to_owned(),
					position,
				}
			} else if reader.is_operator_advance("[") {
				// TODO let is_assignment = reader.after_brackets().starts_with("=");
				let (items, _) = bracketed_items_from_reader::<ArrayElement>(reader, "]")?;
				let end = reader.get_end();
				Expression::ArrayLiteral(items, start.union(end))
			} else if reader.starts_with('{') {
				// TODO let is_assignment = reader.after_brackets().starts_with("=");
				ObjectLiteral::from_reader(reader).map(Expression::ObjectLiteral)?
			} else if reader.starts_with('(') {
				// TODO reuse `_return_annotation`
				let (is_arrow_function, _return_annotation) = reader.is_arrow_function();
				if is_arrow_function {
					let arrow_function = ArrowFunction::from_reader(reader)?;
					Expression::ArrowFunction(arrow_function)
				} else {
					reader.advance(1);
					let parenthesize_expression = MultipleExpression::from_reader(reader)?;
					let end = reader.expect(')')?;
					Expression::Parenthesised(Box::new(parenthesize_expression), start.union(end))
				}
			} else if reader.starts_with('<') {
				let is_generic_arguments = reader.after_brackets().starts_with('(');
				if is_generic_arguments {
					let arrow_function = ArrowFunction::from_reader(reader)?;
					Expression::ArrowFunction(arrow_function)
				} else {
					JSXRoot::from_reader(reader).map(Expression::JSXRoot)?
				}
			} else if reader.starts_with('`') {
				TemplateLiteral::from_reader(reader).map(Expression::TemplateLiteral)?
			} else if crate::lexer::utilities::is_function_header(reader.get_current()) {
				// TODO more cases here
				let mut current = reader.get_current();
				if current.starts_with("async") {
					current = current["async".len()..].trim_start();
				}
				if current.starts_with("server") {
					current = current["server".len()..].trim_start();
				} else if current.starts_with("worker") {
					current = current["worker".len()..].trim_start();
				}
				if current.starts_with("function") {
					Expression::ExpressionFunction(ExpressionFunction::from_reader(reader)?)
				} else {
					Expression::ArrowFunction(ArrowFunction::from_reader(reader)?)
				}
			} else if reader.is_operator_advance("#") {
				let property_name = reader.parse_identifier("property name", false)?.to_owned();
				let _ = reader.expect_keyword("in")?;
				let rhs = Expression::from_reader_with_precedence(reader, RELATION_PRECEDENCE)?;
				let position = start.union(rhs.get_position());
				Expression::SpecialOperators(
					SpecialOperators::In {
						lhs: InExpressionLHS::PrivateProperty(property_name),
						rhs: Box::new(rhs),
					},
					position,
				)
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
				reader.advance(2); // TODO reader.is_one_of_operators_advance
				let precedence = operator.precedence();

				// _with_precedence, _with_precedence
				let operand = VariableOrPropertyAccess::from_reader(reader)?;
				let position = start.union(operand.get_position());
				Expression::UnaryPrefixAssignmentOperation { operand, operator, position }
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
				reader.advance(op.len() as u32);
				let precedence = operator.precedence();
				let operand = Expression::from_reader_with_precedence(reader, precedence)?;
				let position = start.union(operand.get_position());
				Expression::UnaryOperation { operand: Box::new(operand), operator, position }
			} else if let Some(keyword) =
				reader.is_one_of_keywords_advance(&["yield", "typeof", "await", "delete", "void"])
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
			} else if let Some(keyword) = reader.is_one_of_keywords_advance(&["true", "false"]) {
				Expression::BooleanLiteral(keyword == "true", start.with_length(keyword.len()))
			} else if reader.is_keyword_advance("this") {
				Expression::ThisReference(start.with_length(4))
			} else if reader.is_keyword_advance("null") {
				Expression::Null(start.with_length(4))
			} else if reader.is_keyword_advance("new") {
				if reader.is_operator_advance(".") {
					reader.expect_keyword("target")?;
					let end = reader.get_end();
					Expression::NewTarget(start.union(end))
				} else {
					let constructor_expression =
						Self::from_reader_with_precedence(reader, FUNCTION_CALL_PRECEDENCE)?;
					let position = start.union(constructor_expression.get_position());

					let type_arguments = if reader.is_operator_advance("<") {
						let (generic_arguments, _) = bracketed_items_from_reader(reader, ">")?;
						Some(generic_arguments)
					} else {
						None
					};

					let arguments = if reader.is_operator_advance("(") {
						let (arguments, _) = bracketed_items_from_reader(reader, ")")?;
						Some(arguments)
					} else {
						None
					};

					let end = reader.get_end();

					Expression::ConstructorCall {
						constructor: constructor_expression.into(),
						type_arguments,
						arguments,
						position: start.union(end),
					}
				}
			} else if reader.is_keyword_advance("super") {
				let inner = if reader.is_operator_advance(".") {
					let property = reader.parse_identifier("property identifier", true)?.to_owned();
					SuperReference::PropertyAccess { property }
				// TODO PropertyReference::Standard { property, is_private }
				} else if reader.is_operator_advance("(") {
					// TODO generics?
					let (arguments, _) = bracketed_items_from_reader(reader, ")")?;
					SuperReference::Call { arguments }
				} else if reader.is_operator_advance("[") {
					let indexer = Expression::from_reader(reader)?;
					reader.expect(']')?;
					SuperReference::Index { indexer: Box::new(indexer) }
				} else {
					return Err(crate::lexer::utilities::expected_one_of_items(
						reader,
						&[".", "(", "["],
					));
				};
				Expression::SuperExpression(inner, start.union(reader.get_end()))
			} else if reader.is_keyword_advance("import") {
				if reader.is_operator_advance(".") {
					let _ = reader.expect_keyword("meta")?;
					Expression::ImportMeta(start.union(reader.get_end()))
				} else if reader.is_operator_advance("(") {
					let path = Expression::from_reader(reader)?;
					// if let Expression::StringLiteral(path, ..) = &path {
					//     state.constant_imports.push(path.clone());
					// }

					let options = if reader.is_operator_advance(",") {
						Some(Box::new(Expression::from_reader(reader)?))
					} else {
						None
					};
					let end = reader.expect(')')?;
					Expression::DynamicImport {
						path: Box::new(path),
						options,
						position: start.union(end),
					}
				} else {
					let position = reader.get_start().with_length(1);
					let reason = ParseErrors::UnexpectedCharacter {
						expected: &['.', '('],
						found: reader.get_current().chars().next(),
					};
					return Err(ParseError::new(reason, position));
				}
			} else {
				fn after_spaces_or_tabs(slices: &str) -> &str {
					for (idx, chr) in slices.char_indices() {
						if !matches!(chr, ' ' | '\t') {
							return &slices[idx..];
						}
					}
					Default::default()
				}

				let name = reader.parse_identifier("variable reference expression", true)?;

				if reader.get_options().interpolation_points && name == crate::marker::MARKER {
					let position = start.with_length(0);
					let marker_id = reader.new_partial_point_marker(position);
					Expression::Marker { marker_id, position }
				} else {
					let position = start.with_length(name.len());

					// Use this method to also not advance
					// `trim` but without newlines
					if after_spaces_or_tabs(reader.get_current()).starts_with("=>") {
						let identifier =
							crate::VariableIdentifier::Standard(name.to_owned(), position);
						let is_async = false;
						ArrowFunction::from_reader_with_first_parameter(
							reader, is_async, identifier,
						)
						.map(Expression::ArrowFunction)?
					} else {
						Expression::VariableReference(name.to_owned(), position)
					}
				}
			}
		};

		// Operator precedence == 2, nothing can beat so
		if let Expression::ArrowFunction(..) = first_expression {
			return Ok(first_expression);
		}

		Self::from_reader_after_first_expression(reader, return_precedence, first_expression)
	}

	pub fn from_reader_after_first_expression(
		reader: &mut crate::Lexer,
		return_precedence: u8,
		first_expression: Expression,
	) -> ParseResult<Self> {
		// Order is important here
		// TODO "<@>", "|>", disable
		static POSTFIX_BINARY_OPERATORS: &[&str] = &[
			"**", "+", "-", "*", "+", "-", "*", "/", "<@>", "|>", ">>>", "<<", ">>", "<=", ">=",
			"<", ">", "===", "==", "!==", "!=", "%", "??", "&&", "||", "&", "|", "^",
		];

		let mut top = first_expression;
		// TODO expression delimiter
		while !reader.is_finished() {
			// Do this before `.skip` call as `<` needs to be immediate
			if reader.parse_type_annotations()
				&& reader.starts_with('<')
				&& reader.after_brackets().starts_with('(')
			{
				if AssociativityDirection::LeftToRight
					.should_return(return_precedence, FUNCTION_CALL_PRECEDENCE)
				{
					return Ok(top);
				}
				reader.advance("<".len() as u32);
				let (type_arguments, _) = bracketed_items_from_reader(reader, ">")?;
				reader.expect('(')?;
				let type_arguments = Some(type_arguments);
				let (arguments, _) = bracketed_items_from_reader(reader, ")")?;
				let position = top.get_position().union(reader.get_end());
				top = Expression::FunctionCall {
					function: Box::new(top),
					type_arguments,
					arguments,
					position,
					is_optional: false,
				};
				continue;
			}

			reader.skip();

			if reader.starts_with_str(")")
				|| reader.starts_with_str("]")
				|| reader.starts_with_str("}")
			{
				return Ok(top);
			}

			if let Some(operator_str) = reader.is_one_of_operators(&["//", "/*"]) {
				let after = reader.after_comment_literals();
				let expresion_level_comment = after.starts_with(|chr: char| !chr.is_alphanumeric())
					|| after.starts_with("in")
					|| after.starts_with("instanceof")
					|| after.starts_with("as")
					|| after.starts_with("satisfies")
					|| after.starts_with("is");

				if expresion_level_comment {
					let is_multiline = operator_str == "/*";
					reader.advance(operator_str.len() as u32);
					let content = reader.parse_comment_literal(is_multiline)?.to_owned();
					let position = top.get_position().union(reader.get_end());
					top = Expression::Comment {
						is_multiline,
						content,
						position,
						on: Box::new(top),
						prefix: false,
					};
				} else {
					return Ok(top);
				}
			} else if let Some(operator_str) = reader.is_one_of_operators(&["++", "--"]) {
				let operator = UnaryPostfixAssignmentOperator(match operator_str {
					"++" => IncrementOrDecrement::Increment,
					"--" => IncrementOrDecrement::Decrement,
					slice => unreachable!("{slice:?}"),
				});
				if operator
					.associativity_direction()
					.should_return(return_precedence, operator.precedence())
				{
					return Ok(top);
				}
				reader.advance(operator_str.len() as u32);
				let position = top.get_position().union(reader.get_end());
				// Increment and decrement are the only two postfix operations
				top = Expression::UnaryPostfixAssignmentOperation {
					operand: top.try_into()?,
					operator,
					position,
				};
			} else if let Some(operator_str) = reader.is_one_of_operators(POSTFIX_BINARY_OPERATORS)
			{
				// TODO could abstract this as left<->right etc + string options
				let operator = match operator_str {
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

				// TODO double check whitespace inbetween
				let is_equal_after = reader.get_current()[operator_str.len()..].starts_with('=');
				if let (true, Ok(operator)) =
					(is_equal_after, BinaryAssignmentOperator::try_from(operator))
				{
					if operator
						.associativity_direction()
						.should_return(return_precedence, operator.precedence())
					{
						return Ok(top);
					}

					reader.advance(operator_str.len() as u32 + 1);

					let new_rhs = Self::from_reader_with_precedence(reader, operator.precedence())?;
					top = Expression::BinaryAssignmentOperation {
						position: top.get_position().union(new_rhs.get_position()),
						lhs: top.try_into()?,
						operator,
						rhs: Box::new(new_rhs),
					};
				} else {
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
				}
			} else if reader.starts_with('=') {
				if AssociativityDirection::RightToLeft
					.should_return(return_precedence, ASSIGNMENT_PRECEDENCE)
				{
					return Ok(top);
				}

				let position = top.get_position();
				let lhs: LHSOfAssignment = top.try_into()?;

				reader.advance(1);

				let new_rhs = Self::from_reader_with_precedence(reader, return_precedence)?;
				let position = position.union(new_rhs.get_position());
				top = Expression::Assignment { position, lhs, rhs: Box::new(new_rhs) };
			} else if reader.is_operator("`") {
				if AssociativityDirection::RightToLeft
					.should_return(return_precedence, COMMA_PRECEDENCE)
				{
					return Ok(top);
				}

				let mut template_literal = TemplateLiteral::from_reader(reader)?;
				// TODO check expression here
				template_literal.position.start = top.get_position().start;
				template_literal.tag = Some(Box::new(top));
				top = Expression::TemplateLiteral(template_literal);
			} else if reader.starts_with('<') {
				if AssociativityDirection::LeftToRight
					.should_return(return_precedence, FUNCTION_CALL_PRECEDENCE)
				{
					return Ok(top);
				}
				let (is_optional, length) =
					if reader.starts_with('?') { (true, 3) } else { (false, 1) };
				reader.advance(length);
				let (type_arguments, _) = bracketed_items_from_reader(reader, ">")?;
				reader.expect('(')?;
				let (arguments, _) = bracketed_items_from_reader(reader, ")")?;
				let position = top.get_position().union(reader.get_end());
				top = Expression::FunctionCall {
					function: Box::new(top),
					type_arguments: Some(type_arguments),
					arguments,
					position,
					is_optional,
				};
			} else if reader.starts_with_str("?.(")
				|| reader.starts_with_str("?.<")
				|| reader.starts_with('(')
			{
				if AssociativityDirection::LeftToRight
					.should_return(return_precedence, FUNCTION_CALL_PRECEDENCE)
				{
					return Ok(top);
				}
				// TODO bit weird
				let (is_optional) = if reader.starts_with('?') {
					reader.advance(2);
					true
				} else {
					false
				};

				let type_arguments = if reader.is_operator_advance("<") {
					let (type_arguments, _) = bracketed_items_from_reader(reader, ">")?;
					reader.expect('(')?;
					Some(type_arguments)
				} else {
					reader.advance(1);
					None
				};

				let (arguments, _) = bracketed_items_from_reader(reader, ")")?;
				let position = top.get_position().union(reader.get_end());
				top = Expression::FunctionCall {
					function: Box::new(top),
					type_arguments,
					arguments,
					position,
					is_optional,
				};
			} else if reader.starts_with_str("?.[") || reader.starts_with('[') {
				if AssociativityDirection::LeftToRight
					.should_return(return_precedence, INDEX_PRECEDENCE)
				{
					return Ok(top);
				}
				let (is_optional, length) =
					if reader.starts_with('?') { (true, 3) } else { (false, 1) };
				reader.advance(length);

				let indexer = MultipleExpression::from_reader(reader)?;
				let end = reader.expect(']')?;
				let position = top.get_position().union(end);
				top = Expression::Index {
					position,
					indexee: Box::new(top),
					indexer: Box::new(indexer),
					is_optional,
				};
			} else if reader.starts_with_str("?.") || reader.starts_with('.') {
				/// Looks to see if next is not like a property identifier, returns how many characters
				/// it skipped over for position information
				fn get_not_identifier_length(on: &str) -> Option<usize> {
					for (idx, c) in on.char_indices() {
						if c == '#' || crate::lexer::utilities::is_valid_identifier(c) {
							return None;
						} else if !c.is_whitespace() {
							let after = &on[idx..];
							return if after.starts_with("//") || after.starts_with("/*") {
								None
							} else {
								Some(idx)
							};
						}
					}

					// Else nothing exists
					Some(0)
				}

				if AssociativityDirection::LeftToRight
					.should_return(return_precedence, MEMBER_ACCESS_PRECEDENCE)
				{
					return Ok(top);
				}

				let (is_optional, length) =
					if reader.starts_with('?') { (true, 2) } else { (false, 1) };

				reader.advance(length);

				// TODO not sure
				if let Expression::ObjectLiteral(..) = top {
					return Err(ParseError::new(
						ParseErrors::CannotAccessObjectLiteralDirectly,
						source_map::Start(top.get_position().get_end().0).with_length(1),
					));
				}

				let property = if let Some(Some(length)) = reader
					.get_options()
					.partial_syntax
					.then(|| get_not_identifier_length(reader.get_current()))
				{
					let position =
						source_map::Start(top.get_position().get_end().0).with_length(length);
					let marker = reader.new_partial_point_marker(position);
					PropertyReference::Marker(marker)
				} else {
					reader.skip();
					while reader.is_one_of(&["//", "/*"]).is_some() {
						let is_multiline = reader.starts_with_str("/*");
						reader.advance(2);
						let _content = reader.parse_comment_literal(is_multiline)?;
					}

					let is_private = reader.is_operator_advance("#");
					let property = reader.parse_identifier("property name", false)?.to_owned();
					PropertyReference::Standard { property, is_private }
				};
				let position = top.get_position().union(reader.get_end());
				top = Expression::PropertyAccess {
					parent: Box::new(top),
					is_optional,
					property,
					position,
				};
			} else if reader.starts_with('?') {
				if AssociativityDirection::RightToLeft
					.should_return(return_precedence, CONDITIONAL_TERNARY_PRECEDENCE)
				{
					return Ok(top);
				}
				reader.advance(1);
				let condition_position = top.get_position();
				let condition = Box::new(top);
				let truthy_result = Box::new(Self::from_reader(reader)?);
				reader.expect(':')?;
				let falsy_result = Self::from_reader(reader)?;
				let position = condition_position.union(falsy_result.get_position());
				let falsy_result = Box::new(falsy_result);
				top = Expression::ConditionalTernary {
					position,
					condition,
					truthy_result,
					falsy_result,
				};
			} else if let Some(keyword) = reader.is_one_of_keywords(SPECIAL_OPERATORS) {
				if AssociativityDirection::LeftToRight
					.should_return(return_precedence, RELATION_PRECEDENCE)
				{
					return Ok(top);
				}

				reader.advance(keyword.len() as u32);
				let top_position = top.get_position();

				let (special_operators, rhs_position): (SpecialOperators, Span) = match keyword {
					#[cfg(feature = "full-typescript")]
					"as" => {
						let start = reader.get_start();
						let (rhs, rhs_position) = if reader.is_keyword_advance("const") {
							let position = start.with_length("const".len());
							(TypeOrConst::Const(position), position)
						} else {
							let annotation = TypeAnnotation::from_reader_with_precedence(
								reader,
								TypeOperatorKind::Query,
							)?;
							let position = annotation.get_position();
							(TypeOrConst::Type(Box::new(annotation)), position)
						};
						(SpecialOperators::AsCast { value: top.into(), rhs }, rhs_position)
					}
					#[cfg(feature = "full-typescript")]
					"satisfies" => {
						let type_annotation = TypeAnnotation::from_reader_with_precedence(
							reader,
							TypeOperatorKind::Query,
						)?;
						let position = type_annotation.get_position();
						(
							SpecialOperators::Satisfies {
								value: top.into(),
								type_annotation: Box::new(type_annotation),
							},
							position,
						)
					}
					#[cfg(feature = "extras")]
					"is" => {
						// TODO early return if not option
						let type_annotation = TypeAnnotation::from_reader_with_precedence(
							reader,
							TypeOperatorKind::Query,
						)?;
						let position = type_annotation.get_position();
						(
							SpecialOperators::Is {
								value: top.into(),
								type_annotation: Box::new(type_annotation),
							},
							position,
						)
					}
					_ => {
						unreachable!()
					}
				};

				let position = top_position.union(rhs_position);
				crate::lexer::utilities::assert_type_annotations(reader, position)?;
				top = Self::SpecialOperators(special_operators, position);
			} else if let Some(operator) = reader.is_one_of_keywords(&["instanceof", "in"]) {
				if AssociativityDirection::LeftToRight
					.should_return(return_precedence, RELATION_PRECEDENCE)
				{
					return Ok(top);
				}
				reader.advance(operator.len() as u32);
				let rhs = Expression::from_reader_with_precedence(reader, RELATION_PRECEDENCE)?;
				let position = top.get_position().union(rhs.get_position());
				let operation = match operator {
					"instanceof" => {
						SpecialOperators::InstanceOf { lhs: Box::new(top), rhs: Box::new(rhs) }
					}
					"in" => SpecialOperators::In {
						lhs: InExpressionLHS::Expression(Box::new(top)),
						rhs: Box::new(rhs),
					},
					slice => unreachable!("{slice:?}"),
				};
				top = Self::SpecialOperators(operation, position);
			} else {
				#[cfg(feature = "extras")]
				if reader.is_operator_advance("!") {
					// if options.type_annotations
					let position = top.get_position().union(reader.get_end());
					top = Self::SpecialOperators(
						SpecialOperators::NonNullAssertion(Box::new(top)),
						position,
					);
					continue;
				}

				return Ok(top);
			}
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
			| Self::Parenthesised(..)
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
		// let inverted = local2.return_precedence < self_precedence;
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
				buf.push_str(flags);
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
					buf.push_str(" is ");
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
			Self::Parenthesised(expr, _) => {
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
					bracketed_items_to_string(type_arguments, ('<', '>'), buf, options, local);
				}
				arguments_to_string(arguments, buf, options, local);
			}
			Self::ConstructorCall { constructor, type_arguments, arguments, .. } => {
				buf.push_str("new ");
				// let requires_parenthesis = !matches!(
				// 	&**constructor,
				// 	Expression::VariableReference(..)
				// 		| Expression::PropertyAccess { .. }
				// 		| Expression::Parenthesised(..)
				// );
				// if requires_parenthesis {
				// 	buf.push('(');
				// }
				constructor.to_string_from_buffer(buf, options, local);
				// if requires_parenthesis {
				// 	buf.push(')');
				// }
				if let (true, Some(type_arguments)) =
					(options.include_type_annotations, type_arguments)
				{
					bracketed_items_to_string(type_arguments, ('<', '>'), buf, options, local);
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
				bracketed_items_to_string(values, ('[', ']'), buf, options, local);
			}
			Self::ObjectLiteral(object_literal) => {
				if local2.on_left {
					buf.push('(');
				}
				bracketed_items_to_string(&object_literal.members, ('{', '}'), buf, options, local);
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
					// TODO ConstructorCall should not be here
					let requires_parenthesis = !matches!(
						&**tag,
						Expression::VariableReference(..)
							| Expression::PropertyAccess { .. }
							| Expression::ThisReference { .. }
							| Expression::SuperExpression { .. }
							| Expression::Null(..)
							| Expression::Parenthesised(..)
							| Expression::FunctionCall { .. }
							| Expression::ConstructorCall { .. }
							| Expression::SpecialOperators(
								SpecialOperators::NonNullAssertion(..),
								..
							)
					);
					if requires_parenthesis {
						buf.push('(');
					}
					tag.to_string_using_precedence(buf, options, local, local2);
					if requires_parenthesis {
						buf.push(')');
					}
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

#[derive(Clone, Copy)]
pub(crate) struct ExpressionToStringArgument {
	/// On left of statement
	pub on_left: bool,
	pub return_precedence: u8,
}

impl ExpressionToStringArgument {
	pub fn on_right(self) -> Self {
		Self { on_left: false, return_precedence: self.return_precedence }
	}

	pub fn with_precedence(self, precedence: u8) -> Self {
		Self { on_left: self.on_left, return_precedence: precedence }
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

	#[must_use]
	pub fn get_rhs(&self) -> &Expression {
		match self {
			MultipleExpression::Multiple { rhs, .. } | MultipleExpression::Single(rhs) => rhs,
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
					ExpressionToStringArgument { on_left: true, return_precedence: u8::MAX };
				single.to_string_using_precedence(buf, options, local, local2);
			}
		}
	}
}

impl ASTNode for MultipleExpression {
	fn get_position(&self) -> Span {
		match self {
			MultipleExpression::Multiple { position, .. } => *position,
			MultipleExpression::Single(expr) => expr.get_position(),
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
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
}

impl From<Expression> for MultipleExpression {
	fn from(expr: Expression) -> Self {
		MultipleExpression::Single(expr)
	}
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
	let mut added_last = false;
	for node in nodes {
		// Hack for arrays, this is just easier for generators and ends up in a smaller output
		if let FunctionArgument::Spread(Expression::ArrayLiteral(items, _), _) = node {
			if items.is_empty() {
				added_last = false;
				continue;
			}
			if added_last {
				buf.push(',');
				if add_new_lines {
					buf.push_new_line();
					options.add_indent(local.depth + 1, buf);
				} else {
					options.push_gap_optionally(buf);
				}
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
			added_last = true;
		} else {
			if added_last {
				buf.push(',');
				if add_new_lines {
					buf.push_new_line();
					options.add_indent(local.depth + 1, buf);
				} else {
					options.push_gap_optionally(buf);
				}
			}
			node.to_string_from_buffer(buf, options, local);
			added_last = true;
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
	fn get_position(&self) -> Span {
		match self {
			FunctionArgument::Comment { position, .. } | FunctionArgument::Spread(_, position) => {
				*position
			}
			FunctionArgument::Standard(expr) => expr.get_position(),
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		reader.skip();
		let start = reader.get_start();
		if reader.is_operator_advance("...") {
			let expression = Expression::from_reader(reader)?;
			let position = start.union(expression.get_position());
			Ok(Self::Spread(expression, position))
		} else if reader.starts_with_str("//") || reader.starts_with_str("/*") {
			let is_multiline = reader.starts_with_str("/*");
			reader.advance(2);
			let content = reader.parse_comment_literal(is_multiline)?.to_owned();
			if reader.is_one_of_operators(&[")", "}", ","]).is_some() {
				let position = start.union(reader.get_end());
				return Ok(Self::Comment { content, is_multiline, position });
			}

			let inner = Self::from_reader(reader)?;
			let position = start.union(inner.get_position());

			Ok(match inner {
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
				// TODO combine?
				c @ FunctionArgument::Comment { .. } => c,
			})
		} else {
			Expression::from_reader(reader).map(Self::Standard)
		}
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

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		// This is allowed for some reason
		reader.skip();
		if reader.is_one_of_operators(&[",", "]"]).is_some() {
			Ok(Self(None))
		} else {
			FunctionArgument::from_reader(reader).map(Some).map(Self)
		}
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
	type LAST = ();

	fn skip_trailing() -> bool {
		false
	}
}

// Utils for Expression
impl Expression {
	/// IIFE = immediate invoked function execution
	#[must_use]
	pub fn build_iife(block: Block) -> Self {
		let position = block.get_position();
		Expression::FunctionCall {
			function: Expression::Parenthesised(
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
			if let (true, Expression::Parenthesised(expression, _)) =
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
		if let Expression::Parenthesised(inner_multiple_expr, _) = self {
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
						bracketed_items_to_string(
							type_arguments,
							('<', '>'),
							&mut buf,
							options,
							local,
						);
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
						bracketed_items_to_string(type_arguments, ('<', '>'), buf, options, local);
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
			Parenthesised(
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
			Parenthesised(
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
