pub mod arrow_function;
pub mod assignments;
pub mod object_literal;
pub mod operators;
pub mod template_literal;

use crate::{
	are_nodes_over_length, bracketed_items_from_reader, bracketed_items_to_string, derive_ASTNode,
	functions, number::NumberRepresentation, statements_and_declarations::ClassDeclaration,
	ExpressionPosition, ListItem, Marker, ParseErrors, ParseResult, Quoted,
};

use self::{
	assignments::{LHSOfAssignment, VariableOrPropertyAccess},
	object_literal::ObjectLiteral,
	operators::{
		IncrementOrDecrement, Operator, ARROW_FUNCTION_PRECEDENCE, COMMA_PRECEDENCE,
		CONDITIONAL_TERNARY_PRECEDENCE, CONSTRUCTOR_PRECEDENCE,
		CONSTRUCTOR_WITHOUT_PARENTHESIS_PRECEDENCE, INDEX_PRECEDENCE, MEMBER_ACCESS_PRECEDENCE,
		PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE, YIELD_OPERATORS_PRECEDENCE,
	},
};

use super::{jsx::JSXRoot, ASTNode, Block, FunctionBase, ParseError, Span, TypeAnnotation};

#[cfg(feature = "extras")]
use crate::extensions::is_expression::IsExpression;

use get_field_by_type::GetFieldByType;
use source_map::{Nullable, ToString};
use visitable_derive::Visitable;

pub use arrow_function::{ArrowFunction, ExpressionOrBlock};
pub use template_literal::TemplateLiteral;

use operators::{
	AssociativityDirection, BinaryAssignmentOperator, BinaryOperator, UnaryOperator,
	UnaryPostfixAssignmentOperator, UnaryPrefixAssignmentOperator, ASSIGNMENT_PRECEDENCE,
	FUNCTION_CALL_PRECEDENCE, RELATION_PRECEDENCE,
};

pub type ExpressionFunctionBase = functions::GeneralFunctionBase<ExpressionPosition>;
pub type ExpressionFunction = FunctionBase<ExpressionFunctionBase>;

use std::convert::TryInto;

/// Expression structures
///
/// Comma is implemented as a [`BinaryOperator`]
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[visit_self]
pub enum Expression {
	// Literals:
	NumberLiteral(NumberRepresentation, Span),
	StringLiteral(String, Quoted, Span),
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
	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target>
	NewTarget(Span),
	Import(ImportExpression),
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
	ArrowFunction(Box<ArrowFunction>),
	ExpressionFunction(Box<ExpressionFunction>),
	/// Yes classes can exist in expr position :?
	ClassExpression(Box<ClassDeclaration<ExpressionPosition>>),
	Null(Span),
	Comment {
		content: String,
		on: Box<Expression>,
		position: Span,
		is_multiline: bool,
		prefix: bool,
	},
	/// A start of a `JSXNode`
	JSXRoot(Box<JSXRoot>),
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

#[derive(Debug, Clone)]
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

impl Expression {
	pub fn from_reader_with_precedence(
		reader: &mut crate::Lexer,
		return_precedence: u8,
	) -> ParseResult<Self> {
		if reader.get_options().partial_syntax {
			let start = reader.get_start();
			reader.skip();
			let next_is_not_expression_like = reader.starts_with_expression_delimiter()
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
				Expression::StringLiteral(content.into_owned(), quoted, position)
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
			else if reader.starts_with_slice("//") || reader.starts_with_slice("/*") {
				let is_multiline = reader.starts_with_slice("/*");
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
				if is_arrow_function
					&& !AssociativityDirection::LeftToRight
						.should_return(return_precedence, ARROW_FUNCTION_PRECEDENCE)
				{
					let arrow_function = ArrowFunction::from_reader(reader).map(Box::new)?;
					return Ok(Expression::ArrowFunction(arrow_function));
				}
				reader.advance(1);
				let parenthesize_expression = MultipleExpression::from_reader(reader)?;
				let end = reader.expect(')')?;
				Expression::Parenthesised(Box::new(parenthesize_expression), start.union(end))
			} else if reader.starts_with('<') {
				let is_generic_arguments = reader.after_brackets().starts_with('(');
				if is_generic_arguments
					&& !AssociativityDirection::LeftToRight
						.should_return(return_precedence, ARROW_FUNCTION_PRECEDENCE)
				{
					let arrow_function = ArrowFunction::from_reader(reader).map(Box::new)?;
					return Ok(Expression::ArrowFunction(arrow_function));
				} else if reader.get_options().jsx {
					JSXRoot::from_reader(reader).map(Box::new).map(Expression::JSXRoot)?
				} else {
					let (_found, position) = crate::lexer::utilities::next_item(reader);
					return Err(ParseError::new(ParseErrors::ExpectedExpression, position));
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
				} else if current.starts_with("test") {
					current = current["test".len()..].trim_start();
				}
				if current.starts_with("function") {
					Expression::ExpressionFunction(
						ExpressionFunction::from_reader(reader).map(Box::new)?,
					)
				} else if AssociativityDirection::LeftToRight
					.should_return(return_precedence, ARROW_FUNCTION_PRECEDENCE)
				{
					let (_found, position) = crate::lexer::utilities::next_item(reader);
					return Err(ParseError::new(ParseErrors::ExpectedExpression, position));
				} else {
					return ArrowFunction::from_reader(reader)
						.map(Box::new)
						.map(Expression::ArrowFunction);
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
				let _precedence = operator.precedence();

				// _with_precedence, _with_precedence
				let operand = VariableOrPropertyAccess::from_reader(reader)?;
				let position = start.union(operand.get_position());
				Expression::UnaryPrefixAssignmentOperation { operand, operator, position }
			} else if reader.is_keyword("class") {
				ClassDeclaration::from_reader(reader)
					.map(Box::new)
					.map(Expression::ClassExpression)?
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
				reader.is_one_of_keywords_advance(&["typeof", "await", "delete", "void"])
			{
				let operator = match keyword {
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
			} else if reader.is_keyword_advance("yield") {
				let yielded = if reader.starts_with_expression_delimiter() {
					None
				} else {
					let is_delegated = reader.is_operator_advance("*");
					let expression = Expression::from_reader_with_precedence(
						reader,
						YIELD_OPERATORS_PRECEDENCE,
					)?;
					Some((is_delegated, Box::new(expression)))
				};

				let position = start.union(reader.get_end());
				Expression::SpecialOperators(SpecialOperators::Yield { yielded }, position)
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
					let _position = start.union(constructor_expression.get_position());

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
				let inner = if reader.is_operator_advance("(") {
					// TODO generics?
					let (arguments, _) = bracketed_items_from_reader(reader, ")")?;
					SuperReference::Call { arguments }
				} else if reader.is_operator_advance(".") {
					let property = reader.parse_identifier("property identifier", true)?.to_owned();
					// TODO PropertyReference::Standard { property, is_private }
					SuperReference::PropertyAccess(PropertyLike::Fixed(property))
				} else if reader.is_operator_advance("[") {
					let indexer = Expression::from_reader(reader)?;
					reader.expect(']')?;
					SuperReference::PropertyAccess(PropertyLike::Computed(Box::new(indexer)))
				} else {
					return Err(crate::lexer::utilities::expected_one_of_items(
						reader,
						&[".", "(", "["],
					));
				};
				Expression::SuperExpression(inner, start.union(reader.get_end()))
			} else if reader.is_keyword_advance("import") {
				if reader.is_operator_advance(".") {
					let mut expr = None;
					#[cfg(feature = "extras")]
					{
						let is_source = reader.is_keyword_advance("source");
						// TODO
						let _is_defer = reader.is_keyword_advance("defer");
						if is_source {
							reader.expect('(')?;
							let location =
								crate::statements_and_declarations::import_export::ImportLocation::from_reader(
									reader,
								)?;
							reader.expect(')')?;
							let position = start.union(reader.get_end());
							expr = Some(Expression::Import(ImportExpression::ImportSource {
								location,
								position,
							}));
						}
					}

					if let Some(expr) = expr {
						expr
					} else {
						let _ = reader.expect_keyword("meta")?;
						Expression::Import(ImportExpression::ImportMeta(
							start.union(reader.get_end()),
						))
					}
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
					let inner = ImportExpression::DynamicImport {
						path: Box::new(path),
						options,
						position: start.union(end),
					};
					Expression::Import(inner)
				} else {
					let position = reader.get_start().with_length(1);
					let reason = ParseErrors::UnexpectedCharacter {
						expected: &['.', '('],
						found: reader.get_current().chars().next(),
					};
					return Err(ParseError::new(reason, position));
				}
			} else {
				#[cfg(feature = "extras")]
				if reader.get_options().is_expressions
					&& reader.is_keyword("is")
					&& reader.after_brackets().starts_with('{')
				{
					return crate::extensions::is_expression::IsExpression::from_reader(reader)
						.map(Expression::IsExpression);
				}

				let name = reader.parse_identifier("variable reference expression", true)?;

				if reader.get_options().interpolation_points && name == crate::marker::MARKER {
					let position = start.with_length(0);
					let marker_id = reader.new_partial_point_marker(position);
					Expression::Marker { marker_id, position }
				} else {
					let position = start.with_length(name.len());
					let is_arrow_function =
						crate::lexer::utilities::trim_whitespace_not_newlines(reader.get_current())
							.starts_with("=>");
					if is_arrow_function
						&& !AssociativityDirection::LeftToRight
							.should_return(return_precedence, ARROW_FUNCTION_PRECEDENCE)
					{
						let identifier =
							crate::VariableIdentifier::Standard(name.to_owned(), position);
						let is_async = false;
						return ArrowFunction::from_reader_with_first_parameter(
							reader, is_async, identifier,
						)
						.map(Box::new)
						.map(Expression::ArrowFunction);
					}
					Expression::VariableReference(name.to_owned(), position)
				}
				// if let Ok(name) = name {
				// } else {
				// 	return Err(ParseError::new(
				// 		ParseErrors::ExpectedExpression,
				// 		reader.next_item_span(),
				// 	));
				// }
			}
		};

		Self::from_reader_after_first_expression(reader, return_precedence, first_expression)
	}

	pub fn from_reader_after_first_expression(
		reader: &mut crate::Lexer,
		return_precedence: u8,
		first_expression: Expression,
	) -> ParseResult<Self> {
		use derive_finite_automaton::{FiniteAutomata, FiniteAutomataConstructor, GetNextResult};

		#[derive(FiniteAutomataConstructor)]
		#[automaton_mappings(
			"}" => AfterFirst::Exit,
			"]" => AfterFirst::Exit,
			")" => AfterFirst::Exit,
			";" => AfterFirst::Exit,
			"//" => AfterFirst::SingleLineComment,
			"/*" => AfterFirst::MultiLineComment,
			"++" => AfterFirst::UnaryPostfixAssignmentOperator(
				UnaryPostfixAssignmentOperator(IncrementOrDecrement::Increment)
			),
			"--" => AfterFirst::UnaryPostfixAssignmentOperator(
				UnaryPostfixAssignmentOperator(IncrementOrDecrement::Decrement)
			),
			"+" => AfterFirst::BinaryOperator(BinaryOperator::Add),
			"+=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::Add),
			"-" => AfterFirst::BinaryOperator(BinaryOperator::Subtract),
			"-=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::Subtract),
			"*" => AfterFirst::BinaryOperator(BinaryOperator::Multiply),
			"*=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::Multiply),
			"/" => AfterFirst::BinaryOperator(BinaryOperator::Divide),
			"/=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::Divide),
			"%" => AfterFirst::BinaryOperator(BinaryOperator::Remainder),
			"%=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::Remainder),
			"**" => AfterFirst::BinaryOperator(BinaryOperator::Exponent),
			"**=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::Exponent),
			"??" => AfterFirst::BinaryOperator(BinaryOperator::NullCoalescing),
			"??=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::NullCoalescing),
			"&&" => AfterFirst::BinaryOperator(BinaryOperator::LogicalAnd),
			"&&=" => AfterFirst::BinaryOperator(BinaryOperator::LogicalAnd),
			"||" => AfterFirst::BinaryOperator(BinaryOperator::LogicalOr),
			"||=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::LogicalOr),
			"&" => AfterFirst::BinaryOperator(BinaryOperator::BitwiseAnd),
			"&=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::BitwiseAnd),
			"|" => AfterFirst::BinaryOperator(BinaryOperator::BitwiseOr),
			"|=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::BitwiseOr),
			"^" => AfterFirst::BinaryOperator(BinaryOperator::BitwiseXOr),
			"^=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::BitwiseXOr),
			"," => AfterFirst::BinaryOperator(BinaryOperator::Comma),
			"<" => AfterFirst::BinaryOperator(BinaryOperator::LessThan),
			">" => AfterFirst::BinaryOperator(BinaryOperator::GreaterThan),
			"<=" => AfterFirst::BinaryOperator(BinaryOperator::LessThanEqual),
			">=" => AfterFirst::BinaryOperator(BinaryOperator::GreaterThanEqual),
			"<<" => AfterFirst::BinaryOperator(BinaryOperator::BitwiseShiftLeft),
			"<<=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::BitwiseShiftLeft),
			">>" => AfterFirst::BinaryOperator(BinaryOperator::BitwiseShiftRight),
			">>=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::BitwiseShiftRight),
			">>>" => AfterFirst::BinaryOperator(BinaryOperator::BitwiseShiftRightUnsigned),
			">>>=" => AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::BitwiseShiftRightUnsigned),
			"==" => AfterFirst::BinaryOperator(BinaryOperator::Equal),
			"===" => AfterFirst::BinaryOperator(BinaryOperator::StrictEqual),
			"!=" => AfterFirst::BinaryOperator(BinaryOperator::NotEqual),
			"!==" => AfterFirst::BinaryOperator(BinaryOperator::StrictNotEqual),
			"." => AfterFirst::PropertyAccess { optional: false } ,
			"?." => AfterFirst::PropertyAccess { optional: true } ,
			"[" => AfterFirst::Index { optional: false },
			"?.[" => AfterFirst::Index { optional: true },
			"(" => AfterFirst::FunctionCall { optional: false },
			"?.(" => AfterFirst::FunctionCall { optional: true },
			"?.<" => AfterFirst::FunctionCall { optional: true },
			"`" => AfterFirst::TemplateLiteralStart,
			"=" => AfterFirst::Assign,
			"?" => AfterFirst::ConditionalTernary,
			"instanceof" => AfterFirst::InstanceOf,
			"in" => AfterFirst::In,
		)]
		#[cfg_attr(feature = "full-typescript", automaton_mappings(
			"!" => AfterFirst::NonNullAssertion,
			"as" => AfterFirst::As,
			"satisfies" => AfterFirst::Satisfies,
		))]
		#[cfg_attr(feature = "extras", automaton_mappings(
			"<@>" => AfterFirst::BinaryOperator(BinaryOperator::Compose),
			"|>" => AfterFirst::BinaryOperator(BinaryOperator::Pipe),
			"is" => AfterFirst::Is,
		))]
		#[allow(unused)]
		enum AfterFirst {
			SingleLineComment,
			MultiLineComment,
			UnaryPostfixAssignmentOperator(UnaryPostfixAssignmentOperator),
			BinaryOperator(BinaryOperator),
			BinaryAssignmentOperator(BinaryAssignmentOperator),
			Assign,
			TemplateLiteralStart,
			FunctionCall { optional: bool },
			PropertyAccess { optional: bool },
			Index { optional: bool },
			NonNullAssertion,
			ConditionalTernary,
			As,
			Satisfies,
			Is,
			In,
			InstanceOf,
			Exit,
		}

		fn get_after_first(on: &str) -> AfterFirst {
			let mut automaton = AfterFirst::new_automaton();
			for (_idx, chr) in on.char_indices() {
				match automaton.get_next(chr) {
					GetNextResult::Result {
						result,
						ate_item: _, // Should always be true
					} => return result,
					GetNextResult::NewState(new_state) => {
						automaton = new_state;
					}
					GetNextResult::InvalidItem(_err) => {
						// todo!("{}", err)
						return AfterFirst::Exit;
					}
				}
			}
			// I think this is okay
			AfterFirst::Exit
		}

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

			let next = get_after_first(reader.get_current());

			match next {
				c @ (AfterFirst::SingleLineComment | AfterFirst::MultiLineComment) => {
					// Lol this is how it works
					// if return_precedence == 15 {
					// 	return Ok(top);
					// }
					// let after = reader.after_comment_literals();
					// let expresion_level_comment = after
					// 	.starts_with(|chr: char| !chr.is_alphanumeric())
					// 	|| after.starts_with("in")
					// 	|| after.starts_with("instanceof")
					// 	|| after.starts_with("as")
					// 	|| after.starts_with("satisfies")
					// 	|| after.starts_with("is")
					// 	|| after.is_empty();

					// if expresion_level_comment {
					let is_multiline = matches!(c, AfterFirst::MultiLineComment);
					reader.advance(2);
					let content = reader.parse_comment_literal(is_multiline)?.to_owned();
					let position = top.get_position().union(reader.get_end());
					top = Expression::Comment {
						is_multiline,
						content,
						position,
						on: Box::new(top),
						prefix: false,
					};
					// } else {
					// 	return Ok(top);
					// }
				}
				AfterFirst::UnaryPostfixAssignmentOperator(operator) => {
					if operator
						.associativity_direction()
						.should_return(return_precedence, operator.precedence())
					{
						return Ok(top);
					}
					if let Expression::Comment { prefix: false, .. } = top {
						return Ok(top);
					}

					reader.advance(operator.to_str().len() as u32);
					let position = top.get_position().union(reader.get_end());
					// Increment and decrement are the only two postfix operations
					top = Expression::UnaryPostfixAssignmentOperation {
						operand: top.try_into()?,
						operator,
						position,
					};
				}
				AfterFirst::BinaryOperator(operator) => {
					if operator
						.associativity_direction()
						.should_return(return_precedence, operator.precedence())
					{
						return Ok(top);
					}

					let operator_len = operator.to_str().len();
					reader.advance(operator_len as u32);

					if !reader.get_options().extra_operators && operator.is_non_standard() {
						let position =
							source_map::Start(reader.get_end().0).with_length(operator_len);
						return Err(ParseError::new(
							ParseErrors::NonStandardSyntaxUsedWithoutEnabled {
								syntax: operator.to_str(),
							},
							position,
						));
					}

					let rhs = Self::from_reader_with_precedence(reader, operator.precedence())?;

					top = Expression::BinaryOperation {
						position: top.get_position().union(rhs.get_position()),
						lhs: Box::new(top),
						operator,
						rhs: Box::new(rhs),
					};
				}
				AfterFirst::BinaryAssignmentOperator(operator) => {
					if operator
						.associativity_direction()
						.should_return(return_precedence, operator.precedence())
					{
						return Ok(top);
					}

					reader.advance(operator.to_str().len() as u32);

					let new_rhs = Self::from_reader_with_precedence(reader, operator.precedence())?;
					top = Expression::BinaryAssignmentOperation {
						position: top.get_position().union(new_rhs.get_position()),
						lhs: top.try_into()?,
						operator,
						rhs: Box::new(new_rhs),
					};
				}
				AfterFirst::Assign => {
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
				}
				AfterFirst::TemplateLiteralStart => {
					if AssociativityDirection::LeftToRight
						.should_return(return_precedence, FUNCTION_CALL_PRECEDENCE)
					{
						return Ok(top);
					}

					if let Expression::UnaryPostfixAssignmentOperation { .. } = top {
						return Ok(top);
					}

					if top.is_optional_like_expression() {
						return Err(ParseError::new(
							ParseErrors::TaggedTemplateCannotBeUsedWithOptionalChain,
							top.get_position(),
						));
					}

					let mut template_literal = TemplateLiteral::from_reader(reader)?;
					template_literal.position.start = top.get_position().start;
					template_literal.tag = Some(Box::new(top));
					top = Expression::TemplateLiteral(template_literal);
				}
				AfterFirst::FunctionCall { optional: _ } => {
					if AssociativityDirection::LeftToRight
						.should_return(return_precedence, FUNCTION_CALL_PRECEDENCE)
					{
						return Ok(top);
					}
					// TODO bit weird
					let is_optional = if reader.starts_with('?') {
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
				}
				AfterFirst::Index { optional: _ } => {
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
				}
				AfterFirst::PropertyAccess { optional: _ } => {
					if AssociativityDirection::LeftToRight
						.should_return(return_precedence, MEMBER_ACCESS_PRECEDENCE)
					{
						return Ok(top);
					}

					let (is_optional, length) =
						if reader.starts_with('?') { (true, 2) } else { (false, 1) };

					reader.advance(length);

					let property = if let Some(Some(length)) = reader
						.get_options()
						.partial_syntax
						.then(|| crate::lexer::utilities::get_not_identifier_length(reader))
					{
						let position =
							source_map::Start(top.get_position().get_end().0).with_length(length);
						let marker = reader.new_partial_point_marker(position);
						PropertyReference::Marker(marker)
					} else {
						reader.skip();
						// reader.skip_including_comments();
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
				}
				AfterFirst::NonNullAssertion => {
					if let Expression::Comment { prefix: false, .. } = top {
						return Ok(top);
					}

					// TODO
					reader.advance(1);
					#[cfg(feature = "extras")]
					if reader.get_options().type_annotations {
						// if options.type_annotations
						let position = top.get_position().union(reader.get_end());
						top = Self::SpecialOperators(
							SpecialOperators::NonNullAssertion(Box::new(top)),
							position,
						);
					}
				}
				AfterFirst::ConditionalTernary => {
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
				}
				// TODO extras here etc
				c @ (AfterFirst::As | AfterFirst::Satisfies | AfterFirst::Is) => {
					#[allow(clippy::match_same_arms)]
					let len: u32 = match c {
						AfterFirst::As => 2,
						AfterFirst::Satisfies => 9,
						AfterFirst::Is => 2,
						_ => unreachable!(),
					};
					// TODO `reader.get_current()[len as usize..]` temp fix
					// should add a feature in `derive finite automaton` to discern word boundaries
					if AssociativityDirection::LeftToRight
						.should_return(return_precedence, RELATION_PRECEDENCE)
						|| reader.get_current()[len as usize..]
							.chars()
							.next()
							.is_some_and(crate::lexer::utilities::is_valid_identifier)
					{
						return Ok(top);
					}

					let top_position = top.get_position();

					let (special_operators, rhs_position): (SpecialOperators, Span) = match c {
						#[cfg(feature = "full-typescript")]
						AfterFirst::As => {
							reader.advance(2);
							let start = reader.get_start();
							let (rhs, rhs_position) = if reader.is_keyword_advance("const") {
								let position = start.with_length("const".len());
								(TypeOrConst::Const(position), position)
							} else {
								let annotation = TypeAnnotation::from_reader_with_precedence(
									reader,
									crate::types::type_annotations::TypeOperatorKind::Query,
								)?;
								let position = annotation.get_position();
								(TypeOrConst::Type(Box::new(annotation)), position)
							};
							(SpecialOperators::AsCast { value: top.into(), rhs }, rhs_position)
						}
						#[cfg(feature = "full-typescript")]
						AfterFirst::Satisfies => {
							reader.advance(9);
							let type_annotation = TypeAnnotation::from_reader_with_precedence(
								reader,
								crate::types::type_annotations::TypeOperatorKind::Query,
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
						AfterFirst::Is => {
							if !reader.get_options().is_expressions {
								let (_found, position) = crate::lexer::utilities::next_item(reader);
								return Err(ParseError::new(
									ParseErrors::ExpectedExpression,
									position,
								));
							}
							reader.advance(2);
							let type_annotation = TypeAnnotation::from_reader_with_precedence(
								reader,
								crate::types::type_annotations::TypeOperatorKind::Query,
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
				}
				AfterFirst::In => {
					if AssociativityDirection::LeftToRight
						.should_return(return_precedence, RELATION_PRECEDENCE)
						|| reader.get_current()[2..]
							.chars()
							.next()
							.is_some_and(crate::lexer::utilities::is_valid_identifier)
					{
						return Ok(top);
					}
					reader.advance(2);
					let rhs = Expression::from_reader_with_precedence(reader, RELATION_PRECEDENCE)?;
					let position = top.get_position().union(rhs.get_position());
					let operation = SpecialOperators::In {
						lhs: InExpressionLHS::Expression(Box::new(top)),
						rhs: Box::new(rhs),
					};
					top = Self::SpecialOperators(operation, position);
				}
				AfterFirst::InstanceOf => {
					if AssociativityDirection::LeftToRight
						.should_return(return_precedence, RELATION_PRECEDENCE)
						|| reader.get_current()[10..]
							.chars()
							.next()
							.is_some_and(crate::lexer::utilities::is_valid_identifier)
					{
						return Ok(top);
					}
					reader.advance(10);
					let rhs = Expression::from_reader_with_precedence(reader, RELATION_PRECEDENCE)?;
					let position = top.get_position().union(rhs.get_position());
					let operation =
						SpecialOperators::InstanceOf { lhs: Box::new(top), rhs: Box::new(rhs) };
					top = Self::SpecialOperators(operation, position);
				}
				AfterFirst::Exit => {
					return Ok(top);
				}
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
			| Self::Import(..)
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
			Self::SpecialOperators(SpecialOperators::Yield { .. }, _) => YIELD_OPERATORS_PRECEDENCE,
			// I think this is correct
			#[cfg(feature = "full-typescript")]
			Self::SpecialOperators(SpecialOperators::NonNullAssertion(..), _) => 15,
			// All these are relational and have the same precedence
			Self::SpecialOperators(..) => RELATION_PRECEDENCE,
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
				SpecialOperators::Yield { yielded } => {
					buf.push_str("yield");
					// TODO can be dropped sometimes
					buf.push(' ');
					if let Some((is_delegated, ref yielded)) = yielded {
						if *is_delegated {
							buf.push('*');
						}
						yielded.to_string_using_precedence(
							buf,
							options,
							local,
							local2.with_precedence(self_precedence),
						);
					}
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
				let is_reserved = crate::lexer::utilities::is_reserved_word(name);
				if is_reserved && local2.on_left {
					buf.push('(');
				}
				buf.push_str(name);
				if is_reserved && local2.on_left {
					buf.push(')');
				}
			}
			Self::ThisReference(..) => {
				buf.push_str("this");
			}
			Self::NewTarget(..) => {
				buf.push_str("new.target");
			}
			Self::Import(ImportExpression::ImportMeta(..)) => {
				buf.push_str("import.meta");
			}
			#[cfg(feature = "extras")]
			Self::Import(ImportExpression::ImportSource { location, .. }) => {
				buf.push_str("import.source(");
				location.to_string_from_buffer(buf);
				buf.push(')');
			}
			#[cfg(feature = "extras")]
			Self::Import(ImportExpression::ImportDefer { path, .. }) => {
				buf.push_str("import.defer(");
				path.to_string_from_buffer(buf, options, local);
				buf.push(')');
			}
			Self::Import(ImportExpression::DynamicImport { path, .. }) => {
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

				// hmm
				if let Self::NumberLiteral(..) | Self::ObjectLiteral(..) | Self::ArrowFunction(..) =
					parent.get_non_parenthesised()
				{
					buf.push('(');
					parent.get_non_parenthesised().to_string_from_buffer(buf, options, local);
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
				// if matches!(&**expr, MultipleExpression::Single(inner) if inner.get_precedence() == PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE)
				// {
				// 	expr.to_string_on_left(buf, options, local);
				// } else {
				buf.push('(');
				expr.to_string_from_buffer(buf, options, local);
				buf.push(')');
				// }
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
				// TODO requires parenthesis
				buf.push_str("new ");
				constructor.to_string_from_buffer(buf, options, local);
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
					}
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
					// let requires_parenthesis = tag.is_simple();
					// if requires_parenthesis {
					// 	buf.push('(');
					// }
					tag.to_string_using_precedence(buf, options, local, local2);
					// if requires_parenthesis {
					// 	buf.push(')');
					// }
				}
				buf.push('`');
				for (static_part, dynamic_part) in &template_literal.parts {
					buf.push_str_contains_new_line(static_part.as_str());

					buf.push_str("${");
					dynamic_part.to_string_from_buffer(buf, options, local);
					buf.push('}');
				}
				buf.push_str_contains_new_line(template_literal.final_part.as_str());
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
					SuperReference::PropertyAccess(PropertyLike::Fixed(property)) => {
						buf.push('.');
						buf.push_str(property);
					}
					SuperReference::PropertyAccess(PropertyLike::Computed(index)) => {
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

	#[must_use]
	pub fn is_optional_like_expression(&self) -> bool {
		matches!(
			self,
			Expression::PropertyAccess { is_optional: true, .. }
				| Expression::Index { is_optional: true, .. }
				| Expression::FunctionCall { is_optional: true, .. }
		)
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

/// Represents expressions that can be the comma operator. Has a special new type to discern the places
/// where this is allowed
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct MultipleExpression(pub Expression);

impl MultipleExpression {
	#[must_use]
	pub fn is_iife(&self) -> Option<&ExpressionOrBlock> {
		self.0.is_iife()
	}

	pub(crate) fn to_string_on_left<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		self.to_string_from_buffer(buf, options, local);
	}

	#[must_use]
	pub fn get_inner(&self) -> &Expression {
		&self.0
	}

	#[must_use]
	pub fn into_expression(self) -> Expression {
		if let Expression::BinaryOperation { operator: BinaryOperator::Comma, .. } = self.0 {
			let position = self.get_position();
			Expression::Parenthesised(Box::new(self), position)
		} else {
			self.0
		}
	}
}

impl ASTNode for MultipleExpression {
	fn get_position(&self) -> Span {
		self.0.get_position()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		const MINIMUM_PRECEDENCE: u8 = 0;
		Expression::from_reader_with_precedence(reader, MINIMUM_PRECEDENCE).map(Self)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		self.0.to_string_from_buffer(buf, options, local);
	}
}

impl From<Expression> for MultipleExpression {
	fn from(expr: Expression) -> Self {
		MultipleExpression(expr)
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
#[derive(Debug, Clone, Visitable)]
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
	Yield {
		// .0 = 'is_delegated'
		yielded: Option<(bool, Box<Expression>)>,
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
#[derive(Debug, Clone, Visitable)]
pub enum TypeOrConst {
	Type(Box<TypeAnnotation>),
	Const(Span),
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable)]
pub enum InExpressionLHS {
	PrivateProperty(String),
	Expression(Box<Expression>),
}

/// "super" cannot be used alone
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable)]
pub enum SuperReference {
	Call { arguments: Vec<FunctionArgument> },
	PropertyAccess(PropertyLike),
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable)]
pub enum PropertyLike {
	Fixed(String),
	Computed(Box<Expression>),
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum ImportExpression {
	ImportMeta(Span),
	/// [Proposal](https://github.com/tc39/proposal-source-phase-imports)
	#[cfg(feature = "extras")]
	ImportSource {
		location: crate::statements_and_declarations::import_export::ImportLocation,
		position: Span,
	},
	/// [Proposal](https://github.com/tc39/proposal-defer-import-eval)
	#[cfg(feature = "extras")]
	ImportDefer {
		path: Box<Expression>,
		position: Span,
	},
	DynamicImport {
		path: Box<Expression>,
		options: Option<Box<Expression>>,
		position: Span,
	},
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable)]
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
		} else if reader.starts_with_slice("//") || reader.starts_with_slice("/*") {
			let is_multiline = reader.is_operator_advance("/*");
			let content = reader.parse_comment_literal(is_multiline)?.to_owned();
			reader.skip();
			if reader.is_one_of_operators(&[")", "}", "]", ","]).is_some() {
				let position = start.union(reader.get_end());
				Ok(Self::Comment { content, is_multiline, position })
			} else {
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
					// TODO combine comments
					c @ FunctionArgument::Comment { .. } => c,
				})
			}
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

#[derive(Debug, Clone, Visitable)]
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
					Expression::ArrowFunction(Box::new(ArrowFunction {
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
					}))
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
				if let MultipleExpression(Expression::ArrowFunction(function)) = &**expression {
					return Some(&function.body);
				}
			}
		}
		None
	}

	/// Recurses to find first non parenthesized expression
	#[must_use]
	pub fn get_non_parenthesised(&self) -> &Self {
		if let Expression::Parenthesised(inner_multiple_expr, _) = self {
			&inner_multiple_expr.0
			// if let MultipleExpression(expr) = &**inner_multiple_expr {
			// 	expr.get_non_parenthesised()
			// } else {
			// 	// TODO could return a variant here...
			// 	self
			// }
		} else if let Expression::Comment { on, .. } = self {
			on.get_non_parenthesised()
		} else {
			#[cfg(feature = "full-typescript")]
			if let Self::SpecialOperators(SpecialOperators::NonNullAssertion(operand), _) = self {
				return operand.get_non_parenthesised();
			}

			self
		}
	}
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
		dbg!();

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
