pub mod arrow_function;
pub mod assignments;
pub mod object_literal;
pub mod operators;
pub mod precedence;
pub mod template_literal;

use crate::{
	ExpressionPosition, ListItem, Marker, ParseErrors, ParseResult, Quoting, are_nodes_over_length,
	bracketed_items_from_reader, bracketed_items_to_string, derive_ASTNode, functions,
};

use crate::numbers::{BigIntRepresentation, NumberRepresentation};
use crate::statements_and_declarations::ClassDeclaration;

pub use self::{
	assignments::{LHSOfAssignment, VariableOrPropertyAccess},
	object_literal::ObjectLiteral,
};

// TODO can we remove this
use self::precedence::{
	ARROW_FUNCTION_PRECEDENCE, ASSIGNMENT_PRECEDENCE, COMMA_PRECEDENCE,
	CONDITIONAL_TERNARY_PRECEDENCE, CONSTRUCTOR_PRECEDENCE,
	CONSTRUCTOR_WITHOUT_PARENTHESIS_PRECEDENCE, FUNCTION_CALL_PRECEDENCE, INDEX_PRECEDENCE,
	MEMBER_ACCESS_PRECEDENCE, PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE, RELATION_PRECEDENCE,
	YIELD_OPERATORS_PRECEDENCE,
};

use crate::extensions::jsx::JSXRoot;
use crate::{ASTNode, Block, FunctionBase, ParseError, Span, TypeAnnotation};

use self::operators::{
	AssociativityDirection, BinaryAssignmentOperator, BinaryOperator, IncrementOrDecrement,
	Operator, UnaryOperator, UnaryPostfixAssignmentOperator, UnaryPrefixAssignmentOperator,
};

#[cfg(feature = "extras")]
use crate::extensions::is_expression::IsExpression;

use get_field_by_type::GetFieldByType;
use source_map::{Nullable, ToString};
use visitable_derive::Visitable;

pub use arrow_function::{ArrowFunction, ExpressionOrBlock};
pub use template_literal::TemplateLiteral;

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
	BigIntLiteral(BigIntRepresentation, Span),
	StringLiteral(String, Quoting, Span),
	BooleanLiteral(bool, Span),
	RegexLiteral {
		pattern: String,
		/// Can be `""`
		flags: String,
		position: Span,
	},
	Null(Span),
	// Structures
	ArrayLiteral(Vec<ArrayElement>, Span),
	ObjectLiteral(ObjectLiteral),
	TemplateLiteral(TemplateLiteral),
	// `(...)` for changing precedence
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
		arguments: Vec<ExpressionOrSpreadExpression>,
		is_optional: bool,
		position: Span,
	},
	ConstructorCall {
		constructor: Box<Expression>,
		type_arguments: Option<Vec<TypeAnnotation>>,
		arguments: Option<Vec<ExpressionOrSpreadExpression>>,
		position: Span,
	},
	/// e.g `... ? ... ? ...`
	ConditionalTernary {
		condition: Box<Expression>,
		is_keyword: bool,
		truthy_result: Box<Expression>,
		falsy_result: Box<Expression>,
		position: Span,
	},
	// Functions
	ArrowFunction(Box<ArrowFunction>),
	ExpressionFunction(Box<ExpressionFunction>),
	/// Yes classes can exist in expression position :?
	ClassExpression(
		Box<crate::extensions::decorators::Decorated<ClassDeclaration<ExpressionPosition>>>,
	),
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

impl std::cmp::PartialEq<str> for PropertyReference {
	fn eq(&self, other: &str) -> bool {
		if let Some(other) = other.strip_prefix('#') {
			if let Self::Standard { property, is_private: true } = self {
				property == other
			} else {
				false
			}
		} else {
			if let Self::Standard { property, is_private: false } = self {
				property == other
			} else {
				false
			}
		}
	}
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
		let argument = ExpressionToStringArgument { on_left: false, return_precedence: u8::MAX };
		self.to_string_using_precedence(buf, options, local, argument);
	}
}

impl Expression {
	pub fn from_reader_with_precedence(
		reader: &mut crate::Lexer,
		return_precedence: u8,
	) -> ParseResult<Self> {
		if reader.get_options().features.partial_syntax {
			let start = reader.get_start();

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
		}

		let start = reader.get_start();
		// TODO
		let first_byte = reader.get_current().as_bytes().first().copied().unwrap_or(0);
		let first_expression = {
			match first_byte {
				b'"' | b'\'' => {
					let (content, quoting, width) = reader.parse_string_literal()?;
					let position = start.with_length(width as usize);
					Expression::StringLiteral(content.into_owned(), quoting, position)
				}
				b'.' | b'0'..=b'9' => {
					let (value, length) = reader.parse_number_literal()?;
					let position = start.with_length(length as usize);
					match value {
						crate::numbers::ParsedNumberLiteral::Number(value) => {
							Self::NumberLiteral(value, position)
						}
						crate::numbers::ParsedNumberLiteral::BigInt(value) => Self::BigIntLiteral(
							BigIntRepresentation { source: value.to_owned() },
							position,
						),
					}
				}
				b'/' if reader.starts_with_slice("//") || reader.starts_with_slice("/*") => {
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
				}
				b'/' if reader.starts_with('/') => {
					let (pattern, flags) = reader.parse_regex_literal()?;
					let position = start.with_length(2 + pattern.len() + flags.len());
					Expression::RegexLiteral {
						pattern: pattern.to_owned(),
						flags: flags.to_owned(),
						position,
					}
				}
				b'[' => {
					reader.advance(1);
					let (items, _) = bracketed_items_from_reader::<ArrayElement>(reader, "]")?;
					let end = reader.get_end();
					Expression::ArrayLiteral(items, start.union(end))
				}
				b'{' => ObjectLiteral::from_reader(reader).map(Expression::ObjectLiteral)?,
				b'(' => {
					// TODO temp
					if reader.get_current().starts_with("()") {
						Expression::ArrowFunction(Box::new(ArrowFunction::from_reader(reader)?))
					} else {
						// if AssociativityDirection::LeftToRight
						// 	.should_return(return_precedence, FUNCTION_CALL_PRECEDENCE)
						// {
						// 	return Ok(Break::Break);
						// }
						let result = reader.try_parse(|reader: &mut crate::Lexer<'_>| {
							let parameters =
								crate::functions::parameters::FunctionParameters::from_reader(
									reader,
								)?;

							if reader.starts_with(':') || reader.starts_with_slice("=>") {
								Ok(parameters)
							} else {
								Err(ParseError::new(
									ParseErrors::InvalidArrowFunctionParameter,
									reader.get_start().with_length(0),
								))
							}
						});
						if let Ok(parameters) = result {
							let arrow_function = ArrowFunction::from_reader_with_parameters(
								reader, start, false, None, parameters,
							)?;
							return Ok(Expression::ArrowFunction(Box::new(arrow_function)));
						} else {
							reader.advance(1);
							let parenthesize_expression = MultipleExpression::from_reader(reader)?;
							let end = reader.expect_chr(')')?;
							Expression::Parenthesised(
								Box::new(parenthesize_expression),
								start.union(end),
							)
						}
					}
				}
				b'<' if reader.starts_with('<') => {
					match (reader.parse_type_annotations(), reader.get_options().jsx.is_some()) {
						(true, true) => {
							let result: ParseResult<_> =
								reader.try_parse(|reader: &mut crate::Lexer<'_>| {
									reader.advance(1);
									let (type_parameters, _) =
										bracketed_items_from_reader(reader, ">")?;

									if reader.starts_with('(') {
										Ok(type_parameters)
									} else {
										Err(ParseError::new(
											ParseErrors::InvalidArrowFunctionParameter,
											reader.get_start().with_length(0),
										))
									}
								});

							match result {
								Ok(type_parameters) => {
									let parameters = crate::functions::parameters::FunctionParameters::from_reader(reader)?;

									let arrow_function =
										ArrowFunction::from_reader_with_parameters(
											reader,
											start,
											false,
											Some(type_parameters),
											parameters,
										)?;
									return Ok(Expression::ArrowFunction(Box::new(arrow_function)));
								}
								Err(_) => {
									let value = JSXRoot::from_reader(reader)?;
									Expression::JSXRoot(Box::new(value))
								}
							}
						}
						(true, false) => {
							let arrow_function = ArrowFunction::from_reader(reader)?;
							return Ok(Expression::ArrowFunction(Box::new(arrow_function)));
							// if !AssociativityDirection::RightToLeft
							// 		.should_return(return_precedence, ARROW_FUNCTION_PRECEDENCE)
							// {
							// } else {
							// 	return Err(ParseError::new(ParseErrors::ExpectedExpression, position));
							// }
						}
						(false, true) => {
							JSXRoot::from_reader(reader).map(Box::new).map(Expression::JSXRoot)?
						}
						(false, false) => {
							let (_found, position) = crate::lexer::utilities::next_item(reader);
							return Err(ParseError::new(ParseErrors::ExpectedExpression, position));
						}
					}
				}
				b'`' => TemplateLiteral::from_reader(reader).map(Expression::TemplateLiteral)?,
				b'a' | b'f' | b'g' | b's' | b'w' | b't' if reader.starts_with_function_header() => {
					let mut header = crate::functions::FunctionHeader::from_reader_initial(reader)?;
					if reader.is_keyword("function") {
						let header = header.to_full(reader)?;
						let name: crate::ExpressionPosition =
							crate::ExpressionOrStatementPosition::from_reader(reader)?;
						let function = ExpressionFunction::from_reader_with_header_and_name(
							reader, header, name,
						)?;
						Expression::ExpressionFunction(Box::new(function))
					} else if reader.starts_with_expression_delimiter()
						|| (!header.is_async() && reader.get_current().starts_with(['(', '{', '[']))
					{
						if let Ok(name) = header.into_expression() {
							let position = start.with_length(name.len());
							Expression::VariableReference(name.to_owned(), position)
						} else {
							// TODO expected `function`?
							let (_found, position) = crate::lexer::utilities::next_item(reader);
							return Err(ParseError::new(ParseErrors::ExpectedExpression, position));
						}
					} else {
						// `(async(a, b, c))` is valid non-strict syntax
						if !reader.strict_mode() && header.is_async_only() {
							let result = reader.try_parse(ArrowFunction::from_reader);
							if let Ok(mut function) = result {
								function.header = true;
								// if AssociativityDirection::RightToLeft
								// 	.should_return(return_precedence, ARROW_FUNCTION_PRECEDENCE)
								// {
								// 	dbg!(reader.get_current_short());
								// 	let (_found, position) =
								// 	crate::lexer::utilities::next_item(reader);
								// 	return Err(ParseError::new(
								// 		ParseErrors::ExpectedExpression,
								// 		position,
								// 	));
								// }
								Expression::ArrowFunction(Box::new(function))
							} else {
								Expression::VariableReference(
									"async".to_owned(),
									start.with_length(5),
								)
							}
						} else {
							// if AssociativityDirection::RightToLeft
							// 	.should_return(return_precedence, ARROW_FUNCTION_PRECEDENCE)
							// {
							// 	let (_found, position) = crate::lexer::utilities::next_item(reader);
							// 	return Err(ParseError::new(
							// 		ParseErrors::ExpectedExpression,
							// 		position,
							// 	));
							// }
							let function = ArrowFunction::from_reader(reader)?;
							Expression::ArrowFunction(Box::new(function))
						}
					}
				}
				b'#' => {
					reader.advance(1);
					let property_name =
						reader.parse_identifier("property name", false)?.into_owned();
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
				}
				b'+' if reader.is_operator_advance("++") => {
					let operator = UnaryPrefixAssignmentOperator::IncrementOrDecrement(
						IncrementOrDecrement::Increment,
					);
					let _precedence = operator.precedence();
					// _with_precedence, _with_precedence
					let operand = VariableOrPropertyAccess::from_reader(reader)?;
					let position = start.union(operand.get_position());
					Expression::UnaryPrefixAssignmentOperation { operand, operator, position }
				}
				b'-' if reader.is_operator_advance("--") => {
					let operator = UnaryPrefixAssignmentOperator::IncrementOrDecrement(
						IncrementOrDecrement::Decrement,
					);
					let _precedence = operator.precedence();
					// _with_precedence, _with_precedence
					let operand = VariableOrPropertyAccess::from_reader(reader)?;
					let position = start.union(operand.get_position());
					Expression::UnaryPrefixAssignmentOperation { operand, operator, position }
				}
				b'c' if reader.is_keyword("class") => {
					let on = ClassDeclaration::from_reader(reader)?;
					let position = on.get_position(); // FUTURE duplicate position 
					Expression::ClassExpression(Box::new(
						crate::extensions::decorators::Decorated {
							on,
							decorators: Vec::new(),
							position,
						},
					))
				}
				b'+' => {
					reader.advance(1);
					let operator = UnaryOperator::Plus;
					let precedence = operator.precedence();
					let operand = Expression::from_reader_with_precedence(reader, precedence)?;
					let position = start.union(operand.get_position());
					Expression::UnaryOperation { operand: Box::new(operand), operator, position }
				}
				b'-' => {
					reader.advance(1);
					let operator = UnaryOperator::Negation;
					let precedence = operator.precedence();
					let operand = Expression::from_reader_with_precedence(reader, precedence)?;
					let position = start.union(operand.get_position());
					Expression::UnaryOperation { operand: Box::new(operand), operator, position }
				}
				b'~' => {
					reader.advance(1);
					let operator = UnaryOperator::BitwiseNot;
					let precedence = operator.precedence();
					let operand = Expression::from_reader_with_precedence(reader, precedence)?;
					let position = start.union(operand.get_position());
					Expression::UnaryOperation { operand: Box::new(operand), operator, position }
				}
				b'!' => {
					reader.advance(1);
					let operator = UnaryOperator::LogicalNot;
					let precedence = operator.precedence();
					let operand = Expression::from_reader_with_precedence(reader, precedence)?;
					let position = start.union(operand.get_position());
					Expression::UnaryOperation { operand: Box::new(operand), operator, position }
				}
				// TODO should be after to account for function calls
				#[cfg(feature = "extras")]
				b'n' if reader.get_options().extras.keyword_logical_operators
					&& reader.is_keyword_advance("not") =>
				{
					let operator = UnaryOperator::LogicalNot;
					let precedence = operator.precedence();
					let operand = Expression::from_reader_with_precedence(reader, precedence)?;
					let position = start.union(operand.get_position());
					Expression::UnaryOperation { operand: Box::new(operand), operator, position }
				}
				b'a' if reader.is_keyword_advance("await") => {
					// // TEMP fix, what about comments
					// if !reader.strict_mode() && reader.is_operator_advance("()") {
					// 	let on = Expression::VariableReference("await".to_owned(), start.with_length(5));
					// 	Expression::FunctionCall {
					// 		function: Box::new(on),
					// 		type_arguments: None,
					// 		arguments: Vec::new(),
					// 		position: start.union(reader.get_end()),
					// 		is_optional: false,
					// 	}
					// } else {
					let operator = UnaryOperator::Await;
					if reader.starts_with_expression_delimiter() {
						let position = start.with_length(5);
						Expression::VariableReference("await".to_owned(), position)
					} else {
						let operand =
							Expression::from_reader_with_precedence(reader, operator.precedence())?;
						let position = start.union(operand.get_position());
						Expression::UnaryOperation {
							operator,
							operand: Box::new(operand),
							position,
						}
					}
				}
				b't' if reader.is_keyword_advance("typeof") => {
					let operator = UnaryOperator::TypeOf;
					let operand =
						Expression::from_reader_with_precedence(reader, operator.precedence())?;
					let position = start.union(operand.get_position());
					Expression::UnaryOperation { operator, operand: Box::new(operand), position }
				}
				b'd' if reader.is_keyword_advance("delete") => {
					let operator = UnaryOperator::Delete;
					let operand =
						Expression::from_reader_with_precedence(reader, operator.precedence())?;
					let position = start.union(operand.get_position());
					Expression::UnaryOperation { operator, operand: Box::new(operand), position }
				}
				b'v' if reader.is_keyword_advance("void") => {
					let operator = UnaryOperator::Void;
					let operand =
						Expression::from_reader_with_precedence(reader, operator.precedence())?;
					let position = start.union(operand.get_position());
					Expression::UnaryOperation { operator, operand: Box::new(operand), position }
				}
				b'y' if reader.is_keyword_advance("yield") => {
					// TEMP fix, what about comments
					if !reader.strict_mode() && reader.is_operator_advance("()") {
						let on =
							Expression::VariableReference("yield".to_owned(), start.with_length(5));
						Expression::FunctionCall {
							function: Box::new(on),
							type_arguments: None,
							arguments: Vec::new(),
							position: start.union(reader.get_end()),
							is_optional: false,
						}
					} else {
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
					}
				}
				b't' if reader.is_keyword_advance("true") => {
					Expression::BooleanLiteral(true, start.with_length(4))
				}
				b'f' if reader.is_keyword_advance("false") => {
					Expression::BooleanLiteral(false, start.with_length(5))
				}
				b't' if reader.is_keyword_advance("this") => {
					Expression::ThisReference(start.with_length(4))
				}
				b'n' if reader.is_keyword_advance("null") => Expression::Null(start.with_length(4)),
				b'n' if reader.is_keyword_advance("new") => {
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
				}
				b's' if reader.is_keyword_advance("super") => {
					let inner = if reader.is_operator_advance("(") {
						// TODO generics?
						let (arguments, _) = bracketed_items_from_reader(reader, ")")?;
						SuperReference::Call { arguments }
					} else if reader.is_operator_advance(".") {
						let property =
							reader.parse_identifier("property identifier", true)?.into_owned();
						// TODO PropertyReference::Standard { property, is_private }
						SuperReference::PropertyAccess(PropertyLike::Fixed(property))
					} else if reader.is_operator_advance("[") {
						let indexer = Expression::from_reader(reader)?;
						reader.expect_chr(']')?;
						SuperReference::PropertyAccess(PropertyLike::Computed(Box::new(indexer)))
					} else {
						return Err(crate::lexer::utilities::expected_one_of_items(
							reader,
							&[".", "(", "["],
						));
					};
					Expression::SuperExpression(inner, start.union(reader.get_end()))
				}
				b'i' if reader.is_keyword_advance("import") => {
					Expression::Import(parse_after_import(reader, start)?)
				}
				// esid: prod-ClassExpression
				b'@' if !reader.strict_mode() => {
					let mut decorators = Vec::new();
					while reader.is_operator("@") {
						decorators
							.push(crate::extensions::decorators::Decorator::from_reader(reader)?);
					}
					let on = ClassDeclaration::from_reader(reader)?;
					let position = start.union(on.get_position());
					Expression::ClassExpression(Box::new(
						crate::extensions::decorators::Decorated { on, decorators, position },
					))
				}
				_ => {
					let name = reader.parse_identifier("variable reference expression", true)?;

					if reader.get_options().features.interpolation_points
						&& name == crate::marker::MARKER
					{
						let position = start.with_length(0);
						let marker_id = reader.new_partial_point_marker(position);
						Expression::Marker { marker_id, position }
					} else {
						let position = start.with_length(name.len());
						let is_arrow_function =
							crate::lexer::utilities::trim_whitespace_not_newlines(
								reader.get_current(),
							)
							.starts_with("=>");
						if is_arrow_function
							&& AssociativityDirection::RightToLeft
								.should_return(return_precedence, ARROW_FUNCTION_PRECEDENCE)
						{
							let identifier =
								crate::VariableIdentifier::Standard(name.into_owned(), position);
							let is_async = false;
							let function = ArrowFunction::from_reader_with_first_parameter(
								reader,
								is_async,
								identifier.into(),
							)?;

							Expression::ArrowFunction(Box::new(function))
						} else {
							Expression::VariableReference(name.into_owned(), position)
						}
					}
				}
			}
		};

		Self::from_reader_after_first_expression(reader, return_precedence, first_expression)
	}

	pub fn from_reader_after_first_expression(
		reader: &mut crate::Lexer,
		return_precedence: u8,
		first_expression: Expression,
	) -> ParseResult<Self> {
		/// TODO is this needed
		#[allow(unused)]
		enum AfterFirst {
			// SingleLineComment,
			// MultiLineComment,
			UnaryPostfixAssignmentOperator(UnaryPostfixAssignmentOperator),
			BinaryOperator(BinaryOperator),
			BinaryAssignmentOperator(BinaryAssignmentOperator),
			Assign,
			TemplateLiteralStart,
			FunctionCall {
				is_optional: bool,
			},
			PropertyAccess {
				is_optional: bool,
			},
			Index {
				is_optional: bool,
			},
			NonNullAssertion,
			ConditionalTernary,
			As,
			Satisfies,
			Is,
			In,
			InstanceOf,

			// Extras
			BetterGenericInstantation,

			// For nunjucks
			AndKeyword,
			OrKeyword,
			IfKeyword,

			// Colon,
			// ArrowFunction,
			/// Used as trick to exit early
			Exit,
		}

		let mut top = first_expression;
		while !reader.is_finished() {
			// Do this before `.skip` call as `<` needs to be immediate
			if reader.parse_type_annotations() && reader.starts_with('<') {
				enum Break<T> {
					Break,
					Value(T),
				}

				let result: ParseResult<Break<_>> =
					reader.try_parse(|reader: &mut crate::Lexer<'_>| {
						if AssociativityDirection::LeftToRight
							.should_return(return_precedence, FUNCTION_CALL_PRECEDENCE)
						{
							Ok(Break::Break)
						} else {
							reader.advance("<".len() as u32);
							let (value, _) = bracketed_items_from_reader(reader, ">")?;
							Ok(Break::Value(value))
						}
					});

				match result {
					Ok(Break::Value(type_arguments)) => {
						// TODO instantation here if expression delimeter
						reader.expect_chr('(')?;
						let (arguments, _) = bracketed_items_from_reader(reader, ")")?;
						let position = top.get_position().union(reader.get_end());
						top = Expression::FunctionCall {
							function: Box::new(top),
							type_arguments: Some(type_arguments),
							arguments,
							position,
							is_optional: false,
						};
						continue;
					}
					Ok(Break::Break) => {
						return Ok(top);
					}
					_ => {}
				}
			}

			// reader.skip();

			// TODO if not returning and comments, then we want to build the comments up.

			let first = reader.get_current().as_bytes().first().copied().unwrap_or(0);
			let next = match first {
				b'}' | b']' | b')' | b';' => AfterFirst::Exit,
				// b'/' if reader.starts_with_slice("//") => AfterFirst::SingleLineComment,
				// b'/' if reader.starts_with_slice("/*") => AfterFirst::MultiLineComment,
				b'+' if reader.starts_with_slice("++") => {
					AfterFirst::UnaryPostfixAssignmentOperator(UnaryPostfixAssignmentOperator(
						IncrementOrDecrement::Increment,
					))
				}
				b'-' if reader.starts_with_slice("--") => {
					AfterFirst::UnaryPostfixAssignmentOperator(UnaryPostfixAssignmentOperator(
						IncrementOrDecrement::Decrement,
					))
				}
				b'+' if reader.starts_with_slice("+=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::Add)
				}
				b'+' => AfterFirst::BinaryOperator(BinaryOperator::Add),
				b'-' if reader.starts_with_slice("-=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::Subtract)
				}
				b'-' => AfterFirst::BinaryOperator(BinaryOperator::Subtract),
				b'*' if reader.starts_with_slice("**=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::Exponent)
				}
				b'*' if reader.starts_with_slice("**") => {
					AfterFirst::BinaryOperator(BinaryOperator::Exponent)
				}
				b'*' if reader.starts_with_slice("*=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::Multiply)
				}
				b'*' => AfterFirst::BinaryOperator(BinaryOperator::Multiply),
				b'/' if reader.starts_with_slice("/=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::Divide)
				}
				b'/' => AfterFirst::BinaryOperator(BinaryOperator::Divide),
				b'%' if reader.starts_with_slice("%=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::Remainder)
				}
				b'%' => AfterFirst::BinaryOperator(BinaryOperator::Remainder),
				b'?' if reader.starts_with_slice("??=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::NullCoalescing)
				}
				b'?' if reader.starts_with_slice("??") => {
					AfterFirst::BinaryOperator(BinaryOperator::NullCoalescing)
				}
				b'&' if reader.starts_with_slice("&&=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::LogicalAnd)
				}
				b'&' if reader.starts_with_slice("&&") => {
					AfterFirst::BinaryOperator(BinaryOperator::LogicalAnd)
				}
				b'^' if reader.starts_with_slice("^=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::BitwiseXOr)
				}
				b'^' => AfterFirst::BinaryOperator(BinaryOperator::BitwiseXOr),
				b',' => AfterFirst::BinaryOperator(BinaryOperator::Comma),
				b'>' if reader.starts_with_slice(">>>=") => AfterFirst::BinaryAssignmentOperator(
					BinaryAssignmentOperator::BitwiseShiftRightUnsigned,
				),
				b'>' if reader.starts_with_slice(">>>") => {
					AfterFirst::BinaryOperator(BinaryOperator::BitwiseShiftRightUnsigned)
				}
				b'>' if reader.starts_with_slice(">>=") => AfterFirst::BinaryAssignmentOperator(
					BinaryAssignmentOperator::BitwiseShiftRight,
				),
				b'>' if reader.starts_with_slice(">>") => {
					AfterFirst::BinaryOperator(BinaryOperator::BitwiseShiftRight)
				}
				b'<' if reader.starts_with_slice("<=") => {
					AfterFirst::BinaryOperator(BinaryOperator::LessThanEqual)
				}
				b'>' if reader.starts_with_slice(">=") => {
					AfterFirst::BinaryOperator(BinaryOperator::GreaterThanEqual)
				}
				b'<' if reader.starts_with_slice("<<=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::BitwiseShiftLeft)
				}
				b'<' if reader.starts_with_slice("<<") => {
					AfterFirst::BinaryOperator(BinaryOperator::BitwiseShiftLeft)
				}
				b'=' if reader.starts_with_slice("=>") => {
					let non_comment = top.get_non_comment();
					match non_comment.as_identifier() {
						Ok((name, position)) => {
							let identifier =
								crate::VariableIdentifier::Standard(name.to_owned(), position);
							let function = ArrowFunction::from_reader_with_first_parameter(
								reader,
								false,
								identifier.into(),
							)?;
							top = Expression::ArrowFunction(Box::new(function));
							continue;
						}
						Err(top) => {
							if let Expression::Parenthesised(item, _) = top {
								// TODO
								return Err(ParseError::new(
									ParseErrors::InvalidArrowFunctionParameter,
									item.get_position(),
								));
							} else {
								return Err(ParseError::new(
									ParseErrors::InvalidArrowFunctionParameter,
									top.get_position(),
								));
							}
						}
					}
				}
				b'=' if reader.starts_with_slice("===") => {
					AfterFirst::BinaryOperator(BinaryOperator::StrictEqual)
				}
				b'=' if reader.starts_with_slice("==") => {
					AfterFirst::BinaryOperator(BinaryOperator::Equal)
				}
				// Better way of specifying generic arguments
				b'.' if reader.starts_with_slice(".<") => AfterFirst::BetterGenericInstantation,
				b'.' => AfterFirst::PropertyAccess { is_optional: false },
				b'[' => AfterFirst::Index { is_optional: false },
				b'?' if reader.starts_with_slice("?.") => {
					enum Out {
						ConditionalDotNumber,
					}

					// TODO does this break precedence early returns
					let out: Result<AfterFirst, Out> =
						reader.try_parse(|reader: &mut crate::Lexer| -> Result<AfterFirst, Out> {
							reader.advance(2);
							if reader.starts_with('[') {
								Ok(AfterFirst::Index { is_optional: true })
							} else if reader.get_current().starts_with(['<', '(']) {
								Ok(AfterFirst::FunctionCall { is_optional: true })
							} else if reader
								.get_current()
								.starts_with(|chr: char| chr.is_ascii_digit())
							{
								Err(Out::ConditionalDotNumber)
							} else {
								Ok(AfterFirst::PropertyAccess { is_optional: true })
							}
						});

					match out {
						Ok(out) => out,
						Err(Out::ConditionalDotNumber) => AfterFirst::ConditionalTernary,
					}
				}
				b'(' => AfterFirst::FunctionCall { is_optional: false },
				b'`' => AfterFirst::TemplateLiteralStart,
				b'=' => AfterFirst::Assign,
				b'i' if reader.is_keyword("instanceof") => AfterFirst::InstanceOf,
				b'i' if reader.is_keyword("in") => AfterFirst::In,
				#[cfg(feature = "full-typescript")]
				b'a' if reader.is_keyword("as") => AfterFirst::As,
				#[cfg(feature = "full-typescript")]
				b's' if reader.is_keyword("satisfies") => AfterFirst::Satisfies,
				// non enabled caught later
				#[cfg(feature = "extras")]
				b'<' if reader.starts_with_slice("<@>") => AfterFirst::BinaryOperator(BinaryOperator::Compose),
				// non enabled caught later
				#[cfg(feature = "extras")]
				b'|' if reader.starts_with_slice("|>") => AfterFirst::BinaryOperator(BinaryOperator::Pipe),
				#[cfg(feature = "extras")]
				b'i' if reader.is_keyword("is") && reader.get_options().extras.is_expressions => AfterFirst::Is,
				#[cfg(feature = "extras")]
				b'a' if reader.is_keyword("and")
					&& reader.get_options().extras.keyword_logical_operators =>
				{
					AfterFirst::AndKeyword
				}
				#[cfg(feature = "extras")]
				b'o' if reader.is_keyword("or")
					&& reader.get_options().extras.keyword_logical_operators =>
				{
					AfterFirst::OrKeyword
				}
				#[cfg(feature = "extras")]
				b'i' if reader.is_keyword("if")
					&& reader.get_options().extras.keyword_logical_operators =>
				{
					AfterFirst::IfKeyword
				}
				b'!' if reader.starts_with_slice("!==") => {
					AfterFirst::BinaryOperator(BinaryOperator::StrictNotEqual)
				}
				b'!' if reader.starts_with_slice("!=") => {
					AfterFirst::BinaryOperator(BinaryOperator::NotEqual)
				}
				b'?' => AfterFirst::ConditionalTernary,
				b'<' => AfterFirst::BinaryOperator(BinaryOperator::LessThan),
				b'>' => AfterFirst::BinaryOperator(BinaryOperator::GreaterThan),
				b'|' if reader.starts_with_slice("||=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::LogicalOr)
				}
				b'|' if reader.starts_with_slice("||") => {
					AfterFirst::BinaryOperator(BinaryOperator::LogicalOr)
				}
				b'&' if reader.starts_with_slice("&=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::BitwiseAnd)
				}
				b'&' => AfterFirst::BinaryOperator(BinaryOperator::BitwiseAnd),
				b'|' if reader.starts_with_slice("|=") => {
					AfterFirst::BinaryAssignmentOperator(BinaryAssignmentOperator::BitwiseOr)
				}
				b'|' => AfterFirst::BinaryOperator(BinaryOperator::BitwiseOr),
				#[cfg(feature = "full-typescript")]
				b'!' => AfterFirst::NonNullAssertion,
				_ => AfterFirst::Exit,
			};

			match next {
				// c @ (AfterFirst::SingleLineComment | AfterFirst::MultiLineComment) => {
				// 	// TODO only for statements!
				// 	if reader.last_was_from_new_line() == 0 {
				// 		let is_multiline = matches!(c, AfterFirst::MultiLineComment);
				// 		reader.advance(2);
				// 		let content = reader.parse_comment_literal(is_multiline)?.to_owned();
				// 		let position = top.get_position().union(reader.get_end());
				// 		top = Expression::Comment {
				// 			is_multiline,
				// 			content,
				// 			position,
				// 			on: Box::new(top),
				// 			prefix: false,
				// 		};
				// 	} else {
				// 		return Ok(top);
				// 	}
				// }
				AfterFirst::UnaryPostfixAssignmentOperator(operator) => {
					if operator
						.associativity_direction()
						.should_return(return_precedence, operator.precedence())
					{
						return Ok(top);
					}
					if reader.last_was_from_new_line() > 0 {
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

					#[cfg(feature = "extras")]
					if !reader.get_options().extras.extra_operators && operator.is_non_standard() {
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
				AfterFirst::BetterGenericInstantation => {
					reader.advance(".<".len() as u32);
					let (type_arguments, _) = bracketed_items_from_reader(reader, ">")?;
					// TODO instantation here
					reader.expect_chr('(')?;
					let (arguments, _) = bracketed_items_from_reader(reader, ")")?;
					let position = top.get_position().union(reader.get_end());
					top = Expression::FunctionCall {
						function: Box::new(top),
						type_arguments: Some(type_arguments),
						arguments,
						position,
						is_optional: false,
					};
				}
				AfterFirst::FunctionCall { is_optional } => {
					if AssociativityDirection::LeftToRight
						.should_return(return_precedence, FUNCTION_CALL_PRECEDENCE)
					{
						return Ok(top);
					}

					// TODO differentiated between less than?
					let type_arguments = if reader.is_operator_advance("<") {
						let (type_arguments, _) = bracketed_items_from_reader(reader, ">")?;
						reader.expect_chr('(')?;
						Some(type_arguments)
					} else {
						reader.advance(1);
						None
					};

					let (arguments, _) = bracketed_items_from_reader(reader, ")")?;

					#[cfg(feature = "extras")]
					if reader.get_options().extras.is_expressions
						&& reader.is_operator("{")
						&& let Expression::VariableReference(ref variable, ..) = top
						&& variable == "is"
						&& arguments.len() > 0
						&& arguments
							.iter()
							.find(|expr: &&ExpressionOrSpreadExpression| expr.is_spread())
							.is_none()
					{
						let matcher = MultipleExpression::from_expressions(
							arguments
								.into_iter()
								.map(|argument| argument.value_and_spread().1)
								.collect(),
						);
						let expr = IsExpression::from_reader_with_matcher(
							reader,
							top.get_position().get_start(),
							Box::new(matcher),
						)?;
						return Ok(Expression::IsExpression(expr));
					}

					let position = top.get_position().union(reader.get_end());
					top = Expression::FunctionCall {
						function: Box::new(top),
						type_arguments,
						arguments,
						position,
						is_optional,
					};
				}
				AfterFirst::AndKeyword => {
					let operator = BinaryOperator::LogicalAndKeyword;
					if operator
						.associativity_direction()
						.should_return(return_precedence, operator.precedence())
					{
						return Ok(top);
					}

					reader.advance(3);

					// #[cfg(feature = "extras")]
					// if !reader.get_options().extras.extra_operators && operator.is_non_standard() {
					// 	let position =
					// 		source_map::Start(reader.get_end().0).with_length(operator_len);
					// 	return Err(ParseError::new(
					// 		ParseErrors::NonStandardSyntaxUsedWithoutEnabled {
					// 			syntax: operator.to_str(),
					// 		},
					// 		position,
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
				AfterFirst::OrKeyword => {
					// TODO seperate operator?
					let operator = BinaryOperator::LogicalOrKeyword;
					if operator
						.associativity_direction()
						.should_return(return_precedence, operator.precedence())
					{
						return Ok(top);
					}

					reader.advance(2);

					// #[cfg(feature = "extras")]
					// if !reader.get_options().extras.extra_operators && operator.is_non_standard() {
					// 	let position =
					// 		source_map::Start(reader.get_end().0).with_length(operator_len);
					// 	return Err(ParseError::new(
					// 		ParseErrors::NonStandardSyntaxUsedWithoutEnabled {
					// 			syntax: operator.to_str(),
					// 		},
					// 		position,
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
				AfterFirst::IfKeyword => {
					if AssociativityDirection::RightToLeft
						.should_return(return_precedence, CONDITIONAL_TERNARY_PRECEDENCE)
					{
						return Ok(top);
					}
					reader.advance(2);
					let condition_position = top.get_position();
					let condition = Box::new(top);
					let truthy_result = Box::new(Self::from_reader(reader)?);
					let (falsy_result, position) = if reader.is_keyword_advance("else") {
						let falsy_result = Self::from_reader(reader)?;
						let position = condition_position.union(falsy_result.get_position());
						let falsy_result = Box::new(falsy_result);
						(falsy_result, position)
					} else {
						let position = condition_position.union(truthy_result.get_position());
						(Box::new(Expression::Null(source_map::Span::NULL)), position)
					};
					top = Expression::ConditionalTernary {
						position,
						is_keyword: true,
						condition,
						truthy_result,
						falsy_result,
					};
				}
				AfterFirst::Index { is_optional } => {
					if AssociativityDirection::LeftToRight
						.should_return(return_precedence, INDEX_PRECEDENCE)
					{
						return Ok(top);
					}

					reader.advance(1);

					let indexer = MultipleExpression::from_reader(reader)?;
					let end = reader.expect_chr(']')?;
					let position = top.get_position().union(end);
					top = Expression::Index {
						position,
						indexee: Box::new(top),
						indexer: Box::new(indexer),
						is_optional,
					};
				}
				AfterFirst::PropertyAccess { is_optional } => {
					if AssociativityDirection::LeftToRight
						.should_return(return_precedence, MEMBER_ACCESS_PRECEDENCE)
					{
						return Ok(top);
					}

					// Optional already skipped
					if !is_optional {
						reader.advance(1);
					}

					let property = if reader.get_options().features.partial_syntax
						&& let Some(length) = reader.get_current().find(|c: char| c.is_alphabetic())
						&& length > 0
					{
						let position =
							source_map::Start(top.get_position().get_end().0).with_length(length);
						let marker = reader.new_partial_point_marker(position);
						PropertyReference::Marker(marker)
					} else {
						let is_private = reader.is_operator_advance("#");
						let property =
							reader.parse_identifier("property name", false)?.into_owned();
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
					if reader.get_options().type_annotations.type_annotations() {
						// if options.type_annotations
						let position = top.get_position().union(reader.get_end());
						top = Self::SpecialOperators(
							SpecialOperators::NonNullAssertion(Box::new(top)),
							position,
						);
					}
				}
				// AfterFirst::Colon => {
				// 	TODO
				// 	reader.advance(1);
				// 	let ty = TypeAnnotation::from_reader(reader)?;
				// 	let position = top.get_position().union(reader.get_end());
				// 	top = Self::SpecialOperators(
				// 		SpecialOperators::WithTypeAnnotation {
				// 			value: Box::new(top),
				// 			type_annotation: Box::new(ty),
				// 		},
				// 		position,
				// 	);
				// }
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
					reader.expect_chr(':')?;
					let falsy_result = Self::from_reader(reader)?;
					let position = condition_position.union(falsy_result.get_position());
					let falsy_result = Box::new(falsy_result);
					top = Expression::ConditionalTernary {
						position,
						is_keyword: false,
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
							.starts_with(crate::lexer::utilities::is_identifier_continutation)
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
							if !reader.get_options().extras.is_expressions {
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
							.starts_with(crate::lexer::utilities::is_identifier_continutation)
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
							.starts_with(crate::lexer::utilities::is_identifier_continutation)
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
			| Self::BigIntLiteral(..)
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
			Self::Comment { on, .. } => on.get_precedence(),
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
			Self::BigIntLiteral(num, _) => {
				buf.push_str(&num.source);
				buf.push('n');
			}
			Self::StringLiteral(string, quoting, _) => {
				buf.push(quoting.as_char());
				buf.push_str(string);
				buf.push(quoting.as_char());
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
				SpecialOperators::Satisfies { value, type_annotation } => {
					value.to_string_from_buffer(buf, options, local);
					if options.include_type_annotations {
						buf.push_str(" satisfies ");
						type_annotation.to_string_from_buffer(buf, options, local);
					}
				}
				SpecialOperators::WithTypeAnnotation { value, type_annotation } => {
					value.to_string_from_buffer(buf, options, local);
					if options.include_type_annotations {
						buf.push_str(": ");
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
					if let Some((is_delegated, yielded)) = yielded {
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
			Self::Import(ImportExpression::ImportSource { path, .. }) => {
				buf.push_str("import.source(");
				path.to_string_from_buffer(buf, options, local);
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
				// Improves numbers. See: https://github.com/kaleidawave/ezno/pull/158#issuecomment-2169621017
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
				class.on.to_string_from_buffer(buf, options, local);
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

/// because of `await using`
pub(crate) fn parse_after_await(
	reader: &mut crate::Lexer,
	start: source_map::Start,
	using: bool,
) -> ParseResult<MultipleExpression> {
	if reader.starts_with_expression_delimiter() {
		let position = start.with_length(5);
		let expression = Expression::VariableReference("await".to_owned(), position);
		Expression::from_reader_after_first_expression(reader, 0, expression)
			.map(MultipleExpression)
	} else if using {
		// TODO position broken
		let operator = UnaryOperator::Await;
		let position = start.with_length(5);
		let operand = Box::new(Expression::VariableReference("using".to_owned(), position));
		let top = Expression::UnaryOperation { operator, operand, position };
		Expression::from_reader_after_first_expression(reader, 0, top).map(MultipleExpression)
	} else {
		let operator = UnaryOperator::Await;
		let operand =
			Box::new(Expression::from_reader_with_precedence(reader, operator.precedence())?);
		let position = start.union(operand.get_position());
		let top = Expression::UnaryOperation { operator, operand, position };
		Expression::from_reader_after_first_expression(reader, 0, top).map(MultipleExpression)
	}
}

/// because of `import {}`
pub(crate) fn parse_after_import(
	reader: &mut crate::Lexer,
	// start of import keyword
	start: source_map::Start,
) -> ParseResult<ImportExpression> {
	if reader.is_operator_advance(".") {
		#[cfg(feature = "extras")]
		if reader.is_keyword_advance("source") {
			reader.expect_chr('(')?;
			let path = Box::new(Expression::from_reader(reader)?);
			let _ = reader.is_operator_advance(",");
			reader.expect_chr(')')?;
			let position = start.union(reader.get_end());
			Ok(ImportExpression::ImportSource { path, position })
		} else if reader.is_keyword_advance("defer") {
			reader.expect_chr('(')?;
			let path = Box::new(Expression::from_reader(reader)?);
			let _ = reader.is_operator_advance(",");
			reader.expect_chr(')')?;
			let position = start.union(reader.get_end());
			Ok(ImportExpression::ImportDefer { path, position })
		} else if reader.is_keyword_advance("meta") {
			let position = start.union(reader.get_end());
			Ok(ImportExpression::ImportMeta(position))
		} else {
			return Err(crate::lexer::utilities::expected_one_of_items(
				reader,
				&["source", "defer", "meta"],
			));
		}

		#[cfg(not(feature = "extras"))]
		if reader.is_keyword_advance("meta") {
			let position = start.union(reader.get_end());
			Ok(ImportExpression::ImportMeta(position))
		} else {
			Err(crate::lexer::utilities::expected_one_of_items(
				reader,
				&["source", "defer", "meta"],
			))
		}
	} else if reader.is_operator_advance("(") {
		let path = Expression::from_reader(reader)?;
		// if let Expression::StringLiteral(path, ..) = &path {
		//     state.constant_imports.push(path.clone());
		// }

		// TODO may cause issue?
		let options = if reader.is_operator_advance(",") && !reader.is_operator(")") {
			Some(Box::new(Expression::from_reader(reader)?))
		} else {
			None
		};
		let _ = reader.is_operator_advance(",");
		let end = reader.expect_chr(')')?;
		Ok(ImportExpression::DynamicImport {
			path: Box::new(path),
			options,
			position: start.union(end),
		})
	} else {
		let position = reader.get_start().with_length(1);
		let reason = ParseErrors::UnexpectedCharacter {
			expected: &['.', '('],
			found: reader.get_current().chars().next(),
		};
		Err(ParseError::new(reason, position))
	}
}

/// Represents expressions that can be the comma operator. Has a special new type to discern the places
/// where this is allowed
/// TODO pub(crate)
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
	pub fn get_inner(self) -> Expression {
		self.0
	}

	#[must_use]
	pub fn get_inner_ref(&self) -> &Expression {
		&self.0
	}

	#[must_use]
	pub fn get_left_ref(&self) -> &Expression {
		fn get_left_ref_(expr: &Expression) -> &Expression {
			if let Expression::BinaryOperation { operator: BinaryOperator::Comma, lhs, .. } = expr {
				get_left_ref_(lhs)
			} else {
				expr
			}
		}

		get_left_ref_(&self.0)
	}

	#[must_use]
	pub fn get_left(self) -> Expression {
		fn get_left_(expr: Expression) -> Expression {
			if let Expression::BinaryOperation { operator: BinaryOperator::Comma, lhs, .. } = expr {
				get_left_(*lhs)
			} else {
				expr
			}
		}

		get_left_(self.0)
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

	pub(crate) fn from_first_expression(
		reader: &mut crate::Lexer,
		mut top: Expression,
	) -> ParseResult<Self> {
		while reader.is_operator_advance(",") {
			let rhs = Expression::from_reader_with_precedence(
				reader,
				BinaryOperator::Comma.precedence(),
			)?;
			top = Expression::BinaryOperation {
				position: top.get_position().union(rhs.get_position()),
				lhs: Box::new(top),
				operator: BinaryOperator::Comma,
				rhs: Box::new(rhs),
			};
		}
		Ok(Self(top))
	}

	pub fn from_expressions(expressions: Vec<Expression>) -> Self {
		let mut expressions = expressions.into_iter();
		let mut top = expressions.next().unwrap();
		while let Some(next) = expressions.next() {
			top = Expression::BinaryOperation {
				position: top.get_position().union(next.get_position()),
				lhs: Box::new(top),
				operator: BinaryOperator::Comma,
				rhs: Box::new(next),
			};
		}
		MultipleExpression(top)
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
	nodes: &[ExpressionOrSpreadExpression],
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
		if let (true, Expression::ArrayLiteral(items, _)) = node.value_and_spread_ref() {
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
	/// For Flow and arrow function parsing
	WithTypeAnnotation {
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

/// The RHS of `as`. Either `*expr* as *type*` or `*expr* as const`
#[cfg(feature = "full-typescript")]
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable)]
pub enum TypeOrConst {
	Type(Box<TypeAnnotation>),
	Const(Span),
}

/// The LHS of `in`. Either `#*name* in *expr*` or `*expr* in *expr*`
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable)]
pub enum InExpressionLHS {
	PrivateProperty(String),
	Expression(Box<Expression>),
}

impl TryFrom<InExpressionLHS> for Box<Expression> {
	type Error = ();

	fn try_from(in_expression: InExpressionLHS) -> Result<Box<Expression>, ()> {
		match in_expression {
			InExpressionLHS::PrivateProperty(_) => Err(()),
			InExpressionLHS::Expression(expr) => Ok(expr),
		}
	}
}

/// "super" cannot be used alone. Either `super.*name*` or `super(*args*)
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable)]
pub enum SuperReference {
	Call { arguments: Vec<ExpressionOrSpreadExpression> },
	PropertyAccess(PropertyLike),
}

/// The RHS of property access / index. Either `*expr*.*name*` or `*expr*.[*expr*]`
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
		path: Box<Expression>,
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
pub struct ExpressionOrSpreadExpression(Expression);

impl ListItem for ExpressionOrSpreadExpression {
	type LAST = ();
}

impl ASTNode for ExpressionOrSpreadExpression {
	fn get_position(&self) -> Span {
		self.0.get_position()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		// Precedence of `...` is weird (same level as others) so doing this here
		if reader.is_operator_advance("...") {
			let operand = Expression::from_reader(reader)?;
			let position = start.union(operand.get_position());
			let value = Expression::UnaryOperation {
				operand: Box::new(operand),
				operator: UnaryOperator::Spread,
				position,
			};
			Ok(Self(value))
		} else {
			Expression::from_reader(reader).map(Self)
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		Expression::to_string_from_buffer(&self.0, buf, options, local)
	}
}

impl From<Expression> for ExpressionOrSpreadExpression {
	fn from(value: Expression) -> Self {
		ExpressionOrSpreadExpression(value)
	}
}

impl ExpressionOrSpreadExpression {
	pub fn value_and_spread(self) -> (bool, Expression) {
		if let Expression::UnaryOperation {
			operator: UnaryOperator::Spread,
			operand,
			position: _,
		} = self.0
		{
			(true, *operand)
		} else {
			(false, self.0)
		}
	}

	pub fn value_and_spread_ref(&self) -> (bool, &Expression) {
		if let Expression::UnaryOperation {
			operator: UnaryOperator::Spread,
			operand,
			position: _,
		} = &self.0
		{
			(true, &*operand)
		} else {
			(false, &self.0)
		}
	}

	pub fn is_spread(&self) -> bool {
		matches!(&self.0, Expression::UnaryOperation { operator: UnaryOperator::Spread, .. })
	}
}

#[derive(Debug, Clone, Visitable)]
#[apply(derive_ASTNode)]
pub struct ArrayElement(pub Option<ExpressionOrSpreadExpression>);

impl ASTNode for ArrayElement {
	fn get_position(&self) -> Span {
		self.0.as_ref().map_or(Span::NULL, ASTNode::get_position)
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		if reader.is_one_of_operators(&[",", "]"]).is_some() {
			Ok(Self(None))
		} else {
			ExpressionOrSpreadExpression::from_reader(reader).map(Some).map(Self)
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

impl ListItem for ArrayElement {
	type LAST = ();

	fn skip_trailing() -> bool {
		false
	}
}

impl ArrayElement {
	pub fn inner_ref(&self) -> Option<&Expression> {
		if let Some(ref expr) = self.0 { Some(&expr.0) } else { None }
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
		if let Expression::FunctionCall { arguments, function, .. } = self
			&& arguments.is_empty()
			&& let Expression::Parenthesised(expression, _) = &**function
			&& let MultipleExpression(Expression::ArrowFunction(function)) = &**expression
		{
			Some(&function.body)
		} else {
			None
		}
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

	/// Recurses to find first non parenthesized expression
	#[must_use]
	pub fn get_non_comment(self) -> Self {
		if let Expression::Comment { on, .. } = self { on.get_non_comment() } else { self }
	}

	#[must_use]
	pub fn get_non_right_comment(&self) -> &Self {
		if let Expression::Comment { on, prefix: false, .. } = self {
			on.get_non_right_comment()
		} else {
			self
		}
	}

	/// Recurses to find first non parenthesized expression
	#[must_use]
	pub fn as_identifier(self) -> Result<(String, Span), Self> {
		match self.get_non_comment() {
			Self::VariableReference(name, pos) => Ok((name, pos)),
			Self::SpecialOperators(SpecialOperators::Yield { yielded: None }, position) => {
				Ok(("yield".to_owned(), position))
			}
			expr => Err(expr),
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
