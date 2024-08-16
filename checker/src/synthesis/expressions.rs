use std::{borrow::Cow, convert::TryInto, str::FromStr};

use parser::{
	ast::TypeOrConst,
	expressions::{
		object_literal::{ObjectLiteral, ObjectLiteralMember},
		operators::{
			BinaryOperator, IncrementOrDecrement as ParserIncrementOrDecrement, UnaryOperator,
			UnaryPrefixAssignmentOperator,
		},
		ArrayElement, FunctionArgument, MultipleExpression, SpecialOperators, SuperReference,
		TemplateLiteral,
	},
	functions::MethodHeader,
	ASTNode, Expression, ExpressionOrStatementPosition,
};
use source_map::{Nullable, SpanWithSource};

use crate::{
	context::Environment,
	diagnostics::{TypeCheckError, TypeCheckWarning, TypeStringRepresentation},
	features::{
		self,
		assignments::{AssignmentKind, AssignmentReturnStatus, IncrementOrDecrement},
		await_expression,
		conditional::new_conditional_context,
		functions::{
			function_to_property, register_arrow_function, register_expression_function,
			synthesise_function, GetterSetter,
		},
		in_operator,
		objects::ObjectBuilder,
		operations::is_null_or_undefined,
		operations::{
			evaluate_logical_operation_with_expression,
			evaluate_pure_binary_operation_handle_errors, evaluate_pure_unary_operator,
			EqualityAndInequality, MathematicalAndBitwise, PureUnary,
		},
		template_literal::synthesise_template_literal_expression,
		variables::VariableWithValue,
	},
	types::{
		calling::CalledWithNew,
		properties::Publicity,
		{Constant, TypeId},
	},
	types::{
		calling::{CallingInput, UnsynthesisedArgument},
		get_larger_type,
		logical::{Logical, LogicalOrValid},
		printing::{print_property_key, print_type},
		properties::{
			get_properties_on_single_type, get_property_unbound, AccessMode, PropertyKey,
		},
		Constructor,
	},
	CheckingData, Decidable, Instance, PropertyValue, SpecialExpressions,
};

use super::{
	assignments::SynthesiseToAssignable,
	classes::synthesise_class_declaration,
	extensions::{is_expression::synthesise_is_expression, jsx::synthesise_jsx_root},
	parser_property_key_to_checker_property_key,
	type_annotations::synthesise_type_annotation,
	EznoParser,
};

pub(super) fn synthesise_multiple_expression<T: crate::ReadFromFS>(
	expression: &MultipleExpression,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	expecting: TypeId,
) -> TypeId {
	match expression {
		MultipleExpression::Multiple { lhs, rhs, position: _ } => {
			synthesise_multiple_expression(lhs, environment, checking_data, TypeId::ANY_TYPE);
			synthesise_expression(rhs, environment, checking_data, expecting)
		}
		MultipleExpression::Single(expression) => {
			synthesise_expression(expression, environment, checking_data, expecting)
		}
	}
}

pub(super) fn synthesise_expression<T: crate::ReadFromFS>(
	expression: &Expression,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	expecting: TypeId,
) -> TypeId {
	let instance: Instance = match expression {
		Expression::StringLiteral(value, ..) => {
			return checking_data.types.new_constant_type(Constant::String(value.clone()))
		}
		Expression::NumberLiteral(value, ..) => {
			let not_nan = if let Ok(v) = f64::try_from(value.clone()) {
				v.try_into().unwrap()
			} else {
				crate::utilities::notify!("TODO big int");
				return TypeId::ERROR_TYPE;
			};
			// if not_nan == 6. {
			// 	crate::utilities::notify!("{:?}", environment.get_all_named_types());
			// }
			return checking_data.types.new_constant_type(Constant::Number(not_nan));
		}
		Expression::BooleanLiteral(value, ..) => {
			return checking_data.types.new_constant_type(Constant::Boolean(*value))
		}
		Expression::ArrayLiteral(elements, _) => {
			fn synthesise_array_item<T: crate::ReadFromFS>(
				idx: &Decidable<usize>,
				element: &ArrayElement,
				environment: &mut Environment,
				checking_data: &mut CheckingData<T, super::EznoParser>,
			) -> Option<(PropertyKey<'static>, TypeId)> {
				element.0.as_ref().and_then(|element| match element {
					FunctionArgument::Standard(element) => {
						// TODO based off above
						let expecting = TypeId::ANY_TYPE;
						let expression_type =
							synthesise_expression(element, environment, checking_data, expecting);
						let property = match idx {
							Decidable::Known(idx) => PropertyKey::from_usize(*idx),
							Decidable::Unknown(_) => {
								checking_data.raise_unimplemented_error(
									"property after array spread",
									element.get_position().with_source(environment.get_source()),
								);
								PropertyKey::Type(TypeId::NUMBER_TYPE)
							}
						};
						Some((property, expression_type))
					}
					FunctionArgument::Spread(_expr, position) => {
						{
							checking_data.raise_unimplemented_error(
								"Spread elements",
								position.with_source(environment.get_source()),
							);
						}
						crate::utilities::notify!("Skipping spread");
						let property = match idx {
							Decidable::Known(idx) => PropertyKey::from_usize(*idx),
							Decidable::Unknown(_) => {
								checking_data.raise_unimplemented_error(
									"property after array spread",
									element.get_position().with_source(environment.get_source()),
								);
								PropertyKey::Type(TypeId::NUMBER_TYPE)
							}
						};
						Some((property, TypeId::ERROR_TYPE))
					}
					FunctionArgument::Comment { .. } => None,
				})
			}

			let mut basis = ObjectBuilder::new(
				Some(TypeId::ARRAY_TYPE),
				&mut checking_data.types,
				expression.get_position().with_source(environment.get_source()),
				&mut environment.info,
			);

			// TODO remove enumerate, add add function and more
			for (idx, value) in elements.iter().enumerate() {
				let spread_expression_position =
					value.get_position().with_source(environment.get_source());

				if let Some((key, value)) =
					synthesise_array_item(&Decidable::Known(idx), value, environment, checking_data)
				{
					basis.append(
						Publicity::Public,
						key,
						crate::types::properties::PropertyValue::Value(value),
						spread_expression_position,
						&mut environment.info,
					);
				}
			}

			{
				// TODO spread
				let length = checking_data.types.new_constant_type(Constant::Number(
					(elements.len() as f64).try_into().unwrap(),
				));
				let value = crate::types::properties::PropertyValue::Value(length);

				// TODO: Should there be a position here?
				basis.append(
					Publicity::Public,
					PropertyKey::String("length".into()),
					value,
					expression.get_position().with_source(environment.get_source()),
					&mut environment.info,
				);
			}

			Instance::RValue(basis.build_object())
		}
		Expression::ObjectLiteral(object_literal) => Instance::RValue(synthesise_object_literal(
			object_literal,
			checking_data,
			environment,
			object_literal.position.with_source(environment.get_source()),
			expecting,
		)),
		Expression::TemplateLiteral(TemplateLiteral { tag, parts, last, position }) => {
			let tag = tag.as_ref().map(|expr| {
				synthesise_expression(expr, environment, checking_data, TypeId::ANY_TYPE)
			});

			Instance::RValue(synthesise_template_literal_expression::<_, EznoParser>(
				tag,
				parts.iter().map(|(l, r)| (l.as_str(), r)),
				last.as_str(),
				position.with_source(environment.get_source()),
				environment,
				checking_data,
			))
		}
		Expression::BinaryOperation { lhs, operator, rhs, position } => {
			let lhs_ty = synthesise_expression(lhs, environment, checking_data, TypeId::ANY_TYPE);

			if let BinaryOperator::LogicalAnd
			| BinaryOperator::LogicalOr
			| BinaryOperator::NullCoalescing = operator
			{
				let operator = match operator {
					BinaryOperator::LogicalAnd => crate::features::operations::LogicalOperator::And,
					BinaryOperator::LogicalOr => crate::features::operations::LogicalOperator::Or,
					BinaryOperator::NullCoalescing => {
						crate::features::operations::LogicalOperator::NullCoalescing
					}
					_ => unreachable!(),
				};
				return evaluate_logical_operation_with_expression(
					(lhs_ty, lhs.get_position()),
					operator,
					&**rhs,
					checking_data,
					environment,
					expecting, // TODO unwrap
				)
				.unwrap();
			}

			let rhs_ty = synthesise_expression(rhs, environment, checking_data, TypeId::ANY_TYPE);

			if lhs_ty == TypeId::ERROR_TYPE || rhs_ty == TypeId::ERROR_TYPE {
				return TypeId::ERROR_TYPE;
			}

			let lhs_pos = ASTNode::get_position(&**lhs).with_source(environment.get_source());
			let rhs_pos = ASTNode::get_position(&**rhs).with_source(environment.get_source());

			let operator = match operator {
				BinaryOperator::Add => MathematicalAndBitwise::Add.into(),
				BinaryOperator::Subtract => MathematicalAndBitwise::Subtract.into(),
				BinaryOperator::Multiply => MathematicalAndBitwise::Multiply.into(),
				BinaryOperator::Divide => MathematicalAndBitwise::Divide.into(),
				BinaryOperator::Modulo => MathematicalAndBitwise::Modulo.into(),
				BinaryOperator::Exponent => MathematicalAndBitwise::Exponent.into(),
				BinaryOperator::BitwiseShiftLeft => MathematicalAndBitwise::BitwiseShiftLeft.into(),
				BinaryOperator::BitwiseShiftRight => {
					MathematicalAndBitwise::BitwiseShiftRight.into()
				}
				BinaryOperator::BitwiseShiftRightUnsigned => {
					MathematicalAndBitwise::BitwiseShiftRightUnsigned.into()
				}
				BinaryOperator::BitwiseAnd => MathematicalAndBitwise::BitwiseAnd.into(),
				BinaryOperator::BitwiseXOr => MathematicalAndBitwise::BitwiseXOr.into(),
				BinaryOperator::BitwiseOr => MathematicalAndBitwise::BitwiseOr.into(),
				BinaryOperator::StrictEqual => EqualityAndInequality::StrictEqual.into(),
				BinaryOperator::StrictNotEqual => EqualityAndInequality::StrictNotEqual.into(),
				BinaryOperator::Equal => EqualityAndInequality::Equal.into(),
				BinaryOperator::NotEqual => EqualityAndInequality::NotEqual.into(),
				BinaryOperator::GreaterThan => EqualityAndInequality::GreaterThan.into(),
				BinaryOperator::LessThan => EqualityAndInequality::LessThan.into(),
				BinaryOperator::LessThanEqual => EqualityAndInequality::LessThanOrEqual.into(),
				BinaryOperator::GreaterThanEqual => {
					EqualityAndInequality::GreaterThanOrEqual.into()
				}
				BinaryOperator::LogicalAnd
				| BinaryOperator::LogicalOr
				| BinaryOperator::NullCoalescing => {
					unreachable!()
				}
				BinaryOperator::Pipe | BinaryOperator::Compose => {
					checking_data.raise_unimplemented_error(
						"special operations",
						position.with_source(environment.get_source()),
					);
					return TypeId::ERROR_TYPE;
				}
			};
			Instance::RValue(evaluate_pure_binary_operation_handle_errors(
				(lhs_ty, lhs_pos),
				operator,
				(rhs_ty, rhs_pos),
				checking_data,
				environment,
			))
		}
		Expression::UnaryOperation { operand, operator, position } => {
			match operator {
				UnaryOperator::Plus => {
					let operand_type = synthesise_expression(
						operand,
						environment,
						checking_data,
						TypeId::ANY_TYPE,
					);
					return if get_larger_type(operand_type, &checking_data.types)
						== TypeId::NUMBER_TYPE
					{
						// TODO add warning here
						operand_type
					} else {
						checking_data.raise_unimplemented_error(
							"Unary plus operator",
							position.with_source(environment.get_source()),
						);
						TypeId::ERROR_TYPE
					};
				}
				UnaryOperator::Negation | UnaryOperator::BitwiseNot | UnaryOperator::LogicalNot => {
					let operand_type = synthesise_expression(
						operand,
						environment,
						checking_data,
						TypeId::ANY_TYPE,
					);
					let operator = match operator {
						UnaryOperator::Negation => PureUnary::Negation,
						UnaryOperator::BitwiseNot => PureUnary::BitwiseNot,
						UnaryOperator::LogicalNot => PureUnary::LogicalNot,
						_ => unreachable!(),
					};
					Instance::RValue(evaluate_pure_unary_operator(
						operator,
						operand_type,
						&mut checking_data.types,
						checking_data.options.strict_casts,
					))
				}
				UnaryOperator::Await => {
					// TODO get promise T
					let expecting = TypeId::ANY_TYPE;
					let on = synthesise_expression(operand, environment, checking_data, expecting);
					Instance::RValue(await_expression(
						on,
						environment,
						checking_data,
						position.with_source(environment.get_source()),
					))
				}
				UnaryOperator::TypeOf => {
					let operand_type = synthesise_expression(
						operand,
						environment,
						checking_data,
						TypeId::ANY_TYPE,
					);
					Instance::RValue(features::type_of_operator(
						operand_type,
						&mut checking_data.types,
					))
				}
				UnaryOperator::Void => {
					let _operand_type = synthesise_expression(
						operand,
						environment,
						checking_data,
						TypeId::ANY_TYPE,
					);
					return TypeId::UNDEFINED_TYPE;
				}
				UnaryOperator::Delete => {
					// TODO synthesise anyway? and use mappings
					// TODO more expressions
					match &**operand {
						Expression::PropertyAccess {
							parent,
							property,
							is_optional: _,
							position: _,
						} => {
							let on = synthesise_expression(
								parent,
								environment,
								checking_data,
								TypeId::ANY_TYPE,
							);
							match property {
								parser::PropertyReference::Standard { property, is_private } => {
									let publicity = if *is_private {
										Publicity::Private
									} else {
										Publicity::Public
									};
									let property =
										PropertyKey::String(Cow::Owned(property.clone()));

									let position = position.with_source(environment.get_source());
									match crate::features::delete_operator(
										(publicity, property),
										on,
										position,
										environment,
										&mut checking_data.types,
									) {
										Ok(result) => Instance::RValue(result),
										Err(err) => {
											checking_data.diagnostics_container.add_error(
												TypeCheckError::CannotDeleteProperty(err),
											);
											return TypeId::ERROR_TYPE;
										}
									}
								}
								parser::PropertyReference::Marker(_) => {
									crate::utilities::notify!("Deleting property marker found");
									return TypeId::ERROR_TYPE;
								}
							}
						}
						Expression::Index { indexee, indexer, is_optional: _, position: _ } => {
							let being_indexed = synthesise_expression(
								indexee,
								environment,
								checking_data,
								TypeId::ANY_TYPE,
							);
							let indexer = synthesise_multiple_expression(
								indexer,
								environment,
								checking_data,
								TypeId::ANY_TYPE,
							);

							let position = position.with_source(environment.get_source());
							let property = PropertyKey::from_type(indexer, &checking_data.types);
							match crate::features::delete_operator(
								(Publicity::Public, property),
								being_indexed,
								position,
								environment,
								&mut checking_data.types,
							) {
								Ok(result) => Instance::RValue(result),
								Err(err) => {
									checking_data
										.diagnostics_container
										.add_error(TypeCheckError::CannotDeleteProperty(err));
									return TypeId::ERROR_TYPE;
								}
							}
						}
						_ => {
							crate::utilities::notify!("Deleting non property raise warning");
							let _operand_type = synthesise_expression(
								operand,
								environment,
								checking_data,
								TypeId::ANY_TYPE,
							);
							return TypeId::FALSE;
						}
					}
				}
				UnaryOperator::Yield | UnaryOperator::DelegatedYield => {
					checking_data.raise_unimplemented_error(
						"yield expression",
						position.with_source(environment.get_source()),
					);
					return TypeId::ERROR_TYPE;
				}
			}
		}
		Expression::Assignment { lhs, rhs, position } => {
			let lhs =
				SynthesiseToAssignable::synthesise_to_assignable(lhs, environment, checking_data);

			return environment.assign_handle_errors(
				lhs,
				AssignmentKind::Assign,
				Some(&**rhs),
				*position,
				checking_data,
			);
		}
		Expression::BinaryAssignmentOperation { lhs, operator, rhs, position } => {
			let lhs =
				SynthesiseToAssignable::synthesise_to_assignable(lhs, environment, checking_data);

			return environment.assign_handle_errors(
				lhs,
				operator_to_assignment_kind(*operator),
				Some(&**rhs),
				*position,
				checking_data,
			);
		}
		Expression::UnaryPrefixAssignmentOperation { operator, operand, position } => {
			let lhs = SynthesiseToAssignable::synthesise_to_assignable(
				operand,
				environment,
				checking_data,
			);

			match operator {
				UnaryPrefixAssignmentOperator::Invert => {
					checking_data.raise_unimplemented_error(
						"Invert operator",
						position.with_source(environment.get_source()),
					);
					return TypeId::ERROR_TYPE;
				}
				UnaryPrefixAssignmentOperator::IncrementOrDecrement(direction) => {
					return environment.assign_handle_errors(
						lhs,
						AssignmentKind::IncrementOrDecrement(
							match direction {
								ParserIncrementOrDecrement::Increment => {
									IncrementOrDecrement::Increment
								}
								ParserIncrementOrDecrement::Decrement => {
									IncrementOrDecrement::Decrement
								}
							},
							AssignmentReturnStatus::New,
						),
						None::<&Expression>,
						*position,
						checking_data,
					);
				}
			}
		}
		Expression::UnaryPostfixAssignmentOperation { operand, operator, position } => {
			let lhs = SynthesiseToAssignable::synthesise_to_assignable(
				operand,
				environment,
				checking_data,
			);
			match operator {
				parser::expressions::operators::UnaryPostfixAssignmentOperator(direction) => {
					let direction = match direction {
						ParserIncrementOrDecrement::Increment => IncrementOrDecrement::Increment,
						ParserIncrementOrDecrement::Decrement => IncrementOrDecrement::Decrement,
					};
					let operator = AssignmentKind::IncrementOrDecrement(
						direction,
						AssignmentReturnStatus::Previous,
					);

					return environment.assign_handle_errors(
						lhs,
						operator,
						None::<&Expression>,
						*position,
						checking_data,
					);
				}
			}
		}
		Expression::VariableReference(name, position) => {
			let get_variable_or_alternatives = environment.get_variable_handle_error(
				name.as_str(),
				position.with_source(environment.get_source()),
				checking_data,
			);

			match get_variable_or_alternatives {
				Ok(variable) => Instance::LValue(variable),
				Err(_) => Instance::RValue(TypeId::ERROR_TYPE),
			}
		}
		Expression::PropertyAccess { parent, position, property, is_optional, .. } => {
			let on = synthesise_expression(parent, environment, checking_data, TypeId::ANY_TYPE);
			let (property, publicity) = match property {
				parser::PropertyReference::Standard { property, is_private } => (
					PropertyKey::String(Cow::Borrowed(property.as_str())),
					if *is_private { Publicity::Private } else { Publicity::Public },
				),
				parser::PropertyReference::Marker(_) => {
					crate::utilities::notify!("Property marker found. TODO union of properties");
					return TypeId::ERROR_TYPE;
				}
			};

			let site = position.with_source(environment.get_source());
			if *is_optional {
				let null_or_undefined = is_null_or_undefined(on, &mut checking_data.types);
				Instance::RValue(new_conditional_context(
					environment,
					(null_or_undefined, parent.get_position()),
					|_env: &mut Environment, _data: &mut CheckingData<T, EznoParser>| {
						TypeId::UNDEFINED_TYPE
					},
					Some(|env: &mut Environment, data: &mut CheckingData<T, EznoParser>| {
						let result = env.get_property_handle_errors(
							on,
							publicity,
							&property,
							data,
							site,
							AccessMode::Regular,
						);
						match result {
							Ok(i) => i.get_value(),
							Err(()) => TypeId::ERROR_TYPE,
						}
					}),
					checking_data,
				))
			} else {
				let result = environment.get_property_handle_errors(
					on,
					publicity,
					&property,
					checking_data,
					site,
					AccessMode::Regular,
				);
				match result {
					Ok(i) => Instance::RValue(i.get_value()),
					Err(()) => {
						return TypeId::ERROR_TYPE;
					}
				}
			}
		}
		Expression::Index { indexee, indexer, position, is_optional, .. } => {
			let being_indexed =
				synthesise_expression(indexee, environment, checking_data, TypeId::ANY_TYPE);
			let site = position.with_source(environment.get_source());

			if *is_optional {
				let null_or_undefined =
					is_null_or_undefined(being_indexed, &mut checking_data.types);
				Instance::RValue(new_conditional_context(
					environment,
					(null_or_undefined, indexee.get_position()),
					|_env: &mut Environment, _data: &mut CheckingData<T, EznoParser>| {
						TypeId::UNDEFINED_TYPE
					},
					Some(|env: &mut Environment, data: &mut CheckingData<T, EznoParser>| {
						// Indexer is actually side effected here
						let indexer =
							synthesise_multiple_expression(indexer, env, data, TypeId::ANY_TYPE);
						let result = env.get_property_handle_errors(
							being_indexed,
							Publicity::Public,
							&PropertyKey::from_type(indexer, &data.types),
							data,
							site,
							AccessMode::Regular,
						);
						match result {
							Ok(i) => i.get_value(),
							Err(()) => TypeId::ERROR_TYPE,
						}
					}),
					checking_data,
				))
			} else {
				let indexer = synthesise_multiple_expression(
					indexer,
					environment,
					checking_data,
					TypeId::ANY_TYPE,
				);
				let result = environment.get_property_handle_errors(
					being_indexed,
					Publicity::Public,
					&PropertyKey::from_type(indexer, &checking_data.types),
					checking_data,
					site,
					AccessMode::Regular,
				);
				match result {
					Ok(i) => Instance::RValue(i.get_value()),
					Err(()) => {
						return TypeId::ERROR_TYPE;
					}
				}
			}
		}
		Expression::ThisReference(pos) => Instance::RValue(
			environment
				.get_value_of_this(&checking_data.types, pos.with_source(environment.get_source())),
		),
		Expression::SuperExpression(reference, position) => {
			let super_type = environment.get_type_of_super();
			if let Some(super_type) = super_type {
				match reference {
					SuperReference::Call { arguments } => {
						let this_type = environment.get_value_of_this(
							&checking_data.types,
							position.with_source(environment.get_source()),
						);

						crate::utilities::notify!(
							"{:?}",
							checking_data.types.get_type_by_id(super_type)
						);

						// TODO this gives normal errors. Maybe add something about super here
						let (result, special) = call_function(
							super_type,
							CalledWithNew::Super { this_type },
							&None,
							Some(arguments),
							environment,
							checking_data,
							*position,
							expecting,
						);
						if let Some(special) = special {
							checking_data
								.local_type_mappings
								.special_expressions
								.push(*position, special);
						}

						crate::utilities::notify!("TODO unlock reference to `this`");

						Instance::RValue(result)
					}
					SuperReference::PropertyAccess { property: _ } => {
						checking_data.raise_unimplemented_error(
							"Property access on super",
							position.with_source(environment.get_source()),
						);
						return TypeId::ERROR_TYPE;
					}
					SuperReference::Index { indexer: _ } => {
						checking_data.raise_unimplemented_error(
							"Index on super",
							position.with_source(environment.get_source()),
						);
						return TypeId::ERROR_TYPE;
					}
				}
			} else {
				crate::utilities::notify!("TODO error");
				Instance::RValue(TypeId::ERROR_TYPE)
			}
		}
		Expression::NewTarget(..) => {
			return TypeId::NEW_TARGET_ARG;
		}
		Expression::FunctionCall { function, type_arguments, arguments, position, .. } => {
			let on = synthesise_expression(function, environment, checking_data, TypeId::ANY_TYPE);

			let (result, special) = call_function(
				on,
				CalledWithNew::None,
				type_arguments,
				Some(arguments),
				environment,
				checking_data,
				*position,
				expecting,
			);
			if let Some(special) = special {
				checking_data.local_type_mappings.special_expressions.push(*position, special);
			}
			Instance::RValue(result)
		}
		Expression::ConstructorCall { constructor, type_arguments, arguments, position } => {
			let on =
				synthesise_expression(constructor, environment, checking_data, TypeId::ANY_TYPE);
			let called_with_new = CalledWithNew::New { on };
			let (result, _) = call_function(
				on,
				called_with_new,
				type_arguments,
				arguments.as_ref(),
				environment,
				checking_data,
				*position,
				expecting,
			);
			Instance::RValue(result)
		}
		Expression::ConditionalTernary { condition, truthy_result, falsy_result, .. } => {
			let condition_pos = condition.get_position();
			let condition =
				synthesise_expression(condition, environment, checking_data, TypeId::ANY_TYPE);

			Instance::RValue(new_conditional_context(
				environment,
				(condition, condition_pos),
				|env: &mut Environment, data: &mut CheckingData<T, EznoParser>| {
					synthesise_expression(truthy_result, env, data, expecting)
				},
				Some(|env: &mut Environment, data: &mut CheckingData<T, EznoParser>| {
					synthesise_expression(falsy_result, env, data, expecting)
				}),
				checking_data,
			))
		}
		Expression::ArrowFunction(function) => Instance::RValue(register_arrow_function(
			expecting,
			function.header,
			function,
			environment,
			checking_data,
		)),
		Expression::ExpressionFunction(function) => {
			let is_async = function.header.is_async();
			let is_generator = function.header.is_generator();
			let location = function.header.get_location().map(|location| match location {
				parser::functions::FunctionLocationModifier::Server => "server".to_owned(),
				parser::functions::FunctionLocationModifier::Worker => "worker".to_owned(),
			});
			Instance::RValue(register_expression_function(
				expecting,
				is_async,
				is_generator,
				location,
				function.name.as_option_str().map(ToOwned::to_owned),
				function,
				environment,
				checking_data,
			))
		}

		Expression::Null(_) => return TypeId::NULL_TYPE,
		Expression::JSXRoot(jsx_root) => {
			Instance::RValue(synthesise_jsx_root(jsx_root, environment, checking_data))
		}
		Expression::RegexLiteral { pattern, flags: _, position: _ } => {
			let content = checking_data.types.new_constant_type(Constant::String(pattern.clone()));
			Instance::RValue(checking_data.types.register_type(crate::Type::SpecialObject(
				crate::types::SpecialObject::RegularExpression { content },
			)))
		}
		Expression::Comment { on, .. } => {
			return synthesise_expression(on, environment, checking_data, expecting);
		}
		Expression::ParenthesizedExpression(inner_expression, _) => Instance::RValue(
			synthesise_multiple_expression(inner_expression, environment, checking_data, expecting),
		),
		Expression::ClassExpression(class) => Instance::RValue(synthesise_class_declaration(
			class,
			None,
			expecting,
			environment,
			checking_data,
		)),
		Expression::Marker { marker_id: _, position: _ } => {
			crate::utilities::notify!("Marker expression found");
			return TypeId::ERROR_TYPE;
		}
		Expression::SpecialOperators(operator, position) => match operator {
			SpecialOperators::AsCast { value, rhs } => {
				let to_cast = synthesise_expression(value, environment, checking_data, expecting);

				if checking_data.options.allow_type_casts {
					match rhs {
						TypeOrConst::Type(type_annotation) => {
							let cast_to = synthesise_type_annotation(
								type_annotation,
								environment,
								checking_data,
							);

							// TODO
							let as_cast =
								features::tsc::as_cast(to_cast, cast_to, &mut checking_data.types);

							match as_cast {
								Ok(result) => return result,
								Err(_err) => {
									checking_data.diagnostics_container.add_error(
										TypeCheckError::InvalidCast {
											position: position
												.with_source(environment.get_source()),
											from: TypeStringRepresentation::from_type_id(
												to_cast,
												environment,
												&checking_data.types,
												checking_data.options.debug_types,
											),
											to: TypeStringRepresentation::from_type_id(
												cast_to,
												environment,
												&checking_data.types,
												checking_data.options.debug_types,
											),
										},
									);
									return TypeId::ERROR_TYPE;
								}
							}
						}
						TypeOrConst::Const(_) => {
							// TODO
							Instance::RValue(to_cast)
						}
					}
				} else {
					// TODO emit warning
					Instance::RValue(to_cast)
				}
			}
			SpecialOperators::Satisfies { value, type_annotation, .. } => {
				let satisfying =
					synthesise_type_annotation(type_annotation, environment, checking_data);

				let value = synthesise_expression(value, environment, checking_data, satisfying);

				features::tsc::check_satisfies(
					value,
					satisfying,
					ASTNode::get_position(expression).with_source(environment.get_source()),
					environment,
					checking_data,
				);

				return value;
			}
			SpecialOperators::In { lhs, rhs } => {
				let (publicity, key) = match lhs {
					parser::expressions::InExpressionLHS::PrivateProperty(key) => {
						(Publicity::Private, PropertyKey::String(Cow::Borrowed(key)))
					}
					parser::expressions::InExpressionLHS::Expression(lhs) => {
						let key = synthesise_expression(
							lhs,
							environment,
							checking_data,
							TypeId::ANY_TYPE,
						);
						(Publicity::Public, PropertyKey::from_type(key, &checking_data.types))
					}
				};

				let rhs = synthesise_expression(rhs, environment, checking_data, TypeId::ANY_TYPE);

				Instance::RValue(in_operator(
					(publicity, &key),
					rhs,
					environment,
					&mut checking_data.types,
				))
			}
			SpecialOperators::InstanceOf { lhs, rhs } => {
				let lhs = synthesise_expression(lhs, environment, checking_data, expecting);
				let rhs = synthesise_expression(rhs, environment, checking_data, expecting);
				Instance::RValue(features::instance_of_operator(
					lhs,
					rhs,
					environment,
					&mut checking_data.types,
				))
			}
			SpecialOperators::NonNullAssertion(on) => {
				let lhs = synthesise_expression(on, environment, checking_data, expecting);
				Instance::RValue(
					features::tsc::non_null_assertion(lhs, &mut checking_data.types).unwrap(),
				)
			}
			SpecialOperators::Is { value: _, type_annotation: _ } => {
				// Special non-standard
				checking_data.raise_unimplemented_error(
					"is expression",
					position.with_source(environment.get_source()),
				);
				return TypeId::ERROR_TYPE;
			}
		},
		Expression::ImportMeta(_) => {
			Instance::RValue(checking_data.types.new_open_type(TypeId::IMPORT_META))
		}
		Expression::DynamicImport { position, .. } => {
			checking_data.raise_unimplemented_error(
				"dynamic import",
				position.with_source(environment.get_source()),
			);
			return TypeId::ERROR_TYPE;
		}
		Expression::IsExpression(is_expr) => {
			Instance::RValue(synthesise_is_expression(is_expr, environment, checking_data))
		}
	};

	let position = ASTNode::get_position(expression).with_source(environment.get_source());

	if checking_data.options.store_type_mappings {
		checking_data.add_expression_mapping(position, instance.clone());
	}

	instance.get_value()
}

fn operator_to_assignment_kind(
	operator: parser::expressions::operators::BinaryAssignmentOperator,
) -> AssignmentKind {
	use parser::expressions::operators::BinaryAssignmentOperator;

	match operator {
		BinaryAssignmentOperator::LogicalAndAssign => {
			AssignmentKind::ConditionalUpdate(crate::features::operations::LogicalOperator::And)
		}
		BinaryAssignmentOperator::LogicalOrAssign => {
			AssignmentKind::ConditionalUpdate(crate::features::operations::LogicalOperator::Or)
		}
		BinaryAssignmentOperator::LogicalNullishAssignment => AssignmentKind::ConditionalUpdate(
			crate::features::operations::LogicalOperator::NullCoalescing,
		),
		BinaryAssignmentOperator::AddAssign
		| BinaryAssignmentOperator::BitwiseShiftRightUnsigned => {
			AssignmentKind::PureUpdate(MathematicalAndBitwise::Add)
		}
		BinaryAssignmentOperator::SubtractAssign => {
			AssignmentKind::PureUpdate(MathematicalAndBitwise::Subtract)
		}
		BinaryAssignmentOperator::MultiplyAssign => {
			AssignmentKind::PureUpdate(MathematicalAndBitwise::Multiply)
		}
		BinaryAssignmentOperator::DivideAssign => {
			AssignmentKind::PureUpdate(MathematicalAndBitwise::Divide)
		}
		BinaryAssignmentOperator::ModuloAssign => {
			AssignmentKind::PureUpdate(MathematicalAndBitwise::Modulo)
		}
		BinaryAssignmentOperator::ExponentAssign => {
			AssignmentKind::PureUpdate(MathematicalAndBitwise::Exponent)
		}
		BinaryAssignmentOperator::BitwiseShiftLeftAssign => {
			AssignmentKind::PureUpdate(MathematicalAndBitwise::BitwiseShiftLeft)
		}
		BinaryAssignmentOperator::BitwiseShiftRightAssign => {
			AssignmentKind::PureUpdate(MathematicalAndBitwise::BitwiseShiftRight)
		}
		BinaryAssignmentOperator::BitwiseAndAssign => {
			AssignmentKind::PureUpdate(MathematicalAndBitwise::BitwiseAnd)
		}
		BinaryAssignmentOperator::BitwiseXOrAssign => {
			AssignmentKind::PureUpdate(MathematicalAndBitwise::BitwiseXOr)
		}
		BinaryAssignmentOperator::BitwiseOrAssign => {
			AssignmentKind::PureUpdate(MathematicalAndBitwise::BitwiseOr)
		}
	}
}

/// Generic for functions + constructor calls
///
/// TODO error with `function_type_id` should be handled earlier
#[allow(clippy::too_many_arguments)]
fn call_function<T: crate::ReadFromFS>(
	function_type_id: TypeId,
	called_with_new: CalledWithNew,
	type_arguments: &Option<Vec<parser::TypeAnnotation>>,
	arguments: Option<&Vec<FunctionArgument>>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	call_site: parser::Span,
	expecting: TypeId,
) -> (TypeId, Option<SpecialExpressions>) {
	let call_site_type_arguments = type_arguments.as_ref().map(|type_arguments| {
		type_arguments
			.iter()
			.map(|generic_type_argument| {
				(
					synthesise_type_annotation(generic_type_argument, environment, checking_data),
					generic_type_argument.get_position().with_source(environment.get_source()),
				)
			})
			.collect::<Vec<_>>()
	});

	let comment = parser::Expression::VariableReference(
		String::from_str("undefined").unwrap(),
		source_map::BaseSpan::NULL,
	);

	let arguments = arguments
		.map(|arguments| {
			arguments
				.iter()
				.map(|a| match a {
					FunctionArgument::Spread(e, _) => {
						UnsynthesisedArgument { spread: true, expression: e }
					}
					FunctionArgument::Standard(e) => {
						UnsynthesisedArgument { spread: false, expression: e }
					}
					FunctionArgument::Comment { .. } => {
						UnsynthesisedArgument { spread: false, expression: &comment }
					}
				})
				.collect::<Vec<_>>()
		})
		.unwrap_or_default();

	crate::types::calling::call_type_handle_errors(
		function_type_id,
		call_site_type_arguments,
		&arguments,
		CallingInput {
			called_with_new,
			call_site: call_site.with_source(environment.get_source()),
			max_inline: checking_data.options.max_inline_count,
		},
		environment,
		checking_data,
		expecting,
	)
}

pub(super) fn synthesise_object_literal<T: crate::ReadFromFS>(
	ObjectLiteral { members, .. }: &ObjectLiteral,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	environment: &mut Environment,
	position: SpanWithSource,
	expecting: TypeId,
) -> TypeId {
	let mut object_builder =
		ObjectBuilder::new(None, &mut checking_data.types, position, &mut environment.info);

	// {
	// 	let ty = print_type(expecting, &checking_data.types, environment, true);
	// 	crate::utilities::notify!("expecting in obj={}", ty);
	// }

	for member in members {
		let member_position = member.get_position().with_source(environment.get_source());
		match member {
			ObjectLiteralMember::Comment(..) => {
				continue;
			}
			ObjectLiteralMember::Spread(spread, pos) => {
				let spread = synthesise_expression(spread, environment, checking_data, expecting);

				// TODO use what about string, what about enumerable ...

				match checking_data.types.get_type_by_id(spread) {
					crate::Type::Object(_) => {
						let get_properties_on_type = get_properties_on_single_type(
							spread,
							&checking_data.types,
							environment,
							true,
							TypeId::ANY_TYPE,
						);

						for (_, key, value) in get_properties_on_type {
							// TODO evaluate getters & check whether enumerable
							object_builder.append(
								Publicity::Public,
								key,
								value,
								pos.with_source(environment.get_source()),
								&mut environment.info,
							);
						}
					}
					crate::Type::Constructor(Constructor::ConditionalResult {
						condition,
						truthy_result,
						otherwise_result,
						result_union: _,
					}) => {
						let truthy_properties = get_properties_on_single_type(
							*truthy_result,
							&checking_data.types,
							environment,
							true,
							TypeId::ANY_TYPE,
						);
						let otherwise_properties = get_properties_on_single_type(
							*otherwise_result,
							&checking_data.types,
							environment,
							true,
							TypeId::ANY_TYPE,
						);
						crate::utilities::notify!(
							"Here {:?} {:?} {:?} {:?}",
							truthy_properties,
							otherwise_properties,
							print_type(*truthy_result, &checking_data.types, environment, true),
							print_type(*otherwise_result, &checking_data.types, environment, true)
						);
						// Concatenate some types here if they all have the same keys
						// && matches!(
						// 	(lv, rv),
						// 	(PropertyValue::Value(..), PropertyValue::Value(..))
						// ))
						let same_keys = truthy_properties.len() == otherwise_properties.len()
							&& truthy_properties
								.iter()
								.zip(otherwise_properties.iter())
								.all(|((_lp, lk, _lv), (_rp, rk, _rv))| lk == rk);

						// TODO really want to get like a leading prefix

						// Common case
						if same_keys {
							let condition = *condition;
							for ((_, key, lv), (_, _, rv)) in
								truthy_properties.iter().zip(otherwise_properties.iter())
							{
								// TODO really isn't a position but okay
								let position = pos.with_source(environment.get_source());
								let value =
									if let (PropertyValue::Value(l), PropertyValue::Value(r)) =
										(lv, rv)
									{
										checking_data.types.new_conditional_type(condition, *l, *r)
									} else {
										unreachable!("checked above")
									};

								object_builder.append(
									Publicity::Public,
									key.clone(),
									PropertyValue::Value(value),
									position,
									&mut environment.info,
								);
							}
						} else {
							// crate::utilities::notify!("Here in conditional spread");

							for (_, key, value) in truthy_properties {
								object_builder.append(
									Publicity::Public,
									key,
									PropertyValue::ConditionallyExists {
										condition: *condition,
										truthy: Box::new(value),
									},
									member_position,
									&mut environment.info,
								);
							}
							let negation =
								checking_data.types.new_logical_negation_type(*condition);

							for (_, key, value) in otherwise_properties {
								object_builder.append(
									Publicity::Public,
									key,
									PropertyValue::ConditionallyExists {
										condition: negation,
										truthy: Box::new(value),
									},
									member_position,
									&mut environment.info,
								);
							}
						}
					}
					crate::Type::AliasTo { .. }
					| crate::Type::And { .. }
					| crate::Type::Or { .. }
					| crate::Type::RootPolyType { .. }
					| crate::Type::Constructor { .. }
					| crate::Type::PartiallyAppliedGenerics { .. }
					| crate::Type::Interface { .. }
					| crate::Type::Class { .. }
					| crate::Type::Constant { .. }
					| crate::Type::FunctionReference { .. }
					| crate::Type::SpecialObject(_) => {
						checking_data.raise_unimplemented_error(
							"more than binary spread",
							pos.with_source(environment.get_source()),
						);

						return TypeId::ERROR_TYPE;
					}
				}
			}
			ObjectLiteralMember::Shorthand(name, position) => {
				let key = PropertyKey::String(Cow::Owned(name.clone()));
				let get_variable = environment.get_variable_handle_error(
					name,
					position.with_source(environment.get_source()),
					checking_data,
				);
				let value = match get_variable {
					Ok(VariableWithValue(_variable, value)) => value,
					Err(_err) => {
						// checking_data.diagnostics_container.add_error(
						// 	TypeCheckError::CouldNotFindVariable {
						// 		variable: err.name,
						// 		possibles: err.possibles,
						// 		position: position.clone(),
						// 	},
						// );
						TypeId::ERROR_TYPE
					}
				};

				object_builder.append(
					Publicity::Public,
					key,
					crate::types::properties::PropertyValue::Value(value),
					member_position,
					&mut environment.info,
				);
			}
			ObjectLiteralMember::Property { key, value, position, .. } => {
				let key = parser_property_key_to_checker_property_key(
					key.get_ast_ref(),
					environment,
					checking_data,
					true,
				);

				let position_with_source = position.with_source(environment.get_source());

				let maybe_property_expecting = get_property_unbound(
					(expecting, None),
					(Publicity::Public, &key, None),
					false,
					environment,
					&checking_data.types,
				);

				if expecting != TypeId::ANY_TYPE
					&& expecting != TypeId::OBJECT_TYPE
					&& maybe_property_expecting.is_err()
				{
					checking_data.diagnostics_container.add_warning(
						TypeCheckWarning::ExcessProperty {
							position: position_with_source,
							expected_type: TypeStringRepresentation::from_type_id(
								expecting,
								environment,
								&checking_data.types,
								checking_data.options.debug_types,
							),
							excess_property_name: print_property_key(
								&key,
								&checking_data.types,
								environment,
								false,
							),
						},
					);
				}

				// TODO needs improvement
				let property_expecting = maybe_property_expecting
					.ok()
					.and_then(|l| {
						if let LogicalOrValid::Logical(Logical::Pure(l)) = l {
							Some(l.as_get_type(&checking_data.types))
						} else {
							crate::utilities::notify!("TODO expecting {:?}", l);
							None
						}
					})
					.unwrap_or(TypeId::ANY_TYPE);

				let value =
					synthesise_expression(value, environment, checking_data, property_expecting);

				let value = crate::types::properties::PropertyValue::Value(value);
				object_builder.append(
					Publicity::Public,
					key,
					value,
					member_position,
					&mut environment.info,
				);

				// let property_name: PropertyName<'static> = property_key.into();

				// TODO a little temp
				// checking_data
				//     .type_mappings
				//     .properties_to_types
				//     .insert(property_key.get_ast().get_property_id(), value.clone());

				// (
				//     property_name,
				//     Property {
				//         ty: value.into(),
				//         writeable: true,
				//         configurable: true,
				//         enumerable: true,
				//     },
				// )
			}
			// TODO abstract
			ObjectLiteralMember::Method(method) => {
				let key = parser_property_key_to_checker_property_key(
					method.name.get_ast_ref(),
					environment,
					checking_data,
					true,
				);

				// TODO needs improvement
				let property_expecting = get_property_unbound(
					(expecting, None),
					(Publicity::Public, &key, None),
					false,
					environment,
					&checking_data.types,
				)
				.ok()
				.and_then(|l| {
					if let LogicalOrValid::Logical(Logical::Pure(l)) = l {
						Some(l.as_get_type(&checking_data.types))
					} else {
						crate::utilities::notify!("TODO {:?}", l);
						None
					}
				})
				.unwrap_or(TypeId::ANY_TYPE);

				let behavior = crate::features::functions::FunctionRegisterBehavior::ObjectMethod {
					is_async: method.header.is_async(),
					is_generator: method.header.is_generator(),
					expecting: property_expecting,
					name: key.into_name_type(&mut checking_data.types),
				};

				let function = synthesise_function(method, behavior, environment, checking_data);

				let kind = match &method.header {
					MethodHeader::Get => Some(GetterSetter::Getter),
					MethodHeader::Set => Some(GetterSetter::Setter),
					MethodHeader::Regular { .. } => None,
				};

				let property =
					function_to_property(kind, function, &mut checking_data.types, false);

				object_builder.append(
					Publicity::Public,
					key,
					property,
					member_position,
					&mut environment.info,
				);
			}
		}
	}

	object_builder.build_object()
}
