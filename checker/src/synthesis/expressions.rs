use std::{borrow::Cow, convert::TryInto};

use parser::{
	ast::TypeOrConst,
	expressions::{
		object_literal::{ObjectLiteral, ObjectLiteralMember},
		operators::{
			BinaryOperator, IncrementOrDecrement, UnaryOperator, UnaryPrefixAssignmentOperator,
		},
		ArrayElement, FunctionArgument, MultipleExpression, SpecialOperators, SuperReference,
		TemplateLiteral,
	},
	functions::MethodHeader,
	ASTNode, Expression,
};

use crate::{
	context::{
		information::{get_properties_on_type, get_property_unbound},
		Logical,
	},
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	features::{
		self, await_expression,
		functions::{
			function_to_property, register_arrow_function, register_expression_function,
			synthesise_function, GetterSetter,
		},
		variables::VariableWithValue,
	},
	synthesis::parser_property_key_to_checker_property_key,
	types::{
		calling::{CallingInput, UnsynthesisedArgument},
		properties::PropertyKey,
	},
	Decidable,
};

use crate::{
	context::information::Publicity,
	features::{
		assignments::Assignable,
		objects::ObjectBuilder,
		operations::{
			evaluate_logical_operation_with_expression,
			evaluate_pure_binary_operation_handle_errors, evaluate_pure_unary_operator,
			EqualityAndInequality, MathematicalAndBitwise, PureUnary,
		},
		template_literal::synthesise_template_literal,
	},
	types::calling::CalledWithNew,
	types::{Constant, TypeId},
	CheckingData, Environment, Instance, SpecialExpressions,
};

use super::{
	assignments::{synthesise_access_to_reference, synthesise_lhs_of_assignment_to_reference},
	classes::synthesise_class_declaration,
	extensions::{is_expression::synthesise_is_expression, jsx::synthesise_jsx_root},
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
		Expression::RegexLiteral { pattern, flags: _, position: _ } => {
			return checking_data.types.new_regex(pattern.clone());
		}
		Expression::NumberLiteral(value, ..) => {
			let not_nan = if let Ok(v) = f64::try_from(value.clone()) {
				v.try_into().unwrap()
			} else {
				crate::utils::notify!("TODO big int");
				return TypeId::ERROR_TYPE;
			};
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
				element.0.as_ref().map(|element| match element {
					FunctionArgument::Standard(element) => {
						// TODO based off above
						let expecting = TypeId::ANY_TYPE;
						let expression_type =
							synthesise_expression(element, environment, checking_data, expecting);
						(
							PropertyKey::from_usize(match idx {
								Decidable::Known(idx) => *idx,
								Decidable::Unknown(_) => todo!(),
							}),
							expression_type,
						)
					}
					FunctionArgument::Spread(_expr, position) => {
						{
							checking_data.raise_unimplemented_error(
								"Spread elements",
								position.with_source(environment.get_source()),
							);
						}
						crate::utils::notify!("Skipping spread");
						(
							PropertyKey::from_usize(match idx {
								Decidable::Known(idx) => *idx,
								Decidable::Unknown(_) => todo!(),
							}),
							TypeId::ERROR_TYPE,
						)
					}
					FunctionArgument::Comment { .. } => todo!(),
				})
			}

			let mut basis = ObjectBuilder::new(
				Some(TypeId::ARRAY_TYPE),
				&mut checking_data.types,
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
						environment,
						Publicity::Public,
						key,
						crate::types::properties::PropertyValue::Value(value),
						Some(spread_expression_position),
					);
				}
			}

			// TODO spread
			let length = checking_data
				.types
				.new_constant_type(Constant::Number((elements.len() as f64).try_into().unwrap()));

			// TODO: Should there be a position here?
			basis.append(
				environment,
				Publicity::Public,
				PropertyKey::String("length".into()),
				crate::types::properties::PropertyValue::Value(length),
				None,
			);

			Instance::RValue(basis.build_object())
		}
		Expression::ObjectLiteral(object_literal) => Instance::RValue(synthesise_object_literal(
			object_literal,
			checking_data,
			environment,
			expecting,
		)),
		Expression::TemplateLiteral(TemplateLiteral { tag, parts, position }) => {
			let parts_iter = parts.iter().map(|part| match part {
				parser::expressions::TemplateLiteralPart::Static(value) => {
					crate::features::template_literal::TemplateLiteralPart::Static(value.as_str())
				}
				parser::expressions::TemplateLiteralPart::Dynamic(expr) => {
					crate::features::template_literal::TemplateLiteralPart::Dynamic(&**expr)
				}
			});
			let tag = tag.as_ref().map(|expr| {
				synthesise_expression(expr, environment, checking_data, TypeId::ANY_TYPE)
			});

			Instance::RValue(synthesise_template_literal(
				tag,
				parts_iter,
				position,
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
					// TODO unwrap
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
				BinaryOperator::Divides | BinaryOperator::Pipe | BinaryOperator::Compose => {
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
					todo!("cast to number")
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
					Instance::RValue(
						evaluate_pure_unary_operator(
							operator,
							operand_type,
							&mut checking_data.types,
							checking_data.options.strict_casts,
						)
						.unwrap(),
					)
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
								parser::PropertyReference::Standard {
									property,
									is_private: _is_private,
								} => {
									let result = environment.delete_property(
										on,
										&PropertyKey::String(Cow::Owned(property.clone())),
									);
									return if result { TypeId::TRUE } else { TypeId::FALSE };
								}
								parser::PropertyReference::Marker(_) => {
									crate::utils::notify!("Deleting property marker found");
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

							let property = PropertyKey::from_type(indexer, &checking_data.types);
							let result = environment.delete_property(being_indexed, &property);
							return if result { TypeId::TRUE } else { TypeId::FALSE };
						}
						_ => {
							crate::utils::notify!("Deleting non property raise warning");
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
			let lhs: Assignable =
				synthesise_lhs_of_assignment_to_reference(lhs, environment, checking_data);

			return environment.assign_to_assignable_handle_errors(
				lhs,
				crate::features::assignments::AssignmentKind::Assign,
				Some(&**rhs),
				*position,
				checking_data,
			);
		}
		Expression::BinaryAssignmentOperation { lhs, operator, rhs, position } => {
			let lhs: Assignable = Assignable::Reference(synthesise_access_to_reference(
				lhs,
				environment,
				checking_data,
			));

			return environment.assign_to_assignable_handle_errors(
				lhs,
				operator_to_assignment_kind(*operator),
				Some(&**rhs),
				*position,
				checking_data,
			);
		}
		Expression::UnaryPrefixAssignmentOperation { operator, operand, position } => {
			let lhs: Assignable = Assignable::Reference(synthesise_access_to_reference(
				operand,
				environment,
				checking_data,
			));

			match operator {
				UnaryPrefixAssignmentOperator::Invert => todo!(),
				UnaryPrefixAssignmentOperator::IncrementOrDecrement(direction) => {
					return environment.assign_to_assignable_handle_errors(
						lhs,
						crate::features::assignments::AssignmentKind::IncrementOrDecrement(
							match direction {
								IncrementOrDecrement::Increment => {
									crate::features::assignments::IncrementOrDecrement::Increment
								}
								IncrementOrDecrement::Decrement => {
									crate::features::assignments::IncrementOrDecrement::Decrement
								}
							},
							crate::features::assignments::AssignmentReturnStatus::New,
						),
						None::<&Expression>,
						*position,
						checking_data,
					);
				}
			}
		}
		Expression::UnaryPostfixAssignmentOperation { operand, operator, position } => {
			let lhs: Assignable = Assignable::Reference(synthesise_access_to_reference(
				operand,
				environment,
				checking_data,
			));
			match operator {
				parser::expressions::operators::UnaryPostfixAssignmentOperator(direction) => {
					let direction = match direction {
						IncrementOrDecrement::Increment => {
							crate::features::assignments::IncrementOrDecrement::Increment
						}
						IncrementOrDecrement::Decrement => {
							crate::features::assignments::IncrementOrDecrement::Decrement
						}
					};
					let operator =
						crate::features::assignments::AssignmentKind::IncrementOrDecrement(
							direction,
							crate::features::assignments::AssignmentReturnStatus::Previous,
						);

					return environment.assign_to_assignable_handle_errors(
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
				Err(_err) => Instance::RValue(TypeId::ERROR_TYPE),
			}
		}
		Expression::PropertyAccess { parent, position, property, .. } => {
			let on = synthesise_expression(parent, environment, checking_data, TypeId::ANY_TYPE);
			let property = match property {
				parser::PropertyReference::Standard { property, is_private: _ } => {
					PropertyKey::String(Cow::Borrowed(property.as_str()))
				}
				parser::PropertyReference::Marker(_) => {
					crate::utils::notify!("Property marker found. TODO union of properties");
					return TypeId::ERROR_TYPE;
				}
			};

			// TODO
			let publicity = Publicity::Public;

			let result = environment.get_property_handle_errors(
				on,
				publicity,
				&property,
				checking_data,
				position.with_source(environment.get_source()),
				true,
			);

			match result {
				Ok(instance) => instance,
				Err(()) => return TypeId::ERROR_TYPE,
			}
		}
		Expression::Index { indexee, indexer, position, .. } => {
			let being_indexed =
				synthesise_expression(indexee, environment, checking_data, TypeId::ANY_TYPE);
			let indexer = synthesise_multiple_expression(
				indexer,
				environment,
				checking_data,
				TypeId::ANY_TYPE,
			);

			// TODO handle differently?
			let result = environment.get_property_handle_errors(
				being_indexed,
				Publicity::Public,
				&PropertyKey::from_type(indexer, &checking_data.types),
				checking_data,
				position.with_source(environment.get_source()),
				true,
			);

			match result {
				Ok(instance) => instance,
				Err(()) => return TypeId::ERROR_TYPE,
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
						let (result, special) = call_function(
							super_type,
							CalledWithNew::SpecialSuperCall { this_type },
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

						crate::utils::notify!("TODO unlock reference to `this`");

						Instance::RValue(result)
						// let crate::ConstructorInformation {
						// 	fields,
						// 	class_constructor_ty,
						// 	class_instance_ty,
						// 	..
						// } = checking_data
						// 	.functions
						// 	.constructor_information
						// 	.get(&constructor_pointer)
						// 	.unwrap()
						// 	.clone();

						// let super_ty = if let Type::AliasTo { to, .. } =
						// 	environment.get_type_by_id(class_instance_ty)
						// {
						// 	*to
						// } else {
						// 	panic!("No super")
						// };

						// let this_constructor =
						// 	environment.get_constructor_for_prototype(class_instance_ty).unwrap();
						// let super_constructor =
						// 	environment.get_constructor_for_prototype(super_ty).unwrap();

						// let this = environment.create_this(class_instance_ty);

						// let mut chain = Chain::new();
						// let mut chain = Annex::new(&chain);

						// let ty = call_function(
						// 	super_constructor,
						// 	&None,
						// 	Some(arguments),
						// 	environment,
						// 	checking_data,
						// 	&
						// 	position,
						// 	CalledWithNew::SpecialSuperCall { on: this_constructor },
						// );

						// TODO explain
						// synthesise_class_fields(fields, environment, checking_data, &chain);

						// Instance::RValue(ty)
					}
					SuperReference::PropertyAccess { property: _ } => todo!(),
					SuperReference::Index { indexer: _ } => todo!(),
				}
			} else {
				crate::utils::notify!("TODO error");
				Instance::RValue(TypeId::ERROR_TYPE)
			}
		}
		Expression::NewTarget(..) => todo!(),
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

			Instance::RValue(environment.new_conditional_context(
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
				function,
				environment,
				checking_data,
			))
		}

		Expression::Null(_) => return TypeId::NULL_TYPE,
		Expression::JSXRoot(jsx_root) => {
			Instance::RValue(synthesise_jsx_root(jsx_root, environment, checking_data))
		}
		Expression::Comment { on, .. } => {
			return synthesise_expression(on, environment, checking_data, expecting);
		}
		Expression::ParenthesizedExpression(inner_expression, _) => Instance::RValue(
			synthesise_multiple_expression(inner_expression, environment, checking_data, expecting),
		),
		Expression::ClassExpression(class) => {
			Instance::RValue(synthesise_class_declaration(class, environment, checking_data))
		}
		Expression::Marker { marker_id: _, position: _ } => {
			crate::utils::notify!("Marker expression found");
			return TypeId::ERROR_TYPE;
		}
		Expression::SpecialOperators(operator, position) => match operator {
			SpecialOperators::AsCast { value, rhs } => {
				let to_cast = synthesise_expression(value, environment, checking_data, expecting);

				if checking_data.options.allow_cast {
					match rhs {
						TypeOrConst::Type(type_annotation) => {
							let cast_to = synthesise_type_annotation(
								type_annotation,
								environment,
								checking_data,
							);

							// TODO
							let as_cast =
								features::as_cast(to_cast, cast_to, &mut checking_data.types);

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
			SpecialOperators::Is { value: _, type_annotation: _ } => {
				todo!()
			}
			SpecialOperators::Satisfies { value, type_annotation, .. } => {
				let value = synthesise_expression(value, environment, checking_data, expecting);
				let satisfying =
					synthesise_type_annotation(type_annotation, environment, checking_data);

				checking_data.check_satisfies(
					value,
					satisfying,
					ASTNode::get_position(expression).with_source(environment.get_source()),
					environment,
				);

				return value;
			}
			SpecialOperators::In { lhs, rhs } => {
				let lhs = match lhs {
					parser::expressions::InExpressionLHS::PrivateProperty(_) => {
						checking_data.raise_unimplemented_error(
							"in on private",
							position.with_source(environment.get_source()),
						);
						return TypeId::ERROR_TYPE;
					}
					parser::expressions::InExpressionLHS::Expression(lhs) => {
						synthesise_expression(lhs, environment, checking_data, TypeId::ANY_TYPE)
					}
				};
				let rhs = synthesise_expression(rhs, environment, checking_data, TypeId::ANY_TYPE);
				let result = environment
					.property_in(rhs, &PropertyKey::from_type(lhs, &checking_data.types));

				Instance::RValue(if result { TypeId::TRUE } else { TypeId::FALSE })
			}
			SpecialOperators::InstanceOf { .. } => {
				checking_data.raise_unimplemented_error(
					"instanceof expression",
					position.with_source(environment.get_source()),
				);
				return TypeId::ERROR_TYPE;
			}
			SpecialOperators::NonNullAssertion(_) => todo!(),
		},
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

	if checking_data.options.store_expression_type_mappings {
		checking_data.add_expression_mapping(position, instance.clone());
	}

	instance.get_value()
}

fn operator_to_assignment_kind(
	operator: parser::expressions::operators::BinaryAssignmentOperator,
) -> crate::features::assignments::AssignmentKind {
	use crate::features::assignments::AssignmentKind;
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
fn call_function<T: crate::ReadFromFS>(
	function_type_id: TypeId,
	called_with_new: CalledWithNew,
	type_arguments: &Option<Vec<parser::TypeAnnotation>>,
	arguments: Option<&Vec<FunctionArgument>>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	call_site: parser::Span,
	expected: TypeId,
) -> (TypeId, Option<SpecialExpressions>) {
	let generic_type_arguments = type_arguments.as_ref().map(|type_arguments| {
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
					FunctionArgument::Comment { .. } => todo!(),
				})
				.collect::<Vec<_>>()
		})
		.unwrap_or_default();

	crate::types::calling::call_type_handle_errors(
		function_type_id,
		&arguments,
		CallingInput {
			called_with_new,
			this_value: Default::default(),
			call_site: call_site.with_source(environment.get_source()),
			call_site_type_arguments: generic_type_arguments,
		},
		environment,
		checking_data,
		expected,
	)
}

pub(super) fn synthesise_object_literal<T: crate::ReadFromFS>(
	ObjectLiteral { members, .. }: &ObjectLiteral,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	environment: &mut Environment,
	expected: TypeId,
) -> TypeId {
	let mut object_builder =
		ObjectBuilder::new(None, &mut checking_data.types, &mut environment.info);

	for member in members {
		let member_position = member.get_position().with_source(environment.get_source());
		match member {
			ObjectLiteralMember::Spread(spread, pos) => {
				let spread = synthesise_expression(spread, environment, checking_data, expected);

				// TODO use what about string, what about enumerable ...
				for (_, key, value) in
					get_properties_on_type(spread, &checking_data.types, environment)
				{
					object_builder.append(
						environment,
						Publicity::Public,
						key,
						// TODO what about getters
						crate::PropertyValue::Value(value),
						Some(pos.with_source(environment.get_source())),
					);
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
					environment,
					Publicity::Public,
					key,
					crate::types::properties::PropertyValue::Value(value),
					Some(member_position),
				);
			}
			ObjectLiteralMember::Property { key, value, .. } => {
				let key = parser_property_key_to_checker_property_key(
					key.get_ast_ref(),
					environment,
					checking_data,
					true,
				);

				// TODO needs improvement
				let property_expecting = get_property_unbound(
					expected,
					Publicity::Public,
					&key,
					&checking_data.types,
					environment,
				)
				.ok()
				.and_then(|l| if let Logical::Pure(l) = l { Some(l.as_get_type()) } else { None })
				.unwrap_or(TypeId::ANY_TYPE);

				let value =
					synthesise_expression(value, environment, checking_data, property_expecting);

				let value = crate::types::properties::PropertyValue::Value(value);
				object_builder.append(
					environment,
					Publicity::Public,
					key,
					value,
					Some(member_position),
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
					expected,
					Publicity::Public,
					&key,
					&checking_data.types,
					environment,
				)
				.ok()
				.and_then(|l| if let Logical::Pure(l) = l { Some(l.as_get_type()) } else { None })
				.unwrap_or(TypeId::ANY_TYPE);

				let behavior = crate::features::functions::FunctionRegisterBehavior::ObjectMethod {
					is_async: method.header.is_async(),
					is_generator: method.header.is_generator(),
					expecting: property_expecting,
				};

				let function = synthesise_function(method, behavior, environment, checking_data);

				let kind = match &method.header {
					MethodHeader::Get => GetterSetter::Getter,
					MethodHeader::Set => GetterSetter::Setter,
					MethodHeader::Regular { .. } => GetterSetter::None,
				};

				let property =
					function_to_property(kind, function, &mut checking_data.types, false);

				object_builder.append(
					environment,
					Publicity::Public,
					key,
					property,
					Some(member_position),
				);
			}
		}
	}

	object_builder.build_object()
}
