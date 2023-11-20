use std::{borrow::Cow, convert::TryInto, marker::PhantomData};

use parser::{
	expressions::{
		assignments::LHSOfAssignment,
		object_literal::{ObjectLiteral, ObjectLiteralMember},
		MultipleExpression, SpecialOperators, SpreadExpression, SuperReference, TemplateLiteral,
	},
	operators::{BinaryAssignmentOperator, UnaryOperator, UnaryPrefixAssignmentOperator},
	ASTNode, Expression, MethodHeader,
};

use crate::{
	behavior::{
		functions::{
			register_arrow_function, register_expression_function, FunctionRegisterBehavior,
			SynthesisableFunction,
		},
		variables::VariableWithValue,
	},
	synthesis::parser_property_key_to_checker_property_key,
	types::properties::PropertyKey,
	Decidable,
};

use crate::{
	behavior::{
		assignments::Assignable,
		objects::ObjectBuilder,
		operations::{
			evaluate_logical_operation_with_expression,
			evaluate_pure_binary_operation_handle_errors, evaluate_pure_unary_operator,
			EqualityAndInequality, MathematicalAndBitwise, PureUnary,
		},
		template_literal::synthesise_template_literal,
	},
	context::facts::Publicity,
	diagnostics::{TypeCheckError, TypeCheckWarning, TypeStringRepresentation},
	events::Event,
	types::{calling::CalledWithNew, functions::SynthesisedArgument},
	types::{properties::PropertyKind, Constant, TypeId},
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
		Expression::Comment(..) => unreachable!("Should have skipped this higher up"),
		Expression::StringLiteral(value, ..) => {
			return checking_data.types.new_constant_type(Constant::String(value.clone()))
		}
		Expression::RegexLiteral { pattern, flags, position } => {
			// TODO flags
			return checking_data.types.new_constant_type(Constant::Regexp(pattern.clone()));
		}
		Expression::NumberLiteral(value, ..) => {
			let not_nan = f64::from(value.clone()).try_into().unwrap();
			return checking_data.types.new_constant_type(Constant::Number(not_nan));
		}
		Expression::BooleanLiteral(value, ..) => {
			return checking_data.types.new_constant_type(Constant::Boolean(*value))
		}
		Expression::ArrayLiteral(elements, _) => {
			fn synthesise_array_item<T: crate::ReadFromFS>(
				idx: Decidable<usize>,
				element: &SpreadExpression,
				environment: &mut Environment,
				checking_data: &mut CheckingData<T, super::EznoParser>,
			) -> (PropertyKey<'static>, TypeId) {
				match element {
					SpreadExpression::NonSpread(element) => {
						// TODO based off above
						let expecting = TypeId::ANY_TYPE;
						let expression_type =
							synthesise_expression(element, environment, checking_data, expecting);
						(
							PropertyKey::from_usize(match idx {
								Decidable::Known(idx) => idx,
								_ => todo!(),
							}),
							expression_type,
						)
					}
					SpreadExpression::Spread(expr, position) => {
						{
							checking_data.raise_unimplemented_error(
								"Spread elements",
								position.clone().with_source(environment.get_source()),
							);
						}
						crate::utils::notify!("Skipping spread");
						(
							PropertyKey::from_usize(match idx {
								Decidable::Known(idx) => idx,
								_ => todo!(),
							}),
							TypeId::ERROR_TYPE,
						)
					}
					SpreadExpression::Empty => {
						crate::utils::notify!("Empty expression temp as empty. Should be ");
						(
							PropertyKey::from_usize(match idx {
								Decidable::Known(idx) => idx,
								_ => todo!(),
							}),
							TypeId::UNDEFINED_TYPE,
						)
					}
				}
			}

			let mut basis = ObjectBuilder::new(
				Some(TypeId::ARRAY_TYPE),
				&mut checking_data.types,
				&mut environment.facts,
			);

			// TODO remove enumerate, add add function and more
			for (idx, value) in elements.iter().enumerate() {
				let spread_expression_position =
					value.get_position().clone().with_source(environment.get_source());

				let (key, value) =
					synthesise_array_item(Decidable::Known(idx), value, environment, checking_data);

				basis.append(
					environment,
					Publicity::Public,
					key,
					crate::types::properties::PropertyValue::Value(value),
					Some(spread_expression_position),
				);
			}
			let len = checking_data
				.types
				.new_constant_type(Constant::Number((elements.len() as f64).try_into().unwrap()));

			// TODO: Should there be a position here?
			basis.append(
				environment,
				Publicity::Public,
				PropertyKey::String("length".into()),
				crate::types::properties::PropertyValue::Value(len),
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
			let mut parts_iter = parts.iter().map(|part| match part {
				parser::expressions::TemplateLiteralPart::Static(value) => {
					crate::behavior::template_literal::TemplateLiteralPart::Static(value.as_str())
				}
				parser::expressions::TemplateLiteralPart::Dynamic(expr) => {
					crate::behavior::template_literal::TemplateLiteralPart::Dynamic(&**expr)
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
		Expression::BinaryOperation { lhs, operator, rhs, .. } => {
			let lhs_ty = synthesise_expression(&*lhs, environment, checking_data, TypeId::ANY_TYPE);

			if let BinaryOperator::LogicalAnd
			| BinaryOperator::LogicalOr
			| BinaryOperator::NullCoalescing = operator
			{
				return evaluate_logical_operation_with_expression(
					lhs_ty,
					match operator {
						BinaryOperator::LogicalAnd => crate::behavior::operations::Logical::And,
						BinaryOperator::LogicalOr => crate::behavior::operations::Logical::Or,
						BinaryOperator::NullCoalescing => {
							crate::behavior::operations::Logical::NullCoalescing
						}
						_ => unreachable!(),
					},
					&**rhs,
					checking_data,
					environment,
					// TODO unwrap
				)
				.unwrap();
			}

			let rhs_ty = synthesise_expression(&*rhs, environment, checking_data, TypeId::ANY_TYPE);

			if lhs_ty == TypeId::ERROR_TYPE || rhs_ty == TypeId::ERROR_TYPE {
				return TypeId::ERROR_TYPE;
			}

			let lhs_pos =
				ASTNode::get_position(&**lhs).clone().with_source(environment.get_source());
			let rhs_pos =
				ASTNode::get_position(&**rhs).clone().with_source(environment.get_source());
			use parser::operators::BinaryOperator;

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
				BinaryOperator::LessThanEqual => EqualityAndInequality::LessThanEqual.into(),
				BinaryOperator::GreaterThanEqual => EqualityAndInequality::GreaterThanEqual.into(),
				BinaryOperator::LogicalAnd
				| BinaryOperator::LogicalOr
				| BinaryOperator::NullCoalescing => {
					unreachable!()
				}
				BinaryOperator::Divides => todo!(),
				BinaryOperator::Pipe => todo!(),
				BinaryOperator::Compose => todo!(),
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
						&*operand,
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
				UnaryOperator::Await => todo!(),
				UnaryOperator::TypeOf => todo!(),
				UnaryOperator::Void => {
					let _operand_type = synthesise_expression(
						&*operand,
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
						Expression::PropertyAccess { parent, property, is_optional, position } => {
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
										PropertyKey::String(Cow::Owned(property.clone())),
									);
									return if result { TypeId::TRUE } else { TypeId::FALSE };
								}
								parser::PropertyReference::Cursor(_) => todo!(),
							}
						}
						Expression::Index { indexee, indexer, is_optional, position } => {
							let indexee = synthesise_expression(
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
							let result = environment.delete_property(indexee, property);
							return if result { TypeId::TRUE } else { TypeId::FALSE };
						}
						_ => {
							crate::utils::notify!("Deleting non property raise warning");
							let _operand_type = synthesise_expression(
								&*operand,
								environment,
								checking_data,
								TypeId::ANY_TYPE,
							);
							return TypeId::FALSE;
						}
					}
				}
				UnaryOperator::Yield => todo!(),
				UnaryOperator::DelegatedYield => todo!(),
			}
		}
		Expression::Assignment { lhs, rhs, position } => {
			let lhs: Assignable =
				synthesise_lhs_of_assignment_to_reference(lhs, environment, checking_data);

			let assignment_span = position.clone().with_source(environment.get_source());
			return environment.assign_to_assignable_handle_errors(
				lhs,
				crate::behavior::assignments::AssignmentKind::Assign,
				Some(&**rhs),
				assignment_span,
				checking_data,
			);
		}
		Expression::BinaryAssignmentOperation { lhs, operator, rhs, position } => {
			let lhs: Assignable = Assignable::Reference(synthesise_access_to_reference(
				lhs,
				environment,
				checking_data,
			));

			use crate::behavior::assignments::AssignmentKind;
			use parser::operators::BinaryAssignmentOperator;

			let assignment_span = position.clone().with_source(environment.get_source());
			return environment.assign_to_assignable_handle_errors(
				lhs,
				operator_to_assignment_kind(operator),
				Some(&**rhs),
				assignment_span,
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
					let assignment_span = position.clone().with_source(environment.get_source());
					return environment.assign_to_assignable_handle_errors(
						lhs,
						crate::behavior::assignments::AssignmentKind::IncrementOrDecrement(
							match direction {
								parser::operators::IncrementOrDecrement::Increment => {
									crate::behavior::assignments::IncrementOrDecrement::Increment
								}
								parser::operators::IncrementOrDecrement::Decrement => {
									crate::behavior::assignments::IncrementOrDecrement::Decrement
								}
							},
							crate::behavior::assignments::AssignmentReturnStatus::New,
						),
						None::<&Expression>,
						assignment_span,
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
				parser::operators::UnaryPostfixAssignmentOperator(direction) => {
					let direction = match direction {
						parser::operators::IncrementOrDecrement::Increment => {
							crate::behavior::assignments::IncrementOrDecrement::Increment
						}
						parser::operators::IncrementOrDecrement::Decrement => {
							crate::behavior::assignments::IncrementOrDecrement::Decrement
						}
					};
					let operator =
						crate::behavior::assignments::AssignmentKind::IncrementOrDecrement(
							direction,
							crate::behavior::assignments::AssignmentReturnStatus::Previous,
						);

					let assignment_span = position.clone().with_source(environment.get_source());
					return environment.assign_to_assignable_handle_errors(
						lhs,
						operator,
						None::<&Expression>,
						assignment_span,
						checking_data,
					);
				}
			}
		}
		Expression::VariableReference(name, position) => {
			let get_variable_or_alternatives = environment.get_variable_handle_error(
				name.as_str(),
				position.clone().with_source(environment.get_source()),
				checking_data,
			);

			match get_variable_or_alternatives {
				Ok(variable) => Instance::LValue(variable),
				Err(_err) => Instance::RValue(TypeId::ERROR_TYPE),
			}
		}
		Expression::PropertyAccess { parent, position, property, .. } => {
			let on = synthesise_expression(parent, environment, checking_data, TypeId::ANY_TYPE);
			let property =
				if let parser::PropertyReference::Standard { property, is_private: _ } = property {
					PropertyKey::String(Cow::Borrowed(property.as_str()))
				} else {
					todo!()
				};

			let access_position = position.clone().with_source(environment.get_source());
			// TODO
			let publicity = Publicity::Public;

			let result = environment.get_property_handle_errors(
				on,
				publicity,
				property,
				checking_data,
				access_position,
			);

			match result {
				Ok(instance) => instance,
				Err(_) => return TypeId::ERROR_TYPE,
			}
		}
		Expression::Index { indexee, indexer, position, .. } => {
			let indexee =
				synthesise_expression(indexee, environment, checking_data, TypeId::ANY_TYPE);
			let indexer = synthesise_multiple_expression(
				indexer,
				environment,
				checking_data,
				TypeId::ANY_TYPE,
			);

			let index_position = position.clone().with_source(environment.get_source());
			// TODO handle differently?
			let result = environment.get_property_handle_errors(
				indexee,
				Publicity::Public,
				PropertyKey::from_type(indexer, &checking_data.types),
				checking_data,
				index_position,
			);

			match result {
				Ok(instance) => instance,
				Err(_) => return TypeId::ERROR_TYPE,
			}
		}
		Expression::ThisReference(pos) => {
			let position = pos.clone().with_source(environment.get_source());
			Instance::RValue(environment.get_value_of_this(&mut checking_data.types, position))
		}
		Expression::SuperExpression(reference, position) => match reference {
			SuperReference::Call { arguments } => {
				todo!();

				// let constructor =
				// 	environment.get_current_constructor().expect("not in constructor");
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
			SuperReference::PropertyAccess { property } => todo!(),
			SuperReference::Index { indexer } => todo!(),
		},
		Expression::NewTarget(..) => todo!(),
		Expression::FunctionCall { function, type_arguments, arguments, position, .. } => {
			let on =
				synthesise_expression(&*function, environment, checking_data, TypeId::ANY_TYPE);

			let (result, special) = call_function(
				on,
				CalledWithNew::None,
				type_arguments,
				Some(arguments),
				environment,
				checking_data,
				position,
			);
			if let Some(special) = special {
				checking_data.type_mappings.special_expressions.push(position.clone(), special);
			}
			Instance::RValue(result)
		}
		Expression::ConstructorCall { constructor, type_arguments, arguments, position } => {
			let on =
				synthesise_expression(&*constructor, environment, checking_data, TypeId::ANY_TYPE);
			let called_with_new = CalledWithNew::New { on };
			let (result, _) = call_function(
				on,
				called_with_new,
				type_arguments,
				arguments.as_ref(),
				environment,
				checking_data,
				position,
			);
			Instance::RValue(result)
		}
		Expression::ConditionalTernaryExpression {
			condition, truthy_result, falsy_result, ..
		} => {
			let condition =
				synthesise_expression(condition, environment, checking_data, TypeId::ANY_TYPE);

			Instance::RValue(environment.new_conditional_context(
				condition,
				|env: &mut Environment, data: &mut CheckingData<T, EznoParser>| {
					synthesise_expression(&truthy_result, env, data, expecting)
				},
				Some(|env: &mut Environment, data: &mut CheckingData<T, EznoParser>| {
					synthesise_expression(&falsy_result, env, data, expecting)
				}),
				checking_data,
			))
		}
		Expression::ArrowFunction(function) => Instance::RValue(register_arrow_function(
			expecting,
			function.header.is_some(),
			function,
			environment,
			checking_data,
		)),
		Expression::ExpressionFunction(function) => {
			let is_async = function.header.is_async();
			let is_generator = function.header.is_generator();
			let location = function.header.get_location().map(|location| match location {
				parser::functions::FunctionLocationModifier::Server(_) => "server".to_owned(),
				parser::functions::FunctionLocationModifier::Module(_) => "module".to_owned(),
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
		Expression::PostfixComment(expression, _comment, _)
		| Expression::PrefixComment(_comment, expression, _) => {
			return synthesise_expression(expression, environment, checking_data, expecting)
		}
		Expression::ParenthesizedExpression(inner_expression, _) => Instance::RValue(
			synthesise_multiple_expression(inner_expression, environment, checking_data, expecting),
		),
		Expression::ClassExpression(class) => {
			Instance::RValue(synthesise_class_declaration(class, environment, checking_data))
		}
		Expression::Cursor { cursor_id, position } => todo!(),
		Expression::SpecialOperators(operator, position) => match operator {
			SpecialOperators::AsExpression { value, type_annotation, .. } => {
				checking_data.diagnostics_container.add_warning(
					TypeCheckWarning::IgnoringAsExpression(
						position.clone().with_source(environment.get_source()),
					),
				);

				return synthesise_expression(value, environment, checking_data, expecting);
			}
			SpecialOperators::IsExpression { value, is_keyword, type_annotation } => todo!(),
			SpecialOperators::SatisfiesExpression { value, type_annotation, .. } => {
				let value = synthesise_expression(value, environment, checking_data, expecting);
				let satisfying =
					synthesise_type_annotation(type_annotation, environment, checking_data);

				checking_data.check_satisfies(
					value,
					satisfying,
					ASTNode::get_position(expression).clone().with_source(environment.get_source()),
					environment,
				);

				return value;
			}
			SpecialOperators::InExpression { lhs, rhs } => {
				let lhs = match lhs {
					parser::expressions::InExpressionLHS::PrivateProperty(_) => todo!(),
					parser::expressions::InExpressionLHS::Expression(lhs) => {
						synthesise_expression(lhs, environment, checking_data, TypeId::ANY_TYPE)
					}
				};
				let rhs = synthesise_expression(rhs, environment, checking_data, TypeId::ANY_TYPE);
				let result =
					environment.property_in(rhs, PropertyKey::from_type(lhs, &checking_data.types));

				Instance::RValue(if result { TypeId::TRUE } else { TypeId::FALSE })
			}
			SpecialOperators::InstanceOfExpression { .. } => todo!(),
		},
		Expression::DynamicImport { position, .. } => {
			checking_data.raise_unimplemented_error(
				"dynamic import",
				position.clone().with_source(environment.get_source()),
			);
			return TypeId::ERROR_TYPE;
		}
		Expression::IsExpression(is_expr) => {
			Instance::RValue(synthesise_is_expression(is_expr, environment, checking_data))
		}
	};

	let position = ASTNode::get_position(expression).clone().with_source(environment.get_source());

	// TODO should be copy
	checking_data.add_expression_mapping(position, instance.clone());

	instance.get_value()
}

fn operator_to_assignment_kind(
	operator: &parser::operators::BinaryAssignmentOperator,
) -> crate::behavior::assignments::AssignmentKind {
	use crate::behavior::assignments::AssignmentKind;
	use parser::operators::BinaryAssignmentOperator;

	match operator {
		BinaryAssignmentOperator::LogicalAndAssign => {
			AssignmentKind::ConditionalUpdate(crate::behavior::operations::Logical::And)
		}
		BinaryAssignmentOperator::LogicalOrAssign => {
			AssignmentKind::ConditionalUpdate(crate::behavior::operations::Logical::Or)
		}
		BinaryAssignmentOperator::LogicalNullishAssignment => {
			AssignmentKind::ConditionalUpdate(crate::behavior::operations::Logical::NullCoalescing)
		}
		BinaryAssignmentOperator::AddAssign => {
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
		BinaryAssignmentOperator::BitwiseShiftRightUnsigned => {
			AssignmentKind::PureUpdate(MathematicalAndBitwise::Add)
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
/// TODO error with function_type_id should be handled earlier
fn call_function<T: crate::ReadFromFS>(
	function_type_id: TypeId,
	called_with_new: CalledWithNew,
	type_arguments: &Option<Vec<parser::TypeAnnotation>>,
	mut arguments: Option<&Vec<SpreadExpression>>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	call_site: &parser::Span,
) -> (TypeId, Option<SpecialExpressions>) {
	let generic_type_arguments = if let Some(type_arguments) = type_arguments {
		Some(
			type_arguments
				.iter()
				.map(|generic_type_argument| {
					(
						synthesise_type_annotation(
							generic_type_argument,
							environment,
							checking_data,
						),
						generic_type_argument
							.get_position()
							.clone()
							.with_source(environment.get_source()),
					)
				})
				.collect::<Vec<_>>(),
		)
	} else {
		None
	};

	let synthesised_arguments = arguments
		.as_mut()
		.map(|arguments| synthesise_arguments(arguments, environment, checking_data))
		.unwrap_or_default();

	crate::types::calling::call_type_handle_errors(
		function_type_id,
		called_with_new,
		Default::default(),
		generic_type_arguments,
		synthesised_arguments,
		call_site.clone().with_source(environment.get_source()),
		environment,
		checking_data,
	)
}

/// TODO do lazily
fn synthesise_arguments<T: crate::ReadFromFS>(
	arguments: &Vec<SpreadExpression>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> Vec<SynthesisedArgument> {
	arguments
		.iter()
		.flat_map(|argument| match argument {
			SpreadExpression::Spread(expr, _) => {
				todo!()
				// Some(synthesisedFunctionArgument::Spread(synthesise_expression(
				//     expr,
				//     environment,
				//     checking_data,
				//
				//
				// )))
			}
			SpreadExpression::NonSpread(expr) => {
				let non_param = expr.get_non_parenthesized();
				// if matches!(
				// 	non_param,
				// 	Expression::ArrowFunction(_)
				// 		| Expression::ExpressionFunction(ExpressionFunction { name: None, .. })
				// ) {
				// 	crate::utils::notify!("TODO absolute function here");
				// }
				// TODO expecting here
				let ty = synthesise_expression(expr, environment, checking_data, TypeId::ANY_TYPE);
				Some(SynthesisedArgument::NonSpread {
					ty,
					position: ASTNode::get_position(expr)
						.clone()
						.with_source(environment.get_source()),
				})
			}
			SpreadExpression::Empty => None,
		})
		.collect::<Vec<SynthesisedArgument>>()
}

pub(super) fn synthesise_object_literal<T: crate::ReadFromFS>(
	ObjectLiteral { members, .. }: &ObjectLiteral,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	environment: &mut Environment,
	expected: TypeId,
) -> TypeId {
	let mut object_builder =
		ObjectBuilder::new(None, &mut checking_data.types, &mut environment.facts);

	for member in members.iter() {
		let member_position = member.get_position().clone().with_source(environment.get_source());
		match member {
			ObjectLiteralMember::Spread(spread, pos) => {
				checking_data.raise_unimplemented_error(
					"spread in object literal",
					pos.clone().with_source(environment.get_source()),
				);
			}
			ObjectLiteralMember::Shorthand(name, position) => {
				let key = PropertyKey::String(Cow::Owned(name.clone()));
				let get_variable = environment.get_variable_handle_error(
					name,
					position.clone().with_source(environment.get_source()),
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
			ObjectLiteralMember::Property(key, expression, _) => {
				let key = parser_property_key_to_checker_property_key(
					key.get_ast_ref(),
					environment,
					checking_data,
				);

				// TODO base of above
				let expecting = TypeId::ANY_TYPE;
				let value =
					synthesise_expression(expression, environment, checking_data, expecting);

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
			ObjectLiteralMember::Method(method) => {
				let key = parser_property_key_to_checker_property_key(
					method.name.get_ast_ref(),
					environment,
					checking_data,
				);

				let behavior = crate::behavior::functions::FunctionRegisterBehavior::ObjectMethod {
					is_async: method.header.is_async(),
					is_generator: method.header.is_generator(),
				};

				let function = environment.new_function(checking_data, method, behavior);

				let property = match &method.header {
					MethodHeader::Get(_) => crate::PropertyValue::Getter(Box::new(function)),
					MethodHeader::Set(_) => crate::PropertyValue::Setter(Box::new(function)),
					_ => {
						crate::PropertyValue::Value(checking_data.types.new_function_type(function))
					}
				};

				object_builder.append(
					environment,
					Publicity::Public,
					key,
					property,
					Some(member_position),
				)
			}
		}
	}

	object_builder.build_object()
}
