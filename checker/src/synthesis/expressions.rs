use std::{convert::TryInto, marker::PhantomData};

use parser::{
	expressions::{
		assignments::LHSOfAssignment, MultipleExpression, SpecialOperators, SpreadExpression,
		SuperReference, TemplateLiteral,
	},
	operators::{BinaryAssignmentOperator, UnaryOperator, UnaryPrefixAssignmentOperator},
	ASTNode, Expression,
};

use crate::{
	behavior::{
		assignments::{Assignable, SynthesisableExpression},
		functions::{RegisterAsType, RegisterOnExistingObject},
		objects::ObjectBuilder,
		operations::{
			evaluate_logical_operation_with_expression,
			evaluate_pure_binary_operation_handle_errors, evaluate_pure_unary_operator,
			EqualityAndInequality, MathematicalAndBitwise, PureUnary,
		},
		template_literal::synthesise_template_literal,
	},
	diagnostics::{TypeCheckError, TypeCheckWarning, TypeStringRepresentation},
	events::Event,
	types::{calling::CalledWithNew, functions::SynthesisedArgument},
	types::{properties::PropertyKind, Constant, TypeId},
	AssignmentKind, CheckingData, Environment, Instance, SpecialExpressions,
};

use super::{
	assignments::{synthesise_access_to_reference, synthesise_lhs_of_assignment_to_reference},
	extensions::{is_expression::synthesise_is_expression, jsx::synthesise_jsx_root},
	type_annotations::synthesise_type_annotation,
};

pub(super) fn synthesise_multiple_expression<T: crate::ReadFromFS>(
	expression: &MultipleExpression,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, parser::Module>,
) -> TypeId {
	match expression {
		MultipleExpression::Multiple { lhs, rhs, position: _ } => {
			synthesise_multiple_expression(lhs, environment, checking_data);
			synthesise_expression(rhs, environment, checking_data)
		}
		MultipleExpression::Single(expression) => {
			synthesise_expression(expression, environment, checking_data)
		}
	}
}

pub(super) fn synthesise_expression<T: crate::ReadFromFS>(
	expression: &Expression,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, parser::Module>,
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
				idx: usize,
				element: &SpreadExpression,
				environment: &mut Environment,
				checking_data: &mut CheckingData<T, parser::Module>,
			) -> (TypeId, TypeId) {
				match element {
					SpreadExpression::NonSpread(element) => {
						let expression_type =
							synthesise_expression(element, environment, checking_data);
						(
							checking_data.types.new_constant_type(Constant::Number(
								(idx as f64).try_into().unwrap(),
							)),
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
							checking_data.types.new_constant_type(Constant::Number(
								(idx as f64).try_into().unwrap(),
							)),
							TypeId::ERROR_TYPE,
						)
					}
					SpreadExpression::Empty => {
						crate::utils::notify!("Empty expression temp as empty");
						(
							checking_data.types.new_constant_type(Constant::Number(
								(idx as f64).try_into().unwrap(),
							)),
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

			for (idx, value) in elements.iter().enumerate() {
				let (key, value) = synthesise_array_item(idx, value, environment, checking_data);

				basis.append(environment, key, crate::types::properties::Property::Value(value));
			}
			let len = checking_data
				.types
				.new_constant_type(Constant::Number((elements.len() as f64).try_into().unwrap()));

			basis.append(
				environment,
				TypeId::LENGTH_AS_STRING,
				crate::types::properties::Property::Value(len),
			);

			Instance::RValue(basis.build_object())
		}
		Expression::ObjectLiteral(object_literal) => {
			let synthesise_object_literal =
				synthesise_object_literal(object_literal, checking_data, environment);

			Instance::RValue(synthesise_object_literal)
		}
		Expression::TemplateLiteral(TemplateLiteral { tag, parts, position }) => {
			let mut parts_iter = parts.iter().map(|part| match part {
				parser::expressions::TemplateLiteralPart::Static(value) => {
					crate::behavior::template_literal::TemplateLiteralPart::Static(value.as_str())
				}
				parser::expressions::TemplateLiteralPart::Dynamic(expr) => {
					crate::behavior::template_literal::TemplateLiteralPart::Dynamic(
						&**expr,
						PhantomData::default(),
					)
				}
			});
			let tag =
				tag.as_ref().map(|expr| synthesise_expression(expr, environment, checking_data));

			synthesise_template_literal(tag, parts_iter, environment, checking_data)
		}
		Expression::BinaryOperation { lhs, operator, rhs, .. } => {
			let lhs_ty = synthesise_expression(&*lhs, environment, checking_data);

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

			let rhs_ty = synthesise_expression(&*rhs, environment, checking_data);

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
			let operand_type = synthesise_expression(&*operand, environment, checking_data);

			match operator {
				UnaryOperator::Plus => todo!(),
				UnaryOperator::Negation | UnaryOperator::BitwiseNot | UnaryOperator::LogicalNot => {
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
						)
						.unwrap(),
					)
				}
				UnaryOperator::Await => todo!(),
				UnaryOperator::TypeOf => todo!(),
				UnaryOperator::Void => todo!(),
				UnaryOperator::Delete => todo!(),
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
							crate::AssignmentReturnStatus::Previous,
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
			let get_variable_or_alternatives = environment.get_variable_or_error(
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
			let on = synthesise_expression(parent, environment, checking_data);
			let property =
				if let parser::PropertyReference::Standard { property, is_private: _ } = property {
					checking_data.types.new_constant_type(Constant::String(property.clone()))
				} else {
					todo!()
				};
			let get_property =
				environment.get_property(on, property, &mut checking_data.types, None);

			match get_property {
				Some((kind, result)) => match kind {
					PropertyKind::Getter => Instance::GValue(result),
					// TODO instance.property...?
					PropertyKind::Generic | PropertyKind::Direct => Instance::RValue(result),
				},
				None => {
					// TODO could do a positional reference to the variable...?
					checking_data.diagnostics_container.add_error(
						TypeCheckError::PropertyDoesNotExist {
							property: TypeStringRepresentation::from_type_id(
								property,
								&environment.as_general_context(),
								&checking_data.types,
								checking_data.options.debug_types,
							),
							on: TypeStringRepresentation::from_type_id(
								on,
								&environment.as_general_context(),
								&checking_data.types,
								checking_data.options.debug_types,
							),
							site: position.clone().with_source(environment.get_source()),
						},
					);

					return TypeId::ERROR_TYPE;
				}
			}
		}
		Expression::ThisReference(..) => {
			Instance::RValue(environment.get_value_of_this(&mut checking_data.types))
		}
		Expression::SuperExpression(reference, position) => match reference {
			SuperReference::Call { arguments } => {
				let constructor =
					environment.get_current_constructor().expect("not in constructor");

				todo!();

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
			let on = synthesise_expression(&*function, environment, checking_data);

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
				crate::utils::notify!("Special! {:?}", special);
				checking_data.type_mappings.special_expressions.push(position.clone(), special);
			}
			Instance::RValue(result)
		}
		Expression::ConstructorCall { constructor, type_arguments, arguments, position } => {
			let on = synthesise_expression(&*constructor, environment, checking_data);
			let called_with_new = CalledWithNew::New { import_new: on };
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
		Expression::Index { indexee, indexer, position, .. } => {
			let indexee = synthesise_expression(indexee, environment, checking_data);
			let indexer = synthesise_multiple_expression(indexer, environment, checking_data);

			let get_property =
				environment.get_property(indexee, indexer, &mut checking_data.types, None);

			match get_property {
				Some((kind, result)) => match kind {
					PropertyKind::Getter => Instance::GValue(result),
					// TODO instance.property...?
					PropertyKind::Generic | PropertyKind::Direct => Instance::RValue(result),
				},
				None => {
					// TODO could do a positional reference to the variable...?
					checking_data.diagnostics_container.add_error(
						TypeCheckError::PropertyDoesNotExist {
							property: TypeStringRepresentation::from_type_id(
								indexer,
								&environment.as_general_context(),
								&checking_data.types,
								checking_data.options.debug_types,
							),
							on: TypeStringRepresentation::from_type_id(
								indexee,
								&environment.as_general_context(),
								&checking_data.types,
								checking_data.options.debug_types,
							),
							site: position.clone().with_source(environment.get_source()),
						},
					);

					return TypeId::ERROR_TYPE;
				}
			}
		}
		Expression::ConditionalTernaryExpression {
			condition, truthy_result, falsy_result, ..
		} => {
			let condition = synthesise_expression(condition, environment, checking_data);

			Instance::RValue(environment.new_conditional_context(
				condition,
				truthy_result.as_ref(),
				Some(falsy_result.as_ref()),
				checking_data,
			))

			// TODO narrowing here based on condition and conditional events
			// let true_result = synthesise_expression(truthy_result, environment, checking_data);
			// let false_result = synthesise_expression(falsy_result, environment, checking_data);

			// let value = environment.is_type_truthy_falsy(condition, &checking_data.types);

			// match value {
			// 	crate::TruthyFalsy::Decidable(value) => {
			// 		// TODO report issue about LHS RHS unnecessary
			// 		Instance::RValue(match value {
			// 			true => true_result,
			// 			false => false_result,
			// 		})
			// 	}
			// 	crate::TruthyFalsy::Unknown => {
			// 		let constructor =
			// 			crate::Type::Constructor(crate::types::Constructor::ConditionalResult {
			// 				condition,
			// 				truthy_result: true_result,
			// 				else_result: false_result,
			// 				result_union: TypeId::ERROR_TYPE,
			// 			});
			// 		crate::utils::notify!("Here TODO conditional events");
			// 		Instance::RValue(checking_data.types.register_type(constructor))
			// 	}
			// }

			// TypeId::build_union_from_type_iterator(IntoIterator::into_iter([
			// 	truthy_instance,
			// 	falsy_instance,
			// ]))
			// .into()
		}
		Expression::ExpressionFunction(function) => {
			Instance::RValue(environment.new_function(checking_data, function, RegisterAsType))
		}
		Expression::ArrowFunction(arrow_function) => Instance::RValue(environment.new_function(
			checking_data,
			arrow_function,
			RegisterAsType,
		)),
		Expression::Null(_) => return TypeId::NULL_TYPE,
		Expression::JSXRoot(jsx_root) => {
			Instance::RValue(synthesise_jsx_root(jsx_root, environment, checking_data))
		}
		Expression::PostfixComment(expression, _comment, _)
		| Expression::PrefixComment(_comment, expression, _) => {
			Instance::RValue(synthesise_expression(expression, environment, checking_data))
		}
		Expression::ParenthesizedExpression(inner_expression, _) => Instance::RValue(
			synthesise_multiple_expression(inner_expression, environment, checking_data),
		),
		Expression::ClassExpression(_) => {
			todo!()
		}
		Expression::Cursor { cursor_id, position } => todo!(),
		Expression::SpecialOperators(operator, position) => match operator {
			SpecialOperators::AsExpression { value, type_annotation, .. } => {
				checking_data.diagnostics_container.add_warning(
					TypeCheckWarning::IgnoringAsExpression(
						position.clone().with_source(environment.get_source()),
					),
				);

				return synthesise_expression(value, environment, checking_data);
			}
			SpecialOperators::IsExpression { value, is_keyword, type_annotation } => todo!(),
			SpecialOperators::SatisfiesExpression { value, type_annotation, .. } => {
				let value = synthesise_expression(value, environment, checking_data);
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
			SpecialOperators::InExpression { .. } => todo!(),
			SpecialOperators::InstanceOfExpression { .. } => todo!(),
		},
		Expression::DynamicImport { path, position } => todo!(),
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
) -> crate::AssignmentKind {
	use crate::AssignmentKind;
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
	checking_data: &mut CheckingData<T, parser::Module>,
	call_site: &parser::Span,
) -> (TypeId, Option<SpecialExpressions>) {
	let generic_type_arguments = if let Some(type_arguments) = type_arguments {
		Some(
			type_arguments
				.iter()
				.map(|generic_type_argument| {
					(
						generic_type_argument
							.get_position()
							.clone()
							.with_source(environment.get_source()),
						synthesise_type_annotation(
							generic_type_argument,
							environment,
							checking_data,
						),
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

fn synthesise_arguments<T: crate::ReadFromFS>(
	arguments: &Vec<SpreadExpression>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, parser::Module>,
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
				let ty = synthesise_expression(expr, environment, checking_data);
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

pub(super) fn synthesise_class_fields<T: crate::ReadFromFS>(
	fields: Vec<(TypeId, Expression)>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, parser::Module>,
) {
	let this = environment.get_value_of_this(&mut checking_data.types);
	for (under, mut expression) in fields {
		let new = synthesise_expression(&expression, environment, checking_data);
		environment.facts.events.push(Event::Setter {
			on: this,
			new: crate::types::properties::Property::Value(new),
			under,
			reflects_dependency: None,
			initialization: true,
		});
		todo!()
		// environment.new_property(this, under, new, false);
	}
}

impl SynthesisableExpression<parser::Module> for Expression {
	fn synthesise_expression<U: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<U, parser::Module>,
	) -> TypeId {
		synthesise_expression(self, environment, checking_data)
	}

	fn get_position(&self) -> &source_map::Span {
		ASTNode::get_position(self)
	}
}

use parser::expressions::object_literal::{ObjectLiteral, ObjectLiteralMember};

use crate::{structures::variables::VariableWithValue, synthesis::property_key_as_type};

pub(super) fn synthesise_object_literal<T: crate::ReadFromFS>(
	ObjectLiteral { members, .. }: &ObjectLiteral,
	checking_data: &mut CheckingData<T, parser::Module>,
	environment: &mut Environment,
) -> TypeId {
	let mut object_builder =
		ObjectBuilder::new(None, &mut checking_data.types, &mut environment.facts);

	for member in members.iter() {
		match member {
			ObjectLiteralMember::SpreadExpression(spread, pos) => {
				checking_data.raise_unimplemented_error(
					"spread in object literal",
					pos.clone().with_source(environment.get_source()),
				);
			}
			ObjectLiteralMember::Shorthand(name, position) => {
				let key = checking_data.types.new_constant_type(Constant::String(name.clone()));
				let get_variable = environment.get_variable_or_error(
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
					key,
					crate::types::properties::Property::Value(value),
				);
			}
			ObjectLiteralMember::Property(key, expression, _) => {
				let key =
					property_key_as_type(key.get_ast_ref(), environment, &mut checking_data.types);

				let value = synthesise_expression(expression, environment, checking_data);
				let value = crate::types::properties::Property::Value(value);
				object_builder.append(environment, key, value);

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
				let key = property_key_as_type(
					method.name.get_ast_ref(),
					environment,
					&mut checking_data.types,
				);

				let property =
					environment.new_function(checking_data, method, RegisterOnExistingObject);
				object_builder.append(environment, key, property)
			}
		}
	}

	object_builder.build_object()
}
