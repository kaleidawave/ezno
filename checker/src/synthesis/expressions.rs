use std::convert::TryInto;

use parser::{
	expressions::{
		assignments::LHSOfAssignment, MultipleExpression, SpecialOperators, SpreadExpression,
		SuperReference, TemplateLiteral,
	},
	operators::{UnaryOperator, UnaryPrefixAssignmentOperator},
	ASTNode, Expression,
};

use crate::{
	behavior::{
		assignments::{Assignable, SynthesizableExpression},
		functions::{RegisterAsType, RegisterOnExistingObject},
		objects::ObjectBuilder,
	},
	diagnostics::{TypeCheckError, TypeCheckWarning, TypeStringRepresentation},
	evaluate_binary_operator_handle_errors,
	events::{CalledWithNew, Event},
	types::functions::SynthesizedArgument,
	types::{evaluate_unary_operator, properties::PropertyResult, Constant, TypeId},
	CheckingData, Environment, Instance,
};

use super::{
	assignments::{synthesize_access_to_reference, synthesize_lhs_of_assignment_to_reference},
	extensions::{is_expression::synthesize_is_expression, jsx::synthesize_jsx_root},
	type_annotations::synthesize_type_annotation,
};

pub(super) fn synthesize_multiple_expression<T: crate::FSResolver>(
	expression: &MultipleExpression,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) -> TypeId {
	if let Some(lhs) = &expression.lhs {
		synthesize_multiple_expression(lhs, environment, checking_data);
	}
	synthesize_expression(&expression.rhs, environment, checking_data)
}

pub(super) fn synthesize_expression<T: crate::FSResolver>(
	expression: &Expression,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
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
			fn synthesize_array_item<T: crate::FSResolver>(
				idx: usize,
				element: &SpreadExpression,
				environment: &mut Environment,
				checking_data: &mut CheckingData<T>,
			) -> (TypeId, TypeId) {
				match element {
					SpreadExpression::NonSpread(element) => {
						let expression_type =
							synthesize_expression(element, environment, checking_data);
						(
							checking_data.types.new_constant_type(Constant::Number(
								(idx as f64).try_into().unwrap(),
							)),
							expression_type,
						)
					}
					SpreadExpression::Spread(expr, _) => {
						{
							checking_data.diagnostics_container.add_error(
								TypeCheckError::Unsupported {
									thing: "Spread elements",
									at: ASTNode::get_position(expr).into_owned(),
								},
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

			let mut basis =
				ObjectBuilder::new(Some(TypeId::ARRAY_TYPE), &mut checking_data.types, environment);

			for (idx, value) in elements.iter().enumerate() {
				let (key, value) = synthesize_array_item(idx, value, environment, checking_data);

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
			let synthesize_object_literal =
				synthesize_object_literal(object_literal, checking_data, environment);

			Instance::RValue(synthesize_object_literal)
		}
		Expression::TemplateLiteral(TemplateLiteral { tag, parts, position }) => {
			checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
				thing: "template literals",
				at: position.clone(),
			});
			Instance::RValue(TypeId::ERROR_TYPE)
		}
		Expression::BinaryOperation { lhs, operator, rhs, .. } => {
			let lhs_ty = synthesize_expression(&*lhs, environment, checking_data);
			let rhs_ty = synthesize_expression(&*rhs, environment, checking_data);

			if lhs_ty == TypeId::ERROR_TYPE || rhs_ty == TypeId::ERROR_TYPE {
				return TypeId::ERROR_TYPE;
			}

			let operator = super::parser_binary_operator_to_others(*operator);

			Instance::RValue(evaluate_binary_operator_handle_errors(
				operator,
				(lhs_ty, ASTNode::get_position(&**lhs).into_owned()),
				(rhs_ty, ASTNode::get_position(&**rhs).into_owned()),
				environment,
				checking_data,
			))
		}
		Expression::UnaryOperation { operand, operator, position } => {
			let operand_type = synthesize_expression(&*operand, environment, checking_data);

			if let UnaryOperator::Await = operator {
				todo!()
			}
			// TODO might be different by parser
			else if let UnaryOperator::Delete = operator {
				todo!()
			} else if let UnaryOperator::Yield | UnaryOperator::DelegatedYield = operator {
				todo!()
			} else {
				let operator = super::parser_unary_operator_to_others(*operator);
				let result = evaluate_unary_operator(
					operator,
					operand_type,
					environment,
					checking_data.settings.strict_casts,
					&mut checking_data.types,
				);
				match result {
					Ok(value) => Instance::RValue(value),
					Err(_) => {
						todo!("add error");
						return TypeId::ERROR_TYPE;
					}
				}
			}
		}
		Expression::Assignment { lhs, rhs } => {
			let lhs: Assignable =
				synthesize_lhs_of_assignment_to_reference(lhs, environment, checking_data);

			return environment.assign_to_assignable_handle_errors(
				lhs,
				crate::behavior::assignments::AssignmentKind::Assign,
				Some(&**rhs),
				ASTNode::get_position(expression).into_owned(),
				checking_data,
			);
		}
		Expression::BinaryAssignmentOperation { lhs, operator, rhs } => {
			let lhs: Assignable = Assignable::Reference(synthesize_access_to_reference(
				lhs,
				environment,
				checking_data,
			));

			return environment.assign_to_assignable_handle_errors(
				lhs,
				crate::behavior::assignments::AssignmentKind::Update(
					binary_assignment_operator_to_binary_operator(operator),
				),
				Some(&**rhs),
				ASTNode::get_position(expression).into_owned(),
				checking_data,
			);
		}
		Expression::UnaryPrefixAssignmentOperation { operator, operand, position } => {
			let lhs: Assignable = Assignable::Reference(synthesize_access_to_reference(
				operand,
				environment,
				checking_data,
			));

			match operator {
				UnaryPrefixAssignmentOperator::Invert => todo!(),
				UnaryPrefixAssignmentOperator::IncrementOrDecrement(direction) => {
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
						ASTNode::get_position(expression).into_owned(),
						checking_data,
					);
				}
			}
		}
		Expression::UnaryPostfixAssignmentOperation { operand, operator, position } => {
			let lhs: Assignable = Assignable::Reference(synthesize_access_to_reference(
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

					return environment.assign_to_assignable_handle_errors(
						lhs,
						operator,
						None::<&Expression>,
						ASTNode::get_position(expression).into_owned(),
						checking_data,
					);
				}
			}
		}
		Expression::VariableReference(name, position) => {
			let get_variable_or_alternatives =
				environment.get_variable_or_error(name.as_str(), &position, checking_data);

			match get_variable_or_alternatives {
				Ok(variable) => Instance::LValue(variable),
				Err(_err) => Instance::RValue(TypeId::ERROR_TYPE),
			}
		}
		Expression::PropertyAccess { parent, position, property, .. } => {
			let on = synthesize_expression(parent, environment, checking_data);
			let property = if let parser::PropertyReference::Standard(name) = property {
				checking_data.types.new_constant_type(Constant::String(name.clone()))
			} else {
				todo!()
			};
			let get_property =
				environment.get_property(on, property, &mut checking_data.types, None);

			match get_property {
				Some(property_value) => match property_value {
					PropertyResult::Getter(result) => Instance::GValue(result),
					// TODO instance.property...?
					PropertyResult::Generic(result) | PropertyResult::Direct(result) => {
						Instance::RValue(result)
					}
				},
				None => {
					// TODO could do a positional reference to the variable...?
					checking_data.diagnostics_container.add_error(
						TypeCheckError::PropertyDoesNotExist {
							property: TypeStringRepresentation::from_type_id(
								property,
								&environment.into_general_context(),
								&checking_data.types,
								checking_data.settings.debug_types,
							),
							on: TypeStringRepresentation::from_type_id(
								on,
								&environment.into_general_context(),
								&checking_data.types,
								checking_data.settings.debug_types,
							),
							site: position.clone(),
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
				// synthesize_class_fields(fields, environment, checking_data, &chain);

				// Instance::RValue(ty)
			}
			SuperReference::PropertyAccess { property } => todo!(),
			SuperReference::Index { indexer } => todo!(),
		},
		Expression::NewTarget(..) => todo!(),
		Expression::FunctionCall { function, type_arguments, arguments, position, .. } => {
			let on = synthesize_expression(&*function, environment, checking_data);

			let result = call_function(
				on,
				CalledWithNew::None,
				type_arguments,
				Some(arguments),
				environment,
				checking_data,
				position,
			);
			Instance::RValue(result)
		}
		Expression::ConstructorCall { constructor, type_arguments, arguments, position } => {
			let on = synthesize_expression(&*constructor, environment, checking_data);
			let called_with_new = CalledWithNew::New { import_new: on };
			let result = call_function(
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
			let indexee = synthesize_expression(indexee, environment, checking_data);
			let indexer = synthesize_multiple_expression(indexer, environment, checking_data);

			let get_property =
				environment.get_property(indexee, indexer, &mut checking_data.types, None);

			match get_property {
				Some(property_value) => match property_value {
					PropertyResult::Getter(result) => Instance::GValue(result),
					// TODO instance.property...?
					PropertyResult::Generic(result) | PropertyResult::Direct(result) => {
						Instance::RValue(result)
					}
				},
				None => {
					// TODO could do a positional reference to the variable...?
					checking_data.diagnostics_container.add_error(
						TypeCheckError::PropertyDoesNotExist {
							property: TypeStringRepresentation::from_type_id(
								indexer,
								&environment.into_general_context(),
								&checking_data.types,
								checking_data.settings.debug_types,
							),
							on: TypeStringRepresentation::from_type_id(
								indexee,
								&environment.into_general_context(),
								&checking_data.types,
								checking_data.settings.debug_types,
							),
							site: position.clone(),
						},
					);

					return TypeId::ERROR_TYPE;
				}
			}
		}
		Expression::TernaryExpression { condition, truthy_result, falsy_result, .. } => {
			// TODO could do some proof stuff here
			let _condition = synthesize_expression(condition, environment, checking_data);

			let truthy_instance = synthesize_expression(truthy_result, environment, checking_data);
			let falsy_instance = synthesize_expression(falsy_result, environment, checking_data);

			todo!()
			// TypeId::build_union_from_type_iterator(IntoIterator::into_iter([
			// 	truthy_instance,
			// 	falsy_instance,
			// ]))
			// .into()
		}
		Expression::ExpressionFunction(_) => {
			todo!()
		}
		Expression::ArrowFunction(arrow_function) => Instance::RValue(environment.new_function(
			checking_data,
			arrow_function,
			RegisterAsType,
		)),
		Expression::Null(_) => return TypeId::NULL_TYPE,
		Expression::JSXRoot(jsx_root) => {
			Instance::RValue(synthesize_jsx_root(jsx_root, environment, checking_data))
		}
		Expression::PostfixComment(expression, _comment, _)
		| Expression::PrefixComment(_comment, expression, _) => {
			Instance::RValue(synthesize_expression(expression, environment, checking_data))
		}
		Expression::ParenthesizedExpression(inner_expression, _) => Instance::RValue(
			synthesize_multiple_expression(inner_expression, environment, checking_data),
		),
		Expression::ClassExpression(_) => {
			todo!()
		}
		Expression::Cursor { cursor_id, position } => todo!(),
		Expression::SpecialOperators(operator, position) => match operator {
			SpecialOperators::AsExpression { value, type_annotation, .. } => {
				checking_data
					.diagnostics_container
					.add_warning(TypeCheckWarning::IgnoringAsExpression(position.clone()));

				return synthesize_expression(value, environment, checking_data);
			}
			SpecialOperators::IsExpression { value, is_keyword, type_annotation } => todo!(),
			SpecialOperators::SatisfiesExpression { value, type_annotation, .. } => {
				let value = synthesize_expression(value, environment, checking_data);
				let satisfying =
					synthesize_type_annotation(type_annotation, environment, checking_data);

				checking_data.check_satisfies(
					value,
					satisfying,
					ASTNode::get_position(expression).into_owned(),
					environment,
				);

				return value;
			}
		},
		Expression::DynamicImport { path, position } => todo!(),
		Expression::IsExpression(is_expr) => {
			Instance::RValue(synthesize_is_expression(is_expr, environment, checking_data))
		}
	};

	let position = ASTNode::get_position(expression).into_owned();

	// TODO should be copy
	checking_data.add_expression_mapping(position, instance.clone());

	instance.get_value()
}

fn binary_assignment_operator_to_binary_operator(
	operator: &parser::operators::BinaryAssignmentOperator,
) -> crate::structures::operators::BinaryOperator {
	match operator {
		parser::operators::BinaryAssignmentOperator::LogicalNullishAssignment => todo!(),
		parser::operators::BinaryAssignmentOperator::AddAssign => {
			crate::structures::operators::BinaryOperator::Add
		}
		parser::operators::BinaryAssignmentOperator::SubtractAssign => {
			crate::structures::operators::BinaryOperator::Subtract
		}
		parser::operators::BinaryAssignmentOperator::MultiplyAssign => {
			crate::structures::operators::BinaryOperator::Multiply
		}
		parser::operators::BinaryAssignmentOperator::DivideAssign => todo!(),
		parser::operators::BinaryAssignmentOperator::ModuloAssign => todo!(),
		parser::operators::BinaryAssignmentOperator::ExponentAssign => todo!(),
		parser::operators::BinaryAssignmentOperator::LogicalAndAssign => todo!(),
		parser::operators::BinaryAssignmentOperator::LogicalOrAssign => todo!(),
		parser::operators::BinaryAssignmentOperator::BitwiseShiftLeftAssign => todo!(),
		parser::operators::BinaryAssignmentOperator::BitwiseShiftRightAssign => todo!(),
		parser::operators::BinaryAssignmentOperator::BitwiseShiftRightUnsigned => todo!(),
		parser::operators::BinaryAssignmentOperator::BitwiseAndAssign => todo!(),
		parser::operators::BinaryAssignmentOperator::BitwiseXorAssign => todo!(),
		parser::operators::BinaryAssignmentOperator::BitwiseOrAssign => todo!(),
	}
}

/// Generic for functions + constructor calls
///
/// TODO error with function_type_id should be handled earlier
fn call_function<T: crate::FSResolver>(
	function_type_id: TypeId,
	called_with_new: CalledWithNew,
	type_arguments: &Option<Vec<parser::TypeAnnotation>>,
	mut arguments: Option<&Vec<SpreadExpression>>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	call_site: &parser::Span,
) -> TypeId {
	let generic_type_arguments = if let Some(type_arguments) = type_arguments {
		Some(
			type_arguments
				.iter()
				.map(|generic_type_argument| {
					(
						generic_type_argument.get_position().into_owned(),
						synthesize_type_annotation(
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

	let synthesized_arguments = arguments
		.as_mut()
		.map(|arguments| synthesize_arguments(arguments, environment, checking_data))
		.unwrap_or_default();

	crate::types::calling::call_type_handle_errors(
		function_type_id,
		called_with_new,
		None,
		generic_type_arguments,
		synthesized_arguments,
		call_site.clone(),
		environment,
		checking_data,
	)
}

fn synthesize_arguments<T: crate::FSResolver>(
	arguments: &Vec<SpreadExpression>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) -> Vec<SynthesizedArgument> {
	arguments
		.iter()
		.flat_map(|argument| match argument {
			SpreadExpression::Spread(expr, _) => {
				todo!()
				// Some(SynthesizedFunctionArgument::Spread(synthesize_expression(
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
				let ty = synthesize_expression(expr, environment, checking_data);
				Some(SynthesizedArgument::NonSpread {
					ty,
					position: ASTNode::get_position(expr).into_owned(),
				})
			}
			SpreadExpression::Empty => None,
		})
		.collect::<Vec<SynthesizedArgument>>()
}

pub(super) fn synthesize_class_fields<T: crate::FSResolver>(
	fields: Vec<(TypeId, Expression)>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) {
	let this = environment.get_value_of_this(&mut checking_data.types);
	for (under, mut expression) in fields {
		let new = synthesize_expression(&expression, environment, checking_data);
		environment.context_type.events.push(Event::Setter {
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

pub(super) fn synthesize_lhs_of_assignment<T: crate::FSResolver>(
	reference: &parser::ast::LHSOfAssignment,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) -> () {
	match reference {
		LHSOfAssignment::ObjectDestructuring(_, _) => todo!(),
		LHSOfAssignment::ArrayDestructuring(_, _) => todo!(),
		LHSOfAssignment::VariableOrPropertyAccess(_) => todo!(),
	}
}

impl SynthesizableExpression for Expression {
	fn synthesize_expression<U: crate::FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<U>,
	) -> TypeId {
		synthesize_expression(self, environment, checking_data)
	}

	fn get_position(&self) -> source_map::Span {
		ASTNode::get_position(self).into_owned()
	}
}

use parser::expressions::object_literal::{ObjectLiteral, ObjectLiteralMember};

use crate::{structures::variables::VariableWithValue, synthesis::property_key_as_type};

pub(super) fn synthesize_object_literal<T: crate::FSResolver>(
	ObjectLiteral { members, .. }: &ObjectLiteral,
	checking_data: &mut CheckingData<T>,
	environment: &mut Environment,
) -> TypeId {
	let mut object_builder = ObjectBuilder::new(None, &mut checking_data.types, environment);

	for member in members.iter() {
		match member {
			ObjectLiteralMember::SpreadExpression(spread, _) => {
				checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
					thing: "spread in object literal",
					at: ASTNode::get_position(spread).into_owned(),
				});
			}
			ObjectLiteralMember::Shorthand(name, position) => {
				let key = checking_data.types.new_constant_type(Constant::String(name.clone()));
				let get_variable = environment.get_variable_or_error(name, position, checking_data);
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
					property_key_as_type(key.get_ast(), environment, &mut checking_data.types);

				let value = synthesize_expression(expression, environment, checking_data);

				object_builder.append(
					environment,
					key,
					crate::types::properties::Property::Value(value),
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
				let key = property_key_as_type(
					method.name.get_ast(),
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
