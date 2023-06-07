use std::convert::TryInto;

use parser::{
	expressions::{
		assignments::LHSOfAssignment, MultipleExpression, SpecialOperators, SpreadExpression,
		SuperReference, TemplateLiteral,
	},
	operators::{UnaryOperator, UnaryPrefixAssignmentOperator},
	ASTNode, Chain, Expression,
};
use temporary_annex::Annex;

use crate::{
	errors::{TypeCheckError, TypeCheckWarning, TypeStringRepresentation},
	events::{CalledWithNew, Event},
	structures::{functions::SynthesizedArgument, objects::ObjectBuilder},
	types::{
		evaluate_binary_operator, evaluate_unary_operator,
		properties::PropertyResult,
		subtyping::{type_is_subtype, BasicEquality, SubTypeResult},
		Constant, TypeId,
	},
	CheckingData, Environment, Instance,
};

use super::{
	extensions::{is_expression::synthesize_is_expression, jsx::synthesize_jsx_root},
	object_literal::synthesize_object_literal,
};

pub(crate) fn synthesize_multiple_expression<T: crate::FSResolver>(
	expression: &mut MultipleExpression,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) -> TypeId {
	if let Some(lhs) = &mut expression.lhs {
		synthesize_multiple_expression(lhs, environment, checking_data, chain);
	}
	synthesize_expression(&mut expression.rhs, environment, checking_data, chain)
}

pub(crate) fn synthesize_expression<T: crate::FSResolver>(
	expression: &mut Expression,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) -> TypeId {
	let instance: Instance = match expression {
		Expression::Comment(..) => unreachable!("Should have skipped this higher up"),
		Expression::StringLiteral(value, ..) => {
			return checking_data.types.new_constant_type(Constant::String(value.clone()))
		}
		Expression::NumberLiteral(value, ..) => {
			let not_nan = f64::from(value.clone()).try_into().unwrap();
			return checking_data.types.new_constant_type(Constant::Number(not_nan));
		}
		Expression::BooleanLiteral(value, ..) => {
			return checking_data.types.new_constant_type(Constant::Boolean(*value))
		}
		Expression::ArrayLiteral(elements, _, _) => {
			fn synthesize_array_item<T: crate::FSResolver>(
				idx: usize,
				element: &mut SpreadExpression,
				environment: &mut Environment,
				checking_data: &mut CheckingData<T>,
				chain: &mut Annex<Chain>,
			) -> (TypeId, TypeId) {
				match element {
					SpreadExpression::NonSpread(element) => {
						let expression_type =
							synthesize_expression(element, environment, checking_data, chain);
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
									at: expr.get_position().into_owned(),
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

			for (idx, value) in elements.iter_mut().enumerate() {
				let (key, value) =
					synthesize_array_item(idx, value, environment, checking_data, chain);

				basis.append(environment, key, value);
			}
			let len = checking_data
				.types
				.new_constant_type(Constant::Number((elements.len() as f64).try_into().unwrap()));

			basis.append(environment, TypeId::LENGTH_AS_STRING, len);

			Instance::RValue(basis.build_object())
		}
		Expression::ObjectLiteral(object_literal) => {
			let synthesize_object_literal =
				synthesize_object_literal(object_literal, checking_data, environment, chain);

			Instance::RValue(synthesize_object_literal)
		}
		Expression::TemplateLiteral(TemplateLiteral { tag, parts, position, expression_id }) => {
			checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
				thing: "template literals",
				at: position.clone(),
			});
			Instance::RValue(TypeId::ERROR_TYPE)
		}
		Expression::BinaryOperation { lhs, operator, rhs, .. } => {
			let lhs_instance = synthesize_expression(&mut *lhs, environment, checking_data, chain);
			let rhs_instance = synthesize_expression(&mut *rhs, environment, checking_data, chain);

			if lhs_instance == TypeId::ERROR_TYPE || rhs_instance == TypeId::ERROR_TYPE {
				return TypeId::ERROR_TYPE;
			}

			let operator = super::parser_binary_operator_to_others(*operator);

			let evaluate_binary_operator = evaluate_binary_operator(
				operator,
				lhs_instance,
				rhs_instance,
				environment,
				checking_data.settings.strict_casts,
				&mut checking_data.types,
			);

			match evaluate_binary_operator {
				Ok(result) => Instance::RValue(result),
				Err(err) => {
					let union = lhs.get_position().union(&rhs.get_position());
					checking_data.diagnostics_container.add_error(
						TypeCheckError::InvalidMathematicalOperation(
							TypeStringRepresentation::from_type_id(
								lhs_instance,
								&environment.into_general_environment(),
								&checking_data.types,
								checking_data.settings.debug_types,
							),
							TypeStringRepresentation::from_type_id(
								rhs_instance,
								&environment.into_general_environment(),
								&checking_data.types,
								checking_data.settings.debug_types,
							),
							operator,
							union,
						),
					);
					return TypeId::ERROR_TYPE;
				}
			}
		}
		Expression::UnaryOperation { operand, operator, position, id } => {
			let operand_type =
				synthesize_expression(&mut *operand, environment, checking_data, chain);

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
		Expression::Assignment { lhs, rhs, id } => match lhs {
			LHSOfAssignment::ObjectDestructuring(_, _, _) => todo!(),
			LHSOfAssignment::ArrayDestructuring(_, _, _) => todo!(),
			LHSOfAssignment::VariableOrPropertyAccess(lhs) => {
				let position = lhs.get_position().union(&rhs.get_position());
				todo!();
				// let behavior = BinaryAssignment { operator: None, rhs };
				// let result = synthesize_assignment(
				// 	lhs,
				// 	behavior,
				// 	environment,
				// 	checking_data,
				// 	chain,
				// 	position,
				// );
				// Instance::RValue(result)
			}
		},
		Expression::BinaryAssignmentOperation { lhs, operator, rhs, id } => {
			todo!();
			// let position = lhs.get_position().union(&rhs.get_position());
			// let behavior = BinaryAssignment { operator: Some(*operator), rhs };
			// crate::utils::notify!("Synthesizing a binary assign op");
			// Instance::RValue(synthesize_assignment(
			// 	lhs,
			// 	behavior,
			// 	environment,
			// 	checking_data,
			// 	chain,
			// 	position,
			// ))
		}
		Expression::UnaryPrefixAssignmentOperation { operator, operand, position, id } => {
			match operator {
				UnaryPrefixAssignmentOperator::Invert => todo!(),
				UnaryPrefixAssignmentOperator::IncrementOrDecrement(_) => todo!(),
			}
			// let behavior = PrefixUnaryAssignment { operator: *operator };

			// Instance::RValue(synthesize_assignment(
			// 	operand,
			// 	behavior,
			// 	environment,
			// 	checking_data,
			// 	chain,
			// 	position.clone(),
			// ))
		}
		Expression::UnaryPostfixAssignmentOperation { operand, operator, position, id } => {
			match operator {
				_ => todo!(),
			}
			// let behavior = PostfixUnaryAssignment { operator: *operator };

			// Instance::RValue(synthesize_assignment(
			// 	operand,
			// 	behavior,
			// 	environment,
			// 	checking_data,
			// 	chain,
			// 	position.clone(),
			// ))
		}
		Expression::VariableReference(name, position, _) => {
			let get_variable_or_alternatives =
				environment.get_variable_or_alternatives(name.as_str(), &mut checking_data.types);

			match get_variable_or_alternatives {
				Ok(variable) => Instance::LValue(variable),
				Err(err) => {
					checking_data.diagnostics_container.add_error(
						TypeCheckError::CouldNotFindVariable {
							variable: err.name,
							possibles: err.possibles,
							position: position.clone(),
						},
					);
					Instance::RValue(TypeId::ERROR_TYPE)
				}
			}
		}
		Expression::PropertyAccess { parent, position, property, .. } => {
			let parent_type = synthesize_expression(parent, environment, checking_data, chain);
			let property = if let parser::PropertyReference::Standard(name) = property {
				checking_data.types.new_constant_type(Constant::String(name.clone()))
			} else {
				todo!()
			};

			let get_property = environment.get_property(parent_type, property, checking_data, None);

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
								&environment.into_general_environment(),
								&checking_data.types,
								checking_data.settings.debug_types,
							),
							ty: TypeStringRepresentation::from_type_id(
								parent_type,
								&environment.into_general_environment(),
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
		Expression::SuperExpression(reference, position, _) => match reference {
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
				// let mut chain = Annex::new(&mut chain);

				// let ty = call_function(
				// 	super_constructor,
				// 	&mut None,
				// 	Some(arguments),
				// 	environment,
				// 	checking_data,
				// 	&mut chain,
				// 	position,
				// 	CalledWithNew::SpecialSuperCall { on: this_constructor },
				// );

				// TODO explain
				// synthesize_class_fields(fields, environment, checking_data, &mut chain);

				// Instance::RValue(ty)
			}
			SuperReference::PropertyAccess { property } => todo!(),
			SuperReference::Index { indexer } => todo!(),
		},
		Expression::NewTarget(..) => todo!(),
		Expression::FunctionCall { function, type_arguments, arguments, position, .. } => {
			let on = synthesize_expression(&mut *function, environment, checking_data, chain);

			let result = call_function(
				on,
				type_arguments,
				Some(arguments),
				environment,
				checking_data,
				chain,
				position,
				CalledWithNew::None,
			);
			Instance::RValue(result)
		}
		Expression::ConstructorCall {
			constructor,
			type_arguments,
			arguments,
			position,
			expression_id,
		} => {
			let on = synthesize_expression(&mut *constructor, environment, checking_data, chain);
			let called_with_new = CalledWithNew::New { import_new: on };
			let result = call_function(
				on,
				type_arguments,
				arguments.as_mut(),
				environment,
				checking_data,
				chain,
				position,
				called_with_new,
			);
			Instance::RValue(result)
		}
		Expression::Index { indexee, indexer, expression_id, position, .. } => {
			let indexee = synthesize_expression(indexee, environment, checking_data, chain);
			let indexer =
				synthesize_multiple_expression(indexer, environment, checking_data, chain);

			let get_property = environment.get_property(indexee, indexer, checking_data, None);

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
								&environment.into_general_environment(),
								&checking_data.types,
								checking_data.settings.debug_types,
							),
							ty: TypeStringRepresentation::from_type_id(
								indexee,
								&environment.into_general_environment(),
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
			let _condition = synthesize_expression(condition, environment, checking_data, chain);

			let truthy_instance =
				synthesize_expression(truthy_result, environment, checking_data, chain);
			let falsy_instance =
				synthesize_expression(falsy_result, environment, checking_data, chain);

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
		Expression::ArrowFunction(_) => {
			todo!()
		}
		Expression::Null(_, _) => return TypeId::NULL_TYPE,
		Expression::JSXRoot(jsx_root) => {
			Instance::RValue(synthesize_jsx_root(jsx_root, environment, checking_data, chain))
		}
		Expression::PostfixComment(expression, _comment, _, _)
		| Expression::PrefixComment(_comment, expression, _, _) => {
			Instance::RValue(synthesize_expression(expression, environment, checking_data, chain))
		}
		Expression::ParenthesizedExpression(inner_expression, _, _) => Instance::RValue(
			synthesize_multiple_expression(inner_expression, environment, checking_data, chain),
		),
		Expression::ClassExpression(_, _) => {
			todo!()
		}
		Expression::Cursor { cursor_id, position, expression_id } => todo!(),
		Expression::SpecialOperators(operator, position, _) => match operator {
			SpecialOperators::AsExpression { value, type_annotation, .. } => {
				checking_data
					.diagnostics_container
					.add_warning(TypeCheckWarning::IgnoringAsExpression(position.clone()));

				return synthesize_expression(value, environment, checking_data, chain);
			}
			SpecialOperators::IsExpression { value, is_keyword, type_annotation } => todo!(),
			SpecialOperators::SatisfiesExpression { value, type_annotation, .. } => {
				let value = synthesize_expression(value, environment, checking_data, chain);
				let satisfying = environment.get_type_handle_errors(type_annotation, checking_data);
				let mut basic_subtyping =
					BasicEquality { add_property_restrictions: false, position: position.clone() };
				let result = type_is_subtype(
					satisfying,
					value,
					None,
					&mut basic_subtyping,
					environment,
					&checking_data.types,
				);
				if let SubTypeResult::IsNotSubType(not_subtype) = result {
					checking_data.diagnostics_container.add_error(TypeCheckError::NotSatisfied {
						at: position.clone(),
						expected: TypeStringRepresentation::from_type_id(
							satisfying,
							&environment.into_general_environment(),
							&checking_data.types,
							checking_data.settings.debug_types,
						),
						found: TypeStringRepresentation::from_type_id(
							value,
							&environment.into_general_environment(),
							&checking_data.types,
							checking_data.settings.debug_types,
						),
					});
				}
				Instance::RValue(value)
			}
		},
		Expression::DynamicImport { path, position, expression_id } => todo!(),
		Expression::IsExpression(is_expr) => {
			Instance::RValue(synthesize_is_expression(is_expr, environment, checking_data, chain))
		}
		Expression::RegexLiteral { pattern, flags, position, id } => {
			todo!()
		}
	};

	if let Some(id) = expression.get_expression_id() {
		checking_data.type_mappings.expressions_to_instances.insert(id, instance.clone());
	}

	instance.get_value()
}

/// Generic for functions + constructor calls
///
/// TODO error with function_type_id should be handled earlier
fn call_function<T: crate::FSResolver>(
	function_type_id: TypeId,
	type_arguments: &mut Option<Vec<parser::TypeReference>>,
	mut arguments: Option<&mut Vec<SpreadExpression>>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
	position: &parser::Span,
	called_with_new: CalledWithNew,
) -> TypeId {
	let generic_type_arguments = if let Some(type_arguments) = type_arguments {
		Some(
			type_arguments
				.iter()
				.map(|generic_type_argument| {
					(
						generic_type_argument.get_position().into_owned(),
						environment.get_type_handle_errors(generic_type_argument, checking_data),
					)
				})
				.collect::<Vec<_>>(),
		)
	} else {
		None
	};

	let synthesized_arguments = arguments
		.as_mut()
		.map(|arguments| synthesize_arguments(arguments, environment, checking_data, chain))
		.unwrap_or_default();

	if function_type_id == TypeId::ERROR_TYPE
		|| synthesized_arguments.iter().any(|arg| match arg {
			SynthesizedArgument::NonSpread { ty, .. } => *ty == TypeId::ERROR_TYPE,
		}) {
		return TypeId::ERROR_TYPE;
	}

	crate::types::calling::call_type_merge_errors(
		function_type_id,
		synthesized_arguments,
		None,
		generic_type_arguments,
		environment,
		checking_data,
		called_with_new,
		position.clone(),
	)
}

fn synthesize_arguments<T: crate::FSResolver>(
	arguments: &mut Vec<SpreadExpression>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) -> Vec<SynthesizedArgument> {
	arguments
		.iter_mut()
		.flat_map(|argument| match argument {
			SpreadExpression::Spread(expr, _) => {
				todo!()
				// Some(SynthesizedFunctionArgument::Spread(synthesize_expression(
				//     expr,
				//     environment,
				//     checking_data,
				//     chain,
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
				let ty = synthesize_expression(expr, environment, checking_data, chain);
				Some(SynthesizedArgument::NonSpread {
					ty,
					position: expr.get_position().into_owned(),
				})
			}
			SpreadExpression::Empty => None,
		})
		.collect::<Vec<SynthesizedArgument>>()
}

pub(crate) fn synthesize_class_fields<T: crate::FSResolver>(
	fields: Vec<(TypeId, Expression)>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) {
	let this = environment.get_value_of_this(&mut checking_data.types);
	for (under, mut expression) in fields {
		let new = synthesize_expression(&mut expression, environment, checking_data, chain);
		environment.context_type.events.push(Event::Setter {
			on: this,
			new,
			under,
			reflects_dependency: None,
			initialization: true,
		});
		todo!()
		// environment.new_property(this, under, new, false);
	}
}
