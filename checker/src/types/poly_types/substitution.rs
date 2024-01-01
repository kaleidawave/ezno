//! How type parameters are resolved

use crate::{
	behavior::{
		functions::ThisValue,
		operations::{
			evaluate_equality_inequality_operation, evaluate_mathematical_operation,
			evaluate_pure_unary_operator,
		},
	},
	types::{
		get_constraint, is_type_truthy_falsy, Constructor, PolyNature, StructureGenerics, Type,
		TypeStore,
	},
	Decidable, Environment, TypeId,
};

use super::generic_type_arguments::{StructureGenericArguments, TypeArgumentStore};

pub(crate) fn substitute(
	id: TypeId,
	arguments: &mut impl TypeArgumentStore,
	// TODO temp
	environment: &mut Environment,
	types: &mut TypeStore,
) -> TypeId {
	if let Some(value) = arguments.get_argument(id) {
		return value;
	}

	let ty = types.get_type_by_id(id);

	match ty {
		Type::Object(..) => {
			// TODO only sometimes
			curry_arguments(arguments, types, id)
		}
		Type::Function(f, t) => {
			// Also sub the this type
			let id = if let ThisValue::Passed(p) = t {
				let function_id = *f;
				let passed = ThisValue::Passed(substitute(*p, arguments, environment, types));
				types.register_type(Type::Function(function_id, passed))
			} else {
				id
			};
			curry_arguments(arguments, types, id)
		}
		Type::FunctionReference(_f, _t) => curry_arguments(arguments, types, id),
		Type::And(lhs, rhs) => {
			let rhs = *rhs;
			let lhs = substitute(*lhs, arguments, environment, types);
			let rhs = substitute(rhs, arguments, environment, types);
			types.register_type(Type::And(lhs, rhs))
		}
		Type::Or(lhs, rhs) => {
			let rhs = *rhs;
			let lhs = substitute(*lhs, arguments, environment, types);
			let rhs = substitute(rhs, arguments, environment, types);
			types.register_type(Type::Or(lhs, rhs))
		}
		Type::Constant(_) | Type::AliasTo { .. } | Type::Interface { .. } => id,
		Type::RootPolyType(nature) => {
			if let PolyNature::Open(_) = nature {
				id
			} else if let PolyNature::Generic { .. } = nature {
				crate::utils::notify!("Could not find argument for explicit generic");
				id
			} else {
				// Other root poly types cases handled by the early return
				let on = crate::types::printing::print_type(
					id,
					types,
					&environment.as_general_context(),
					true,
				);
				crate::utils::notify!("Could not find argument for {}", on);
				TypeId::ERROR_TYPE
			}
		}

		// TODO environment should hold what dependents on what to reduce excess here
		Type::Constructor(constructor) => match constructor.clone() {
			Constructor::BinaryOperator { lhs, operator, rhs, .. } => {
				let lhs = substitute(lhs, arguments, environment, types);
				let rhs = substitute(rhs, arguments, environment, types);

				match evaluate_mathematical_operation(lhs, operator, rhs, types, false) {
					Ok(result) => result,
					Err(()) => {
						unreachable!(
							"Cannot {lhs:?} {operator:?} {rhs:?} (restriction or something failed)"
						);
					}
				}
			}
			Constructor::UnaryOperator { operand, operator, .. } => {
				match evaluate_pure_unary_operator(
					operator, operand, types,
					// Restrictions should have been made ahead of time
					false,
				) {
					Ok(result) => result,
					Err(()) => {
						unreachable!(
							"Cannot {operand:?} {operator:?} (restriction or something failed)"
						);
					}
				}
			}
			Constructor::ConditionalResult {
				condition,
				truthy_result,
				else_result,
				result_union: _,
			} => {
				let condition = substitute(condition, arguments, environment, types);

				// crate::utils::notify!(
				// 	"after on={} true={} false={}",
				// 	crate::types::printing::print_type(
				// 		condition,
				// 		types,
				// 		&environment.as_general_context(),
				// 		true
				// 	),
				// 	crate::types::printing::print_type(
				// 		truthy_result,
				// 		types,
				// 		&environment.as_general_context(),
				// 		true
				// 	),
				// 	crate::types::printing::print_type(
				// 		else_result,
				// 		types,
				// 		&environment.as_general_context(),
				// 		true
				// 	)
				// );

				if let Decidable::Known(result) = is_type_truthy_falsy(condition, types) {
					if result {
						substitute(truthy_result, arguments, environment, types)
					} else {
						substitute(else_result, arguments, environment, types)
					}
				} else {
					crate::utils::notify!("{:?} is undecidable", condition);
					let truthy_result = substitute(truthy_result, arguments, environment, types);
					let else_result = substitute(else_result, arguments, environment, types);
					// TODO result_union
					let ty = Constructor::ConditionalResult {
						condition,
						truthy_result,
						else_result,
						result_union: types.new_or_type(truthy_result, else_result),
					};

					types.register_type(Type::Constructor(ty))
				}
			}
			Constructor::Property { on, under, result, bind_this } => {
				let id = get_constraint(on, types).unwrap_or(on);

				if let Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
					on: TypeId::ARRAY_TYPE,
					arguments,
				})) = types.get_type_by_id(id)
				{
					// Try get the constant
					if under.as_number().is_some() {
						crate::utils::notify!("Temp array index property get");
						let value = arguments.get_argument(TypeId::T_TYPE).unwrap();
						types.new_or_type(value, TypeId::UNDEFINED_TYPE)
					} else {
						let mut structure_generic_arguments = arguments.clone();
						let new_result = substitute(
							result,
							// TODO
							&mut structure_generic_arguments,
							environment,
							types,
						);
						crate::utils::notify!(
							"Specialising the constraint {:?} to {:?} using {:?} (which is strange)",
							result,
							new_result,
							structure_generic_arguments
						);
						types.register_type(Type::Constructor(Constructor::Property {
							on,
							under,
							result: new_result,
							bind_this,
						}))
					}
				} else {
					todo!(
						"Constructor::Property ({:?}[{:?}]) should be covered by events",
						on,
						under
					);
				}
			}
			Constructor::Image { .. } => {
				todo!("Constructor::Image {id:?} should be covered by events");
				// id

				// let on = substitute(on, arguments, environment);

				// crate::utils::notify!("Substituted {}", environment.debug_type(on));

				// let func_arguments = with
				// 	.into_iter()
				// 	.map(|argument| match argument {
				// 		synthesisedArgument::NonSpread { ty, pos } => {
				// 			let ty = substitute(*ty, arguments, environment);
				// 			synthesisedArgument::NonSpread { ty, pos: pos.clone() }
				// 		}
				// 	})
				// 	.collect::<Vec<_>>();

				// let FunctionCallResult { returned_type, warnings } =
				// 	call_type(on, func_arguments, None, None, environment, checking_data)
				// 		.expect("Inferred constraints and checking failed");

				// crate::utils::notify!("TODO getting a property not substituted during calling");

				// let on = substitute(on, arguments, environment, checking_data);
				// let property = substitute(property, arguments, environment, checking_data);

				// environment
				// 	.get_property(on, property, checking_data, None)
				// 	.expect("Inferred constraints and checking failed for a property")
			}
			Constructor::StructureGenerics(StructureGenerics {
				on,
				arguments: structure_arguments,
			}) => {
				let type_arguments = structure_arguments
					.type_arguments
					.into_iter()
					.map(|(lhs, (with, pos))| {
						(lhs, (substitute(with, arguments, environment, types), pos))
					})
					.collect();

				types.register_type(Type::Constructor(Constructor::StructureGenerics(
					StructureGenerics {
						on,
						arguments: StructureGenericArguments {
							type_arguments,
							closures: structure_arguments.closures,
						},
					},
				)))
			}

			// Constructor::PrototypeOf(prototype) => {
			// 	let prototype = substitute(prototype, arguments, environment, types);
			// 	if let Type::AliasTo { to, .. } = types.get_type_by_id(prototype) {
			// 		crate::utils::notify!(
			// 			"TODO temp might have to do more here when specialising a prototype"
			// 		);
			// 		*to
			// 	} else {
			// 		todo!()
			// 	}
			// }
			Constructor::CanonicalRelationOperator { lhs, operator, rhs } => {
				let operator = match operator {
					crate::behavior::operations::CanonicalEqualityAndInequality::StrictEqual => {
						crate::behavior::operations::EqualityAndInequality::StrictEqual
					}
					crate::behavior::operations::CanonicalEqualityAndInequality::LessThan => {
						crate::behavior::operations::EqualityAndInequality::LessThan
					}
				};
				let lhs = substitute(lhs, arguments, environment, types);
				let rhs = substitute(rhs, arguments, environment, types);

				match evaluate_equality_inequality_operation(lhs, &operator, rhs, types, false) {
					Ok(result) => result,
					Err(()) => {
						unreachable!(
							"Cannot {lhs:?} {operator:?} {rhs:?} (restriction or something failed)"
						);
					}
				}
			}
			Constructor::TypeOperator(..) => todo!(),
			Constructor::TypeRelationOperator(op) => match op {
				crate::types::TypeRelationOperator::Extends { ty, extends } => {
					let _ty = substitute(ty, arguments, environment, types);
					let _extends = substitute(extends, arguments, environment, types);

					todo!();
					// TODO special behavior that doesn't have errors...
					// let result = type_is_subtype(
					// 	extends,
					// 	ty,
					// 	None,
					// 	&mut super::super::subtyping::BasicEquality {
					// 		add_property_restrictions: false,
					// 		position: source_map::Span::NULL_SPAN,
					// 	},
					// 	environment,
					// 	types,
					// );

					// crate::utils::notify!(
					// 	"Extends result {:?} extends={}, ty={},",
					// 	result,
					// 	types.debug_type(extends),
					// 	types.debug_type(ty)
					// );

					// TODO what about unknown...

					// let does_extend = matches!(result, SubTypeResult::IsSubType);

					// if does_extend {
					// 	TypeId::TRUE
					// } else {
					// 	TypeId::FALSE
					// }
				}
			},
		},
		Type::SpecialObject(_) => todo!(),
	}
}

pub(crate) fn curry_arguments(
	arguments: &impl TypeArgumentStore,
	types: &mut TypeStore,
	id: TypeId,
) -> TypeId {
	if arguments.is_empty() {
		id
	} else {
		crate::utils::notify!("Storing arguments onto object");
		// TODO only carry arguments that are used
		let arguments = arguments.to_structural_generic_arguments();

		types.register_type(Type::Constructor(Constructor::StructureGenerics(
			crate::types::StructureGenerics { on: id, arguments },
		)))
	}
}
