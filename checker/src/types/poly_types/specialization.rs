//! How type parameters are resolved

use crate::{
	behavior::operations::{
		evaluate_equality_inequality_operation, evaluate_mathematical_operation,
	},
	types::{is_type_truthy_falsy, Constructor, PolyNature, Type, TypeStore},
	GeneralContext, TruthyFalsy, TypeId,
};

use super::TypeArguments;

pub(crate) fn specialize(
	id: TypeId,
	arguments: &mut TypeArguments,
	environment: &GeneralContext,
	types: &mut TypeStore,
) -> TypeId {
	if let Some(value) = arguments.get_arg(id) {
		return value;
	}

	let ty = types.get_type_by_id(id);

	match ty {
		Type::Constant(_) | Type::Object(..) => id,
		// TODO temp
		Type::Function(_, _) => id,
		Type::AliasTo { .. } | Type::And(_, _) | Type::Or(_, _) | Type::NamedRooted { .. } => id,
		Type::RootPolyType(nature) => {
			if let PolyNature::Open(_) = nature {
				id
			} else {
				// Other root poly types cases handled by the early return
				let on = crate::types::printing::print_type(id, types, environment, true);
				crate::utils::notify!("Could not find argument for {}", on);
				id
			}
		}
		// TODO environment should hold what dependents on what to reduce excess here
		Type::Constructor(constructor) => match constructor.clone() {
			Constructor::BinaryOperator { lhs, operator, rhs, .. } => {
				let lhs = specialize(lhs, arguments, environment, types);
				let rhs = specialize(rhs, arguments, environment, types);

				evaluate_mathematical_operation(lhs, operator, rhs, types)
					.expect("restriction about binary operator failed")
			}
			Constructor::UnaryOperator { operand, operator, .. } => {
				todo!()
				// evaluate_unary_operator(
				// 	operator,
				// 	operand,
				// 	environment,
				// 	// Restrictions should have been made ahead of time
				// 	false,
				// 	types,
				// )
				// .unwrap()
			}
			Constructor::ConditionalResult {
				condition,
				truthy_result,
				else_result,
				result_union,
			} => {
				// crate::utils::notify!(
				// 	"before on={} true={} false={}",
				// 	crate::types::printing::print_type(
				// 		on,
				// 		types,
				// 		&environment.into_general_context(),
				// 		true
				// 	),
				// 	crate::types::printing::print_type(
				// 		true_res,
				// 		types,
				// 		&environment.into_general_context(),
				// 		true
				// 	),
				// 	crate::types::printing::print_type(
				// 		false_res,
				// 		types,
				// 		&environment.into_general_context(),
				// 		true
				// 	)
				// );

				let condition = specialize(condition, arguments, environment, types);

				// crate::utils::notify!(
				// 	"after on={} true={} false={}",
				// 	crate::types::printing::print_type(
				// 		on,
				// 		types,
				// 		&environment.into_general_context(),
				// 		true
				// 	),
				// 	crate::types::printing::print_type(
				// 		true_result,
				// 		types,
				// 		&environment.into_general_context(),
				// 		true
				// 	),
				// 	crate::types::printing::print_type(
				// 		false_result,
				// 		types,
				// 		&environment.into_general_context(),
				// 		true
				// 	)
				// );

				if let TruthyFalsy::Decidable(result) = is_type_truthy_falsy(condition, types) {
					if result {
						specialize(truthy_result, arguments, environment, types)
					} else {
						specialize(else_result, arguments, environment, types)
					}
				} else {
					let truthy_result = specialize(truthy_result, arguments, environment, types);
					let else_result = specialize(else_result, arguments, environment, types);
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
			Constructor::Property { .. } | Constructor::FunctionResult { .. } => {
				unreachable!("this should have covered by event specialization");

				// let on = specialize(on, arguments, environment);

				// crate::utils::notify!("Specialized {}", environment.debug_type(on));

				// let func_arguments = with
				// 	.into_iter()
				// 	.map(|argument| match argument {
				// 		SynthesizedArgument::NonSpread { ty, pos } => {
				// 			let ty = specialize(*ty, arguments, environment);
				// 			SynthesizedArgument::NonSpread { ty, pos: pos.clone() }
				// 		}
				// 	})
				// 	.collect::<Vec<_>>();

				// let FunctionCallResult { returned_type, warnings } =
				// 	call_type(on, func_arguments, None, None, environment, checking_data)
				// 		.expect("Inferred constraints and checking failed");

				// crate::utils::notify!("TODO getting a property not specialized during calling");

				// let on = specialize(on, arguments, environment, checking_data);
				// let property = specialize(property, arguments, environment, checking_data);

				// environment
				// 	.get_property(on, property, checking_data, None)
				// 	.expect("Inferred constraints and checking failed for a property")
			}
			Constructor::StructureGenerics { on, with } => {
				let on = specialize(on, arguments, environment, types);
				let with = with
					.into_iter()
					.map(|(lhs, with)| (lhs, specialize(with, arguments, environment, types)))
					.collect();

				types.register_type(Type::Constructor(Constructor::StructureGenerics { on, with }))
			}

			// Constructor::PrototypeOf(prototype) => {
			// 	let prototype = specialize(prototype, arguments, environment, types);
			// 	if let Type::AliasTo { to, .. } = types.get_type_by_id(prototype) {
			// 		crate::utils::notify!(
			// 			"TODO temp might have to do more here when specializing a prototype"
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
				let lhs = specialize(lhs, arguments, environment, types);
				let rhs = specialize(rhs, arguments, environment, types);

				evaluate_equality_inequality_operation(lhs, operator, rhs, types)
					.expect("restriction about binary operator failed")
			}
			Constructor::TypeOperator(..) => todo!(),
			Constructor::TypeRelationOperator(op) => match op {
				crate::types::TypeRelationOperator::Extends { ty, extends } => {
					let ty = specialize(ty, arguments, environment, types);
					let extends = specialize(extends, arguments, environment, types);

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
	}
}
