//! How type parameters are resolved

use crate::{
	context::Environment,
	subtyping::{type_is_subtype, SubTypeResult},
	types::{cast_as_boolean, Constructor, PolyNature, Type, TypeStore},
	TypeId,
};

pub use crate::types::operations::{evaluate_binary_operator, evaluate_unary_operator};

use super::TypeArguments;

pub(crate) fn specialize(
	id: TypeId,
	arguments: &mut TypeArguments,
	environment: &mut Environment,
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
				let on = crate::types::printing::print_type(
					id,
					types,
					&environment.into_general_context(),
					true,
				);
				crate::utils::notify!("Could not find argument for {}", on);
				id
			}
		}
		// TODO environment should hold what dependents on what to reduce excess here
		Type::Constructor(constructor) => match constructor.clone() {
			Constructor::BinaryOperator { operator, lhs, rhs, .. } => {
				let lhs = specialize(lhs, arguments, environment, types);
				let rhs = specialize(rhs, arguments, environment, types);

				let evaluate_binary_operator = evaluate_binary_operator(
					operator,
					lhs,
					rhs,
					environment,
					// Restrictions should have been made ahead of time
					false,
					types,
				);

				// match e

				// crate::utils::notify!(
				// 	"Specialized returned {}",
				// 	environment.debug_type(value, types)
				// );

				// .expect("restriction about binary operator failed")
				evaluate_binary_operator.unwrap_or(TypeId::ERROR_TYPE)
			}
			Constructor::UnaryOperator { operand, operator, .. } => {
				evaluate_unary_operator(
					operator,
					operand,
					environment,
					// Restrictions should have been made ahead of time
					false,
					types,
				)
				.unwrap()
			}
			Constructor::ConditionalTernary { on, true_res, false_res, result_union } => {
				crate::utils::notify!(
					"before {:?} {:?} {:?}",
					crate::types::printing::print_type(
						on,
						types,
						&environment.into_general_context(),
						true
					),
					crate::types::printing::print_type(
						true_res,
						types,
						&environment.into_general_context(),
						true
					),
					crate::types::printing::print_type(
						false_res,
						types,
						&environment.into_general_context(),
						true
					)
				);
				let on = specialize(on, arguments, environment, types);
				let true_res = specialize(true_res, arguments, environment, types);
				let false_res = specialize(false_res, arguments, environment, types);

				crate::utils::notify!(
					"after {:?} {:?} {:?}",
					crate::types::printing::print_type(
						on,
						types,
						&environment.into_general_context(),
						true
					),
					crate::types::printing::print_type(
						true_res,
						types,
						&environment.into_general_context(),
						true
					),
					crate::types::printing::print_type(
						false_res,
						types,
						&environment.into_general_context(),
						true
					)
				);

				// TODO falsy
				if let Type::Constant(cst) = types.get_type_by_id(on) {
					let result = cast_as_boolean(cst, false).unwrap();
					if result {
						true_res
					} else {
						false_res
					}
				} else {
					// TODO result_union
					let ty =
						Constructor::ConditionalTernary { on, true_res, false_res, result_union };

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
			Constructor::RelationOperator { lhs, operator, rhs } => todo!(),
			Constructor::LogicalOperator { lhs, operator, rhs } => todo!(),
			Constructor::TypeOperator(..) => todo!(),
			Constructor::TypeRelationOperator(op) => match op {
				crate::types::TypeRelationOperator::Extends { ty, extends } => {
					let ty = specialize(ty, arguments, environment, types);
					let extends = specialize(extends, arguments, environment, types);

					// TODO special behavior that doesn't have errors...
					let result = type_is_subtype(
						extends,
						ty,
						None,
						&mut super::super::subtyping::BasicEquality {
							add_property_restrictions: false,
							position: source_map::Span::NULL_SPAN,
						},
						environment,
						types,
					);

					// crate::utils::notify!(
					// 	"Extends result {:?} extends={}, ty={},",
					// 	result,
					// 	types.debug_type(extends),
					// 	types.debug_type(ty)
					// );

					// TODO what about unknown...

					let does_extend = matches!(result, SubTypeResult::IsSubType);

					if does_extend {
						TypeId::TRUE
					} else {
						TypeId::FALSE
					}
				}
			},
		},
	}
}
