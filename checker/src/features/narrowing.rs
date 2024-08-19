use crate::{
	context::InformationChain,
	types::{
		as_logical_and, as_logical_or, get_conditional, helpers::get_origin, Constant, Constructor,
		PolyNature, TypeOperator, TypeStore,
	},
	Map, Type, TypeId,
};

use super::operations::{CanonicalEqualityAndInequality, MathematicalAndBitwise, PureUnary};

pub fn narrow_based_on_expression_into_vec(
	condition: TypeId,
	negate: bool,
	information: &impl InformationChain,
	types: &mut TypeStore,
) -> Map<TypeId, TypeId> {
	let mut into = Default::default();
	narrow_based_on_expression(condition, negate, &mut into, information, types);
	into
}

pub fn narrow_based_on_expression(
	condition: TypeId,
	negate: bool,
	into: &mut Map<TypeId, TypeId>,
	information: &impl InformationChain,
	types: &mut TypeStore,
) {
	let r#type = types.get_type_by_id(condition);
	if let Type::Constructor(constructor) = r#type {
		match constructor {
			Constructor::CanonicalRelationOperator {
				lhs,
				operator: CanonicalEqualityAndInequality::StrictEqual,
				rhs,
			} => {
				if let Type::Constructor(Constructor::TypeOperator(TypeOperator::TypeOf(on))) =
					types.get_type_by_id(*lhs)
				{
					let from = get_origin(*on, types);
					if let Type::Constant(Constant::String(c)) = types.get_type_by_id(*rhs) {
						let narrowed_to = match c.as_str() {
							"number" => TypeId::NUMBER_TYPE,
							"string" => TypeId::STRING_TYPE,
							"boolean" => TypeId::BOOLEAN_TYPE,
							"function" => TypeId::FUNCTION_TYPE,
							"undefined" => TypeId::UNDEFINED_TYPE,
							"object" => TypeId::OBJECT_TYPE,
							rhs => {
								// TODO also never
								crate::utilities::notify!("typeof rhs={}", rhs);
								return;
							}
						};
						if negate {
							let mut result = Vec::new();
							build_union_from_filter(
								from,
								narrowed_to,
								&mut result,
								information,
								types,
							);
							let narrowed_to = types.new_or_type_from_iterator(result);
							let narrowed = types.new_narrowed(from, narrowed_to);
							into.insert(from, narrowed);
						} else {
							let narrowed = types.new_narrowed(from, narrowed_to);
							into.insert(from, narrowed);
						}
					} else {
						crate::utilities::notify!("Here?");
					}
				} else if let Type::Constructor(Constructor::BinaryOperator {
					lhs: operand,
					operator: MathematicalAndBitwise::Modulo,
					rhs: modulo,
				}) = types.get_type_by_id(*lhs)
				{
					if *rhs == TypeId::ZERO {
						crate::utilities::notify!("TODO only if sensible");

						let (from, modulo) = (*operand, *modulo);
						if negate {
							todo!()
						}
						let narrowed_to = types.new_intrinsic(
							&crate::types::intrinsics::Intrinsic::MultipleOf,
							modulo,
						);
						let narrowed = types.new_narrowed(from, narrowed_to);
						into.insert(from, narrowed);
					} else {
						crate::utilities::notify!("maybe subtract LHS");
					}
				} else {
					if let Type::RootPolyType(PolyNature::Parameter { .. }) =
						types.get_type_by_id(*lhs)
					{
						crate::utilities::notify!(
							"lhs is {:?} with {:?}",
							lhs,
							types.get_type_by_id(*rhs)
						);
					}

					let lhs = get_origin(*lhs, types);

					let result = if negate {
						// TODO wip
						let narrowed_to = if get_conditional(lhs, types).is_some() {
							let filter = *rhs;
							let mut result = Vec::new();
							build_union_from_filter(lhs, filter, &mut result, information, types);
							crate::utilities::notify!("Here {:?} {:?}", (filter, lhs), result);

							let narrowed_to = types.new_or_type_from_iterator(result);
							types.new_narrowed(lhs, narrowed_to)
						} else {
							types.new_intrinsic(&crate::types::intrinsics::Intrinsic::Not, *rhs)
						};
						types.new_narrowed(lhs, narrowed_to)
					} else {
						*rhs
					};

					// TODO reflexive ?
					into.insert(lhs, result);
				}
			}
			// TODO instance of
			// Constructor::TypeOperator(TypeOperator::TypeOf(on)) => {
			// }
			Constructor::CanonicalRelationOperator {
				lhs,
				operator: CanonicalEqualityAndInequality::LessThan,
				rhs,
			} => {
				let (lhs, rhs) = (*lhs, *rhs);
				if negate {
					return;
				}
				if types.get_type_by_id(lhs).is_dependent() {
					let narrowed_to =
						types.new_intrinsic(&crate::types::intrinsics::Intrinsic::LessThan, rhs);
					let narrowed = types.new_narrowed(lhs, narrowed_to);
					into.insert(lhs, narrowed);
				} else if types.get_type_by_id(rhs).is_dependent() {
					let narrowed_to =
						types.new_intrinsic(&crate::types::intrinsics::Intrinsic::GreaterThan, lhs);
					let narrowed = types.new_narrowed(rhs, narrowed_to);
					into.insert(rhs, narrowed);
				}
			}
			Constructor::UnaryOperator { operator: PureUnary::LogicalNot, operand } => {
				narrow_based_on_expression(*operand, !negate, into, information, types);
			}
			Constructor::TypeOperator(TypeOperator::IsPrototype { lhs, rhs_prototype }) => {
				let (lhs, rhs_prototype) = (*lhs, *rhs_prototype);
				let constraint = crate::types::get_constraint(lhs, types).unwrap_or(lhs);
				// TODO want a mix of two
				let narrowed_to = if constraint == TypeId::ANY_TYPE {
					prototype_narrowing::generate_item(rhs_prototype, types)
				} else {
					let mut items = Vec::new();
					prototype_narrowing::pick_items_with_protoypes(
						lhs,
						rhs_prototype,
						negate,
						&mut items,
						information,
						types,
					);
					types.new_or_type_from_iterator(items)
				};
				let narrowed = types.new_narrowed(lhs, narrowed_to);
				into.insert(lhs, narrowed);
			}
			Constructor::TypeOperator(TypeOperator::HasProperty(on, _key)) => {
				let lhs = *on;
				let constraint = crate::types::get_constraint(lhs, types).unwrap_or(lhs);
				// TODO want a mix of two
				let _narrowed_to = if constraint == TypeId::ANY_TYPE {
					crate::utilities::notify!("TODO");
				} else {
				};
				// let narrowed = types.new_narrowed(lhs, narrowed_to);
				// into.insert(lhs, narrowed);
			}
			constructor => {
				if let Some((lhs, rhs)) = as_logical_and(constructor, types) {
					// TODO what about if A and B
					crate::utilities::notify!("Here AND");
					narrow_based_on_expression(lhs, negate, into, information, types);
					narrow_based_on_expression(rhs, negate, into, information, types);
				} else if let Some((lhs, rhs)) = as_logical_or(constructor, types) {
					crate::utilities::notify!("Here OR");
					let lhs_requests =
						narrow_based_on_expression_into_vec(lhs, negate, information, types);
					let rhs_requests =
						narrow_based_on_expression_into_vec(rhs, negate, information, types);

					// crate::utilities::notify!("Here {:?} {:?}", lhs_requests.0, rhs_requests.0);
					// {
					// 	for (on, lhs_request) in lhs_requests.iter() {
					// 		crate::utilities::notify!(
					// 			"on={}, r={}",
					// 			crate::types::printing::print_type(*on, types, information, true),
					// 			crate::types::printing::print_type(
					// 				*lhs_request,
					// 				types,
					// 				information,
					// 				true
					// 			)
					// 		);
					// 	}
					// 	for (on, rhs_request) in rhs_requests.iter() {
					// 		crate::utilities::notify!(
					// 			"on={}, r={}",
					// 			crate::types::printing::print_type(*on, types, information, true),
					// 			crate::types::printing::print_type(
					// 				*rhs_request,
					// 				types,
					// 				information,
					// 				true
					// 			)
					// 		);
					// 	}
					// }

					for (on, lhs_request) in lhs_requests {
						if let Some(rhs_request) = rhs_requests.get(&on) {
							// TODO
							// let narrowed = types.new_narrowed(rhs, narrowed_to);
							into.insert(on, types.new_or_type(lhs_request, *rhs_request));
						} else {
							// Only when we have two results is it useful
						}
					}
				} else {
					crate::utilities::notify!("Here?, {:?}", constructor);
				}
			}
		}
	} else if let Type::RootPolyType(rpt) = r#type {
		if rpt.get_constraint() == TypeId::BOOLEAN_TYPE {
			let result = if negate { TypeId::FALSE } else { TypeId::TRUE };
			into.insert(condition, result);
		} else {
			crate::utilities::notify!("Set, {:?} as truthy", r#type);
		}
	}
}

/// 'string | number | boolean', filter=boolean => 'string | number'
///
/// TODO: - negation, property, combine with equality
#[allow(clippy::used_underscore_binding)]
fn build_union_from_filter(
	on: TypeId,
	filter: TypeId,
	found: &mut Vec<TypeId>,
	_information: &impl InformationChain,
	types: &TypeStore,
) {
	if let Some((_condition, lhs, rhs)) = get_conditional(on, types) {
		crate::utilities::notify!("{:?}, filter={:?}", (lhs, rhs), filter);
		build_union_from_filter(lhs, filter, found, _information, types);
		build_union_from_filter(rhs, filter, found, _information, types);
	} else if let Some(constraint) = crate::types::get_constraint(on, types) {
		build_union_from_filter(constraint, filter, found, _information, types);
	} else if filter != on {
		found.push(on);
	}
}

#[allow(clippy::used_underscore_binding)]
pub(crate) fn build_union_from_filter_slice(
	on: TypeId,
	filter: &[TypeId],
	found: &mut Vec<TypeId>,
	_information: &impl InformationChain,
	types: &TypeStore,
) {
	if let Some((_condition, lhs, rhs)) = get_conditional(on, types) {
		crate::utilities::notify!("{:?}, filter={:?}", (lhs, rhs), filter);
		build_union_from_filter_slice(lhs, filter, found, _information, types);
		build_union_from_filter_slice(rhs, filter, found, _information, types);
	} else if let Some(constraint) = crate::types::get_constraint(on, types) {
		build_union_from_filter_slice(constraint, filter, found, _information, types);
	} else if !filter.contains(&on) {
		found.push(on);
	}
}

mod prototype_narrowing {
	use super::*;

	pub fn pick_items_with_protoypes(
		on: TypeId,
		prototype: TypeId,
		negate: bool,
		found: &mut Vec<TypeId>,
		information: &impl InformationChain,
		types: &TypeStore,
	) {
		if let Some((_condition, lhs, rhs)) = get_conditional(on, types) {
			pick_items_with_protoypes(lhs, prototype, negate, found, information, types);
			pick_items_with_protoypes(rhs, prototype, negate, found, information, types);
		} else if let Some(constraint) = crate::types::get_constraint(on, types) {
			pick_items_with_protoypes(constraint, prototype, negate, found, information, types);
		} else if let Type::PartiallyAppliedGenerics(crate::types::PartiallyAppliedGenerics {
			on: gen_on,
			arguments: _,
		}) = types.get_type_by_id(on)
		{
			if !negate && prototype == *gen_on {
				found.push(on);
			} else if negate && prototype != *gen_on {
				found.push(on);
			}
		} else if let Type::Object(crate::types::ObjectNature::RealDeal) = types.get_type_by_id(on)
		{
			let extends = crate::features::extends_prototype(on, prototype, information);
			if !negate && extends {
				found.push(on);
			} else if negate && !extends {
				found.push(on);
			}
		}
	}

	/// Takes `Array` constructor and generates `Array<any>`
	pub fn generate_item(constructor: TypeId, types: &mut TypeStore) -> TypeId {
		use source_map::{Nullable, SpanWithSource};

		if let Some(parameters) = types.get_type_by_id(constructor).get_parameters() {
			let arguments = crate::types::GenericArguments::ExplicitRestrictions(
				parameters
					.into_iter()
					.map(|key| (key, (TypeId::ANY_TYPE, SpanWithSource::NULL)))
					.collect(),
			);
			types.register_type(Type::PartiallyAppliedGenerics(
				crate::types::PartiallyAppliedGenerics { on: constructor, arguments },
			))
		} else {
			constructor
		}
	}
}
