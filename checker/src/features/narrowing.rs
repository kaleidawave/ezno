use crate::{
	context::InformationChain,
	types::{
		self, as_logical_and, as_logical_not, as_logical_or,
		helpers::{get_origin, get_type_as_conditional},
		properties, Constant, Constructor, PolyNature, TypeOperator, TypeStore,
	},
	Map, Type, TypeId,
};

use super::operations::{CanonicalEqualityAndInequality, MathematicalOrBitwiseOperation};

pub fn narrow_based_on_expression_into_vec(
	condition: TypeId,
	negate: bool,
	information: &impl InformationChain,
	types: &mut TypeStore,
) -> Map<TypeId, TypeId> {
	let mut into = Default::default();
	narrow_based_on_expression(condition, negate, &mut into, information, types);
	into.iter_mut().for_each(|(on, value)| *value = types.new_narrowed(*on, *value));
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
				let lhs_type = types.get_type_by_id(*lhs);
				if let Type::Constructor(Constructor::TypeOperator(TypeOperator::TypeOf(on))) =
					lhs_type
				{
					let from = *on;
					let origin = get_origin(from, types);

					if let Type::Constant(Constant::String(c)) = types.get_type_by_id(*rhs) {
						let type_from_name = crate::features::string_name_to_type(c);
						if let Some(type_from_name) = type_from_name {
							if negate {
								crate::utilities::notify!("{:?}", from);
								// TODO temp fix
								let narrowed_to = if let Some(TypeId::ANY_TYPE) =
									crate::types::get_constraint(from, types)
								{
									crate::types::intrinsics::new_intrinsic(
										&crate::types::intrinsics::Intrinsic::Not,
										type_from_name,
										types,
									)
								} else {
									let mut result = Vec::new();
									build_union_from_filter(
										from,
										Filter::Not(&Filter::IsType(type_from_name)),
										&mut result,
										information,
										types,
									);
									types.new_or_type_from_iterator(result)
								};
								into.insert(origin, narrowed_to);
							} else {
								into.insert(origin, type_from_name);
							}
						} else {
							crate::utilities::notify!("Type name was (shouldn't be here)");
						}
					} else {
						crate::utilities::notify!("Here?");
					}
				} else if let Type::Constructor(Constructor::BinaryOperator {
					lhs: operand,
					operator: MathematicalOrBitwiseOperation::Modulo,
					rhs: modulo,
					result: _,
				}) = lhs_type
				{
					if negate {
						crate::utilities::notify!("TODO do we not divisable by?");
						return;
					}
					let (operand, rhs, modulo) = (*operand, *rhs, *modulo);
					let operand = get_origin(operand, types);
					crate::utilities::notify!("Here {:?}", types.get_type_by_id(modulo));
					let narrowed_to = crate::types::intrinsics::new_intrinsic(
						&crate::types::intrinsics::Intrinsic::MultipleOf,
						modulo,
						types,
					);

					// TODO also from == x - 1 etc
					let narrowed_to = if rhs == TypeId::ZERO {
						narrowed_to
					} else {
						types.register_type(Type::Constructor(
							crate::types::Constructor::BinaryOperator {
								lhs: narrowed_to,
								operator: super::operations::MathematicalOrBitwiseOperation::Add,
								rhs,
								result: TypeId::NUMBER_TYPE,
							},
						))
					};
					into.insert(operand, narrowed_to);
				} else {
					if let Type::RootPolyType(PolyNature::Parameter { .. }) = lhs_type {
						crate::utilities::notify!("lhs is {:?} with {:?}", lhs_type, rhs);
					}

					if negate && lhs == rhs {
						into.insert(*lhs, TypeId::NOT_NOT_A_NUMBER);
						return;
					}

					let lhs = *lhs;
					let rhs = *rhs;

					let result = if negate {
						// TODO wip
						let narrowed_to = if get_type_as_conditional(lhs, types).is_some() {
							let mut result = Vec::new();
							build_union_from_filter(
								lhs,
								Filter::Not(&Filter::IsType(rhs)),
								&mut result,
								information,
								types,
							);
							// crate::utilities::notify!("Here {:?} {:?}", (filter, lhs), result);

							types.new_or_type_from_iterator(result)
						} else {
							crate::types::intrinsics::new_intrinsic(
								&crate::types::intrinsics::Intrinsic::Not,
								rhs,
								types,
							)
						};
						narrowed_to
					} else {
						rhs
					};

					into.insert(lhs, result);

					// CONDITION NARROWING HERE ((x ? 1 : 2) == 1 => x)
					// There are missed conditons around things like `typeof` etc (oh well)
					// it should be done higher up
					if let Type::Constructor(Constructor::ConditionalResult {
						condition,
						truthy_result,
						otherwise_result,
						result_union: _,
					}) = types.get_type_by_id(get_origin(lhs, types))
					{
						if crate::types::helpers::type_equal(*truthy_result, rhs, types) {
							narrow_based_on_expression(*condition, false, into, information, types);
						} else if crate::types::helpers::type_equal(*otherwise_result, rhs, types) {
							narrow_based_on_expression(*condition, true, into, information, types);
						}
					}
					// PROPERTY NARROWING HERE (x.a: b => x: {a: b})
					else if let Type::Constructor(Constructor::Property {
						on,
						under,
						result: _,
						mode: _,
					}) = types.get_type_by_id(get_origin(lhs, types))
					{
						let on = *on;
						let narrowed_to = if !negate
							&& crate::types::get_constraint(on, types)
								.is_some_and(|c| c == TypeId::ANY_TYPE)
						{
							generate_new_type_with_property(under.into_owned(), result, types)
						} else {
							let mut items = Vec::new();
							build_union_from_filter(
								on,
								Filter::HasProperty {
									property: under,
									filter: &Filter::IsType(result),
								},
								&mut items,
								information,
								types,
							);
							types.new_or_type_from_iterator(items)
						};
						// crate::utilities::notify!(
						// 	"Here {:?} to {:?}",
						// 	on,
						// 	types.get_type_by_id(narrowed_to)
						// );
						into.insert(on, narrowed_to);
					}
				}
			}
			Constructor::CanonicalRelationOperator {
				lhs,
				operator: CanonicalEqualityAndInequality::LessThan,
				rhs,
			} => {
				if negate {
					crate::utilities::notify!("Skipping negate on less");
					return;
				}
				let lhs = get_origin(*lhs, types);
				let rhs = get_origin(*rhs, types);
				if types.get_type_by_id(lhs).is_dependent() {
					let narrowed_to = crate::types::intrinsics::new_intrinsic(
						&crate::types::intrinsics::Intrinsic::LessThan,
						rhs,
						types,
					);
					// TODO need to merge. This is very bad
					let narrowed_to = if let Some(existing) = into.get(&lhs) {
						crate::utilities::notify!("Here");
						types.new_and_type(*existing, narrowed_to)
					} else {
						narrowed_to
					};
					into.insert(lhs, narrowed_to);
				}
				if types.get_type_by_id(rhs).is_dependent() {
					let narrowed_to = crate::types::intrinsics::new_intrinsic(
						&crate::types::intrinsics::Intrinsic::GreaterThan,
						lhs,
						types,
					);
					// TODO need to merge. This is very bad
					let narrowed_to = if let Some(existing) = into.get(&rhs) {
						crate::utilities::notify!("Here");
						types.new_and_type(narrowed_to, *existing)
					} else {
						narrowed_to
					};
					into.insert(rhs, narrowed_to);
				}
			}
			Constructor::TypeOperator(TypeOperator::IsPrototype { lhs, rhs_prototype }) => {
				let (lhs, rhs_prototype) = (*lhs, *rhs_prototype);
				let constraint = crate::types::get_constraint(lhs, types).unwrap_or(lhs);
				// TODO want a mix of two
				let narrowed_to = if !negate && constraint == TypeId::ANY_TYPE {
					generate_new_type_with_prototype(rhs_prototype, types)
				} else {
					let mut result = Vec::new();
					let filter = Filter::HasPrototype(rhs_prototype);
					let filter = if negate { Filter::Not(&filter) } else { filter };
					build_union_from_filter(constraint, filter, &mut result, information, types);
					types.new_or_type_from_iterator(result)
				};
				into.insert(lhs, narrowed_to);
			}
			Constructor::TypeExtends(crate::types::TypeExtends { item, extends }) => {
				let (item, extends) = (*item, *extends);
				// experimental
				// TODO want a mix of two
				let constraint = crate::types::get_constraint(item, types).unwrap_or(item);
				let narrowed_to = if !negate && constraint == TypeId::ANY_TYPE {
					extends
				} else {
					let mut result = Vec::new();
					let filter = Filter::IsType(extends);
					let filter = if negate { Filter::Not(&filter) } else { filter };
					build_union_from_filter(constraint, filter, &mut result, information, types);
					types.new_or_type_from_iterator(result)
				};
				into.insert(item, narrowed_to);
			}
			Constructor::TypeOperator(TypeOperator::HasProperty(on, under)) => {
				let on = *on;
				let constraint = types::get_constraint(on, types).unwrap_or(on);
				let narrowed_to = if !negate && constraint == TypeId::ANY_TYPE {
					generate_new_type_with_property(under.into_owned(), TypeId::ANY_TYPE, types)
				} else {
					let mut items = Vec::new();
					build_union_from_filter(
						constraint,
						Filter::HasProperty {
							property: under,
							filter: &Filter::IsType(TypeId::ANY_TYPE),
						},
						&mut items,
						information,
						types,
					);
					types.new_or_type_from_iterator(items)
				};
				into.insert(on, narrowed_to);
			}
			constructor => {
				if let Some(condition) = as_logical_not(constructor, types) {
					narrow_based_on_expression(condition, !negate, into, information, types);
				} else if let Some((lhs, rhs)) = as_logical_and(constructor, types) {
					// De Morgan's laws
					if negate {
						// OR: Pull assertions from left and right, merge if both branches assert something
						let lhs_requests =
							narrow_based_on_expression_into_vec(lhs, negate, information, types);
						let rhs_requests =
							narrow_based_on_expression_into_vec(rhs, negate, information, types);

						for (on, lhs_request) in lhs_requests {
							if let Some(rhs_request) = rhs_requests.get(&on) {
								let rhs_request = *rhs_request;
								let (lhs_request, rhs_request) = (
									crate::types::get_constraint(lhs_request, types)
										.unwrap_or(lhs_request),
									crate::types::get_constraint(rhs_request, types)
										.unwrap_or(rhs_request),
								);
								// TODO
								// let narrowed = types.new_narrowed(rhs, narrowed_to);
								into.insert(on, types.new_or_type(lhs_request, rhs_request));
							} else {
								// Only when we have two results is it useful
							}
						}
					} else {
						// AND: Pull assertions from left and right
						narrow_based_on_expression(lhs, negate, into, information, types);
						narrow_based_on_expression(rhs, negate, into, information, types);
					}
				} else if let Some((lhs, rhs)) = as_logical_or(constructor, types) {
					// De Morgan's laws
					if negate {
						// AND: Pull assertions from left and right
						narrow_based_on_expression(lhs, negate, into, information, types);
						narrow_based_on_expression(rhs, negate, into, information, types);
					} else {
						// OR: Pull assertions from left and right, merge if both branches assert something
						let lhs_requests =
							narrow_based_on_expression_into_vec(lhs, negate, information, types);
						let rhs_requests =
							narrow_based_on_expression_into_vec(rhs, negate, information, types);

						for (on, lhs_request) in lhs_requests {
							if let Some(rhs_request) = rhs_requests.get(&on) {
								let rhs_request = *rhs_request;
								let (lhs_request, rhs_request) = (
									crate::types::get_constraint(lhs_request, types)
										.unwrap_or(lhs_request),
									crate::types::get_constraint(rhs_request, types)
										.unwrap_or(rhs_request),
								);
								into.insert(on, types.new_or_type(lhs_request, rhs_request));
							} else {
								// Only when we have two results is it useful
							}
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
		} else if !negate {
			let mut result = Vec::new();
			super::narrowing::build_union_from_filter(
				condition,
				super::narrowing::NOT_FASLY,
				&mut result,
				information,
				types,
			);
			let narrowed_to = types.new_or_type_from_iterator(result);
			into.insert(condition, narrowed_to);
		}
	}
}

/// Pseudo type for keeping or removing types
#[derive(Debug, Clone, Copy)]
pub(crate) enum Filter<'a> {
	Not(&'a Self),
	IsType(TypeId),
	HasPrototype(TypeId),
	HasProperty {
		property: &'a properties::PropertyKey<'a>,
		filter: &'a Filter<'a>,
	},
	/// For non null assertions and
	NullOrUndefined,
	Falsy,
}

static NULL_OR_UNDEFINED: Filter<'static> = Filter::NullOrUndefined;
pub(crate) static NOT_NULL_OR_UNDEFINED: Filter<'static> = Filter::Not(&NULL_OR_UNDEFINED);

pub(crate) static FASLY: Filter<'static> = Filter::Falsy;
pub(crate) static NOT_FASLY: Filter<'static> = Filter::Not(&FASLY);

impl Filter<'_> {
	// Returns `true` if `value` passes filter
	pub(crate) fn type_matches_filter(
		&self,
		value: TypeId,
		information: &impl InformationChain,
		types: &TypeStore,
		negate: bool,
	) -> bool {
		match self {
			Filter::Not(filter) => filter.type_matches_filter(value, information, types, !negate),
			Filter::IsType(ty) => {
				if negate {
					types::disjoint::types_are_disjoint(
						*ty,
						value,
						&mut Vec::new(),
						information,
						types,
					)
				} else {
					// value has to satisfies ty
					types::helpers::simple_subtype(value, *ty, information, types)
				}
			}
			Filter::HasPrototype(prototype) => {
				if let Type::PartiallyAppliedGenerics(types::PartiallyAppliedGenerics {
					on: gen_on,
					arguments: _,
				}) = types.get_type_by_id(value)
				{
					let is_equal = prototype == gen_on;
					let allowed_match = !negate;
					(allowed_match && is_equal) || (!allowed_match && !is_equal)
				} else if let Type::Object(types::ObjectNature::RealDeal) =
					types.get_type_by_id(value)
				{
					// This branch can be triggered by conditionals
					let extends =
						crate::features::extends_prototype(value, *prototype, information);
					let allowed_match = !negate;
					(allowed_match && extends) || (!allowed_match && !extends)
				} else {
					let is_equal = value == *prototype;
					let allowed_match = !negate;
					(allowed_match && is_equal) || (!allowed_match && !is_equal)
				}
			}
			Filter::HasProperty { property, filter } => {
				let value = types::properties::get_simple_property_value(
					information,
					value,
					property,
					types,
				);
				if let Some(value) = value {
					let matches = filter.type_matches_filter(value, information, types, negate);
					crate::utilities::notify!("Value {:?}", (value, negate, matches));
					matches
				} else {
					negate
				}
			}
			Filter::NullOrUndefined => {
				let is_null_or_undefined =
					[TypeId::NULL_TYPE, TypeId::UNDEFINED_TYPE].contains(&value);
				let allowed_match = !negate;
				(allowed_match && is_null_or_undefined) || (!allowed_match && !is_null_or_undefined)
			}
			Filter::Falsy => {
				let can_be_falsy = [TypeId::NUMBER_TYPE, TypeId::STRING_TYPE, TypeId::BOOLEAN_TYPE];

				if can_be_falsy.contains(&value) {
					return true;
				}

				let is_falsy = [
					TypeId::NULL_TYPE,
					TypeId::UNDEFINED_TYPE,
					TypeId::FALSE,
					TypeId::EMPTY_STRING,
					TypeId::ZERO,
				]
				.contains(&value);
				let allowed_match = !negate;
				(allowed_match && is_falsy) || (!allowed_match && !is_falsy)
			}
		}
	}
}

/// Important that this does not handle `any` well with negated filters. It needs to generate negated types but only has non-mutable access to `TypeStore`
#[allow(clippy::used_underscore_binding)]
pub(crate) fn build_union_from_filter(
	on: TypeId,
	filter: Filter,
	found: &mut Vec<TypeId>,
	information: &impl InformationChain,
	types: &TypeStore,
) {
	if let Some(constraint) = crate::types::get_constraint(on, types) {
		build_union_from_filter(constraint, filter, found, information, types);
	} else if let TypeId::BOOLEAN_TYPE = on {
		build_union_from_filter(TypeId::TRUE, filter, found, information, types);
		build_union_from_filter(TypeId::FALSE, filter, found, information, types);
	} else if let Some((_condition, lhs, rhs)) = get_type_as_conditional(on, types) {
		build_union_from_filter(lhs, filter, found, information, types);
		build_union_from_filter(rhs, filter, found, information, types);
	} else {
		let not_already_added = !found.contains(&on);
		if not_already_added && filter.type_matches_filter(on, information, types, false) {
			found.push(on);
		}
	}
}

/// Takes `Array` constructor and generates `Array<any>`
pub fn generate_new_type_with_prototype(constructor: TypeId, types: &mut TypeStore) -> TypeId {
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

pub fn generate_new_type_with_property(
	key: properties::PropertyKey<'static>,
	value: TypeId,
	types: &mut TypeStore,
) -> TypeId {
	let property = (properties::Publicity::Public, key, properties::PropertyValue::Value(value));

	types.register_type(Type::Object(crate::types::ObjectNature::AnonymousTypeAnnotation(vec![
		property,
	])))
}
