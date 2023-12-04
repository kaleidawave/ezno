//! Type subtyping / order / subtype checking.

use source_map::{BaseSpan, SourceId, Span, SpanWithSource};

use crate::{
	context::{Environment, GeneralContext, Logical},
	types::{
		poly_types::generic_type_arguments::TypeArgumentStore, printing::print_type, FunctionType,
		TypeStore,
	},
	FunctionId, PropertyValue, TypeId,
};

use super::{
	poly_types::{generic_type_arguments::StructureGenericArguments, SeedingContext},
	properties::PropertyKey,
	Constructor, PolyNature, StructureGenerics, Type,
};

pub use super::{BasicEquality, NonEqualityReason, PropertyError, SubTypeResult, SubtypeBehavior};

type TypeArguments = map_vec::Map<TypeId, (TypeId, SpanWithSource)>;

/// `base_type :>= ty` (`ty <=: base_type`)
///
/// TODO `TypeArguments` as a chain?
pub fn type_is_subtype<T: SubtypeBehavior>(
	base_type: TypeId,
	ty: TypeId,
	behavior: &mut T,
	environment: &mut Environment,
	types: &TypeStore,
) -> SubTypeResult {
	let result = type_is_subtype2(base_type, ty, None, None, behavior, environment, types, false);
	if let SubTypeResult::IsSubType = result {
		if behavior.add_property_restrictions() {
			set_object_restriction(environment, ty, base_type);
		}
	}
	result
}

fn set_object_restriction(environment: &mut Environment, object: TypeId, restriction: TypeId) {
	match environment.object_constraints.entry(object) {
		std::collections::hash_map::Entry::Occupied(mut entry) => {
			entry.get_mut().push(restriction);
		}
		std::collections::hash_map::Entry::Vacant(vacant) => {
			vacant.insert(vec![restriction]);
		}
	}
}

/// TODO integrate `set_restriction`, but it can't create a type ? maybe object restriction should be logically.
/// maybe sub function
pub fn type_is_subtype_of_property<T: SubtypeBehavior>(
	property: Logical<PropertyValue>,
	// TODO chain with annex
	property_generics: Option<&StructureGenericArguments>,
	ty: TypeId,
	behavior: &mut T,
	environment: &mut Environment,
	types: &TypeStore,
) -> SubTypeResult {
	match property {
		Logical::Pure(prop) => type_is_subtype2(
			prop.as_set_type(),
			ty,
			None,
			None,
			behavior,
			environment,
			types,
			false,
		),
		Logical::Or { left, right } => {
			let left_result = type_is_subtype_of_property(
				*left,
				property_generics,
				ty,
				behavior,
				environment,
				types,
			);
			if let SubTypeResult::IsSubType = left_result {
				left_result
			} else {
				type_is_subtype_of_property(
					*right,
					property_generics,
					ty,
					behavior,
					environment,
					types,
				)
			}
		}
		Logical::Implies { on, antecedent } => {
			if property_generics.is_some() {
				todo!("nesting of generics");
			}
			type_is_subtype_of_property(*on, Some(&antecedent), ty, behavior, environment, types)
		}
	}
}

#[allow(clippy::too_many_arguments)]
fn type_is_subtype2<T: SubtypeBehavior>(
	base_type: TypeId,
	ty: TypeId,
	base_type_arguments: Option<&TypeArguments>,
	right_type_arguments: Option<&TypeArguments>,
	behavior: &mut T,
	environment: &mut Environment,
	types: &TypeStore,
	// For generics when parameters
	restriction_mode: bool,
) -> SubTypeResult {
	// crate::utils::notify!("Checking {} <: {}", print_type(base_type), print_type(ty));

	if (base_type == TypeId::ERROR_TYPE || base_type == TypeId::ANY_TYPE)
		|| (ty == TypeId::ERROR_TYPE || ty == TypeId::NEVER_TYPE)
	{
		return SubTypeResult::IsSubType;
	}

	if base_type == ty {
		return SubTypeResult::IsSubType;
	}

	let left_ty = types.get_type_by_id(base_type);
	let right_ty = types.get_type_by_id(ty);

	// TODO
	if let Type::Or(left, right) = right_ty {
		let right = *right;
		let left_result = type_is_subtype2(
			base_type,
			*left,
			base_type_arguments,
			right_type_arguments,
			behavior,
			environment,
			types,
			restriction_mode,
		);

		return if let SubTypeResult::IsSubType = left_result {
			type_is_subtype2(
				base_type,
				right,
				base_type_arguments,
				right_type_arguments,
				behavior,
				environment,
				types,
				restriction_mode,
			)
		} else {
			left_result
		};
	}

	match left_ty {
		Type::FunctionReference(left_func, _) | Type::Function(left_func, _) => {
			if let Type::FunctionReference(right_func, _) | Type::Function(right_func, _) = right_ty
			{
				let left_func = types.functions.get(left_func).unwrap();
				let right_func = types.functions.get(right_func).unwrap();

				for (idx, lhs_param) in left_func.parameters.parameters.iter().enumerate() {
					match right_func.parameters.get_type_constraint_at_index(idx) {
						Some(right_param_ty) => {
							let result = type_is_subtype2(
								lhs_param.ty,
								right_param_ty,
								base_type_arguments,
								right_type_arguments,
								behavior,
								environment,
								types,
								// !!!
								true,
							);

							match result {
								SubTypeResult::IsSubType => {}
								err @ SubTypeResult::IsNotSubType(_) => {
									// TODO don't short circuit
									return err;
								}
							}
						}
						None => {
							return SubTypeResult::IsNotSubType(NonEqualityReason::MissingParameter)
						}
					}
				}
				// TODO optional and rest parameters

				// TODO NonEqualityReason::InvalidReturnType
				type_is_subtype2(
					left_func.return_type,
					right_func.return_type,
					base_type_arguments,
					right_type_arguments,
					behavior,
					environment,
					types,
					restriction_mode,
				)
			} else {
				crate::utils::notify!("Not function!!");
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			}
		}
		Type::Constant(lhs) => {
			if let Type::Constant(rhs) = right_ty {
				if lhs == rhs {
					SubTypeResult::IsSubType
				} else {
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			} else {
				// TODO what about if LHS has inferred constraint
				crate::utils::notify!("Constant {:?} against RHS {:#?}", lhs, right_ty);
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			}
		}
		Type::Object(..) => {
			let result = check_properties(
				base_type,
				ty,
				types,
				base_type_arguments,
				right_type_arguments,
				behavior,
				environment,
				restriction_mode,
			);
			let left = print_type(base_type, types, &environment.as_general_context(), true);
			crate::utils::notify!("Left {}", left);

			if let SubTypeResult::IsNotSubType(..) = result {
				result
			} else {
				SubTypeResult::IsSubType
			}
		}
		Type::And(left, right) => {
			let right = *right;
			let left_result = type_is_subtype2(
				*left,
				ty,
				base_type_arguments,
				right_type_arguments,
				behavior,
				environment,
				types,
				restriction_mode,
			);

			if let SubTypeResult::IsSubType = left_result {
				type_is_subtype2(
					right,
					ty,
					base_type_arguments,
					right_type_arguments,
					behavior,
					environment,
					types,
					restriction_mode,
				)
			} else {
				left_result
			}
		}
		Type::Or(left, right) => {
			let right = *right;
			let left_result = type_is_subtype2(
				*left,
				ty,
				base_type_arguments,
				right_type_arguments,
				behavior,
				environment,
				types,
				restriction_mode,
			);

			if let SubTypeResult::IsSubType = left_result {
				SubTypeResult::IsSubType
			} else {
				type_is_subtype2(
					right,
					ty,
					base_type_arguments,
					right_type_arguments,
					behavior,
					environment,
					types,
					restriction_mode,
				)
			}
		}
		Type::RootPolyType(nature) => {
			// TODO little weird, handing two very different cases beside each other. Might introduce bugs.. :(
			if let Some((value, _pos)) =
				base_type_arguments.as_ref().and_then(|ta| ta.get(&base_type))
			{
				type_is_subtype2(
					*value,
					ty,
					base_type_arguments,
					right_type_arguments,
					behavior,
					environment,
					types,
					restriction_mode,
				)
			} else {
				if !T::INFER_GENERICS && base_type_arguments.is_none() {
					// TODO temp fix
					if let Type::Constructor(c) = right_ty {
						crate::utils::notify!("TODO right hand side maybe okay");
						if let Some(to) = c.get_base() {
							if to == base_type {
								return SubTypeResult::IsSubType;
							}
						}
					}
					if let PolyNature::Generic { .. } = nature {
						// Already eliminated == case above, so always invalid here
						return SubTypeResult::IsNotSubType(
							NonEqualityReason::GenericParameterMismatch,
						);
					}
				}

				let constraint = environment.get_poly_base(base_type, types).unwrap();
				if let SubTypeResult::IsNotSubType(reasons) = type_is_subtype2(
					constraint,
					ty,
					base_type_arguments,
					right_type_arguments,
					behavior,
					environment,
					types,
					restriction_mode,
				) {
					crate::utils::notify!("RPT not subtype");
					return SubTypeResult::IsNotSubType(reasons);
				}

				let set_type_argument = behavior.set_type_argument(base_type, ty, restriction_mode);
				if let Err(reason) = set_type_argument {
					// TODO specialisation error
					SubTypeResult::IsNotSubType(reason)
				} else {
					SubTypeResult::IsSubType
				}
			}
		}
		Type::Constructor(super::Constructor::StructureGenerics(StructureGenerics {
			on,
			arguments,
		})) => {
			// Overwrites for special types with proofs
			if let TypeId::ARRAY_TYPE = *on {
				let backing_type =
					arguments.get_argument(TypeId::T_TYPE).expect("array T argument not set ?");

				// TODO temp fix for general parameters
				if let Type::Object(_) = right_ty {
					for (publicity, property, value) in environment.get_properties_on_type(ty) {
						// Assume every property on itself is either number or 'length'
						match property {
							PropertyKey::String(a) if a == "length" => {
								continue;
							}
							PropertyKey::String(a) => {
								crate::utils::notify!("looking at prototype {}", a);
							}
							PropertyKey::Type(_) => (),
						}
						let result = type_is_subtype2(
							backing_type,
							value,
							Some(&arguments.type_arguments),
							right_type_arguments,
							behavior,
							environment,
							types,
							restriction_mode,
						);
						// TODO collect
						if !matches!(result, SubTypeResult::IsSubType) {
							return result;
						}

						// TODO cheaper subtype checker
						// if let SubTypeResult::IsSubType = type_is_subtype2(
						// 	*base_property,
						// 	property,
						// 	Some(&arguments.type_arguments),
						// 	right_type_arguments,
						// 	behavior,
						// 	environment,
						// 	types,
						// 	false,
						// ) {
						// }
					}

					SubTypeResult::IsSubType
				} else {
					crate::utils::notify!("Else here :?");
					todo!("get right type structure generics match parameters");
				}
			} else {
				if base_type_arguments.is_some() {
					todo!("need chain to do nesting")
				}
				type_is_subtype2(
					*on,
					ty,
					Some(&arguments.type_arguments),
					right_type_arguments,
					behavior,
					environment,
					types,
					restriction_mode,
				)
			}
		}
		Type::Constructor(cst) => match cst {
			Constructor::BinaryOperator { .. }
			| Constructor::CanonicalRelationOperator { .. }
			| Constructor::UnaryOperator { .. } => unreachable!("invalid constructor on LHS"),
			Constructor::TypeOperator(_) => todo!(),
			Constructor::TypeRelationOperator(_) => todo!(),
			Constructor::ConditionalResult {
				condition,
				truthy_result,
				else_result,
				result_union,
			} => todo!(),
			Constructor::FunctionResult { on, with, result } => todo!(),
			Constructor::Property { on, under, result: _ } => {
				// Ezno custom behavior
				// TODO might be based of T
				if let Type::Constructor(Constructor::Property {
					on: r_on,
					under: r_under,
					result: _,
				}) = right_ty
				{
					if on == r_on && under == r_under {
						SubTypeResult::IsSubType
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				} else {
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			}
			Constructor::StructureGenerics(_) => unreachable!(),
		},
		// TODO aliasing might work differently
		Type::AliasTo { to, parameters, name: _ } => {
			if parameters.is_some() {
				todo!()
			}
			type_is_subtype2(
				*to,
				ty,
				base_type_arguments,
				right_type_arguments,
				behavior,
				environment,
				types,
				restriction_mode,
			)
		}
		Type::NamedRooted { nominal: base_type_nominal, .. } => {
			// If type matched type it would have been cleared before. So looking at properties and
			// prototypes here

			// These do **NOT** check for properties
			let skip_check = *base_type_nominal
				&& !matches!(
					right_ty,
					Type::RootPolyType(..)
						| Type::Constructor(..) | Type::Constant(..)
						| Type::Or(..)
				);
			if skip_check {
				crate::utils::notify!("Short circuited for RHS ={:?} as it is nominal", right_ty);
				// TODO not primitive error
				// TODO this might break with *properties* proofs on primitives
				// e.g. number :< Nat
				return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
			}

			// TODO a bit messy
			match right_ty {
				Type::Constant(constant) => {
					if constant.get_backing_type_id() == base_type {
						SubTypeResult::IsSubType
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				}
				Type::Object(..) => check_properties(
					base_type,
					ty,
					types,
					base_type_arguments,
					right_type_arguments,
					behavior,
					environment,
					restriction_mode,
				),
				Type::Function(..) => {
					crate::utils::notify!("TODO implement function checking");
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
				Type::And(_, _) => todo!(),
				Type::Or(left, right) => {
					unreachable!()
					// TODO fails if RHS is also OR type :(
					// let right = *right;
					// let left = type_is_subtype2(
					// 	base_type,
					// 	*left,
					// 	ty_arguments.as_deref(),
					// 	behavior,
					// 	environment,
					// 	types,
					// );
					// if let SubTypeResult::IsSubType = left {
					// 	type_is_subtype2(
					// 		base_type,
					// 		right,
					// 		ty_arguments,
					// 		behavior,
					// 		environment,
					// 		types,
					// 	)
					// } else {
					// 	crate::utils::notify!("Left failed");
					// 	SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					// }
				}
				Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
					on,
					arguments,
				})) => {
					if right_type_arguments.is_some() {
						todo!()
					}
					type_is_subtype2(
						base_type,
						*on,
						base_type_arguments,
						Some(&arguments.type_arguments),
						behavior,
						environment,
						types,
						restriction_mode,
					)
				}
				Type::AliasTo { .. } | Type::NamedRooted { .. } => {
					crate::utils::notify!("lhs={:?} rhs={:?}", left_ty, right_ty);
					// TODO
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
				Type::Constructor(..) | Type::RootPolyType(..) => {
					if let Some((argument, _)) =
						base_type_arguments.and_then(|ty_args| ty_args.get(&base_type))
					{
						type_is_subtype2(
							base_type,
							*argument,
							base_type_arguments,
							right_type_arguments,
							behavior,
							environment,
							types,
							restriction_mode,
						)
					} else {
						let to = environment.get_poly_base(ty, types).unwrap();

						if to == TypeId::ANY_TYPE {
							crate::utils::notify!("Modify constraint for equality");
						}

						type_is_subtype2(
							base_type,
							to,
							base_type_arguments,
							right_type_arguments,
							behavior,
							environment,
							types,
							restriction_mode,
						)
					}
				}
				Type::FunctionReference(_, _) => todo!(),
				Type::SpecialObject(_) => todo!(),
			}
		}
		Type::SpecialObject(_) => todo!(),
	}
}

/// TODO temp

#[allow(clippy::too_many_arguments)]
fn check_properties<T: SubtypeBehavior>(
	base_type: TypeId,
	ty: TypeId,
	types: &TypeStore,
	base_type_arguments: Option<&TypeArguments>,
	right_type_arguments: Option<&TypeArguments>,
	behavior: &mut T,
	environment: &mut Environment,
	restriction_mode: bool,
) -> SubTypeResult {
	let mut property_errors = Vec::new();
	for (publicity, key, property) in environment.get_properties_on_type(base_type) {
		let rhs_property = environment.get_property_unbound(ty, publicity, key.clone(), types);

		match rhs_property {
			Some(rhs_property) => {
				match rhs_property {
					Logical::Pure(rhs_property) => {
						let rhs_type = rhs_property.as_set_type();
						// crate::utils::notify!(
						// 	"Checking {} with {}, against {}, left={:?}",
						// 	print_type(key, types, &environment.as_general_context(), true),
						// 	print_type(property, types, &environment.as_general_context(), true),
						// 	print_type(rhs_type, types, &environment.as_general_context(), true),
						// 	base_type_arguments
						// );

						let result = type_is_subtype2(
							property,
							rhs_type,
							base_type_arguments,
							right_type_arguments,
							behavior,
							environment,
							types,
							restriction_mode,
						);
						// TODO add to property errors
						match result {
							SubTypeResult::IsSubType => {
								if behavior.add_property_restrictions() {
									set_object_restriction(environment, rhs_type, property);
								}
							}
							SubTypeResult::IsNotSubType(mismatch) => {
								property_errors.push((
									key,
									PropertyError::Invalid {
										expected: property,
										found: rhs_type,
										mismatch,
									},
								));
							}
						}
					}
					Logical::Or { .. } => todo!(),
					Logical::Implies { .. } => todo!(),
				}
			}
			// TODO
			None => {
				property_errors.push((key, PropertyError::Missing));
			}
		}
	}
	if property_errors.is_empty() {
		SubTypeResult::IsSubType
	} else {
		SubTypeResult::IsNotSubType(NonEqualityReason::PropertiesInvalid {
			errors: property_errors,
		})
	}
}

type ReadableSubTypeErrorMessage = Vec<String>;

impl NonEqualityReason {
	pub(crate) fn into_error_message(
		self,
		environment: &GeneralContext,
	) -> ReadableSubTypeErrorMessage {
		match self {
			NonEqualityReason::GenericParameterMismatch
			| NonEqualityReason::MissingParameter
			| NonEqualityReason::Mismatch => Vec::new(),
			NonEqualityReason::PropertiesInvalid { errors } => {
				errors.into_iter().map(|error| format!("{error:?}")).collect()
			}
			NonEqualityReason::TooStrict => todo!(),
		}
	}
}

pub(crate) fn check_satisfies(
	expr_ty: TypeId,
	to_satisfy: TypeId,
	types: &TypeStore,
	environment: &mut Environment,
) -> bool {
	if expr_ty == TypeId::ERROR_TYPE {
		false
	} else {
		let mut basic_equality =
			BasicEquality { add_property_restrictions: false, position: SpanWithSource::NULL_SPAN };
		let result = type_is_subtype(to_satisfy, expr_ty, &mut basic_equality, environment, types);
		matches!(result, SubTypeResult::IsSubType)
	}
}
