//! Type subtyping / order / subtype checking.

use source_map::{BaseSpan, SourceId, Span, SpanWithSource};

use crate::{
	context::{Environment, GeneralContext, Logical},
	types::{printing::print_type, FunctionType, TypeStore},
	FunctionId, TypeId,
};

use super::{poly_types::SeedingContext, Constructor, StructureGenerics, Type};

pub use super::{BasicEquality, NonEqualityReason, PropertyError, SubTypeResult, SubtypeBehavior};

type TypeArguments = map_vec::Map<TypeId, (TypeId, SpanWithSource)>;

/// `base_type :>= ty` (`ty <=: base_type`)
///
/// TODO TypeArguments as a chain?
pub fn type_is_subtype<T: SubtypeBehavior>(
	base_type: TypeId,
	ty: TypeId,
	behavior: &mut T,
	environment: &mut Environment,
	types: &TypeStore,
) -> SubTypeResult {
	type_is_subtype2(base_type, ty, None, None, behavior, environment, types, false)
}

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

	match right_ty {
		// This is the reverse of Type::And
		Type::Or(left, right) => {
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
		// TODO this causes problems when the LHS has a parameters that are substituted
		// Type::Constructor(..) | Type::RootPolyType(..) => {
		// 	if let Some(argument) = ty_arguments.and_then(|ty_args| ty_args.get(&base_type)) {
		// 		return type_is_subtype2(
		// 			base_type,
		// 			*argument,
		// 			ty_arguments.as_deref(),
		// 			behavior,
		// 			environment,
		// 			types,
		// 		);
		// 	} else {
		// 		let constraint = environment.get_poly_base(ty, types).unwrap();

		// 		crate::utils::notify!(
		// 			"Checking via constraint {:?}... think this is okay in function bodies",
		// 			constraint
		// 		);

		// 		return match constraint {
		// 			crate::context::PolyBase::Fixed { to, .. } => {
		// 				type_is_subtype2(base_type, to, ty_arguments, behavior, environment, types)
		// 			}
		// 			_ => todo!(),
		// 		};
		// 	}
		// }
		_ => {}
	}

	match left_ty {
		Type::Class(_) => todo!(),
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
				environment,
				base_type,
				ty,
				types,
				base_type_arguments,
				right_type_arguments,
				behavior,
				restriction_mode,
			);
			if matches!(result, SubTypeResult::IsNotSubType(..)) {
				result
			} else {
				if behavior.add_property_restrictions() {
					match environment.object_constraints.entry(ty) {
						std::collections::hash_map::Entry::Occupied(entry) => {
							todo!()
							// let new = types.new_and_type(lhs, rhs);
							// entry.insert(new);
						}
						std::collections::hash_map::Entry::Vacant(vacant) => {
							vacant.insert(base_type);
						}
					};
				}

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
		Type::Constructor(constructor) => {
			if let super::Constructor::StructureGenerics(StructureGenerics { on, arguments }) =
				constructor
			{
				// Overwrites for special types with proofs
				match *on {
					// TODO is nominal
					TypeId::ARRAY_TYPE => {
						// TODO maybe need a marker (named characteristic) rather than just picking first
						let get_properties_on_type = environment.get_properties_on_type(*on);
						let (base_property, publicity, base_value) =
							get_properties_on_type.first().unwrap();

						crate::utils::notify!(
							"{} : {} = {}",
							print_type(
								*base_property,
								types,
								&environment.as_general_context(),
								true
							),
							print_type(*base_value, types, &environment.as_general_context(), true),
							print_type(ty, types, &environment.as_general_context(), true)
						);

						// TODO temp fix for general parameters
						if !matches!(types.get_type_by_id(*base_property), Type::Constant(_)) {
							for (property, publicity, value) in
								environment.get_properties_on_type(ty)
							{
								// TODO cheaper subtype checker
								if let SubTypeResult::IsSubType = type_is_subtype2(
									*base_property,
									property,
									Some(&arguments.type_arguments),
									right_type_arguments,
									behavior,
									environment,
									types,
									restriction_mode,
								) {
									let result = type_is_subtype2(
										*base_value,
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
								}
							}

							SubTypeResult::IsSubType
						} else {
							let ty = environment.get_property_unbound(
								ty,
								*base_property,
								*publicity,
								types,
							);
							if let Some(ty) = ty {
								type_is_subtype2(
									base_type,
									ty.prop_to_type(),
									Some(&arguments.type_arguments),
									right_type_arguments,
									behavior,
									environment,
									types,
									restriction_mode,
								)
							} else {
								todo!()
							}
						}
					}
					_ => {
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
			} else {
				unreachable!("left constructor")
			}
		}
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
					environment,
					base_type,
					ty,
					types,
					base_type_arguments,
					right_type_arguments,
					behavior,
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
					todo!()
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
				Type::Class(_) => todo!(),
			}
		}
	}
}

/// TODO temp
fn check_properties<T: SubtypeBehavior>(
	environment: &mut Environment,
	base_type: TypeId,
	ty: TypeId,
	types: &TypeStore,
	base_type_arguments: Option<&TypeArguments>,
	right_type_arguments: Option<&TypeArguments>,
	behavior: &mut T,
	restriction_mode: bool,
) -> SubTypeResult {
	let mut property_errors = Vec::new();
	for (key, publicity, property) in environment.get_properties_on_type(base_type) {
		let rhs_property = environment.get_property_unbound(ty, key, publicity, types);

		match rhs_property {
			Some(rhs_property) => {
				match rhs_property {
					Logical::Pure(rhs_property) => {
						let rhs_type = rhs_property.as_get_type();
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
						if let SubTypeResult::IsNotSubType(mismatch) = result {
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
					Logical::Or(_) => todo!(),
					Logical::Implies { .. } => todo!(),
				}
			}
			// TODO
			None => {
				property_errors.push((key, PropertyError::Missing));
			}
		}
	}
	if !property_errors.is_empty() {
		SubTypeResult::IsNotSubType(NonEqualityReason::PropertiesInvalid {
			errors: property_errors,
		})
	} else {
		SubTypeResult::IsSubType
	}
}

type ReadableSubTypeErrorMessage = Vec<String>;

impl NonEqualityReason {
	pub(crate) fn into_error_message(
		self,
		environment: &GeneralContext,
	) -> ReadableSubTypeErrorMessage {
		match self {
			NonEqualityReason::MissingParameter | NonEqualityReason::Mismatch => Vec::new(),
			NonEqualityReason::PropertiesInvalid { errors } => {
				errors.into_iter().map(|error| format!("{:?}", error)).collect()
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
