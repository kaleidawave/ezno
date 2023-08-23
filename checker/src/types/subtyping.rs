//! Type subtyping / order / subtype checking.

use source_map::Span;

use crate::{
	context::{Environment, GeneralContext, Logical},
	types::{FunctionType, TypeStore},
	FunctionId, TypeId,
};

use super::{poly_types::SeedingContext, Constructor, Type};

/// TODO add_property_restrictions via const generics
pub struct BasicEquality {
	pub add_property_restrictions: bool,
	pub position: Span,
}

/// For subtyping
pub trait SubtypeBehavior {
	fn set_type_argument(
		&mut self,
		parameter: TypeId,
		value: TypeId,
		environment: &mut Environment,
		types: &TypeStore,
	) -> Result<(), NonEqualityReason>;

	fn add_property_restrictions(&self) -> bool;

	fn add_function_restriction(
		&mut self,
		environment: &mut Environment,
		function_id: FunctionId,
		function_type: FunctionType,
	);

	// TODO
	// object reference type needs to meet constraint
	// LHS is dependent + RHS argument
}

impl SubtypeBehavior for SeedingContext {
	/// Does not check thingy
	fn set_type_argument(
		&mut self,
		type_id: TypeId,
		value: TypeId,
		environment: &mut Environment,
		types: &TypeStore,
	) -> Result<(), NonEqualityReason> {
		let restriction = self.type_arguments.get_restriction_for_id(type_id);

		// Check restriction from call site type argument
		if let Some((pos, restriction)) = restriction {
			if let SubTypeResult::IsNotSubType(reason) =
				type_is_subtype(restriction, value, None, self, environment, types)
			{
				return Err(NonEqualityReason::GenericRestrictionMismatch {
					restriction,
					reason: Box::new(reason),
					pos,
				});
			}
		}

		self.type_arguments.set_id(type_id, value, types);

		Ok(())
	}

	fn add_property_restrictions(&self) -> bool {
		false
	}

	fn add_function_restriction(
		&mut self,
		_environment: &mut Environment,
		function_id: FunctionId,
		function_type: FunctionType,
	) {
		self.locally_held_functions.insert(function_id, function_type);
	}
}

impl SubtypeBehavior for BasicEquality {
	fn set_type_argument(
		&mut self,
		parameter: TypeId,
		value: TypeId,
		environment: &mut Environment,
		types: &TypeStore,
	) -> Result<(), NonEqualityReason> {
		Ok(())
	}

	fn add_property_restrictions(&self) -> bool {
		self.add_property_restrictions
	}

	fn add_function_restriction(
		&mut self,
		environment: &mut Environment,
		function_id: FunctionId,
		function_type: FunctionType,
	) {
		let result = environment
			.deferred_function_constraints
			.insert(function_id, (function_type, self.position.clone()));

		debug_assert!(result.is_none());
	}
}

#[derive(Debug)]
pub enum SubTypeResult {
	IsSubType,
	IsNotSubType(NonEqualityReason),
}

// impl SubTypeResult {
// 	type Error = NonEqualityReason;
// }

// TODO implement `?` on SupertypeResult

// TODO maybe positions and extra information here
// SomeLiteralMismatch
// GenericParameterCollision
#[derive(Debug)]
pub enum NonEqualityReason {
	Mismatch,
	PropertiesInvalid {
		errors: Vec<(TypeId, PropertyError)>,
	},
	// For function call-site type arguments
	GenericRestrictionMismatch {
		restriction: TypeId,
		reason: Box<NonEqualityReason>,
		pos: Span,
	},
	TooStrict,
	/// TODO more information
	MissingParameter,
}

#[derive(Debug)]
pub enum PropertyError {
	Missing,
	Invalid { expected: TypeId, found: TypeId, mismatch: NonEqualityReason },
}

/// TODO tidy function
/// TODO account for getters and setters
///
/// `ty <: base_type`
pub fn type_is_subtype<T: SubtypeBehavior>(
	base_type: TypeId,
	ty: TypeId,
	// TODO bad
	ty_arguments: Option<&map_vec::Map<TypeId, TypeId>>,
	behavior: &mut T,
	environment: &mut Environment,
	types: &TypeStore,
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
		// This is reverse and
		Type::Or(left, right) => {
			let right = *right;
			let left_result = type_is_subtype(
				base_type,
				*left,
				ty_arguments.as_deref(),
				behavior,
				environment,
				types,
			);

			return if let SubTypeResult::IsSubType = left_result {
				type_is_subtype(
					base_type,
					right,
					ty_arguments.as_deref(),
					behavior,
					environment,
					types,
				)
			} else {
				left_result
			};
		}
		Type::Constructor(..) | Type::RootPolyType(..) => {
			if let Some(argument) = ty_arguments.and_then(|ty_args| ty_args.get(&base_type)) {
				return type_is_subtype(
					base_type,
					*argument,
					ty_arguments.as_deref(),
					behavior,
					environment,
					types,
				);
			} else {
				let constraint = environment.get_poly_base(ty, types).unwrap();

				crate::utils::notify!(
					"Checking via constraint {:?}... think this is okay in function bodies",
					constraint
				);

				return match constraint {
					crate::context::PolyBase::Fixed { to, .. } => {
						type_is_subtype(base_type, to, ty_arguments, behavior, environment, types)
					}
					_ => todo!(),
				};
			}
		}
		_ => {}
	}

	match left_ty {
		Type::Function(left_func, _) => {
			if let Type::Function(func, _) = right_ty {
				// TODO optional and rest parameters
				for (idx, lhs_param) in left_func.parameters.parameters.iter().enumerate() {
					match func.parameters.get_type_constraint_at_index(idx) {
						Some(ty) => {
							// TODO order here
							let result = type_is_subtype(
								ty,
								lhs_param.ty,
								ty_arguments,
								behavior,
								environment,
								types,
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
				SubTypeResult::IsSubType
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
			let result =
				check_properties(environment, base_type, ty, types, ty_arguments, behavior);
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
			let left_result =
				type_is_subtype(*left, ty, ty_arguments.as_deref(), behavior, environment, types);

			if let SubTypeResult::IsSubType = left_result {
				type_is_subtype(right, ty, ty_arguments.as_deref(), behavior, environment, types)
			} else {
				left_result
			}
		}
		Type::Or(left, right) => {
			let right = *right;
			let left_result =
				type_is_subtype(*left, ty, ty_arguments.as_deref(), behavior, environment, types);

			if let SubTypeResult::IsSubType = left_result {
				SubTypeResult::IsSubType
			} else {
				type_is_subtype(right, ty, ty_arguments.as_deref(), behavior, environment, types)
			}
		}
		Type::RootPolyType(nature) => {
			// let name = if let crate::types::PolyNature::Generic { name, .. } = nature {
			// 	Some(name)
			// } else {
			// 	crate::utils::notify!("Here {:?}", nature);
			// 	None
			// };

			let constraint = environment.get_poly_base(base_type, types).unwrap().get_type();

			if let SubTypeResult::IsNotSubType(reasons) = type_is_subtype(
				constraint,
				ty,
				ty_arguments.as_deref(),
				behavior,
				environment,
				types,
			) {
				crate::utils::notify!("RPT not subtype");
				return SubTypeResult::IsNotSubType(reasons);
			}

			let set_type_argument = behavior.set_type_argument(base_type, ty, environment, types);
			if let Err(reason) = set_type_argument {
				// TODO specialisation error
				SubTypeResult::IsNotSubType(reason)
			} else {
				SubTypeResult::IsSubType
			}
		}
		Type::Constructor(constructor) => {
			if let super::Constructor::StructureGenerics { on, with } = constructor {
				// Overwrites for special types with proofs
				match *on {
					TypeId::ARRAY_TYPE => {
						todo!("Check property proofs");
					}
					_ => {
						todo!("Needs a check property proofs with LHS type arguments (currently only have right arguments sent through)");
					}
				}
			} else {
				unreachable!()
			}
		}
		// TODO aliasing might work differently
		Type::AliasTo { to, parameters, name: _ } => {
			if parameters.is_some() {
				todo!()
			}
			type_is_subtype(*to, ty, ty_arguments, behavior, environment, types)
		}
		Type::NamedRooted { .. } => {
			// If type matched type it would have been cleared before. So looking at properties and
			// prototypes here

			// These do **NOT** check for properties
			// TODO Type is nominal
			if matches!(base_type, TypeId::STRING_TYPE | TypeId::NUMBER_TYPE | TypeId::BOOLEAN_TYPE)
				&& !matches!(
					right_ty,
					Type::RootPolyType(..)
						| Type::Constructor(..) | Type::Constant(..)
						| Type::Or(..)
				) {
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
				Type::Object(..) => {
					check_properties(environment, base_type, ty, types, ty_arguments, behavior)
				}
				Type::Function(..) => {
					crate::utils::notify!("TODO implement function checking");
					return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
				}
				Type::And(_, _) => todo!(),
				Type::Or(left, right) => {
					unreachable!()
					// TODO fails if RHS is also OR type :(
					// let right = *right;
					// let left = type_is_subtype(
					// 	base_type,
					// 	*left,
					// 	ty_arguments.as_deref(),
					// 	behavior,
					// 	environment,
					// 	types,
					// );
					// if let SubTypeResult::IsSubType = left {
					// 	type_is_subtype(
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
				Type::Constructor(Constructor::StructureGenerics { on, with }) => {
					todo!()
				}
				Type::AliasTo { .. } | Type::NamedRooted { .. } => {
					todo!()
				}
				Type::Constructor(..) | Type::RootPolyType(..) => {
					if let Some(argument) = ty_arguments.and_then(|ty_args| ty_args.get(&base_type))
					{
						type_is_subtype(
							base_type,
							*argument,
							ty_arguments.as_deref(),
							behavior,
							environment,
							types,
						)
					} else {
						let constraint = environment.get_poly_base(ty, types).unwrap();

						crate::utils::notify!(
							"Checking via constraint {:?}... think this is okay in function bodies",
							constraint
						);

						match constraint {
							crate::context::PolyBase::Fixed { to, .. } => type_is_subtype(
								base_type,
								to,
								ty_arguments,
								behavior,
								environment,
								types,
							),
							crate::context::PolyBase::Dynamic { to, boundary: under } => {
								let type_is_subtype = type_is_subtype(
									base_type,
									to,
									ty_arguments,
									behavior,
									environment,
									types,
								);
								return if let SubTypeResult::IsSubType = type_is_subtype {
									SubTypeResult::IsSubType
								} else {
									if to == TypeId::ANY_TYPE {
										environment.attempt_to_modify_base(ty, under, base_type)
									} else {
										todo!()
										// let new_type =
										// 	types.new_type(Type::And(dynamic, base_type));
										// environment
										// 	.attempt_to_modify_constraint_or_alias(ty, new_type)
									}
									SubTypeResult::IsSubType
								};
							}
						}
					}
				}
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
	ty_arguments: Option<&map_vec::Map<TypeId, TypeId>>,
	behavior: &mut T,
) -> SubTypeResult {
	let mut property_errors = Vec::new();
	for (key, property) in environment.get_properties_on_type(base_type) {
		// TODO
		let rhs_property = environment.get_property_unbound(ty, key, types);

		match rhs_property {
			Some(rhs_property) => {
				match rhs_property {
					Logical::Pure(rhs_property) => {
						let rhs_type = rhs_property.as_get_type();
						let result = type_is_subtype(
							property,
							rhs_type,
							ty_arguments,
							behavior,
							environment,
							types,
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
					Logical::Implies(_, _) => todo!(),
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
			NonEqualityReason::GenericRestrictionMismatch { restriction, reason, pos } => todo!(),
			NonEqualityReason::PropertiesInvalid { errors } => {
				errors.into_iter().map(|error| format!("{:?}", error)).collect()
			}
			NonEqualityReason::TooStrict => todo!(),
		}
	}
}

#[cfg(test)]
mod tests {}
