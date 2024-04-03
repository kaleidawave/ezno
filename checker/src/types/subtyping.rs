//! Type subtype checking. (making sure the RHS type contains all the properties the LHS type requires)

use source_map::SpanWithSource;

use crate::{
	context::{
		information::{get_property_unbound, InformationChain, Publicity},
		Environment, GeneralContext, Logical,
	},
	features::objects::SpecialObjects,
	types::{
		poly_types::generic_type_arguments::StructureGenericArguments, printing::print_type,
		GenericChainLink, TypeStore,
	},
	PropertyValue, TypeId,
};

use super::{
	get_constraint, properties::PropertyKey, Constructor, GenericChain, PolyNature,
	StructureGenerics, Type,
};

pub use super::{BasicEquality, NonEqualityReason, PropertyError, SubTypeBehavior, SubTypeResult};

/// TODO document which one is which
#[derive(Clone, Copy)]
pub enum SubTypingMode {
	/// *output*
	///
	/// ```ts
	/// <T>(t: T) => T
	///        ^
	/// ```
	Contravariant { depth: u8 },
	/// From subtyping against a function
	///
	/// ```ts
	/// <T>(cb: (t: T) => bool) => T
	///             ^
	/// ```
	Covariant {
		/// From parameters or explicit generic position
		position: SpanWithSource,
	},
}

impl Default for SubTypingMode {
	fn default() -> Self {
		Self::Contravariant { depth: 0 }
	}
}

impl SubTypingMode {
	pub(crate) fn one_deeper(self) -> SubTypingMode {
		match self {
			SubTypingMode::Contravariant { depth } => Self::Contravariant { depth: depth + 1 },
			o @ SubTypingMode::Covariant { .. } => o,
		}
	}
}

/// `base_type :>= ty` (`ty <=: base_type`)
///
/// TODO `TypeArguments` as a chain?
pub fn type_is_subtype<'a, T: SubTypeBehavior<'a>>(
	base_type: TypeId,
	ty: TypeId,
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
) -> SubTypeResult {
	type_is_subtype_with_generics(
		base_type,
		GenericChain::None,
		ty,
		GenericChain::None,
		behavior,
		environment,
		types,
		Default::default(),
		&mut Default::default(),
	)
}

/// Vec as it needs to do a sequential removal
pub type AlreadyChecked = Vec<(TypeId, TypeId)>;

#[allow(clippy::too_many_arguments)]
pub(crate) fn type_is_subtype_with_generics<'a, T: SubTypeBehavior<'a>>(
	base_type: TypeId,
	base_structure_arguments: GenericChain,
	ty: TypeId,
	ty_structure_arguments: GenericChain,
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
	mode: SubTypingMode,
	already_checked: &mut AlreadyChecked,
) -> SubTypeResult {
	{
		let debug = true;
		crate::utils::notify!(
			"Checking {} :>= {}",
			print_type(base_type, types, environment, debug),
			print_type(ty, types, environment, debug)
		);
	}

	if behavior.allow_errors() && (base_type == TypeId::ERROR_TYPE || ty == TypeId::ERROR_TYPE) {
		return SubTypeResult::IsSubType;
	}

	if base_type == TypeId::ANY_TYPE || ty == TypeId::NEVER_TYPE {
		return SubTypeResult::IsSubType;
	}

	if base_type == ty {
		return SubTypeResult::IsSubType;
	}

	{
		// Prevents cycles
		if already_checked.iter().any(|(a, b)| *a == base_type && *b == ty) {
			return SubTypeResult::IsSubType;
		}

		already_checked.push((base_type, ty));
	}

	let left_ty = types.get_type_by_id(base_type);
	let right_ty = types.get_type_by_id(ty);

	// Eager things
	match right_ty {
		Type::Or(left, right) => {
			let right = *right;
			crate::utils::notify!("OR RHS: left and right");
			let left_result = type_is_subtype_with_generics(
				base_type,
				base_structure_arguments,
				*left,
				ty_structure_arguments,
				behavior,
				environment,
				types,
				mode,
				already_checked,
			);

			return if let SubTypeResult::IsSubType = left_result {
				type_is_subtype_with_generics(
					base_type,
					base_structure_arguments,
					right,
					ty_structure_arguments,
					behavior,
					environment,
					types,
					mode,
					already_checked,
				)
			} else {
				// else return the failing result
				left_result
			};
		}
		// Type::And(left, right) => {
		// 	let right = *right;
		// 	let left_result = type_is_subtype_with_generics(
		// 		base_type,
		// 		base_structure_arguments,
		// 		*left,
		// 		ty_structure_arguments,
		// 		behavior,
		// 		environment,
		// 		types,
		// 		mode,
		// 		already_checked
		// 	);

		// 	return if let SubTypeResult::IsSubType = left_result {
		// 		left_result
		// 	} else {
		// 		type_is_subtype_with_generics(
		// 			base_type,
		// 			base_structure_arguments,
		// 			right,
		// 			ty_structure_arguments,
		// 			behavior,
		// 			environment,
		// 			types,
		// 			mode,
		// 			already_checked
		// 		)
		// 	};
		// }
		Type::Constructor(Constructor::StructureGenerics(..)) => {}
		Type::RootPolyType(..) | Type::Constructor(..) => {
			if let Some(args) =
				ty_structure_arguments.and_then(|tas| tas.get_argument(ty, environment, types))
			{
				// TODO what
				for arg in args {
					let result = type_is_subtype_with_generics(
						base_type,
						base_structure_arguments,
						arg,
						ty_structure_arguments,
						behavior,
						environment,
						types,
						mode,
						already_checked,
					);

					if let e @ SubTypeResult::IsNotSubType(_) = result {
						return e;
					}
				}

				return SubTypeResult::IsSubType;
			}

			// If lhs is not operator unless argument is operator
			// if !T::INFER_GENERICS && ty_structure_arguments.is_none() {
			let right_arg = get_constraint(ty, types).unwrap();
			// This is important that LHS is not operator
			let left_is_operator_right_is_not =
				left_ty.is_operator() && !types.get_type_by_id(right_arg).is_operator();

			// edge cases on edge cases
			// If any of these are true. Then do not perform constraint argument lookup
			let edge_case = left_is_operator_right_is_not
				|| matches!(
					left_ty,
					Type::RootPolyType(rpt)
					if rpt.is_substitutable()
				) || matches!(
				   left_ty,
				   Type::Constructor(cst)
				   if !cst.is_structure_generics()
			);

			if !edge_case {
				return type_is_subtype_with_generics(
					base_type,
					base_structure_arguments,
					right_arg,
					ty_structure_arguments,
					behavior,
					environment,
					types,
					mode,
					already_checked,
				);
				//  else {
				// 	return match mode {
				// 		SubTypingMode::Contravariant { depth } => {
				// 			// TODO map error to say it came from a specialisation
				// 			behavior.set_type_argument(base_type, ty, depth, environment, types)
				// 		}
				// 		SubTypingMode::Covariant { position } => {
				// 			// TODO are the arguments in the correct position
				// 			behavior.try_set_contravariant(base_type, ty, position, environment, types)
				// 		}
				// 	};
				// }
			}
		}
		_ => (),
	}

	match left_ty {
		Type::FunctionReference(left_func)
		| Type::SpecialObject(SpecialObjects::Function(left_func, _)) => subtype_function(
			*left_func,
			base_structure_arguments,
			(right_ty, ty, ty_structure_arguments),
			behavior,
			environment,
			types,
			mode,
			already_checked,
		),
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
			let result = subtype_properties(
				base_type,
				base_structure_arguments,
				ty,
				ty_structure_arguments,
				behavior,
				environment,
				types,
				mode,
				already_checked,
			);
			let _left = print_type(base_type, types, environment, true);

			// crate::utils::notify!("Left object {}", left);

			if let SubTypeResult::IsNotSubType(..) = result {
				result
			} else {
				SubTypeResult::IsSubType
			}
		}
		Type::And(left, right) => {
			let right = *right;
			crate::utils::notify!("AND: Checking left and right");
			let left_result = type_is_subtype_with_generics(
				*left,
				base_structure_arguments,
				ty,
				ty_structure_arguments,
				behavior,
				environment,
				types,
				mode,
				already_checked,
			);

			if let SubTypeResult::IsSubType = left_result {
				type_is_subtype_with_generics(
					right,
					base_structure_arguments,
					ty,
					ty_structure_arguments,
					behavior,
					environment,
					types,
					mode,
					already_checked,
				)
			} else {
				// Return bad result
				left_result
			}
		}
		Type::Or(left, right) => {
			let right = *right;
			let start = already_checked.len();
			let left_result = type_is_subtype_with_generics(
				*left,
				base_structure_arguments,
				ty,
				ty_structure_arguments,
				behavior,
				environment,
				types,
				mode,
				already_checked,
			);

			if let SubTypeResult::IsSubType = left_result {
				// TODO only for double generics specialisation. Otherwise short-circuiting is fine
				let _res = type_is_subtype_with_generics(
					right,
					base_structure_arguments,
					ty,
					ty_structure_arguments,
					behavior,
					environment,
					types,
					mode,
					already_checked,
				);
				SubTypeResult::IsSubType
			} else {
				// IMPORTANT: Invalidate any already checked types
				already_checked.drain(start..);

				type_is_subtype_with_generics(
					right,
					base_structure_arguments,
					ty,
					ty_structure_arguments,
					behavior,
					environment,
					types,
					mode,
					already_checked,
				)
			}
		}
		Type::RootPolyType(nature) => {
			// TODO little weird, handing two very different cases beside each other. Might introduce bugs.. :(
			if let Some(args) = base_structure_arguments
				.and_then(|args| args.get_argument(base_type, environment, types))
			{
				// TODO what
				for arg in args {
					let result = type_is_subtype_with_generics(
						arg,
						base_structure_arguments,
						ty,
						ty_structure_arguments,
						behavior,
						environment,
						types,
						mode,
						already_checked,
					);

					if let e @ SubTypeResult::IsNotSubType(_) = result {
						return e;
					}
				}

				SubTypeResult::IsSubType
			} else if let Some(contributions) = behavior.get_contributions() {
				match mode {
					SubTypingMode::Contravariant { depth } => {
						// TODO map error to say it came from a specialisation
						contributions.try_set_contravariant(
							base_type,
							ty,
							depth,
							environment,
							types,
							already_checked,
						)
					}
					SubTypingMode::Covariant { position } => {
						// TODO are the arguments in the correct position
						contributions.try_set_covariant(
							base_type,
							ty,
							position,
							environment,
							types,
							already_checked,
						)
					}
				}
			} else {
				// TODO what does this do
				// TODO temp fix
				if let Type::Constructor(c) = right_ty {
					crate::utils::notify!("TODO right hand side maybe okay");
					if let Some(to) = c.get_base() {
						if to == base_type {
							return SubTypeResult::IsSubType;
						}
					}
				}
				if let PolyNature::FunctionGeneric { .. } = nature {
					/// WIP
					fn check_and_includes(expecting: TypeId, rhs: &Type) -> bool {
						if let Type::And(left, right) = rhs {
							*left == expecting || *right == expecting
						} else {
							false
						}
					}

					return if check_and_includes(base_type, right_ty) {
						SubTypeResult::IsSubType
					} else {
						// Already eliminated equal == case above, so always invalid here
						SubTypeResult::IsNotSubType(NonEqualityReason::GenericParameterMismatch)
					};
				}

				crate::utils::notify!(
					"Subtyping LHS={:?} against RHS, without setting type arguments",
					nature
				);

				let constraint = get_constraint(base_type, types).unwrap();

				type_is_subtype_with_generics(
					constraint,
					base_structure_arguments,
					ty,
					ty_structure_arguments,
					behavior,
					environment,
					types,
					mode,
					already_checked,
				)
			}
		}
		Type::Constructor(super::Constructor::StructureGenerics(StructureGenerics {
			on,
			arguments,
		})) => {
			if let Some(lookup) = types.lookup_generic_map.get(on) {
				fn get_structure_generics_on(
					r#type: &Type,
					expected: TypeId,
				) -> Option<&StructureGenericArguments> {
					match r#type {
						Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
							on,
							arguments,
						})) if expected == *on => Some(arguments),
						_ => None,
					}
				}

				behavior.add_object_mutation_constraint(ty, base_type);
				// TODO a bit of a mess

				return if let Some(_sgs) = get_structure_generics_on(right_ty, *on) {
					crate::utils::notify!("TODO here");
					SubTypeResult::IsSubType
				} else if let Type::Object(super::ObjectNature::RealDeal) = right_ty {
					let prototype =
						environment.get_chain_of_info().find_map(|info| info.prototypes.get(&ty));

					crate::utils::notify!("prototype is {:?}", prototype);

					if prototype.is_some_and(|prototype| prototype == on) {
						for (argument, lookup) in lookup.iter() {
							// TODO no vec
							let backing_type =
								arguments.get_structure_restriction(*argument).unwrap();
							for value in lookup.calculate_lookup(environment, ty) {
								let type_is_subtype = type_is_subtype(
									backing_type,
									value,
									behavior,
									environment,
									types,
								);
								if let e @ SubTypeResult::IsNotSubType(_) = type_is_subtype {
									return e;
								}
							}
						}
						SubTypeResult::IsSubType
					} else {
						crate::utils::notify!("Here");
						SubTypeResult::IsSubType
					}
				} else {
					crate::utils::notify!("Here");
					SubTypeResult::IsSubType
				};
			}

			if let TypeId::ARRAY_TYPE = *on {
				let backing_type = arguments
					.get_structure_restriction(TypeId::T_TYPE)
					.expect("array T argument not set ?");

				crate::utils::notify!(
					"Array type is {}",
					print_type(backing_type, types, environment, false)
				);

				// TODO temp fix for general parameters
				if let Type::Object(_) = right_ty {
					// let Some(lookup_restriction) =
					// 	types.get_look_up_generic_from_prototype(TypeId::ARRAY_TYPE, ty)
					// else {
					// 	crate::utils::notify!("Here");
					// 	return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
					// };

					// TODO don't create vec
					// for value in lookup_restriction.calculate_lookup(environment) {

					// }

					behavior.add_object_mutation_constraint(ty, base_type);

					SubTypeResult::IsSubType
				} else if let Type::Constructor(Constructor::StructureGenerics(
					StructureGenerics { on: TypeId::ARRAY_TYPE, arguments: right_arguments },
				)) = right_ty
				{
					let left_arg = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
					let right_arg =
						right_arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();

					crate::utils::notify!("{:?} :> {:?}", left_arg, right_arg);

					// TODO unsure about arguments here
					type_is_subtype_with_generics(
						left_arg,
						base_structure_arguments,
						right_arg,
						ty_structure_arguments,
						behavior,
						environment,
						types,
						mode,
						already_checked,
					)
				} else {
					crate::utils::notify!("Not array-ish {:?}", right_ty);
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			} else {
				let base_type_arguments =
					GenericChainLink::append(base_structure_arguments.as_ref(), arguments);

				type_is_subtype_with_generics(
					*on,
					base_type_arguments,
					ty,
					ty_structure_arguments,
					behavior,
					environment,
					types,
					mode,
					already_checked,
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
				condition: _,
				truthy_result: _,
				otherwise_result: _,
				result_union: _,
			} => todo!(),
			Constructor::Image { on: _, with: _, result: _ } => todo!(),
			Constructor::Property { on, under, result: _, bind_this: _ } => {
				// Ezno custom behavior
				// TODO might be based of T
				if let Type::Constructor(Constructor::Property {
					on: r_on,
					under: r_under,
					result: _,
					bind_this: _,
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
			Constructor::Awaited { .. } => todo!(),
		},
		// TODO aliasing might work differently
		Type::AliasTo { to, parameters, name: _ } => {
			if base_type == TypeId::LITERAL_RESTRICTION {
				crate::utils::notify!("Here");
				return if let Type::Constant(rhs_constant) = right_ty {
					type_is_subtype_with_generics(
						*to,
						base_structure_arguments,
						rhs_constant.get_backing_type_id(),
						ty_structure_arguments,
						behavior,
						environment,
						types,
						mode,
						already_checked,
					)
				} else {
					// TODO what about if the rhs == TypeId::CONSTANT_RESTRICTION
					// TODO non-constant error
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				};
			}

			let base_structure_arguments = if let Some(parameters) = parameters {
				crate::utils::notify!("Skipping looking at parameters {:?}", parameters);
				base_structure_arguments
			} else {
				base_structure_arguments
			};

			type_is_subtype_with_generics(
				*to,
				base_structure_arguments,
				ty,
				ty_structure_arguments,
				behavior,
				environment,
				types,
				mode,
				already_checked,
			)
		}
		// TODO WIP
		Type::Class { .. } => match right_ty {
			Type::Constant(constant) => {
				if constant.get_backing_type_id() == base_type {
					SubTypeResult::IsSubType
				} else {
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			}
			Type::Object(..) => subtype_properties(
				base_type,
				base_structure_arguments,
				ty,
				ty_structure_arguments,
				behavior,
				environment,
				types,
				mode,
				already_checked,
			),
			_ => SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch),
		},
		Type::Interface { nominal: base_type_nominal, .. } => {
			// If type matched type it would have been cleared before. So looking at properties and
			// prototypes here

			// These do **NOT** check for properties
			let skip_nominal_branch = *base_type_nominal
				&& !matches!(
					right_ty,
					Type::RootPolyType(..)
						| Type::Constructor(..) | Type::Constant(..)
						| Type::Or(..) | Type::And(..)
				);

			if skip_nominal_branch {
				crate::utils::notify!(
					"Short circuited {:?} is nominal and RHS={:?}",
					left_ty,
					right_ty
				);
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
				Type::Object(..) => subtype_properties(
					base_type,
					base_structure_arguments,
					ty,
					ty_structure_arguments,
					behavior,
					environment,
					types,
					mode,
					already_checked,
				),
				Type::SpecialObject(SpecialObjects::Function(..)) => {
					crate::utils::notify!("TODO implement function checking");
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
				Type::And(a, b) => {
					// TODO more
					crate::utils::notify!("Here LHS interface, RHS and");
					if *a == base_type || *b == base_type {
						SubTypeResult::IsSubType
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				}
				Type::Or(_left, _right) => {
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
				})) => type_is_subtype_with_generics(
					base_type,
					base_structure_arguments,
					*on,
					GenericChainLink::append(ty_structure_arguments.as_ref(), arguments),
					behavior,
					environment,
					types,
					mode,
					already_checked,
				),
				Type::AliasTo { .. } | Type::Interface { .. } => {
					crate::utils::notify!("lhs={:?} rhs={:?}", left_ty, right_ty);
					// TODO
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
				Type::Constructor(..) | Type::RootPolyType(..) => {
					let arg = base_structure_arguments
						.and_then(|args| args.get_argument(base_type, environment, types));

					if let Some(args) = arg {
						for arg in args {
							let result = type_is_subtype_with_generics(
								arg,
								base_structure_arguments,
								ty,
								ty_structure_arguments,
								behavior,
								environment,
								types,
								mode,
								already_checked,
							);

							if let e @ SubTypeResult::IsNotSubType(_) = result {
								return e;
							}
						}
						SubTypeResult::IsSubType
					} else {
						let to = get_constraint(ty, types).unwrap();

						if to == TypeId::ANY_TYPE {
							crate::utils::notify!("Modify constraint for equality");
						}

						type_is_subtype_with_generics(
							base_type,
							base_structure_arguments,
							to,
							ty_structure_arguments,
							behavior,
							environment,
							types,
							mode,
							already_checked,
						)
					}
				}
				Type::FunctionReference(_) => todo!(),
				Type::SpecialObject(_) => todo!(),
				Type::Class { .. } => todo!(),
			}
		}
		Type::SpecialObject(_) => todo!(),
	}
}

#[allow(clippy::too_many_arguments)]
fn subtype_function<'a, T: SubTypeBehavior<'a>>(
	left_func: crate::FunctionId,
	base_type_arguments: GenericChain,
	(right_ty, ty, right_type_arguments): (&Type, TypeId, GenericChain),
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
	mode: SubTypingMode,
	already_checked: &mut AlreadyChecked,
) -> SubTypeResult {
	crate::utils::notify!("Subtyping a function");

	let right_func = if let Type::FunctionReference(right_func)
	| Type::SpecialObject(SpecialObjects::Function(right_func, _)) = right_ty
	{
		right_func
	} else if let Some(constraint) = get_constraint(ty, types) {
		// TODO explain why get_constraint early breaks a bunch of tests
		let right_ty = types.get_type_by_id(constraint);
		if let Type::FunctionReference(right_func)
		| Type::SpecialObject(SpecialObjects::Function(right_func, _)) = right_ty
		{
			right_func
		} else {
			crate::utils::notify!("Not function after constraint!! {:?}", right_ty);
			return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
		}
	} else {
		crate::utils::notify!("Not function!! {:?}", right_ty);
		return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
	};

	let left_func = types.functions.get(&left_func).unwrap();
	let right_func = types.functions.get(right_func).unwrap();

	for (idx, lhs_param) in left_func.parameters.parameters.iter().enumerate() {
		match right_func.parameters.get_parameter_type_at_index(idx) {
			Some((right_param_ty, position)) => {
				// Reverse is important
				let result = type_is_subtype_with_generics(
					right_param_ty,
					right_type_arguments,
					lhs_param.ty,
					base_type_arguments,
					behavior,
					environment,
					types,
					// !!!
					SubTypingMode::Covariant { position },
					already_checked,
				);

				if let err @ SubTypeResult::IsNotSubType(_) = result {
					let lhs = print_type(right_param_ty, types, environment, true);
					let rhs = print_type(lhs_param.ty, types, environment, true);
					crate::utils::notify!(
						"Parameter invalid rhs ({:?} {:?}) <- lhs ({:?} {:?})",
						rhs,
						right_type_arguments,
						lhs,
						base_type_arguments
					);
					// TODO don't short circuit
					return err;
				}
			}
			None => {
				if !lhs_param.is_optional {
					crate::utils::notify!("Expected parameter, for non optional parameter");
					return SubTypeResult::IsNotSubType(NonEqualityReason::MissingParameter);
				}
			}
		}
	}
	// TODO optional and rest parameters

	// `void` return type means anything goes here
	if TypeId::VOID_TYPE == left_func.return_type {
		SubTypeResult::IsSubType
	} else {
		let type_is_subtype_with_generics = type_is_subtype_with_generics(
			left_func.return_type,
			base_type_arguments,
			right_func.return_type,
			right_type_arguments,
			behavior,
			environment,
			types,
			mode,
			already_checked,
		);

		if let SubTypeResult::IsNotSubType(_) = type_is_subtype_with_generics {
			crate::utils::notify!("return type invalid");
		}

		type_is_subtype_with_generics
	}
}

#[allow(clippy::too_many_arguments)]
fn subtype_properties<'a, T: SubTypeBehavior<'a>>(
	base_type: TypeId,
	base_type_arguments: GenericChain,
	ty: TypeId,
	right_type_arguments: GenericChain,
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
	mode: SubTypingMode,
	already_checked: &mut AlreadyChecked,
) -> SubTypeResult {
	let mode = mode.one_deeper();

	let mut property_errors = Vec::new();
	let reversed_flattened_properties = environment
		.get_chain_of_info()
		.filter_map(|info| info.current_properties.get(&base_type).map(|v| v.iter().rev()))
		.flatten();

	for (publicity, key, lhs_property) in reversed_flattened_properties {
		crate::utils::notify!("key {:?} with {:?}", key, base_type_arguments);

		let key = match key {
			PropertyKey::Type(ty) => PropertyKey::from_type(
				base_type_arguments.unwrap().get_single_argument(*ty).unwrap_or(*ty),
				types,
			),
			PropertyKey::String(_) => key.clone(),
		};

		let result = check_lhs_property_is_super_type_of_rhs(
			&key,
			lhs_property,
			base_type_arguments,
			ty,
			right_type_arguments,
			*publicity,
			types,
			environment,
			behavior,
			mode,
			already_checked,
		);

		if let Err(err) = result {
			property_errors.push((key.into_owned(), err));
		}
	}

	if property_errors.is_empty() {
		// TODO type arguments
		behavior.add_object_mutation_constraint(ty, base_type);

		if let Some(extends) = types.interface_extends.get(&base_type) {
			type_is_subtype_with_generics(
				*extends,
				base_type_arguments,
				ty,
				right_type_arguments,
				behavior,
				environment,
				types,
				mode,
				already_checked,
			)
		} else {
			SubTypeResult::IsSubType
		}
	} else {
		SubTypeResult::IsNotSubType(NonEqualityReason::PropertiesInvalid {
			errors: property_errors,
		})
	}
}

#[allow(clippy::too_many_arguments)]
fn check_lhs_property_is_super_type_of_rhs<'a, T: SubTypeBehavior<'a>>(
	key: &PropertyKey<'_>,
	lhs_property: &PropertyValue,
	base_type_arguments: GenericChain,
	ty: TypeId,
	right_type_arguments: GenericChain,
	publicity: Publicity,
	types: &TypeStore,
	environment: &Environment,
	behavior: &mut T,
	mode: SubTypingMode,
	already_checked: &mut AlreadyChecked,
) -> Result<(), PropertyError> {
	match lhs_property {
		PropertyValue::Value(lhs_value) => {
			let rhs_property = get_property_unbound(ty, publicity, key, types, environment);
			crate::utils::notify!("looking for {:?} found {:?}", key, rhs_property);

			match rhs_property {
				Ok(rhs_property) => {
					let res = check_logical_property(
						*lhs_value,
						base_type_arguments,
						rhs_property,
						right_type_arguments,
						behavior,
						environment,
						types,
						mode,
						already_checked,
					);
					match res {
						SubTypeResult::IsSubType => Ok(()),
						SubTypeResult::IsNotSubType(err) => Err(PropertyError::Invalid {
							expected: TypeId::UNIMPLEMENTED_ERROR_TYPE,
							found: TypeId::UNIMPLEMENTED_ERROR_TYPE,
							mismatch: err,
						}),
					}
				}
				// TODO
				Err(..) => Err(PropertyError::Missing),
			}
		}
		PropertyValue::Getter(_) => todo!(),
		PropertyValue::Setter(_) => todo!(),
		PropertyValue::Deleted => {
			// TODO WIP
			let res = get_property_unbound(ty, publicity, key, types, environment);
			if res.is_ok() {
				// TODO the opposite of missing
				Err(PropertyError::Missing)
			} else {
				// Fine !
				Ok(())
			}
		}
		PropertyValue::Dependent { on: _, truthy, otherwise } => {
			let lhs = check_lhs_property_is_super_type_of_rhs(
				key,
				truthy,
				base_type_arguments,
				ty,
				right_type_arguments,
				publicity,
				types,
				environment,
				behavior,
				mode,
				already_checked,
			);
			if lhs.is_err() {
				check_lhs_property_is_super_type_of_rhs(
					key,
					otherwise,
					base_type_arguments,
					ty,
					right_type_arguments,
					publicity,
					types,
					environment,
					behavior,
					mode,
					already_checked,
				)
			} else {
				lhs
			}
		}
	}
}

#[allow(clippy::too_many_arguments)]
fn check_logical_property<'a, T: SubTypeBehavior<'a>>(
	base: TypeId,
	base_type_arguments: GenericChain,
	rhs_property: Logical<PropertyValue>,
	right_type_arguments: GenericChain,
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
	mode: SubTypingMode,
	already_checked: &mut AlreadyChecked,
) -> SubTypeResult {
	match rhs_property {
		Logical::Pure(rhs_property) => {
			let rhs_type = rhs_property.as_set_type();
			// crate::utils::notify!(
			// 	"Checking {} with {}, against {}, left={:?}",
			// 	print_type(key, types, environment, true),
			// 	print_type(property, types, environment, true),
			// 	print_type(rhs_type, types, environment, true),
			// 	base_type_arguments
			// );

			type_is_subtype_with_generics(
				base,
				base_type_arguments,
				rhs_type,
				right_type_arguments,
				behavior,
				environment,
				types,
				mode,
				already_checked,
			)
		}
		Logical::Or { .. } => todo!(),
		Logical::Implies { on, antecedent } => check_logical_property(
			base,
			GenericChainLink::append(base_type_arguments.as_ref(), &antecedent),
			*on,
			right_type_arguments,
			behavior,
			environment,
			types,
			mode,
			already_checked,
		),
	}
}

/// TODO integrate `set_restriction`, but it can't create a type ? maybe object restriction should be logically.
/// maybe sub function
pub fn type_is_subtype_of_property<'a, T: SubTypeBehavior<'a>>(
	property: &Logical<PropertyValue>,
	property_generics: GenericChain,
	ty: TypeId,
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
) -> SubTypeResult {
	match property {
		Logical::Pure(prop) => type_is_subtype_with_generics(
			prop.as_set_type(),
			property_generics,
			ty,
			GenericChain::None,
			behavior,
			environment,
			types,
			Default::default(),
			&mut Default::default(),
		),
		Logical::Or { .. } => {
			todo!()
			// let left_result = type_is_subtype_of_property(
			// 	left,
			// 	property_generics,
			// 	ty,
			// 	behavior,
			// 	environment,
			// 	types,
			// );
			// if let SubTypeResult::IsSubType = left_result {
			// 	left_result
			// } else {
			// 	type_is_subtype_of_property(
			// 		right,
			// 		property_generics,
			// 		ty,
			// 		behavior,
			// 		environment,
			// 		types,
			// 	)
			// }
		}
		Logical::Implies { on, antecedent } => type_is_subtype_of_property(
			on,
			GenericChainLink::append(property_generics.as_ref(), antecedent),
			ty,
			behavior,
			environment,
			types,
		),
	}
}

impl NonEqualityReason {
	pub(crate) fn _into_error_message(self, _environment: &GeneralContext) -> Vec<String> {
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
