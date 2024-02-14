//! Type subtype checking. (making sure the RHS type contains all the properties the LHS type requires)

use source_map::SpanWithSource;

use crate::{
	context::{
		information::{get_properties_on_type, get_property_unbound},
		Environment, GeneralContext, Logical,
	},
	features::objects::SpecialObjects,
	types::{printing::print_type, TypeStore},
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
	/// 	   ^
	/// ```
	Contravariant { depth: u8 },
	/// From subtyping against a function
	///
	/// ```ts
	/// <T>(cb: (t: T) => bool) => T
	/// 	        ^
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
pub fn type_is_subtype<T: SubTypeBehavior>(
	base_type: TypeId,
	ty: TypeId,
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
) -> SubTypeResult {
	type_is_subtype_with_generics(
		base_type,
		None,
		ty,
		None,
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
pub(crate) fn type_is_subtype_with_generics<T: SubTypeBehavior>(
	base_type: TypeId,
	base_structure_arguments: Option<GenericChain>,
	ty: TypeId,
	ty_structure_arguments: Option<GenericChain>,
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
	mode: SubTypingMode,
	already_checked: &mut AlreadyChecked,
) -> SubTypeResult {
	{
		let debug = false;
		crate::utils::notify!(
			"Checking {} :>= {}",
			print_type(base_type, types, environment, debug),
			print_type(ty, types, environment, debug)
		);
	}

	if (base_type == TypeId::ERROR_TYPE || base_type == TypeId::ANY_TYPE)
		|| (ty == TypeId::ERROR_TYPE || ty == TypeId::NEVER_TYPE)
	{
		return SubTypeResult::IsSubType;
	}

	if base_type == ty {
		return SubTypeResult::IsSubType;
	}

	// Prevents cycles
	if already_checked.iter().any(|(a, b)| *a == base_type && *b == ty) {
		return SubTypeResult::IsSubType;
	} else {
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
				ty_structure_arguments.and_then(|args| args.get_arg(ty, environment))
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
			} else {
				// If lhs is not operator unless argument is operator
				// if !T::INFER_GENERICS && ty_structure_arguments.is_none() {
				let arg = get_constraint(ty, types).unwrap();
				// This is important that LHS is not operator
				let edge_case = (left_ty.is_operator() && !types.get_type_by_id(arg).is_operator())
					|| matches!(
						left_ty,
						Type::RootPolyType(PolyNature::Generic { .. }) | Type::Constructor(..)
					);

				if !edge_case {
					return type_is_subtype_with_generics(
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
				}
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
			left_func,
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
			let result = check_properties(
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
				let type_is_subtype_with_generics = type_is_subtype_with_generics(
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

				type_is_subtype_with_generics
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
			if let Some(args) =
				base_structure_arguments.and_then(|args| args.get_arg(base_type, environment))
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

				return SubTypeResult::IsSubType;
			} else if !T::INFER_GENERICS && base_structure_arguments.is_none() {
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
				if let PolyNature::Generic { .. } = nature {
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
			} else {
				match mode {
					SubTypingMode::Contravariant { depth } => {
						// TODO map error to say it came from a specialisation
						behavior.set_type_argument(base_type, ty, depth, environment, types)
					}
					SubTypingMode::Covariant { position } => {
						// TODO are the arguments in the correct position
						behavior.try_set_contravariant(base_type, ty, position, environment, types)
					}
				}
			}
		}
		Type::Constructor(super::Constructor::StructureGenerics(StructureGenerics {
			on,
			arguments,
		})) => {
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
					let Some(lookup_restriction) =
						types.get_look_up_generic_from_prototype(TypeId::ARRAY_TYPE, ty)
					else {
						crate::utils::notify!("Here");
						return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
					};

					// TODO don't create vec
					for value in lookup_restriction.calculate_lookup(environment) {
						if let e @ SubTypeResult::IsNotSubType(_) =
							type_is_subtype(backing_type, value, behavior, environment, types)
						{
							return e;
						}
					}

					behavior.add_object_mutation_constraint(ty, base_type);

					SubTypeResult::IsSubType
				} else if let Type::Constructor(Constructor::StructureGenerics(
					StructureGenerics { on: TypeId::ARRAY_TYPE, arguments: right_arguments },
				)) = right_ty
				{
					let left_arg = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
					let right_arg =
						right_arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
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
					crate::utils::notify!("Not array-ish");
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			} else {
				let base_type_arguments =
					Some(GenericChain::append(base_structure_arguments.as_ref(), arguments));
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
				else_result: _,
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
		},
		// TODO aliasing might work differently
		Type::AliasTo { to, parameters, name: _ } => {
			if parameters.is_some() {
				todo!()
			}
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
				Type::Object(..) => check_properties(
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
					Some(GenericChain::append(ty_structure_arguments.as_ref(), arguments)),
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
						.and_then(|args| args.get_arg(base_type, environment));

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
						return SubTypeResult::IsSubType;
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
			}
		}
		Type::SpecialObject(_) => todo!(),
	}
}

fn subtype_function<T: SubTypeBehavior>(
	left_func: &crate::FunctionId,
	base_type_arguments: Option<GenericChain>,
	(right_ty, ty, right_type_arguments): (&Type, TypeId, Option<GenericChain>),
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

	let left_func = types.functions.get(left_func).unwrap();
	let right_func = types.functions.get(right_func).unwrap();

	for (idx, lhs_param) in left_func.parameters.parameters.iter().enumerate() {
		match right_func.parameters.get_type_constraint_at_index(idx) {
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
				if !lhs_param.optional {
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
fn check_properties<T: SubTypeBehavior>(
	base_type: TypeId,
	base_type_arguments: Option<GenericChain>,
	ty: TypeId,
	right_type_arguments: Option<GenericChain>,
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
	mode: SubTypingMode,
	already_checked: &mut AlreadyChecked,
) -> SubTypeResult {
	let mode = mode.one_deeper();

	let mut property_errors = Vec::new();
	for (publicity, key, property) in get_properties_on_type(base_type, environment) {
		let rhs_property = get_property_unbound(ty, publicity, key.clone(), types, environment);

		match rhs_property {
			Some(rhs_property) => {
				check_logical(
					rhs_property,
					base_type_arguments,
					property,
					right_type_arguments,
					behavior,
					environment,
					types,
					mode,
					already_checked,
					&mut property_errors,
					key,
				);
			}
			// TODO
			None => {
				property_errors.push((key, PropertyError::Missing));
			}
		}
	}

	if property_errors.is_empty() {
		// TODO type arguments
		behavior.add_object_mutation_constraint(ty, base_type);

		SubTypeResult::IsSubType
	} else {
		SubTypeResult::IsNotSubType(NonEqualityReason::PropertiesInvalid {
			errors: property_errors,
		})
	}
}

fn check_logical<T: SubTypeBehavior>(
	rhs_property: Logical<PropertyValue>,
	base_type_arguments: Option<GenericChain>,
	property: TypeId,
	right_type_arguments: Option<GenericChain>,
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
	mode: SubTypingMode,
	already_checked: &mut AlreadyChecked,
	property_errors: &mut Vec<(PropertyKey<'static>, PropertyError)>,
	key: PropertyKey<'_>,
) {
	match rhs_property {
		Logical::Error => {}
		Logical::Pure(rhs_property) => {
			let rhs_type = rhs_property.as_set_type();
			// crate::utils::notify!(
			// 	"Checking {} with {}, against {}, left={:?}",
			// 	print_type(key, types, environment, true),
			// 	print_type(property, types, environment, true),
			// 	print_type(rhs_type, types, environment, true),
			// 	base_type_arguments
			// );

			let result = type_is_subtype_with_generics(
				property,
				base_type_arguments,
				rhs_type,
				right_type_arguments,
				behavior,
				environment,
				types,
				mode,
				already_checked,
			);

			if let SubTypeResult::IsNotSubType(mismatch) = result {
				property_errors.push((
					key.into_owned(),
					PropertyError::Invalid { expected: property, found: rhs_type, mismatch },
				));
			}
		}
		Logical::Or { .. } => todo!(),
		Logical::Implies { on, antecedent } => check_logical(
			*on,
			Some(GenericChain::append(base_type_arguments.as_ref(), &antecedent)),
			property,
			right_type_arguments,
			behavior,
			environment,
			types,
			mode,
			already_checked,
			property_errors,
			key,
		),
	}
}

/// TODO integrate `set_restriction`, but it can't create a type ? maybe object restriction should be logically.
/// maybe sub function
pub fn type_is_subtype_of_property<T: SubTypeBehavior>(
	property: &Logical<PropertyValue>,
	property_generics: Option<GenericChain>,
	ty: TypeId,
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
) -> SubTypeResult {
	match property {
		Logical::Error => SubTypeResult::IsSubType,
		Logical::Pure(prop) => type_is_subtype_with_generics(
			prop.as_set_type(),
			property_generics,
			ty,
			None,
			behavior,
			environment,
			types,
			Default::default(),
			&mut Default::default(),
		),
		Logical::Or { left, right } => {
			let left_result = type_is_subtype_of_property(
				left,
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
					right,
					property_generics,
					ty,
					behavior,
					environment,
					types,
				)
			}
		}
		Logical::Implies { on, antecedent } => type_is_subtype_of_property(
			on,
			Some(GenericChain::append(property_generics.as_ref(), antecedent)),
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
