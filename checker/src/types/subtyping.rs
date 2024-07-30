//! Type subtype checking. (making sure the RHS type contains all the properties the LHS type requires)

use source_map::SpanWithSource;

use crate::{
	context::{GeneralContext, InformationChain},
	features::{objects::SpecialObject, operations::MathematicalAndBitwise},
	types::{
		generics::{
			chain::{GenericChain, GenericChainLink, SpecialGenericChainLink},
			contributions::{ContributionDepth, Contributions, CovariantContribution, TriMap},
			generic_type_arguments::GenericArguments,
		},
		intrinsics::apply_string_intrinsic,
		logical::*,
		printing::print_type,
		properties::{
			get_properties_on_single_type2, get_property_unbound, key_matches, Publicity,
		},
		ObjectNature, Type, TypeStore,
	},
	Constant, Environment, PropertyValue, TypeId,
};

use super::{
	get_constraint, properties::PropertyKey, Constructor, PartiallyAppliedGenerics, PolyNature,
};

pub use super::{NonEqualityReason, PropertyError};

// TODO implement `Try` / `?` on `SubTypeResult`
#[derive(Debug)]
pub enum SubTypeResult {
	IsSubType,
	IsNotSubType(NonEqualityReason),
}

impl SubTypeResult {
	#[must_use]
	pub fn is_mismatch(&self) -> bool {
		matches!(self, Self::IsNotSubType(..))
	}

	#[must_use]
	pub fn is_subtype(&self) -> bool {
		matches!(self, Self::IsSubType)
	}
}

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

// TODO these methods are bound to cause trouble
impl SubTypingMode {
	pub(crate) fn one_deeper(self) -> SubTypingMode {
		match self {
			SubTypingMode::Contravariant { depth } => Self::Contravariant { depth: depth + 1 },
			o @ SubTypingMode::Covariant { .. } => o,
		}
	}

	pub(crate) fn one_shallower(self) -> SubTypingMode {
		match self {
			SubTypingMode::Contravariant { depth } => {
				Self::Contravariant { depth: depth.saturating_sub(1) }
			}
			o @ SubTypingMode::Covariant { .. } => o,
		}
	}
}

mod musing {
	use crate::TypeId;

	enum _AddPropertyConstraint {
		/// For `satisfies`
		No,
		/// For variables
		Yes,
		/// Only for functions with things, specified by a field that doesn't exist
		OnlyFor(Vec<TypeId>),
	}
}

#[derive(Debug, Clone, Copy)]
pub struct SubTypingOptions {
	/// Don't allow `ERROR_TYPE` to pass as everything. This allows using `satisfies` to check that LHS != error
	pub allow_errors: bool,
}

impl Default for SubTypingOptions {
	fn default() -> Self {
		Self { allow_errors: true }
	}
}

impl SubTypingOptions {
	#[must_use]
	pub fn satisfies() -> Self {
		Self { allow_errors: false }
	}
}

/// Use for assignments, declaration etc
pub fn type_is_subtype_object(
	base_type: TypeId,
	ty: TypeId,
	environment: &mut Environment,
	types: &mut TypeStore,
) -> SubTypeResult {
	let mut state = State {
		already_checked: Vec::new(),
		mode: SubTypingMode::default(),
		contributions: None,
		others: SubTypingOptions { allow_errors: true },
		object_constraints: Some(Vec::new()),
	};

	let result = type_is_subtype(base_type, ty, &mut state, environment, types);

	environment.add_object_constraints(state.object_constraints.unwrap().into_iter(), types);
	// TODO information.add_inferred_constraints(x, types);

	result
}

/// Checks whether `ty` is a subtype of the `base_type`
/// - equivalently ...`base_type :>= ty` (`ty <=: base_type`)
/// - equivalently ... whether `ty` could be substituted as `base_type`.
/// - equivalently ... whether `ty`'s properties imply the existence of `base_type` properties.
pub fn type_is_subtype(
	base_type: TypeId,
	ty: TypeId,
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> SubTypeResult {
	type_is_subtype_with_generics(
		(base_type, GenericChain::None),
		(ty, GenericChain::None),
		state,
		information,
		types,
	)
}

/// Using `Vec` as it needs to do a sequential removal
pub type AlreadyChecked = Vec<(TypeId, TypeId)>;

/// Additional information during subtype checking
// TODO pub constraint_inference_requests: Vec<TypeId, TypeId>
pub struct State<'a> {
	/// Prevents cycles
	pub already_checked: AlreadyChecked,
	pub mode: SubTypingMode,
	/// TODO with slices and commit / head length
	pub contributions: Option<Contributions<'a>>,
	/// `None` if satisfies or parameters
	pub object_constraints: Option<Vec<(TypeId, TypeId)>>,
	pub others: SubTypingOptions,
}

pub type StateSavePoint = [u16; 4];

impl<'a> State<'a> {
	/// For `or`s, some items might have to be removed if the branch fails
	#[must_use]
	pub fn produce_save_point(&self) -> StateSavePoint {
		let contributions = self.contributions.as_ref();
		[
			self.already_checked.len() as u16,
			contributions.map_or(0, |c| c.staging_covariant.len().try_into().unwrap()),
			contributions.map_or(0, |c| c.staging_contravariant.len().try_into().unwrap()),
			self.object_constraints.as_ref().map_or(0, |c| c.len().try_into().unwrap()),
		]
	}

	/// For setting the state back to where it was at the point of [`Self::produce_save_point`]
	pub fn reset(&mut self, last: StateSavePoint) {
		let [already_checked, contributions_covariant, contributions_contravariant, object_constraint_count] =
			last;

		self.already_checked.drain((already_checked as usize)..);
		if let Some(ref mut contributions) = self.contributions {
			contributions.staging_covariant.drop_range((contributions_covariant as usize)..);
			contributions
				.staging_contravariant
				.drop_range((contributions_contravariant as usize)..);
		}
		if let Some(ref mut object_constraints) = self.object_constraints {
			object_constraints.drain((object_constraint_count as usize)..);
		}
	}
}

pub(crate) fn type_is_subtype_with_generics(
	(base_type, base_type_arguments): (TypeId, GenericChain),
	(ty, ty_structure_arguments): (TypeId, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> SubTypeResult {
	{
		let debug = true;
		crate::utilities::notify!(
			"Checking {} :>= {}, with {:?}",
			print_type(base_type, types, information, debug),
			print_type(ty, types, information, debug),
			base_type_arguments
		);
	}

	if base_type == TypeId::ANY_TYPE || ty == TypeId::NEVER_TYPE {
		return SubTypeResult::IsSubType;
	}

	if base_type == ty {
		return SubTypeResult::IsSubType;
	}

	{
		// Prevents cycles
		if state.already_checked.iter().any(|(a, b)| *a == base_type && *b == ty) {
			return SubTypeResult::IsSubType;
		}

		state.already_checked.push((base_type, ty));
	}

	let left_ty = types.get_type_by_id(base_type);
	let right_ty = types.get_type_by_id(ty);

	// Eager things
	match right_ty {
		Type::Or(left, right) => {
			let right = *right;
			crate::utilities::notify!("OR RHS: left and right");
			let left_result = type_is_subtype_with_generics(
				(base_type, base_type_arguments),
				(*left, ty_structure_arguments),
				state,
				information,
				types,
			);

			return if let SubTypeResult::IsSubType = left_result {
				type_is_subtype_with_generics(
					(base_type, base_type_arguments),
					(right, ty_structure_arguments),
					state,
					information,
					types,
				)
			} else {
				// else return the failing result
				left_result
			};
		}
		// Type::And(left, right) => {
		// 	let left_is_operator_right_is_not =
		// 		left_ty.is_operator();

		// 	// edge cases on edge cases
		// 	// If any of these are true. Then do not perform constraint argument lookup
		// 	let edge_case = left_is_operator_right_is_not;

		// 	if !edge_case {

		// 	let right = *right;
		// 	let left_result = type_is_subtype_with_generics(
		// 		(base_type, base_type_arguments),
		// 		(*left, ty_structure_arguments),
		// 		state,
		// 		information,
		// 		types,
		// 	);

		// 	return if let SubTypeResult::IsSubType = left_result {
		// 		left_result
		// 	} else {
		// 		type_is_subtype_with_generics(
		// 			(base_type, base_type_arguments),
		// 			(right, ty_structure_arguments),
		// 			state,
		// 			information,
		// 			types,
		// 		)
		// 	};
		// }
		// }
		// TODO others
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: on @ TypeId::NOT_RESTRICTION,
			arguments,
		}) => {
			match *on {
				TypeId::NOT_RESTRICTION => {
					let inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
					// https://leanprover-community.github.io/mathlib4_docs/Mathlib/Data/Set/Basic.html#Set.subset_compl_comm -> https://leanprover-community.github.io/mathlib4_docs/Mathlib/Data/Set/Basic.html#Set.subset_compl_iff_disjoint_left

					// Swapped
					let result = super::disjoint::types_are_disjoint(
						inner,
						ty,
						&mut state.already_checked,
						information,
						types,
					);
					// crate::utilities::notify!("Here {:?}", (&result, inner));
					return if result {
						SubTypeResult::IsSubType
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					};
				}
				_ => unreachable!(),
			}
		}
		t @ (Type::RootPolyType(..) | Type::Constructor(..)) => {
			if let Type::RootPolyType(PolyNature::Error(to)) = t {
				// (unless specified) treat as subtype as error would have already been thrown
				return if state.others.allow_errors && *to == TypeId::ANY_TYPE {
					SubTypeResult::IsSubType
				} else {
					type_is_subtype(base_type, *to, state, information, types)
				};
			}

			if let Some(args) =
				ty_structure_arguments.and_then(|tas| tas.get_argument(ty, information, types))
			{
				// TODO what
				for arg in args {
					let result = type_is_subtype_with_generics(
						(base_type, base_type_arguments),
						(arg, ty_structure_arguments),
						state,
						information,
						types,
					);

					if let e @ SubTypeResult::IsNotSubType(_) = result {
						return e;
					}
				}

				return SubTypeResult::IsSubType;
			}

			if let SubTypingMode::Covariant { position: _ } = state.mode {
				crate::utilities::notify!("Here covariant parameter chaos?");
				// if let Some(ref mut contributions) = state.contributions {
				// 	contributions.staging_covariant.insert(ty, (base_type, position))
				// }
				// return SubTypeResult::IsSubType;
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
				) || matches!(left_ty, Type::Constructor(..));

			if !edge_case {
				let result = type_is_subtype_with_generics(
					(base_type, base_type_arguments),
					(right_arg, ty_structure_arguments),
					state,
					information,
					types,
				);

				// TODO is the above event needed or constructor with constraint == TypeId::ANY_TYPE
				return if result.is_mismatch()
					&& matches!(right_ty, Type::RootPolyType(root) if root.is_inferrable())
				{
					crate::utilities::notify!("Setting inferred request");
					// state.add_request(ty, base_type);
					SubTypeResult::IsSubType
				} else {
					result
				};
			}
		}
		Type::PartiallyAppliedGenerics(..) => {}

		_ => (),
	}

	match left_ty {
		Type::FunctionReference(left_func)
		| Type::SpecialObject(SpecialObject::Function(left_func, _)) => subtype_function(
			(*left_func, base_type_arguments),
			(right_ty, ty, ty_structure_arguments),
			state,
			information,
			types,
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
				crate::utilities::notify!("Constant {:?} against RHS {:#?}", lhs, right_ty);
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			}
		}
		Type::Object(nature) => {
			if let ObjectNature::RealDeal = nature {
				crate::utilities::notify!(
					"what {:?} (subtyping where LHS = ObjectNature::RealDeal)",
					ty
				);
			}
			// assert!(matches!(nature, ObjectNature::AnonymousTypeAnnotation));

			subtype_properties(
				(base_type, base_type_arguments),
				(ty, ty_structure_arguments),
				state,
				information,
				types,
			)

			// let _left = print_type(base_type, types, information, true);

			// crate::utilities::notify!("Left object {}", left);

			// if let SubTypeResult::IsNotSubType(..) = result {
			// 	result
			// } else {
			// 	SubTypeResult::IsSubType
			// }
		}
		Type::And(left, right) => {
			let right = *right;
			// crate::utilities::notify!("AND: Checking left and right");
			let left_result = type_is_subtype_with_generics(
				(*left, base_type_arguments),
				(ty, ty_structure_arguments),
				state,
				information,
				types,
			);

			if let SubTypeResult::IsSubType = left_result {
				type_is_subtype_with_generics(
					(right, base_type_arguments),
					(ty, ty_structure_arguments),
					state,
					information,
					types,
				)
			} else {
				// Return bad result
				left_result
			}
		}
		Type::Or(left, right) => {
			let right = *right;
			let save_point = state.produce_save_point();

			let left_result = type_is_subtype_with_generics(
				(*left, base_type_arguments),
				(ty, ty_structure_arguments),
				state,
				information,
				types,
			);

			if let SubTypeResult::IsSubType = left_result {
				if state.contributions.is_some() {
					// only for double generics specialisation. Otherwise short-circuiting is fine
					let _res = type_is_subtype_with_generics(
						(right, base_type_arguments),
						(ty, ty_structure_arguments),
						state,
						information,
						types,
					);
				}
				SubTypeResult::IsSubType
			} else {
				// IMPORTANT: Invalidate any already checked types
				state.reset(save_point);

				type_is_subtype_with_generics(
					(right, base_type_arguments),
					(ty, ty_structure_arguments),
					state,
					information,
					types,
				)
			}
		}
		Type::RootPolyType(nature) => {
			if let PolyNature::Error(_) = nature {
				return SubTypeResult::IsSubType;
			}

			// TODO little weird, handing two very different cases beside each other. Might introduce bugs.. :(
			let base_argument_for_current = base_type_arguments
				.and_then(|args| args.get_argument(base_type, information, types));

			if let Some(args) = base_argument_for_current {
				// TODO what
				for arg in args {
					let result = type_is_subtype_with_generics(
						(arg, base_type_arguments),
						(ty, ty_structure_arguments),
						state,
						information,
						types,
					);

					if let e @ SubTypeResult::IsNotSubType(_) = result {
						return e;
					}
				}

				SubTypeResult::IsSubType
			} else if let Some(ref mut contributions) = state.contributions {
				match state.mode {
					SubTypingMode::Contravariant { depth } => {
						// With <*base_type* extends *under> check ty is under

						let result = if let Some(under) =
							contributions.get_standard_restriction(base_type)
						{
							type_is_subtype_with_generics(
								(under, GenericChain::None),
								(ty, ty_structure_arguments),
								state,
								information,
								types,
							)
						} else if let Some(constraint) = nature.try_get_constraint() {
							type_is_subtype_with_generics(
								(constraint, GenericChain::None),
								(ty, ty_structure_arguments),
								state,
								information,
								types,
							)
						} else {
							crate::utilities::notify!("TODO no constraint for {:?}", nature);
							SubTypeResult::IsSubType
						};

						state
							.contributions
							.as_mut()
							.unwrap()
							.staging_contravariant
							.insert(base_type, (ty.into(), depth));

						result
					}
					SubTypingMode::Covariant { position } => {
						state
							.contributions
							.as_mut()
							.unwrap()
							.staging_covariant
							.insert(base_type, (ty, position));

						// TODO temp
						SubTypeResult::IsSubType
					}
				}
			} else {
				// TODO what does this do
				// TODO temp fix
				if let Type::Constructor(c) = right_ty {
					crate::utilities::notify!("TODO right hand side maybe okay");
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

				crate::utilities::notify!(
					"Subtyping LHS={:?} against RHS, without setting type arguments",
					nature
				);

				let constraint = get_constraint(base_type, types).unwrap();

				type_is_subtype_with_generics(
					(constraint, base_type_arguments),
					(ty, ty_structure_arguments),
					state,
					information,
					types,
				)
			}
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, arguments }) => {
			match *on {
				TypeId::READONLY_RESTRICTION => {
					crate::utilities::notify!("TODO temp readonly inner check");
					let inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
					return type_is_subtype_with_generics(
						(
							inner,
							Some(GenericChainLink::SpecialGenericChainLink {
								parent_link: ty_structure_arguments.as_ref(),
								special: SpecialGenericChainLink::Readonly,
							}),
						),
						(ty, base_type_arguments),
						state,
						information,
						types,
					);
				}
				TypeId::EXCLUSIVE_RESTRICTION => {
					let inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
					return type_is_subtype_with_generics(
						(
							inner,
							Some(GenericChainLink::SpecialGenericChainLink {
								parent_link: ty_structure_arguments.as_ref(),
								special: SpecialGenericChainLink::Exclusive,
							}),
						),
						(ty, base_type_arguments),
						state,
						information,
						types,
					);
				}
				TypeId::NOT_RESTRICTION => {
					let inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
					// https://leanprover-community.github.io/mathlib4_docs/Mathlib/Data/Set/Basic.html#Set.subset_compl_iff_disjoint_left

					let result = super::disjoint::types_are_disjoint(
						ty,
						inner,
						&mut state.already_checked,
						information,
						types,
					);
					// crate::utilities::notify!("Here {:?}", (&result, inner));
					return if result {
						SubTypeResult::IsSubType
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					};
				}
				// TSC intrinsics
				TypeId::STRING_CAPITALIZE
				| TypeId::STRING_UNCAPITALIZE
				| TypeId::STRING_LOWERCASE
				| TypeId::STRING_UPPERCASE => {
					if let Type::Constant(Constant::String(rs)) = right_ty {
						let contributions =
							state.contributions.as_mut().map(|n| &mut n.staging_contravariant);
						let matches = slice_matches_type(
							(base_type, base_type_arguments),
							rs,
							contributions,
							information,
							types,
							false,
						);
						return if matches {
							SubTypeResult::IsSubType
						} else {
							// TODO remove contributions
							SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						};
					}
				}
				// Ezno intrinsic
				TypeId::LITERAL_RESTRICTION => {
					let inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
					return if let Type::Constant(rhs_constant) = right_ty {
						type_is_subtype_with_generics(
							(inner, base_type_arguments),
							(rhs_constant.get_backing_type_id(), ty_structure_arguments),
							state,
							information,
							types,
						)
					} else {
						// TODO what about if the rhs == TypeId::CONSTANT_RESTRICTION
						// TODO non-constant error
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					};
				}
				TypeId::NO_INFER => {
					let on = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
					let current_contributing =
						state.contributions.as_ref().map(|c| c.staging_contravariant.len());
					let result = type_is_subtype_with_generics(
						(on, base_type_arguments),
						(ty, ty_structure_arguments),
						state,
						information,
						types,
					);
					// Remove any infer-ed results
					if let (Some(contributions), Some(current_contributing)) =
						(state.contributions.as_mut(), current_contributing)
					{
						contributions.staging_contravariant.drop_range(current_contributing..);
					}
					return result;
				}
				TypeId::LESS_THAN | TypeId::GREATER_THAN | TypeId::MULTIPLE_OF => {
					let argument =
						arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
					return if let (
						Type::Constant(Constant::Number(argument)),
						Type::Constant(Constant::Number(value)),
					) = (types.get_type_by_id(argument), right_ty)
					{
						let result = match *on {
							TypeId::LESS_THAN => value < argument,
							TypeId::GREATER_THAN => value > argument,
							TypeId::MULTIPLE_OF => value % argument == 0f64,
							_ => unreachable!(),
						};
						if result {
							SubTypeResult::IsSubType
						} else {
							SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						}
					} else {
						crate::utilities::notify!(
							"Returning NonEqualityReason::Mismatch {:?}",
							right_ty
						);
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					};
				}
				TypeId::CASE_INSENSITIVE => {
					if let Type::Constant(Constant::String(rs)) = right_ty {
						let contributions =
							state.contributions.as_mut().map(|n| &mut n.staging_contravariant);
						let matches = slice_matches_type(
							(base_type, base_type_arguments),
							rs,
							contributions,
							information,
							types,
							false,
						);
						return if matches {
							SubTypeResult::IsSubType
						} else {
							// TODO remove contributions
							SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						};
					}
				}
				_ => {}
			}

			if let Some(lookup) = types.lookup_generic_map.get(on) {
				fn get_structure_generics_on(
					r#type: &Type,
					expected: TypeId,
				) -> Option<&GenericArguments> {
					match r#type {
						Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
							on,
							arguments,
						}) if expected == *on => Some(arguments),
						_ => None,
					}
				}

				if let Some(ref mut object_constraints) = state.object_constraints {
					object_constraints.push((ty, base_type));
				}
				// TODO a bit of a mess

				return if let Some(sgs) = get_structure_generics_on(right_ty, *on) {
					match (arguments, sgs) {
						(
							GenericArguments::ExplicitRestrictions(left),
							GenericArguments::ExplicitRestrictions(right),
						) => {
							for (lk, (lv, _)) in left.iter() {
								let (rv, _) = right.get(lk).unwrap();
								let argument_is_subtype = type_is_subtype_with_generics(
									(*lv, base_type_arguments),
									(*rv, ty_structure_arguments),
									state,
									information,
									types,
								);
								if let err @ SubTypeResult::IsNotSubType(_) = argument_is_subtype {
									return err;
								}
							}
							SubTypeResult::IsSubType
						}
						pair => todo!("{:?}", pair),
					}
				} else if let Type::Object(super::ObjectNature::RealDeal) = right_ty {
					let prototype =
						information.get_chain_of_info().find_map(|info| info.prototypes.get(&ty));

					crate::utilities::notify!("prototype is {:?}", prototype);

					if prototype.is_some_and(|prototype| prototype == on) {
						for (argument, lookup) in lookup.iter() {
							// TODO no vec
							let backing_type =
								arguments.get_structure_restriction(*argument).unwrap();
							for value in lookup.calculate_lookup(information, types, ty) {
								let type_is_subtype =
									type_is_subtype(backing_type, value, state, information, types);
								if let e @ SubTypeResult::IsNotSubType(_) = type_is_subtype {
									return e;
								}
							}
						}
						SubTypeResult::IsSubType
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				} else {
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				};
			}

			if let TypeId::ARRAY_TYPE = *on {
				let backing_type = arguments
					.get_structure_restriction(TypeId::T_TYPE)
					.expect("array T argument not set ?");

				crate::utilities::notify!(
					"Array type is {}",
					print_type(backing_type, types, information, false)
				);

				// TODO temp fix for general parameters
				if let Type::Object(_) = right_ty {
					// let Some(lookup_restriction) =
					// 	types.get_look_up_generic_from_prototype(TypeId::ARRAY_TYPE, ty)
					// else {
					// 	crate::utilities::notify!("Here");
					// 	return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
					// };

					// TODO don't create vec
					// for value in lookup_restriction.calculate_lookup(information) {

					// }

					if let Some(ref mut object_constraints) = state.object_constraints {
						object_constraints.push((ty, base_type));
					}

					SubTypeResult::IsSubType
				} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
					on: TypeId::ARRAY_TYPE,
					arguments: right_arguments,
				}) = right_ty
				{
					let left_arg = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
					let right_arg =
						right_arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();

					crate::utilities::notify!("{:?} :> {:?}", left_arg, right_arg);

					// TODO unsure about arguments here
					type_is_subtype_with_generics(
						(left_arg, base_type_arguments),
						(right_arg, ty_structure_arguments),
						state,
						information,
						types,
					)
				} else {
					crate::utilities::notify!("Not array-ish {:?}", right_ty);
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			} else {
				let into = arguments.clone();

				let base_type_arguments =
					GenericChainLink::append(base_type, base_type_arguments.as_ref(), &into);

				type_is_subtype_with_generics(
					(*on, base_type_arguments),
					(ty, ty_structure_arguments),
					state,
					information,
					types,
				)
			}
		}
		Type::Constructor(cst) => match cst {
			// For template literal types
			Constructor::BinaryOperator {
				operator: crate::types::MathematicalAndBitwise::Add,
				..
			} => {
				if let Type::Constant(Constant::String(rs)) = right_ty {
					let matches = slice_matches_type(
						(base_type, base_type_arguments),
						rs,
						state.contributions.as_mut().map(|n| &mut n.staging_contravariant),
						information,
						types,
						false,
					);
					if matches {
						SubTypeResult::IsSubType
					} else {
						// TODO clear contributions
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				} else {
					crate::utilities::notify!("RHS not string");
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			}
			Constructor::BinaryOperator { .. }
			| Constructor::CanonicalRelationOperator { .. }
			| Constructor::UnaryOperator { .. } => unreachable!("invalid constructor on LHS"),
			Constructor::TypeOperator(_) => todo!(),
			Constructor::TypeRelationOperator(_) => todo!(),
			Constructor::ConditionalResult {
				condition: _,
				truthy_result,
				otherwise_result,
				result_union: _,
			} => {
				todo!();
				// if truthy_result (or otherwise result) is generic, set the generic. Then it is subtype if the condition holds {
				// not quite sure how the condition evaluation works. extends is okay (currently only done in substitution)
				// I don't know what other conditions there is.
				//
				// }
			}
			Constructor::Image { on: _, with: _, result: _ } => todo!(),
			Constructor::Property { on, under, result: _, mode: _ } => {
				// Ezno custom state
				// TODO might be based of T
				if let Type::Constructor(Constructor::Property {
					on: r_on,
					under: r_under,
					result: _,
					mode: _,
				}) = right_ty
				{
					if on == r_on && under == r_under {
						return SubTypeResult::IsSubType;
					}
				}

				// TODO this only seems to work in simple cases. For mapped types
				// crate::utilities::notify!(
				// 	"base_structure_arguments={:?}, ty_structure_arguments={:?}, *on={:?}",
				// 	base_type_arguments,
				// 	ty_structure_arguments,
				// 	on
				// );

				if let Some(on) = base_type_arguments.and_then(|args| args.get_single_argument(*on))
				{
					let new_under;
					let under = if let PropertyKey::Type(original) = under {
						// let ty = types.get_type_by_id(*original);
						crate::utilities::notify!(
							"original={:?}, bta={:?}",
							original,
							base_type_arguments
						);
						// let original = if let Type::RootPolyType(
						// 	crate::types::PolyNature::MappedGeneric { name, eager_fixed },
						// ) = ty
						// {
						// 	*eager_fixed
						// } else {
						// 	*original
						// };

						let original = *original;
						new_under = if let Some(under) = base_type_arguments.and_then(|args| {
							args.get_argument_covariant(original, information, types)
						}) {
							under.as_property_key()
						} else {
							crate::utilities::notify!(
								"Could not find key type {:?} {:?}",
								original,
								base_type_arguments
							);
							PropertyKey::from_type(original, types)
						};
						&new_under
					} else {
						under
					};

					crate::utilities::notify!(
						"Here got under={:?}, on={:?}",
						under,
						types.get_type_by_id(on)
					);
					let property = get_property_unbound(
						(on, base_type_arguments),
						(Publicity::Public, &under, ty_structure_arguments),
						information,
						types,
					);
					if let Ok(LogicalOrValid::Logical(property)) = property {
						crate::utilities::notify!("Here 3");
						match property {
							Logical::Pure(property) => {
								match property {
									property => {
										crate::utilities::notify!("Here 4 {:?}", property);
										let property_value = property.as_get_type(types);
										return type_is_subtype_with_generics(
											(property_value, base_type_arguments),
											(ty, ty_structure_arguments),
											state,
											information,
											types,
										);
									} // HMM
									  // PropertyValue::ConditionallyExists { condition, truthy } => {
									  // }
								}
							}
							Logical::BasedOnKey(LeftRight::Right { on, filter }) => {
								let properties =
									crate::types::properties::get_properties_on_single_type(
										on,
										types,
										information,
										false,
										filter,
									);

								for (_, _, value) in properties {
									crate::utilities::notify!("{:?}", value);
									// let result = type_is_subtype_with_generics(
									// 	(property, base_type_arguments),
									// 	(value.as_set_type(), ty_structure_arguments),
									// 	state,
									// 	information,
									// 	types,
									// );

									// if let SubTypeResult::IsNotSubType(_) = result {
									// 	return result;
									// }
								}
							}
							value => {
								crate::utilities::notify!("TODO not checking with {:?}", value);
								// SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
							} // Logical::Or { based_on, left, right } => todo!(),
							  // Logical::Implies { on, antecedent } => todo!(),
						}
					}
				} else {
					crate::utilities::notify!(
						"Could not find argument for {:?}",
						(on, base_type_arguments)
					);
				}
				// else if let Type::Interface { .. }
				// | Type::Object(ObjectNature::AnonymousTypeAnnotation)
				// | Type::AliasTo { .. } = types.get_type_by_id(*on)
				// {
				// 	let property = get_property_unbound(
				// 		(*on, base_structure_arguments),
				// 		(Publicity::Public, under, ty_structure_arguments),
				// 		information,
				// 		types,
				// 	);
				// 	if let Ok(property) = property {
				// 		crate::utilities::notify!("Here");
				// 		match property {
				// 			Logical::Pure(PropertyValue::Value(property)) => {
				// 				crate::utilities::notify!("Here");
				// 				return type_is_subtype_with_generics(
				// 					(property, base_structure_arguments),
				// 					(ty, ty_structure_arguments),
				// 					state,
				// 					information,
				// 					types,
				// 				);
				// 			}
				// 			value => todo!("{:?}", value), // Logical::Or { based_on, left, right } => todo!(),
				// 			                               // Logical::Implies { on, antecedent } => todo!(),
				// 		}
				// 	}
				// }

				crate::utilities::notify!("Here *on={:?}", types.get_type_by_id(*on));
				crate::utilities::notify!("Mismatched property");

				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			}
			Constructor::Awaited { .. } => todo!(),
			Constructor::KeyOf(on) => {
				if let Type::Constant(crate::Constant::String(s)) = right_ty {
					let get_property_unbound = get_property_unbound(
						(*on, base_type_arguments),
						(
							Publicity::Public,
							&PropertyKey::String(std::borrow::Cow::Borrowed(s)),
							ty_structure_arguments,
						),
						information,
						types,
					);
					if get_property_unbound.is_ok() {
						SubTypeResult::IsSubType
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				} else {
					crate::utilities::notify!("TODO keyof stuff");
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			}
		},
		Type::AliasTo { to, parameters, name: _ } => {
			let base_structure_arguments = if let Some(parameters) = parameters {
				crate::utilities::notify!("Skipping looking at parameters {:?}", parameters);
				base_type_arguments
			} else {
				base_type_arguments
			};

			type_is_subtype_with_generics(
				(*to, base_structure_arguments),
				(ty, ty_structure_arguments),
				state,
				information,
				types,
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
			Type::Object(..) => {
				// WIP Nominal-ness for #128
				if let Some(prototype) =
					information.get_chain_of_info().find_map(|info| info.prototypes.get(&ty))
				{
					if *prototype == base_type {
						SubTypeResult::IsSubType
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				} else {
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			}
			Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, arguments }) => {
				let into = arguments.clone();
				let right =
					(*on, GenericChainLink::append(ty, ty_structure_arguments.as_ref(), &into));
				type_is_subtype_with_generics(
					(base_type, base_type_arguments),
					right,
					state,
					information,
					types,
				)
			}
			Type::And(a, b) => {
				// TODO more
				crate::utilities::notify!("Here LHS class, RHS and");
				if *a == base_type || *b == base_type {
					SubTypeResult::IsSubType
				} else {
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			}
			ty => {
				crate::utilities::notify!("Does {:?} not match class", ty);
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			}
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
				crate::utilities::notify!(
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
					(base_type, base_type_arguments),
					(ty, ty_structure_arguments),
					state,
					information,
					types,
				),
				Type::SpecialObject(SpecialObject::Function(..)) => {
					crate::utilities::notify!("TODO implement function checking");
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
				Type::And(a, b) => {
					// TODO more
					crate::utilities::notify!("Here LHS interface, RHS and");
					if *a == base_type || *b == base_type {
						SubTypeResult::IsSubType
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				}
				Type::Or(_left, _right) => {
					unreachable!()
				}
				Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, arguments }) => {
					let into = arguments.clone();
					let append =
						GenericChainLink::append(ty, ty_structure_arguments.as_ref(), &into);
					type_is_subtype_with_generics(
						(base_type, base_type_arguments),
						(*on, append),
						state,
						information,
						types,
					)
				}
				Type::AliasTo { .. } | Type::Interface { .. } => {
					crate::utilities::notify!("lhs={:?} rhs={:?}", left_ty, right_ty);
					// TODO
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
				Type::Constructor(..) | Type::RootPolyType(..) => {
					let arg = base_type_arguments
						.and_then(|args| args.get_argument(base_type, information, types));

					if let Some(args) = arg {
						for arg in args {
							let result = type_is_subtype_with_generics(
								(arg, base_type_arguments),
								(ty, ty_structure_arguments),
								state,
								information,
								types,
							);

							if let e @ SubTypeResult::IsNotSubType(_) = result {
								return e;
							}
						}
						SubTypeResult::IsSubType
					} else {
						let to = get_constraint(ty, types).unwrap();

						if to == TypeId::ANY_TYPE {
							crate::utilities::notify!("Modify constraint for equality");
						}

						type_is_subtype_with_generics(
							(base_type, base_type_arguments),
							(to, ty_structure_arguments),
							state,
							information,
							types,
						)
					}
				}
				Type::FunctionReference(_) => todo!(),
				Type::SpecialObject(_) => todo!(),
				Type::Class { .. } => todo!(),
			}
		}
		Type::SpecialObject(SpecialObject::Null) => {
			crate::utilities::notify!("rhs={:?}", right_ty);
			SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
		}
		Type::SpecialObject(_) => todo!(),
	}
}

fn subtype_function(
	(left_func, base_type_arguments): (crate::FunctionId, GenericChain),
	(right_ty, ty, right_type_arguments): (&Type, TypeId, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> SubTypeResult {
	crate::utilities::notify!("Subtyping a function");

	let right_func = if let Type::FunctionReference(right_func)
	| Type::SpecialObject(SpecialObject::Function(right_func, _)) = right_ty
	{
		right_func
	} else if let Some(constraint) = get_constraint(ty, types) {
		// TODO explain why get_constraint early breaks a bunch of tests
		let right_ty = types.get_type_by_id(constraint);
		if let Type::FunctionReference(right_func)
		| Type::SpecialObject(SpecialObject::Function(right_func, _)) = right_ty
		{
			right_func
		} else {
			crate::utilities::notify!("Not function after constraint!! {:?}", right_ty);
			return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
		}
	} else {
		crate::utilities::notify!("Not function!! {:?}", right_ty);
		return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
	};

	let left_func = types.functions.get(&left_func).unwrap();
	let right_func = types.functions.get(right_func).unwrap();

	for (idx, lhs_param) in left_func.parameters.parameters.iter().enumerate() {
		match right_func.parameters.get_parameter_type_at_index(idx) {
			Some((right_param_ty, position)) => {
				let last_mode =
					std::mem::replace(&mut state.mode, SubTypingMode::Covariant { position });

				// Reverse is important
				let result = type_is_subtype_with_generics(
					(right_param_ty, right_type_arguments),
					(lhs_param.ty, base_type_arguments),
					state,
					information,
					types,
				);

				if let err @ SubTypeResult::IsNotSubType(_) = result {
					let lhs = print_type(right_param_ty, types, information, true);
					let rhs = print_type(lhs_param.ty, types, information, true);
					crate::utilities::notify!(
						"Parameter invalid rhs ({:?} {:?}) <- lhs ({:?} {:?})",
						rhs,
						right_type_arguments,
						lhs,
						base_type_arguments
					);
					// TODO don't short circuit
					return err;
				}

				state.mode = last_mode;
			}
			None => {
				// This is allowed. TODO only in some cases
				// if !lhs_param.is_optional {
				// 	crate::utilities::notify!("Expected parameter, for non optional parameter");
				// 	return SubTypeResult::IsNotSubType(NonEqualityReason::MissingParameter);
				// }
			}
		}
	}

	// TODO optional and rest parameters

	// `void` return type means anything goes here
	if TypeId::VOID_TYPE == left_func.return_type {
		SubTypeResult::IsSubType
	} else {
		let type_is_subtype_with_generics = type_is_subtype_with_generics(
			(left_func.return_type, base_type_arguments),
			(right_func.return_type, right_type_arguments),
			state,
			information,
			types,
		);

		if type_is_subtype_with_generics.is_mismatch() {
			crate::utilities::notify!("return type invalid");
		}

		type_is_subtype_with_generics
	}
}

fn subtype_properties(
	(base_type, base_type_arguments): (TypeId, GenericChain),
	(ty, right_type_arguments): (TypeId, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> SubTypeResult {
	// TODO this will cause problems
	state.mode = state.mode.one_deeper();

	// TODO (#128): This is a compromise where only boolean and number types are treated as nominal
	// match base_type {
	// 	TypeId::BOOLEAN_TYPE | TypeId::NUMBER_TYPE if base_type != ty => {
	// 		crate::utilities::notify!("Here");
	// 		state.mode = state.mode.one_shallower();
	// 		return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
	// 	}
	// 	_ => {}
	// }

	let mut property_errors = Vec::new();
	let reversed_flattened_properties_on_base = information
		.get_chain_of_info()
		.filter_map(|info| info.current_properties.get(&base_type).map(|v| v.iter().rev()))
		.flatten();

	// Note this won't check for conditional stuff being true etc or things being deleted
	for (publicity, key, lhs_property) in reversed_flattened_properties_on_base {
		// crate::utilities::notify!(
		// 	"key {:?} with base_type_arguments={:?}",
		// 	key,
		// 	base_type_arguments
		// );
		let holding_key;
		let key = match key {
			PropertyKey::Type(key_ty) => {
				if let Some(base_type_arguments) = base_type_arguments {
					let key_ty =
						base_type_arguments.get_single_argument(*key_ty).unwrap_or(*key_ty);
					holding_key = PropertyKey::from_type(key_ty, types);
					&holding_key
				} else {
					key
				}
			}
			PropertyKey::String(_) => key,
		};

		let result = check_lhs_property_is_super_type_of_rhs(
			(*publicity, key),
			(lhs_property, base_type_arguments, false),
			(ty, right_type_arguments),
			state,
			information,
			types,
		);

		if let Err(err) = result {
			property_errors.push((key.into_owned(), err));
		}
	}

	let result = if property_errors.is_empty() {
		let mut result = SubTypeResult::IsSubType;
		if let Some(extends) = types.interface_extends.get(&base_type) {
			result = type_is_subtype_with_generics(
				(*extends, base_type_arguments),
				(ty, right_type_arguments),
				state,
				information,
				types,
			);
		}

		// Exclusive check
		if result.is_subtype() && base_type_arguments.is_some_and(|base| base.exclusive_mode()) {
			use crate::types::properties;

			let get_properties = properties::get_properties_on_single_type(
				ty,
				types,
				information,
				false,
				TypeId::ANY_TYPE,
			);
			let mut excess = false;
			// Assert base_type contains all the keys of the LHS
			for (publicity, key, _value) in get_properties.into_iter() {
				let result = properties::get_property_unbound(
					(base_type, base_type_arguments),
					(publicity, &key, None),
					information,
					types,
				);

				// TODO more
				if result.is_err() {
					excess = true;
					break;
				}
			}

			result = if excess {
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			} else {
				SubTypeResult::IsSubType
			};
		}

		if result.is_subtype() {
			// TODO type arguments
			if let Some(ref mut object_constraints) = state.object_constraints {
				let base_type =
					if let Some(GenericChainLink::PartiallyAppliedGenericArgumentsLink {
						ref from,
						parent_link,
						value: _,
					}) = base_type_arguments
					{
						if parent_link.is_some() {
							crate::utilities::notify!("TODO recursive get_from");
						}
						*from
					} else {
						base_type
					};
				object_constraints.push((ty, base_type));
			}
		}

		result
	} else {
		SubTypeResult::IsNotSubType(NonEqualityReason::PropertiesInvalid {
			errors: property_errors,
		})
	};

	state.mode = state.mode.one_shallower();

	result
}

fn check_lhs_property_is_super_type_of_rhs(
	(publicity, key): (Publicity, &PropertyKey<'_>),
	(lhs_property, base_type_arguments, optional): (&PropertyValue, GenericChain, bool),
	(ty, right_type_arguments): (TypeId, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> Result<(), PropertyError> {
	match lhs_property {
		PropertyValue::Value(lhs_value) => {
			let right_result = get_property_unbound(
				(ty, right_type_arguments),
				(publicity, key, base_type_arguments),
				information,
				types,
			);

			{
				crate::utilities::notify!("LHS value is {:?}", lhs_value);
				crate::utilities::notify!(
					"RHS value is {:?} {:?}",
					right_result,
					(key, base_type_arguments)
				);
			}

			match right_result {
				Ok(LogicalOrValid::Logical(res)) => {
					let res = check_logical_property(
						(*lhs_value, base_type_arguments, optional),
						(res, right_type_arguments),
						state,
						information,
						types,
					);
					match res {
						SubTypeResult::IsSubType => Ok(()),
						SubTypeResult::IsNotSubType(err) => Err(PropertyError::Invalid {
							expected: *lhs_value,
							// TODO logical -> TypeId
							found: TypeId::UNIMPLEMENTED_ERROR_TYPE,
							mismatch: err,
						}),
					}
				}
				// TODO
				res => {
					if optional {
						Ok(())
					} else {
						// crate::utilities::notify!("One missing");
						Err(PropertyError::Missing)
					}
				}
			}
		}
		PropertyValue::GetterAndSetter { getter, setter } => {
			todo!()
		}
		PropertyValue::Getter(getter) => {
			let res = get_property_unbound((ty, None), (publicity, key, None), information, types);
			crate::utilities::notify!("looking for {:?} found {:?}", key, res);

			match res {
				Ok(LogicalOrValid::Logical(res)) => {
					todo!("get get return type")
				}
				// TODO
				res => {
					crate::utilities::notify!("res={:?}", res);
					Err(PropertyError::Missing)
				}
			}
		}
		PropertyValue::Setter(_) => {
			let rhs = get_property_unbound((ty, None), (publicity, key, None), information, types);

			match rhs {
				Ok(ok) => {
					crate::utilities::notify!("Set vs {:?}", ok);
					Ok(())
				}
				Err(err) => Err(PropertyError::Missing),
			}
		}
		PropertyValue::Deleted => {
			// TODO WIP
			let res = get_property_unbound((ty, None), (publicity, key, None), information, types);
			if res.is_ok() {
				// TODO the opposite of missing
				Err(PropertyError::Missing)
			} else {
				// Fine !
				Ok(())
			}
		}
		PropertyValue::ConditionallyExists { condition, truthy } => {
			crate::utilities::notify!("Here {:?}", (key, ty, condition, truthy));
			check_lhs_property_is_super_type_of_rhs(
				(publicity, key),
				(&*truthy, base_type_arguments, true),
				(ty, right_type_arguments),
				state,
				information,
				types,
			)
			// if let PropertyValue::Value(lhs_value) = &**truthy {
			// let property = get_property_unbound(
			// 	(ty, right_type_arguments),
			// 	(publicity, key, base_type_arguments),
			// 	information,
			// 	types,
			// );
			// crate::utilities::notify!("property={:?}", property);

			// if let Ok(LogicalOrValid::Logical(property)) = property {
			// 	// TODO for error reporting
			// 	let found = if let Logical::Pure(PropertyValue::Value(ref found)) = property {
			// 		*found
			// 	} else {
			// 		TypeId::ERROR_TYPE
			// 	};

			// 	crate::utilities::notify!("{:?}", property);

			// 	let res = check_logical_property(
			// 		(*lhs_value, base_type_arguments),
			// 		(property, right_type_arguments),
			// 		state,
			// 		information,
			// 		types,
			// 	);

			// 	if let SubTypeResult::IsNotSubType(reason) = res {
			// 		Err(PropertyError::Invalid {
			// 			expected: *lhs_value,
			// 			found,
			// 			mismatch: reason,
			// 		})
			// 	} else {
			// 		Ok(())
			// 	}
			// } else {
			// 	crate::utilities::notify!("Here");
			// 	// Err(PropertyError::Missing)
			// 	// Okay if missing because of the above
			// 	Ok(())
			// }
			// } else {
			// 	crate::utilities::notify!("Here maybe errors needs to continue checking {:?}", truthy);
			// 	Ok(())
			// }
		}
		PropertyValue::Configured { on, .. } => {
			crate::utilities::notify!("TODO check readonly");
			check_lhs_property_is_super_type_of_rhs(
				(publicity, key),
				(&on, base_type_arguments, optional),
				(ty, right_type_arguments),
				state,
				information,
				types,
			)
		}
	}
}

fn check_logical_property(
	(lhs_property_value, lhs_property_value_type_arguments, optional): (TypeId, GenericChain, bool),
	(rhs_property, right_type_arguments): (Logical<PropertyValue>, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> SubTypeResult {
	match rhs_property {
		Logical::Pure(rhs_property) => {
			let rhs_type = rhs_property.as_get_type(types);
			// crate::utilities::notify!(
			// 	"Checking {} with {}, against {}, left={:?}",
			// 	print_type(key, types, information, true),
			// 	print_type(property, types, information, true),
			// 	print_type(rhs_type, types, information, true),
			// 	lhs_property_value_type_arguments
			// );

			type_is_subtype_with_generics(
				(lhs_property_value, lhs_property_value_type_arguments),
				(rhs_type, right_type_arguments),
				state,
				information,
				types,
			)
		}
		Logical::Or { condition, left, right } => {
			crate::utilities::notify!("{:?} {:?}", left, right);

			if let (LogicalOrValid::Logical(left), LogicalOrValid::Logical(right)) = (*left, *right)
			{
				let left_result = check_logical_property(
					(lhs_property_value, lhs_property_value_type_arguments, optional),
					(left, right_type_arguments),
					state,
					information,
					types,
				);

				if let SubTypeResult::IsSubType = left_result {
					check_logical_property(
						(lhs_property_value, lhs_property_value_type_arguments, optional),
						(right, right_type_arguments),
						state,
						information,
						types,
					)
				} else {
					// else return the failing result
					left_result
				}
			} else if optional {
				SubTypeResult::IsSubType
			} else {
				crate::utilities::notify!("One missing");
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			}
		}
		Logical::Implies { on, antecedent } => check_logical_property(
			(lhs_property_value, lhs_property_value_type_arguments, optional),
			(
				*on,
				GenericChainLink::append(
					TypeId::ERROR_TYPE,
					right_type_arguments.as_ref(),
					&antecedent,
				),
			),
			state,
			information,
			types,
		),
		Logical::BasedOnKey(kind) => match kind {
			LeftRight::Left { value, key_arguments } => {
				todo!("{:?}", (value, key_arguments));
			}
			LeftRight::Right { on, filter } => {
				if let Type::RootPolyType(PolyNature::MappedGeneric { name: _, eager_fixed }) =
					types.get_type_by_id(filter)
				{
					type_is_subtype_of_property_mapped_key(
						MappedKey { value: eager_fixed.clone().into(), key: filter },
						(lhs_property_value, lhs_property_value_type_arguments, optional),
						(on, right_type_arguments),
						state,
						information,
						types,
					)
				} else {
					let properties = get_properties_on_single_type2(
						(on, right_type_arguments),
						types,
						information,
						filter,
					);
					for (key, rhs_property, _args) in properties {
						let result = check_logical_property(
							(lhs_property_value, lhs_property_value_type_arguments, optional),
							(Logical::Pure(rhs_property), right_type_arguments),
							state,
							information,
							types,
						);
						if result.is_mismatch() {
							return result;
						}
					}
					SubTypeResult::IsSubType
				}
			}
		},
	}
}

pub struct MappedKey {
	/// covariant contribution allows for slices and PropertyKey::String
	pub value: CovariantContribution,
	/// This points towards the `Type::RootPolyType(PolyNature::MappedGeneric)`
	pub key: TypeId,
}

pub fn type_is_subtype_of_property_mapped_key(
	mapped_key: MappedKey,
	(base, property_generics, optional): (TypeId, GenericChain, bool),
	(ty, right_type_arguments): (TypeId, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> SubTypeResult {
	// TODO use covariant contribution as property key. Also what about slices on types?
	match mapped_key.value {
		CovariantContribution::String(ref s) => {
			{
				crate::utilities::notify!(
					"Reading {:?}, with {:?} {:?}",
					types.get_type_by_id(ty),
					s,
					(property_generics.as_ref(), right_type_arguments.as_ref())
				);
			}
			let right_property = get_property_unbound(
				(ty, right_type_arguments),
				(
					Publicity::Public,
					&PropertyKey::String(std::borrow::Cow::Owned(s.to_owned())),
					None,
				),
				information,
				types,
			);

			match right_property {
				Ok(LogicalOrValid::Logical(right_property)) => {
					let map = crate::Map::from_iter([(mapped_key.key, (mapped_key.value, 0))]);
					let property_generics = Some(GenericChainLink::MappedPropertyLink {
						parent_link: property_generics.as_ref(),
						value: &map,
					});
					let result = check_logical_property(
						(base, property_generics, optional),
						(right_property, right_type_arguments),
						state,
						information,
						types,
					);

					crate::utilities::notify!("Got {:?}", result);

					result
				}
				// TODO
				_res => {
					crate::utilities::notify!("Missing");
					if optional {
						SubTypeResult::IsSubType
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				}
			}
		}
		CovariantContribution::TypeId(key_ty) => {
			match types.get_type_by_id(key_ty) {
				Type::AliasTo { to, name: _, parameters: _ } => {
					type_is_subtype_of_property_mapped_key(
						MappedKey { value: to.clone().into(), key: mapped_key.key },
						(base, property_generics, optional),
						(ty, right_type_arguments),
						state,
						information,
						types,
					)
				}
				Type::And(left, right) => todo!(),
				Type::Or(left, right) => {
					let left = type_is_subtype_of_property_mapped_key(
						MappedKey { value: left.clone().into(), key: mapped_key.key },
						(base, property_generics, optional),
						(ty, right_type_arguments),
						state,
						information,
						types,
					);
					if left.is_mismatch() {
						left
					} else {
						type_is_subtype_of_property_mapped_key(
							MappedKey { value: right.clone().into(), key: mapped_key.key },
							(base, property_generics, optional),
							(ty, right_type_arguments),
							state,
							information,
							types,
						)
					}
				}
				Type::RootPolyType(_) => {
					// TODO get_covariant contribution
					if let Some(value) =
						property_generics.and_then(|args| args.get_single_argument(key_ty))
					{
						type_is_subtype_of_property_mapped_key(
							MappedKey { value: value.into(), key: mapped_key.key },
							(base, property_generics, optional),
							(ty, right_type_arguments),
							state,
							information,
							types,
						)
					} else {
						todo!("no value {:?}", (ty, property_generics))
					}
				}
				Type::Constructor(Constructor::Property { on, under, result, mode }) => {
					todo!()
				}
				Type::Constructor(Constructor::KeyOf(key_of_ty)) => {
					let properties = get_properties_on_single_type2(
						(*key_of_ty, property_generics),
						types,
						information,
						TypeId::ANY_TYPE,
					);
					for (key, _, _) in properties {
						let value = match key {
							PropertyKey::Type(ty) => CovariantContribution::TypeId(ty),
							PropertyKey::String(str) => {
								CovariantContribution::String(str.into_owned())
							}
						};
						crate::utilities::notify!("Here {:?}", value);
						let result = type_is_subtype_of_property_mapped_key(
							MappedKey { value, key: mapped_key.key },
							(base, property_generics, optional),
							(ty, right_type_arguments),
							state,
							information,
							types,
						);

						if result.is_mismatch() {
							return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
						}
					}
					SubTypeResult::IsSubType
				}
				Type::Constructor(_) => todo!(),
				Type::PartiallyAppliedGenerics(_) => todo!(),
				Type::Interface { name, nominal, parameters } => todo!(),
				Type::Class { name, parameters } => todo!(),
				Type::Constant(_) => {
					let right_property = get_property_unbound(
						(ty, right_type_arguments),
						(Publicity::Public, &PropertyKey::Type(key_ty), right_type_arguments),
						information,
						types,
					);

					match right_property {
						Ok(LogicalOrValid::Logical(right_property)) => {
							let map =
								crate::Map::from_iter([(mapped_key.key, (mapped_key.value, 0))]);
							let property_generics = Some(GenericChainLink::MappedPropertyLink {
								parent_link: property_generics.as_ref(),
								value: &map,
							});
							check_logical_property(
								(base, property_generics, optional),
								(right_property, right_type_arguments),
								state,
								information,
								types,
							)
						}
						// TODO
						_res => {
							if optional {
								SubTypeResult::IsSubType
							} else {
								SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
							}
						}
					}
				}
				Type::FunctionReference(_) => todo!(),
				Type::Object(_) => todo!(),
				Type::SpecialObject(_) => todo!(),
			}
		}
		_value => todo!("{:?}", _value),
	}
}

/// TODO integrate `set_restriction`, but it can't create a type ? maybe object restriction should be logically.
/// maybe sub function
pub fn type_is_subtype_of_property(
	(property, property_generics): (&Logical<PropertyValue>, GenericChain),
	ty: TypeId,
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> SubTypeResult {
	match property {
		Logical::Pure(prop) => type_is_subtype_with_generics(
			(prop.as_set_type(types), property_generics),
			(ty, GenericChain::None),
			state,
			information,
			types,
		),
		Logical::Or { condition: _, left, right } => {
			let left_result = if let LogicalOrValid::Logical(left) = &**left {
				type_is_subtype_of_property(
					(left, property_generics),
					ty,
					state,
					information,
					types,
				)
			} else {
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			};
			if let SubTypeResult::IsSubType = left_result {
				left_result
			} else if let LogicalOrValid::Logical(right) = &**right {
				type_is_subtype_of_property(
					(right, property_generics),
					ty,
					state,
					information,
					types,
				)
			} else {
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			}
		}
		Logical::Implies { on, antecedent } => {
			let property_generics = GenericChainLink::append(
				TypeId::UNIMPLEMENTED_ERROR_TYPE,
				property_generics.as_ref(),
				antecedent,
			);
			type_is_subtype_of_property((on, property_generics), ty, state, information, types)
		}
		Logical::BasedOnKey(on) => {
			// if let LeftRight::Right { on, filter } = on {
			// 	if let Type::RootPolyType(PolyNature::MappedGeneric { name: _, eager_fixed }) =
			// 		types.get_type_by_id(*filter)
			// 	{
			// 		type_is_subtype_of_(
			// 			Some(MappedKey { value: *eager_fixed, key: *filter }),
			// 			(property, property_generics),
			// 			ty,
			// 			state,
			// 			information,
			// 			types,
			// 		)
			// 	} else {
			// 		crate::utilities::notify!("TODO, returning IsSubType {:?}", on);
			// 		SubTypeResult::IsSubType
			// 	}
			// } else {
			crate::utilities::notify!("TODO, returning IsSubType {:?}", on);
			SubTypeResult::IsSubType
			// }
		}
	}
}

impl NonEqualityReason {
	pub(crate) fn _into_error_message(self, _information: &GeneralContext) -> Vec<String> {
		match self {
			NonEqualityReason::GenericParameterMismatch
			| NonEqualityReason::MissingParameter
			| NonEqualityReason::Mismatch => Vec::new(),
			NonEqualityReason::PropertiesInvalid { errors } => {
				errors.into_iter().map(|error| format!("{error:?}")).collect()
			}
			NonEqualityReason::TooStrict => todo!(),
			NonEqualityReason::Excess => todo!(),
		}
	}
}

pub type SliceArguments =
	TriMap<TypeId, super::generics::contributions::CovariantContribution, ContributionDepth>;

// #[derive(Debug, Default)]
// pub struct SliceArguments {
// 	pub(crate) covariant: TriMap<TypeId, super::generics::contributions::CovariantContribution, ContributionDepth>,
// 	/// WIP for mapped inference
// 	pub(crate) contravariant: TriMap<TypeId, super::generics::contributions::CovariantContribution, ContributionDepth>
// }

/// `allow_casts=true` is for property keys
pub(crate) fn slice_matches_type(
	(base, base_type_arguments): (TypeId, Option<super::GenericChainLink>),
	slice: &str,
	mut contributions: Option<&mut SliceArguments>,
	information: &impl InformationChain,
	types: &TypeStore,
	allow_casts: bool,
) -> bool {
	let base_ty = types.get_type_by_id(base);

	// {
	// 	crate::utilities::notify!(
	// 		"Slice checking {} ({:?}) :>= '{}'",
	// 		print_type(base, types, information, true),
	// 		base_type_arguments,
	// 		slice
	// 	);
	// }

	// TODO cast string
	if allow_casts {
		if base == TypeId::ANY_TYPE {
			return true;
		} else if base == TypeId::BOOLEAN_TYPE {
			return slice == "true" || slice == "false";
		} else if base == TypeId::NUMBER_TYPE {
			return slice.parse::<usize>().is_ok();
		} else if base == TypeId::STRING_TYPE {
			// crate::utilities::notify!("Here!");
			// TODO is this okay?
			return slice.parse::<usize>().is_err();
		}
	}
	match base_ty {
		Type::Constant(Constant::String(base_string)) => {
			if let Some(transform) = base_type_arguments.and_then(|a| a.get_string_transform()) {
				apply_string_intrinsic(transform, base_string).as_str() == slice
			} else if base_type_arguments.is_some_and(|a| a.is_case_insensitive()) {
				base_string.to_lowercase() == slice.to_lowercase()
			} else {
				base_string == slice
			}
		}
		Type::RootPolyType(rpt) => {
			if let Some(argument) = base_type_arguments.and_then(|v| v.get_single_argument(base)) {
				slice_matches_type(
					(argument, base_type_arguments),
					slice,
					contributions,
					information,
					types,
					allow_casts,
				)
			} else if let Some(contributions) = contributions {
				let constraint = rpt.try_get_constraint().unwrap();
				let res = slice_matches_type(
					(constraint, base_type_arguments),
					slice,
					Some(contributions),
					information,
					types,
					allow_casts,
				);
				if res {
					contributions.insert(base, (CovariantContribution::String(slice.to_owned()), 0))
				}
				res
			} else {
				false
			}
		}
		Type::AliasTo { to, .. } => slice_matches_type(
			(*to, base_type_arguments),
			slice,
			contributions,
			information,
			types,
			allow_casts,
		),
		Type::Or(l, r) => {
			// TODO temp for
			let mut new_contributions = SliceArguments::default();
			let matches = slice_matches_type(
				(*l, base_type_arguments),
				slice,
				Some(&mut new_contributions),
				information,
				types,
				allow_casts,
			);
			if matches {
				if let Some(ref mut contributions) = contributions {
					contributions.extend(new_contributions.into_iter());
				}
				true
			} else {
				// TODO clear contributions
				slice_matches_type(
					(*r, base_type_arguments),
					slice,
					contributions,
					information,
					types,
					allow_casts,
				)
			}
		}
		Type::And(l, r) => {
			let mut new_contributions = SliceArguments::default();
			let matches = slice_matches_type(
				(*l, base_type_arguments),
				slice,
				Some(&mut new_contributions),
				information,
				types,
				allow_casts,
			);
			if matches {
				if let Some(ref mut contributions) = contributions {
					contributions.extend(new_contributions.into_iter());
				}
				slice_matches_type(
					(*r, base_type_arguments),
					slice,
					contributions,
					information,
					types,
					allow_casts,
				)
			} else {
				false
			}
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on:
				transform @ (TypeId::STRING_CAPITALIZE
				| TypeId::STRING_UNCAPITALIZE
				| TypeId::STRING_LOWERCASE
				| TypeId::STRING_UPPERCASE),
			arguments,
		}) => {
			let matches_constraint = match *transform {
				TypeId::STRING_CAPITALIZE => slice.chars().next().map_or(true, char::is_uppercase),
				TypeId::STRING_UNCAPITALIZE => {
					slice.chars().next().map_or(true, char::is_lowercase)
				}
				TypeId::STRING_LOWERCASE => slice.chars().all(char::is_lowercase),
				TypeId::STRING_UPPERCASE => slice.chars().all(char::is_uppercase),
				_ => unreachable!(),
			};

			if matches_constraint {
				let generic_chain_link = Some(GenericChainLink::SpecialGenericChainLink {
					parent_link: base_type_arguments.as_ref(),
					special: SpecialGenericChainLink::CaseTransform { transform: *transform },
				});
				let inner = arguments.get_structure_restriction(TypeId::STRING_GENERIC).unwrap();
				// TODO any contributions in here SHOULD be wrapped in case insensitive
				slice_matches_type(
					(inner, generic_chain_link),
					slice,
					contributions,
					information,
					types,
					allow_casts,
				)
			} else {
				false
			}
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::CASE_INSENSITIVE,
			arguments,
		}) => {
			let generic_chain_link = Some(GenericChainLink::SpecialGenericChainLink {
				parent_link: base_type_arguments.as_ref(),
				special: SpecialGenericChainLink::CaseInsensitive,
			});
			let inner = arguments.get_structure_restriction(TypeId::STRING_GENERIC).unwrap();
			slice_matches_type(
				(inner, generic_chain_link),
				slice,
				contributions,
				information,
				types,
				allow_casts,
			)
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: on @ (TypeId::MULTIPLE_OF | TypeId::LESS_THAN | TypeId::GREATER_THAN),
			arguments,
		}) if allow_casts => {
			// Special behavior here to treat numerical property keys (which are strings) as numbers
			// TODO unify with the subtyping
			let argument = arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
			if let (Type::Constant(Constant::Number(argument)), Ok(value)) =
				(types.get_type_by_id(argument), slice.parse::<usize>())
			{
				let value: ordered_float::NotNan<f64> = (value as f64).try_into().unwrap();
				crate::utilities::notify!("value={:?}, arg={:?}", value, argument);
				let result = match *on {
					TypeId::LESS_THAN => *argument < value,
					TypeId::GREATER_THAN => *argument > value,
					TypeId::MULTIPLE_OF => value % *argument == 0f64,
					_ => unreachable!(),
				};
				result
			} else {
				false
			}
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::NOT_RESTRICTION,
			arguments,
		}) => {
			// Here don't have to use disjoint
			let argument = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
			// TODO what to do about contributions :/
			let matches = slice_matches_type(
				(argument, base_type_arguments),
				slice,
				contributions,
				information,
				types,
				allow_casts,
			);
			// crate::utilities::notify!("negated slice arguments={:?}", _k);
			!matches
		}
		Type::Constructor(super::Constructor::KeyOf(on)) => {
			let argument =
				(Publicity::Public, &PropertyKey::String(std::borrow::Cow::Borrowed(slice)), None);

			let arg = base_type_arguments
				.as_ref()
				.and_then(|link| link.get_single_argument(*on))
				.unwrap_or(*on);

			let property =
				get_property_unbound((arg, base_type_arguments), argument, information, types);

			// crate::utilities::notify!("Here {:?}", property);

			if let Ok(LogicalOrValid::Logical(property)) = property {
				// For mapped types
				if let Some(contributions) = contributions {
					// WIP!!
					let is_writable =
						if let Logical::Pure(PropertyValue::Configured { on: _, ref descriptor }) =
							property
						{
							descriptor.writable
						} else {
							// TODO might be missing something here via LogicalOr etc
							crate::utilities::notify!("Might be missing {:?}", property);
							TypeId::TRUE
						};

					// WIP!!
					let is_defined = if let Logical::Pure(PropertyValue::ConditionallyExists {
						ref condition,
						..
					}) = property
					{
						*condition
					} else {
						// TODO might be missing something here via LogicalOr etc
						crate::utilities::notify!("Might be missing {:?}", property);
						TypeId::TRUE
					};
					contributions.insert(
						TypeId::WRITABLE_KEY_ARGUMENT,
						(CovariantContribution::TypeId(is_writable), 0),
					);
					contributions.insert(
						TypeId::NON_OPTIONAL_KEY_ARGUMENT,
						(CovariantContribution::TypeId(is_defined), 0),
					);
					// crate::utilities::notify!("For MT set: (is_writable, is_defined)={:?}", (is_writable, is_defined));
				}

				true
			} else {
				false
			}
		}
		Type::Constructor(super::Constructor::BinaryOperator {
			lhs,
			rhs,
			operator: MathematicalAndBitwise::Add,
		}) => {
			let lhs = base_type_arguments
				.as_ref()
				.and_then(|link| link.get_single_argument(*lhs))
				.unwrap_or(*lhs);

			let rhs = base_type_arguments
				.as_ref()
				.and_then(|link| link.get_single_argument(*rhs))
				.unwrap_or(*rhs);

			if let Type::Constant(Constant::String(prefix)) = types.get_type_by_id(lhs) {
				if let Some(after) = slice.strip_prefix(prefix) {
					slice_matches_type(
						(rhs, base_type_arguments),
						after,
						contributions,
						information,
						types,
						allow_casts,
					)
				} else {
					false
				}
			} else if let Type::Constant(Constant::String(suffix)) = types.get_type_by_id(rhs) {
				if let Some(before) = slice.strip_suffix(suffix) {
					slice_matches_type(
						(lhs, base_type_arguments),
						before,
						contributions,
						information,
						types,
						allow_casts,
					)
				} else {
					false
				}
			} else {
				let lhs = types.get_type_by_id(lhs);
				let rhs = types.get_type_by_id(rhs);
				crate::utilities::notify!(
					"More complex type here, returning false. lhs={:?}, rhs={:?}, {:?}",
					lhs,
					rhs,
					base_type_arguments
				);
				false
			}
		}
		_ => {
			if base == TypeId::STRING_TYPE || base == TypeId::ANY_TYPE {
				true
			} else {
				crate::utilities::notify!("Cannot match key {:?}", base_ty);
				false
			}
		}
	}
}
