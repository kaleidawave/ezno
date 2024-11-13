//! Type subtype checking. (making sure the RHS type contains all the properties the LHS type requires)

use source_map::SpanWithSource;

use crate::{
	context::{GeneralContext, InformationChain},
	features::{
		objects::{self, SpecialObject},
		operations::MathematicalOrBitwiseOperation,
	},
	Constant, Environment, PropertyValue, TypeId,
};

use super::{
	generics::{
		chain::{GenericChain, GenericChainLink, SpecialGenericChainLink},
		contributions::{ContributionDepth, Contributions, CovariantContribution, TriMap},
		generic_type_arguments::GenericArguments,
	},
	get_constraint,
	intrinsics::{self, apply_string_intrinsic},
	logical::{BasedOnKey, Logical, LogicalOrValid, NeedsCalculation, PropertyOn},
	printing::print_type,
	properties::PropertyKey,
	properties::{get_properties_on_single_type2, get_property_unbound, Publicity},
	Constructor, ObjectNature, PartiallyAppliedGenerics, PolyNature, Type, TypeStore,
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

		let _ = self.already_checked.drain((already_checked as usize)..);
		if let Some(ref mut contributions) = self.contributions {
			let _ =
				contributions.staging_covariant.drop_range((contributions_covariant as usize)..);
			let _ = contributions
				.staging_contravariant
				.drop_range((contributions_contravariant as usize)..);
		}
		if let Some(ref mut object_constraints) = self.object_constraints {
			let _ = object_constraints.drain((object_constraint_count as usize)..);
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
			crate::utilities::notify!("Subtyping recursion");
			return SubTypeResult::IsSubType;
		}

		state.already_checked.push((base_type, ty));
	}

	let supertype = types.get_type_by_id(base_type);
	let subtype = types.get_type_by_id(ty);

	// Eager things
	match subtype {
		Type::Narrowed { narrowed_to: right, .. } | Type::AliasTo { to: right, .. } => {
			let result = type_is_subtype_with_generics(
				(base_type, base_type_arguments),
				(*right, ty_structure_arguments),
				state,
				information,
				types,
			);
			// Temp fix for narrowing constants
			// crate::utilities::notify!("{:?}", super::helpers::is_not_of_constant(*right, types));
			// SubTypeResult::IsNotSubType(_)
			return if let (Type::Narrowed { from, .. }, _, true) =
				(subtype, &result, super::helpers::is_not_of_constant(*right, types))
			{
				type_is_subtype_with_generics(
					(base_type, base_type_arguments),
					(*from, ty_structure_arguments),
					state,
					information,
					types,
				)
			} else {
				result
			};
		}
		Type::Or(left, right) => {
			let right = *right;
			// crate::utilities::notify!("OR RHS: left and right");
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
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: on @ TypeId::NOT_RESTRICTION,
			arguments: _,
		}) => {
			match *on {
				TypeId::NOT_RESTRICTION => {
					// This only happens when subtype ∪ supertype = `any`. This is only true when
					// one is `any`. `Not<any>` is already `never` and `supertype = any` is handled above
					return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
				}
				_ => unreachable!(),
			}
		}
		t @ (Type::RootPolyType(..) | Type::Constructor(..)) => {
			if let Type::RootPolyType(PolyNature::Error(to)) = t {
				// (unless specified) treat as subtype as error would have already been thrown
				crate::utilities::notify!("Here {:?}", state.others.allow_errors);
				return if state.others.allow_errors && *to == TypeId::ANY_TYPE {
					SubTypeResult::IsSubType
				} else {
					type_is_subtype(base_type, *to, state, information, types)
				};
			}

			// crate::utilities::notify!("Looking for {:?} with {:?}", ty, ty_structure_arguments);

			if let Some(arg) = ty_structure_arguments.and_then(|tas| tas.get_argument_covariant(ty))
			{
				return match arg {
					CovariantContribution::TypeId(ty) => type_is_subtype_with_generics(
						(base_type, base_type_arguments),
						(ty, ty_structure_arguments),
						state,
						information,
						types,
					),
					CovariantContribution::String(string) => {
						let contributions =
							state.contributions.as_mut().map(|n| &mut n.staging_contravariant);
						let matches = slice_matches_type(
							(base_type, base_type_arguments),
							&string,
							contributions,
							information,
							types,
							false,
						);
						if matches {
							SubTypeResult::IsSubType
						} else {
							SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						}
					}
					CovariantContribution::SliceOf(s, (l, r)) => todo!("{:?}", (s, (l, r))),
					CovariantContribution::CaseInsensitive(ci) => todo!("{:?}", ci),
					CovariantContribution::Number(n) => {
						let contributions =
							state.contributions.as_mut().map(|n| &mut n.staging_contravariant);
						let matches = number_matches_type(
							(base_type, base_type_arguments),
							n,
							contributions,
							information,
							types,
						);
						if matches {
							SubTypeResult::IsSubType
						} else {
							SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						}
					}
				};
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
			let right_constraint = get_constraint(ty, types).unwrap();

			// crate::utilities::notify!(
			// 	"RHS is parameter, edge case results to {:?}",
			// 	(
			// 		types.get_type_by_id(ty),
			// 		types.get_type_by_id(right_constraint),
			// 		types.get_type_by_id(right_constraint).is_operator()
			// 	)
			// );

			// This is important that LHS is not operator
			let left_is_operator_right_is_not =
				supertype.is_operator() && !types.get_type_by_id(right_constraint).is_operator();

			// edge cases on edge cases
			// If any of these are true. Then do not perform constraint argument lookup
			let edge_case = left_is_operator_right_is_not
				|| matches!(
					supertype,
					Type::RootPolyType(rpt)
					if rpt.is_substitutable()
				) || matches!(supertype, Type::Constructor(..))
				|| base_type_arguments
					.and_then(|args| args.get_argument_covariant(base_type))
					.is_some();
			if !edge_case {
				let result = type_is_subtype_with_generics(
					(base_type, base_type_arguments),
					(right_constraint, ty_structure_arguments),
					state,
					information,
					types,
				);

				// TODO is the above event needed or constructor with constraint == TypeId::ANY_TYPE
				return if result.is_mismatch()
					&& matches!(subtype, Type::RootPolyType(root) if root.is_inferrable())
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

	match supertype {
		Type::FunctionReference(left_func)
		| Type::SpecialObject(SpecialObject::Function(left_func, _)) => subtype_function(
			(*left_func, base_type_arguments),
			(subtype, ty, ty_structure_arguments),
			state,
			information,
			types,
		),
		Type::Constant(lhs) => {
			if let Type::Constant(rhs) = subtype {
				if lhs == rhs {
					SubTypeResult::IsSubType
				} else {
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			} else {
				// TODO what about if LHS has inferred constraint
				// crate::utilities::notify!("Constant {:?} against RHS {:#?}", lhs, subtype);
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			}
		}
		Type::Object(ObjectNature::AnonymousTypeAnnotation(properties)) => subtype_properties(
			(base_type, properties.iter(), base_type_arguments),
			(ty, ty_structure_arguments),
			state,
			information,
			types,
		),
		Type::Object(ObjectNature::RealDeal) => {
			crate::utilities::notify!(
				"what {:?} (subtyping where LHS = ObjectNature::RealDeal)",
				ty
			);

			subtype_floating_properties(
				(base_type, base_type_arguments),
				(ty, ty_structure_arguments),
				state,
				information,
				types,
			)
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
			let base_arg =
				base_type_arguments.and_then(|args| args.get_argument_covariant(base_type));

			if let Some(base_arg) = base_arg {
				match base_arg {
					CovariantContribution::TypeId(base_arg) => type_is_subtype_with_generics(
						(base_arg, base_type_arguments),
						(ty, ty_structure_arguments),
						state,
						information,
						types,
					),
					CovariantContribution::String(left_string) => {
						if let Type::Constant(Constant::String(right_string)) = subtype {
							if &left_string == right_string {
								SubTypeResult::IsSubType
							} else {
								SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
							}
						} else {
							SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						}
					}
					CovariantContribution::SliceOf(s, (l, r)) => todo!("{:?}", (s, (l, r))),
					CovariantContribution::CaseInsensitive(ci) => todo!("{:?}", (ci)),
					CovariantContribution::Number(n) => {
						unreachable!("{:?}", n)
						// crate::utilities::notify!("Here?");
						// if let Type::Constant(Constant::String(right_string)) = subtype {
						// 	if left_string == right_string {
						// 		SubTypeResult::IsSubType
						// 	} else {
						// 		SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						// 	}
						// } else {
						// 	SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						// }
					}
				}
			} else if let Some(ref mut contributions) = state.contributions {
				match state.mode {
					SubTypingMode::Contravariant { depth } => {
						// With <*base_type* extends *under> check ty is under

						crate::utilities::notify!(
							"contributions.parent={:?}",
							contributions.parent
						);

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
						} else {
							type_is_subtype_with_generics(
								(nature.get_constraint(), GenericChain::None),
								(ty, ty_structure_arguments),
								state,
								information,
								types,
							)
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
				if let Type::Constructor(c) = subtype {
					crate::utilities::notify!("TODO right hand side maybe okay");
					if c.get_constraint() == base_type {
						return SubTypeResult::IsSubType;
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

					return if check_and_includes(base_type, subtype) {
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
		Type::Narrowed { narrowed_to, .. } => {
			crate::utilities::notify!("Narrowed on Left?");
			type_is_subtype_with_generics(
				(*narrowed_to, base_type_arguments),
				(ty, ty_structure_arguments),
				state,
				information,
				types,
			)
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, arguments }) => {
			match *on {
				TypeId::READONLY_RESTRICTION => {
					crate::utilities::notify!("TODO temp readonly inner check");
					let inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
					// Some(GenericChainLink::SpecialGenericChainLink {
					// 	parent_link: ty_structure_arguments.as_ref(),
					// 	special: SpecialGenericChainLink::Readonly,
					// })
					return if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
						on: TypeId::READONLY_RESTRICTION,
						arguments,
					}) = subtype
					{
						let ty = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
						type_is_subtype_with_generics(
							(inner, ty_structure_arguments),
							(ty, base_type_arguments),
							state,
							information,
							types,
						)
					} else if information
						.get_chain_of_info()
						.any(|info| info.frozen.contains_key(&ty))
						|| matches!(subtype, Type::Constant(_))
						|| matches!(
							ty,
							TypeId::STRING_TYPE | TypeId::BOOLEAN_TYPE | TypeId::NUMBER_TYPE
						) {
						type_is_subtype_with_generics(
							(inner, ty_structure_arguments),
							(ty, base_type_arguments),
							state,
							information,
							types,
						)
					} else {
						// TODO is not readonly
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					};
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
					if let Type::Constant(Constant::String(rs)) = subtype {
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
					return if let Type::Constant(_)
					| Type::Object(ObjectNature::RealDeal)
					| Type::SpecialObject(..) = subtype
					{
						type_is_subtype_with_generics(
							(inner, base_type_arguments),
							(ty, ty_structure_arguments),
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
					// Drop any infer-ed results
					if let (Some(contributions), Some(current_contributing)) =
						(state.contributions.as_mut(), current_contributing)
					{
						let _ =
							contributions.staging_contravariant.drop_range(current_contributing..);
					}
					return result;
				}
				TypeId::MULTIPLE_OF => {
					let argument =
						arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();

					let right_multiple = crate::types::intrinsics::get_multiple(ty, types);
					return if let (
						Type::Constant(Constant::Number(argument)),
						Some(Type::Constant(Constant::Number(right_multiple))),
					) = (
						types.get_type_by_id(argument),
						right_multiple.map(|right_multiple| types.get_type_by_id(right_multiple)),
					) {
						if (right_multiple % argument) == 0. {
							SubTypeResult::IsSubType
						} else {
							SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						}
					} else {
						crate::utilities::notify!("TODO multiple of {:?}", (argument, ty, subtype));
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					};
				}
				TypeId::GREATER_THAN => {
					let argument =
						arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
					let argument_type = types.get_type_by_id(argument);
					return if let (
						Type::Constant(Constant::Number(value)),
						Type::Constant(Constant::Number(subtype_number)),
					) = (argument_type, subtype)
					{
						if subtype_number > value {
							SubTypeResult::IsSubType
						} else {
							SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						}
					} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
						on: TypeId::GREATER_THAN,
						arguments,
					}) = subtype
					{
						let subtype_argument =
							arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
						// Transitivity
						if argument == subtype_argument {
							SubTypeResult::IsSubType
						} else {
							let subtype_argument = types.get_type_by_id(subtype_argument);
							if let (
								Type::Constant(Constant::Number(subtype_number)),
								Type::Constant(Constant::Number(value)),
							) = (argument_type, subtype_argument)
							{
								if subtype_number > value {
									SubTypeResult::IsSubType
								} else {
									SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
								}
							} else {
								crate::utilities::notify!("Here");
								SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
							}
						}
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					};
				}
				TypeId::LESS_THAN => {
					let argument =
						arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
					let argument_type = types.get_type_by_id(argument);
					return if let (
						Type::Constant(Constant::Number(value)),
						Type::Constant(Constant::Number(subtype_number)),
					) = (argument_type, subtype)
					{
						if subtype_number < value {
							SubTypeResult::IsSubType
						} else {
							SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						}
					} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
						on: TypeId::GREATER_THAN,
						arguments,
					}) = subtype
					{
						let subtype_argument =
							arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
						// Transitivity
						if argument == subtype_argument {
							SubTypeResult::IsSubType
						} else {
							let subtype_argument = types.get_type_by_id(subtype_argument);
							if let (
								Type::Constant(Constant::Number(subtype_number)),
								Type::Constant(Constant::Number(value)),
							) = (argument_type, subtype_argument)
							{
								if subtype_number < value {
									SubTypeResult::IsSubType
								} else {
									SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
								}
							} else {
								crate::utilities::notify!("Here");
								SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
							}
						}
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					};
				}
				TypeId::CASE_INSENSITIVE => {
					if let Type::Constant(Constant::String(rs)) = subtype {
						let contributions =
							state.contributions.as_mut().map(|n| &mut n.staging_contravariant);
						// Slice matches handles this
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

				return if let Some(sgs) = get_structure_generics_on(subtype, *on) {
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
				} else if let Type::Object(super::ObjectNature::RealDeal) = subtype {
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
				if let Type::Object(_) = subtype {
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
				}) = subtype
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
					crate::utilities::notify!("Not array-ish {:?}", subtype);
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			} else {
				fn is_arguments_cyclic(a: &GenericArguments) -> bool {
					if let GenericArguments::ExplicitRestrictions(arguments) = a {
						arguments.iter().any(|(left, (right, _))| left == right)
					} else {
						false
					}
				}
				// TODO temp fix
				if is_arguments_cyclic(arguments) {
					let GenericArguments::ExplicitRestrictions(arguments) = arguments else {
						unreachable!();
					};

					let filtered: crate::Map<_, _> = arguments
						.iter()
						.filter(|(left, (right, _))| left != right)
						.copied()
						.collect();
					let refe = GenericArguments::ExplicitRestrictions(filtered);
					let base_type_arguments =
						GenericChainLink::append(base_type, base_type_arguments.as_ref(), &refe);

					type_is_subtype_with_generics(
						(*on, base_type_arguments),
						(ty, ty_structure_arguments),
						state,
						information,
						types,
					)
				} else {
					let base_type_arguments = GenericChainLink::append(
						base_type,
						base_type_arguments.as_ref(),
						arguments,
					);

					type_is_subtype_with_generics(
						(*on, base_type_arguments),
						(ty, ty_structure_arguments),
						state,
						information,
						types,
					)
				}
			}
		}
		Type::Constructor(cst) => match cst {
			// For template literal types
			Constructor::BinaryOperator {
				operator: crate::types::MathematicalOrBitwiseOperation::Add,
				result: TypeId::STRING_TYPE,
				..
			} => {
				if let Type::Constant(Constant::String(rs)) = subtype {
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
				} else if let Type::Constructor(Constructor::BinaryOperator {
					operator: crate::types::MathematicalOrBitwiseOperation::Add,
					result: TypeId::STRING_TYPE,
					..
				}) = subtype
				{
					crate::utilities::notify!("TODO test prefixes");
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				} else {
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			}
			Constructor::BinaryOperator {
				operator: crate::types::MathematicalOrBitwiseOperation::Add,
				result: TypeId::NUMBER_TYPE,
				..
			} => {
				crate::utilities::notify!("TODO here!");
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			}
			Constructor::BinaryOperator { .. } | Constructor::CanonicalRelationOperator { .. } => {
				unreachable!("invalid constructor on LHS")
			}
			Constructor::TypeOperator(_) => todo!(),
			Constructor::TypeExtends(_) => todo!(),
			Constructor::Image { on: _, with: _, result } => {
				crate::utilities::notify!("Here");
				type_is_subtype_with_generics(
					(*result, base_type_arguments),
					(ty, ty_structure_arguments),
					state,
					information,
					types,
				)
			}
			Constructor::ConditionalResult {
				condition,
				truthy_result: _,
				otherwise_result,
				result_union: result,
			} => {
				// implements `assert is condition annotation`
				if let (
					Type::Constructor(Constructor::TypeExtends(extends)),
					TypeId::NEVER_TYPE,
					Type::Constructor(Constructor::ConditionalResult {
						condition: rhs_condition,
						truthy_result: _,
						otherwise_result: TypeId::NEVER_TYPE,
						result_union: _,
					}),
				) = (types.get_type_by_id(*condition), *otherwise_result, subtype)
				{
					if extends.equal_to_rhs(*rhs_condition, types) {
						SubTypeResult::IsSubType
					} else {
						crate::utilities::notify!(
							"Here {:?}",
							types.get_type_by_id(*rhs_condition)
						);
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				} else {
					crate::utilities::notify!("Here {:?}", subtype);

					type_is_subtype_with_generics(
						(*result, base_type_arguments),
						(ty, ty_structure_arguments),
						state,
						information,
						types,
					)
				}
			}
			Constructor::Property { on, under, result: _, mode: _ } => {
				// Ezno custom state
				// TODO might be based of T
				if let Type::Constructor(Constructor::Property {
					on: r_on,
					under: r_under,
					result: _,
					mode: _,
				}) = subtype
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
						// 	crate::types::PolyNature::MappedGeneric { name, extends },
						// ) = ty
						// {
						// 	*extends
						// } else {
						// 	*original
						// };

						let original = *original;
						new_under = if let Some(under) = base_type_arguments
							.and_then(|args| args.get_argument_covariant(original))
						{
							under.into_property_key()
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
						(Publicity::Public, under, ty_structure_arguments),
						false,
						information,
						types,
					);
					if let Ok(LogicalOrValid::Logical(property)) = property {
						crate::utilities::notify!("Here 3");
						match property {
							Logical::Pure(property) => {
								crate::utilities::notify!("Here 4 {:?}", property);
								let property_value = property.as_get_type(types);
								return type_is_subtype_with_generics(
									(property_value, base_type_arguments),
									(ty, ty_structure_arguments),
									state,
									information,
									types,
								);
							}
							Logical::BasedOnKey(BasedOnKey::Right(PropertyOn { on, key })) => {
								crate::utilities::notify!("TODO {:?}", (on, key));
								// let filter = get_constraint(key, types).unwrap_or(key);

								// let properties =
								// 	crate::types::properties::get_properties_on_single_type(
								// 		on,
								// 		types,
								// 		information,
								// 		false,
								// 		filter,
								// 	);

								// for (_, _, value) in properties {
								// 	crate::utilities::notify!("{:?}", value);
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
								// }
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
				if let Type::Constant(crate::Constant::String(s)) = subtype {
					let get_property_unbound = get_property_unbound(
						(*on, base_type_arguments),
						(
							Publicity::Public,
							&PropertyKey::String(std::borrow::Cow::Borrowed(s)),
							ty_structure_arguments,
						),
						false,
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
		// TODO WIP nominal mechanism
		Type::Class { .. } => match subtype {
			Type::Constant(constant) => {
				if constant.get_backing_type() == base_type {
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
						crate::utilities::notify!(
							"Mismatched prototype {:?} != {:?}",
							prototype,
							base_type
						);
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				} else {
					crate::utilities::notify!("No prototype");
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
			Type::And(left, right) => {
				// This only happens in predicate edge cases (with numbers)
				let left_result = type_is_subtype_with_generics(
					(base_type, base_type_arguments),
					(*left, ty_structure_arguments),
					state,
					information,
					types,
				);

				if let SubTypeResult::IsSubType = left_result {
					left_result
				} else {
					type_is_subtype_with_generics(
						(base_type, base_type_arguments),
						(*right, ty_structure_arguments),
						state,
						information,
						types,
					)
				}
			}
			Type::SpecialObject(SpecialObject::Function(..)) | Type::FunctionReference(..)
				if base_type == TypeId::FUNCTION_TYPE =>
			{
				SubTypeResult::IsSubType
			}
			_ty => {
				// crate::utilities::notify!("{:?} does not match class", base_type);
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			}
		},
		Type::Interface { .. } => {
			// TODO weird that these are interfaces
			// If not captured above
			if matches!(base_type, TypeId::UNDEFINED_TYPE | TypeId::NULL_TYPE | TypeId::NEVER_TYPE)
			{
				return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
			}

			// TODO a bit messy
			match subtype {
				Type::Constant(constant) => {
					if constant.get_backing_type() == base_type {
						SubTypeResult::IsSubType
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				}
				Type::Object(..) => subtype_floating_properties(
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
				Type::FunctionReference(_)
				| Type::SpecialObject(_)
				| Type::Class { .. }
				| Type::AliasTo { .. }
				| Type::Interface { .. } => {
					crate::utilities::notify!("supertype={:?}, subtype={:?}", supertype, subtype);
					// TODO
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
				Type::Narrowed { .. } | Type::Constructor(..) | Type::RootPolyType(..) => {
					let arg =
						base_type_arguments.and_then(|args| args.get_argument_covariant(base_type));

					crate::utilities::notify!("TODO {:?}", arg);
					SubTypeResult::IsSubType

					// if let Some(args) = arg {
					// 	for arg in args {
					// 		let result = type_is_subtype_with_generics(
					// 			(arg, base_type_arguments),
					// 			(ty, ty_structure_arguments),
					// 			state,
					// 			information,
					// 			types,
					// 		);

					// 		if let e @ SubTypeResult::IsNotSubType(_) = result {
					// 			return e;
					// 		}
					// 	}
					// 	SubTypeResult::IsSubType
					// } else {
					// 	let to = get_constraint(ty, types).unwrap();

					// 	if to == TypeId::ANY_TYPE {
					// 		crate::utilities::notify!("Modify constraint for equality");
					// 	}

					// 	type_is_subtype_with_generics(
					// 		(base_type, base_type_arguments),
					// 		(to, ty_structure_arguments),
					// 		state,
					// 		information,
					// 		types,
					// 	)
					// }
				}
			}
		}
		Type::SpecialObject(SpecialObject::Null) => {
			crate::utilities::notify!("rhs={:?}", subtype);
			SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
		}
		Type::SpecialObject(_) => todo!(),
	}
}

fn subtype_function(
	(left_func, base_type_arguments): (crate::FunctionId, GenericChain),
	(subtype, ty, subtypepe_arguments): (&Type, TypeId, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> SubTypeResult {
	let right_func = if let Type::FunctionReference(right_func)
	| Type::SpecialObject(SpecialObject::Function(right_func, _)) = subtype
	{
		right_func
	} else if let Some(constraint) = get_constraint(ty, types) {
		// TODO explain why get_constraint early breaks a bunch of tests
		let subtype = types.get_type_by_id(constraint);
		if let Type::FunctionReference(right_func)
		| Type::SpecialObject(SpecialObject::Function(right_func, _)) = subtype
		{
			right_func
		} else {
			crate::utilities::notify!("Not function after constraint!! {:?}", subtype);
			return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
		}
	} else {
		crate::utilities::notify!("Not function!! {:?}", subtype);
		return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
	};

	let left_func = types.functions.get(&left_func).unwrap();
	let right_func = types.functions.get(right_func).unwrap();

	for (idx, lhs_param) in left_func.parameters.parameters.iter().enumerate() {
		if let Some((right_param_ty, position)) =
			right_func.parameters.get_parameter_type_at_index(idx)
		{
			let last_mode =
				std::mem::replace(&mut state.mode, SubTypingMode::Covariant { position });

			// Reverse is important
			let result = type_is_subtype_with_generics(
				(right_param_ty, subtypepe_arguments),
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
					subtypepe_arguments,
					lhs,
					base_type_arguments
				);
				// TODO don't short circuit
				return err;
			}

			state.mode = last_mode;
		} else {
			// This is allowed. TODO only in some cases
			// if !lhs_param.is_optional {
			// 	crate::utilities::notify!("Expected parameter, for non optional parameter");
			// 	return SubTypeResult::IsNotSubType(NonEqualityReason::MissingParameter);
			// }
		}
	}

	// TODO optional and rest parameters

	// `void` return type means anything goes here
	if TypeId::VOID_TYPE == left_func.return_type {
		SubTypeResult::IsSubType
	} else {
		let type_is_subtype_with_generics = type_is_subtype_with_generics(
			(left_func.return_type, base_type_arguments),
			(right_func.return_type, subtypepe_arguments),
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

fn subtype_floating_properties(
	(base_type, base_type_arguments): (TypeId, GenericChain),
	(ty, subtypepe_arguments): (TypeId, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> SubTypeResult {
	let reversed_flattened_properties_on_base = information
		.get_chain_of_info()
		.filter_map(|info| info.current_properties.get(&base_type).map(|v| v.iter().rev()))
		.flatten();

	subtype_properties(
		(base_type, reversed_flattened_properties_on_base, base_type_arguments),
		(ty, subtypepe_arguments),
		state,
		information,
		types,
	)
}

fn subtype_properties<'a, T>(
	(base_type, base_properties, base_type_arguments): (TypeId, T, GenericChain),
	(ty, subtypepe_arguments): (TypeId, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> SubTypeResult
where
	T: Iterator<Item = &'a (Publicity, PropertyKey<'static>, PropertyValue)> + 'a,
{
	// TODO this will cause problems if not reversed at end
	state.mode = state.mode.one_deeper();
	let mut property_errors = Vec::new();

	// Note this won't check for conditional stuff being true etc or things being deleted
	for (publicity, key, lhs_property) in base_properties {
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
			(ty, subtypepe_arguments),
			state,
			information,
			types,
		);

		if let Err(err) = result {
			property_errors.push((key.into_owned(), err));
		}
	}

	state.mode = state.mode.one_shallower();

	if property_errors.is_empty() {
		if let Type::Interface { extends: Some(extends), .. } = types.get_type_by_id(base_type) {
			let extends_result = type_is_subtype_with_generics(
				(*extends, base_type_arguments),
				(ty, subtypepe_arguments),
				state,
				information,
				types,
			);
			if let e @ SubTypeResult::IsNotSubType(_) = extends_result {
				return e;
			}
		}

		// Exclusive check
		if base_type_arguments.is_some_and(|base| base.exclusive_mode()) {
			use crate::types::properties;

			let get_properties = properties::get_properties_on_single_type(
				ty,
				types,
				information,
				false,
				TypeId::ANY_TYPE,
			);

			// Assert base_type contains all the keys of the LHS
			for (publicity, key, _value) in get_properties {
				let result = properties::get_property_unbound(
					(base_type, base_type_arguments),
					(publicity, &key, None),
					true,
					information,
					types,
				);

				// TODO more
				if result.is_err() {
					// TODO excess property
					return SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch);
				}
			}
		}

		// TODO type arguments
		if let Some(ref mut object_constraints) = state.object_constraints {
			let base_type = if let Some(GenericChainLink::PartiallyAppliedGenericArgumentsLink {
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

		SubTypeResult::IsSubType
	} else {
		SubTypeResult::IsNotSubType(NonEqualityReason::PropertiesInvalid {
			errors: property_errors,
		})
	}
}

fn check_lhs_property_is_super_type_of_rhs(
	(publicity, key): (Publicity, &PropertyKey<'_>),
	(lhs_property, base_type_arguments, optional): (&PropertyValue, GenericChain, bool),
	(ty, subtypepe_arguments): (TypeId, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> Result<(), PropertyError> {
	match lhs_property {
		PropertyValue::Value(lhs_value) => {
			let right_result = get_property_unbound(
				(ty, subtypepe_arguments),
				(publicity, key, base_type_arguments),
				false,
				information,
				types,
			);

			// {
			// 	crate::utilities::notify!("LHS value is {:?}", lhs_value);
			// 	crate::utilities::notify!(
			// 		"RHS value is {:?} {:?}",
			// 		right_result,
			// 		(key, base_type_arguments)
			// 	);
			// }

			match right_result {
				Ok(LogicalOrValid::Logical(res)) => {
					let res = check_logical_property(
						(*lhs_value, base_type_arguments, optional),
						(res, subtypepe_arguments),
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
				// PROXY HANDLING!!
				Ok(LogicalOrValid::NeedsCalculation(NeedsCalculation::Proxy(
					objects::Proxy { handler, over },
					_,
				))) => {
					crate::utilities::notify!("TODO set as well?");
					let get_handler = get_property_unbound(
						(handler, subtypepe_arguments),
						(
							Publicity::Public,
							&PropertyKey::String(std::borrow::Cow::Borrowed("get")),
							base_type_arguments,
						),
						false,
						information,
						types,
					);

					if let Ok(LogicalOrValid::Logical(Logical::Pure(get_res))) = get_handler {
						let function = get_res.as_get_type(types);
						if let Type::SpecialObject(SpecialObject::Function(id, _)) =
							types.get_type_by_id(function)
						{
							let function = types.get_function_from_id(*id);
							let mut map = crate::Map::new();
							// `Some` weird but accounts for missing parameters
							if let Some((first, _)) =
								function.parameters.get_parameter_type_at_index(0)
							{
								map.insert(first, (CovariantContribution::TypeId(over), 0));
							}
							if let Some((second, _)) =
								function.parameters.get_parameter_type_at_index(1)
							{
								map.insert(
									second,
									(CovariantContribution::from(key.clone().into_owned()), 0),
								);
							}
							if let Some((third, _)) =
								function.parameters.get_parameter_type_at_index(2)
							{
								map.insert(third, (CovariantContribution::TypeId(handler), 0));
							}

							let subtypepe_arguments = Some(GenericChainLink::MappedPropertyLink {
								parent_link: subtypepe_arguments.as_ref(),
								value: &map,
							});

							let result = type_is_subtype_with_generics(
								(*lhs_value, base_type_arguments),
								(function.return_type, subtypepe_arguments),
								state,
								information,
								types,
							);
							if let SubTypeResult::IsSubType = result {
								Ok(())
							} else {
								// crate::utilities::notify!("One missing");
								Err(PropertyError::Missing)
							}
						} else {
							crate::utilities::notify!("{:?}", get_res);

							check_lhs_property_is_super_type_of_rhs(
								(publicity, key),
								(lhs_property, base_type_arguments, optional),
								(handler, subtypepe_arguments),
								state,
								information,
								types,
							)
						}
					} else {
						check_lhs_property_is_super_type_of_rhs(
							(publicity, key),
							(lhs_property, base_type_arguments, optional),
							(handler, subtypepe_arguments),
							state,
							information,
							types,
						)
					}
				}
				Ok(LogicalOrValid::NeedsCalculation(NeedsCalculation::Infer { .. })) => {
					crate::utilities::notify!("TODO add constraint candidate");
					Ok(())
				}
				Err(_) => {
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
			todo!("{:?}", (getter, setter));
		}
		PropertyValue::Getter(_getter) => {
			let res =
				get_property_unbound((ty, None), (publicity, key, None), true, information, types);
			crate::utilities::notify!("looking for {:?} found {:?}", key, res);

			match res {
				Ok(LogicalOrValid::Logical(_res)) => {
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
			let rhs =
				get_property_unbound((ty, None), (publicity, key, None), true, information, types);

			match rhs {
				Ok(ok) => {
					crate::utilities::notify!("Set vs {:?}", ok);
					Ok(())
				}
				Err(_err) => Err(PropertyError::Missing),
			}
		}
		PropertyValue::Deleted => {
			// TODO WIP
			let res =
				get_property_unbound((ty, None), (publicity, key, None), true, information, types);
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

			// TODO `NON_OPTIONAL_KEY_ARGUMENT` temp
			let is_optional =
				!matches!(*condition, TypeId::TRUE | TypeId::NON_OPTIONAL_KEY_ARGUMENT);

			check_lhs_property_is_super_type_of_rhs(
				(publicity, key),
				(truthy, base_type_arguments, is_optional),
				(ty, subtypepe_arguments),
				state,
				information,
				types,
			)
			// if let PropertyValue::Value(lhs_value) = &**truthy {
			// let property = get_property_unbound(
			// 	(ty, subtypepe_arguments),
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
			// 		TypeId::UNIMPLEMENTED_ERROR_TYPE
			// 	};

			// 	crate::utilities::notify!("{:?}", property);

			// 	let res = check_logical_property(
			// 		(*lhs_value, base_type_arguments),
			// 		(property, subtypepe_arguments),
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
				(on, base_type_arguments, optional),
				(ty, subtypepe_arguments),
				state,
				information,
				types,
			)
		}
	}
}

fn check_logical_property(
	(lhs_property_value, lhs_property_value_type_arguments, optional): (TypeId, GenericChain, bool),
	(rhs_property, subtypepe_arguments): (Logical<PropertyValue>, GenericChain),
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
				(rhs_type, subtypepe_arguments),
				state,
				information,
				types,
			)
		}
		Logical::Or { condition, left, right } => {
			crate::utilities::notify!("{:?}", (condition, &left, &right));

			if let (LogicalOrValid::Logical(left), LogicalOrValid::Logical(right)) = (*left, *right)
			{
				let left_result = check_logical_property(
					(lhs_property_value, lhs_property_value_type_arguments, optional),
					(left, subtypepe_arguments),
					state,
					information,
					types,
				);

				if let SubTypeResult::IsSubType = left_result {
					check_logical_property(
						(lhs_property_value, lhs_property_value_type_arguments, optional),
						(right, subtypepe_arguments),
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
		Logical::Implies { on, antecedent } => {
			crate::utilities::notify!("{:?}", antecedent);
			check_logical_property(
				(lhs_property_value, lhs_property_value_type_arguments, optional),
				(
					*on,
					GenericChainLink::append(
						TypeId::UNIMPLEMENTED_ERROR_TYPE,
						subtypepe_arguments.as_ref(),
						&antecedent,
					),
				),
				state,
				information,
				types,
			)
		}
		Logical::BasedOnKey(kind) => match kind {
			BasedOnKey::Left { value, key_arguments } => {
				let property_generics = Some(GenericChainLink::MappedPropertyLink {
					parent_link: subtypepe_arguments.as_ref(),
					value: &key_arguments,
				});
				check_logical_property(
					(lhs_property_value, lhs_property_value_type_arguments, optional),
					(*value, property_generics),
					state,
					information,
					types,
				)
			}
			BasedOnKey::Right(PropertyOn { on, key }) => {
				if let Type::RootPolyType(PolyNature::MappedGeneric { name: _, extends }) =
					types.get_type_by_id(key)
				{
					type_is_subtype_of_property_mapped_key(
						MappedKey { value: (*extends).into(), key },
						(lhs_property_value, lhs_property_value_type_arguments, optional),
						(on, subtypepe_arguments),
						state,
						information,
						types,
					)
				} else {
					let filter = get_constraint(key, types).unwrap_or(key);

					let properties = get_properties_on_single_type2(
						(on, subtypepe_arguments),
						types,
						information,
						filter,
					);
					for (_key, rhs_property, _args) in properties {
						let result = check_logical_property(
							(lhs_property_value, lhs_property_value_type_arguments, optional),
							(Logical::Pure(rhs_property), subtypepe_arguments),
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
	/// covariant contribution allows for slices and `PropertyKey::String`
	pub value: CovariantContribution,
	/// This points towards the `Type::RootPolyType(PolyNature::MappedGeneric)`
	pub key: TypeId,
}

pub fn type_is_subtype_of_property_mapped_key(
	mapped_key: MappedKey,
	(base, property_generics, optional): (TypeId, GenericChain, bool),
	(ty, subtypepe_arguments): (TypeId, GenericChain),
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
					(property_generics.as_ref(), subtypepe_arguments.as_ref())
				);
			}
			let right_property = get_property_unbound(
				(ty, subtypepe_arguments),
				(
					Publicity::Public,
					&PropertyKey::String(std::borrow::Cow::Owned(s.to_owned())),
					None,
				),
				false,
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
						(right_property, subtypepe_arguments),
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
				Type::Narrowed { narrowed_to: to, .. }
				| Type::AliasTo { to, name: _, parameters: _ } => type_is_subtype_of_property_mapped_key(
					MappedKey { value: (*to).into(), key: mapped_key.key },
					(base, property_generics, optional),
					(ty, subtypepe_arguments),
					state,
					information,
					types,
				),
				Type::And(left, right) => {
					let left = type_is_subtype_of_property_mapped_key(
						MappedKey { value: (*left).into(), key: mapped_key.key },
						(base, property_generics, optional),
						(ty, subtypepe_arguments),
						state,
						information,
						types,
					);
					if left.is_mismatch() {
						type_is_subtype_of_property_mapped_key(
							MappedKey { value: (*right).into(), key: mapped_key.key },
							(base, property_generics, optional),
							(ty, subtypepe_arguments),
							state,
							information,
							types,
						)
					} else {
						left
					}
				}
				Type::Or(left, right) => {
					let left = type_is_subtype_of_property_mapped_key(
						MappedKey { value: (*left).into(), key: mapped_key.key },
						(base, property_generics, optional),
						(ty, subtypepe_arguments),
						state,
						information,
						types,
					);
					if left.is_mismatch() {
						left
					} else {
						type_is_subtype_of_property_mapped_key(
							MappedKey { value: (*right).into(), key: mapped_key.key },
							(base, property_generics, optional),
							(ty, subtypepe_arguments),
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
							(ty, subtypepe_arguments),
							state,
							information,
							types,
						)
					} else {
						todo!("no value {:?}", (ty, property_generics))
					}
				}
				Type::Constructor(Constructor::Property { .. }) => {
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
							(ty, subtypepe_arguments),
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
				Type::Interface { .. } => todo!(),
				Type::Class { .. } => todo!(),
				Type::Constant(_) => {
					let right_property = get_property_unbound(
						(ty, subtypepe_arguments),
						(Publicity::Public, &PropertyKey::Type(key_ty), subtypepe_arguments),
						true,
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
								(right_property, subtypepe_arguments),
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
		value => todo!("{:?}", value),
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
			// if let BasedOnKey::Right { on, key } = on {
			// 	if let Type::RootPolyType(PolyNature::MappedGeneric { name: _, extends }) =
			// 		types.get_type_by_id(*filter)
			// 	{
			// 		type_is_subtype_of_(
			// 			Some(MappedKey { value: *extends, key: *filter }),
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
			// TODO temp fix to set keyof arguments
			{
				let constraint = rpt.get_constraint();
				if let Type::Constructor(Constructor::KeyOf { .. }) =
					types.get_type_by_id(constraint)
				{
					let mut new_contributions = SliceArguments::default();
					let _ = slice_matches_type(
						(constraint, base_type_arguments),
						slice,
						Some(&mut new_contributions),
						information,
						types,
						allow_casts,
					);
					if let Some(ref mut contributions) = contributions {
						contributions.extend(new_contributions);
					}
				}
			}

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
				// assert!(rpt.is_substitutable(), "{:?}", rpt);
				let constraint = rpt.get_constraint();
				let res = slice_matches_type(
					(constraint, base_type_arguments),
					slice,
					Some(contributions),
					information,
					types,
					allow_casts,
				);
				if res {
					contributions
						.insert(base, (CovariantContribution::String(slice.to_owned()), 0));
				}
				res
			} else {
				false
			}
		}
		Type::Narrowed { narrowed_to: to, .. } | Type::AliasTo { to, .. } => slice_matches_type(
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
					contributions.extend(new_contributions);
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
					contributions.extend(new_contributions);
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

				let mut new_contributions = SliceArguments::default();
				// TODO any contributions in here SHOULD be wrapped in case insensitive
				let matches = slice_matches_type(
					(inner, generic_chain_link),
					slice,
					Some(&mut new_contributions),
					information,
					types,
					allow_casts,
				);
				if let (true, Some(current)) = (matches, contributions) {
					crate::utilities::notify!("{:?}", new_contributions);
					for (id, (c, d)) in new_contributions {
						current
							.insert(id, (CovariantContribution::CaseInsensitive(Box::new(c)), d));
					}
					crate::utilities::notify!("{:?}", current);
				}
				matches
			} else {
				false
			}
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::CASE_INSENSITIVE,
			arguments,
		}) => {
			let base_type_arguments = Some(GenericChainLink::SpecialGenericChainLink {
				parent_link: base_type_arguments.as_ref(),
				special: SpecialGenericChainLink::CaseInsensitive,
			});
			let inner = arguments.get_structure_restriction(TypeId::STRING_GENERIC).unwrap();
			slice_matches_type(
				(inner, base_type_arguments),
				slice,
				contributions,
				information,
				types,
				allow_casts,
			)
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
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, arguments: _ })
			if allow_casts && intrinsics::is_ezno_number_intrinsic(*on) =>
		{
			// Special behavior here to treat numerical property keys (which are strings) as numbers
			if let Ok(value) = slice.parse::<f64>() {
				number_matches_type(
					(base, base_type_arguments),
					value,
					contributions,
					information,
					types,
				)
			} else {
				false
			}
		}
		Type::Constructor(super::Constructor::KeyOf(on)) => {
			let argument =
				(Publicity::Public, &PropertyKey::String(std::borrow::Cow::Borrowed(slice)), None);

			let arg = base_type_arguments
				.as_ref()
				.and_then(|link| link.get_single_argument(*on))
				.unwrap_or(*on);

			let property = get_property_unbound(
				(arg, base_type_arguments),
				argument,
				true,
				information,
				types,
			);

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
					crate::utilities::notify!(
						"For MT set: (is_writable, is_defined)={:?}",
						(is_writable, is_defined)
					);
				}

				true
			} else {
				false
			}
		}
		Type::Constant(Constant::Number(base)) => {
			crate::utilities::notify!("Here");
			if let Ok(slice_as_float) = slice.parse::<f64>() {
				*base == slice_as_float
			} else {
				false
			}
		}
		Type::Constructor(super::Constructor::BinaryOperator {
			lhs,
			rhs,
			operator: MathematicalOrBitwiseOperation::Add,
			result: _,
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

// TODO keyof
#[allow(clippy::only_used_in_recursion)]
pub(crate) fn number_matches_type(
	(base, base_type_arguments): (TypeId, Option<super::GenericChainLink>),
	number: f64,
	mut contributions: Option<&mut SliceArguments>,
	information: &impl InformationChain,
	types: &TypeStore,
) -> bool {
	match types.get_type_by_id(base) {
		Type::Constant(cst) => {
			if let Constant::Number(base_number) = cst {
				*base_number == number
			} else {
				false
			}
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::MULTIPLE_OF,
			arguments,
		}) => {
			let argument = arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
			if let Type::Constant(Constant::Number(argument)) = types.get_type_by_id(argument) {
				let number: ordered_float::NotNan<f64> = number.try_into().unwrap();
				(number % argument) == 0.
			} else {
				crate::utilities::notify!("Here?");
				false
			}
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::LESS_THAN,
			arguments: _,
		}) => {
			todo!()
			// let lhs_range = intrinsics::get_range(base, types).unwrap();
			// intrinsics::FloatRange::new_single(number.try_into().unwrap()).contained_in(lhs_range)
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::GREATER_THAN,
			arguments: _,
		}) => {
			todo!()
			// let lhs_range = intrinsics::get_range(base, types).unwrap();
			// intrinsics::FloatRange::new_single(number.try_into().unwrap()).contained_in(lhs_range)
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::NOT_RESTRICTION,
			arguments,
		}) => {
			let argument = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();

			!number_matches_type(
				(argument, base_type_arguments),
				number,
				contributions,
				information,
				types,
			)
		}
		Type::Or(l, r) => {
			// TODO temp for
			let mut new_contributions = SliceArguments::default();
			let matches = number_matches_type(
				(*l, base_type_arguments),
				number,
				Some(&mut new_contributions),
				information,
				types,
			);
			if matches {
				if let Some(ref mut contributions) = contributions {
					contributions.extend(new_contributions);
				}
				true
			} else {
				// TODO clear contributions
				number_matches_type(
					(*r, base_type_arguments),
					number,
					contributions,
					information,
					types,
				)
			}
		}
		Type::And(l, r) => {
			let mut new_contributions = SliceArguments::default();
			let matches = number_matches_type(
				(*l, base_type_arguments),
				number,
				Some(&mut new_contributions),
				information,
				types,
			);
			if matches {
				if let Some(ref mut contributions) = contributions {
					contributions.extend(new_contributions);
				}
				number_matches_type(
					(*r, base_type_arguments),
					number,
					contributions,
					information,
					types,
				)
			} else {
				false
			}
		}
		ty => {
			crate::utilities::notify!("TODO number matches ty={:?}", ty);
			true
		}
	}
}
