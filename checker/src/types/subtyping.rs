//! Type subtype checking. (making sure the RHS type contains all the properties the LHS type requires)

use source_map::SpanWithSource;

use crate::{
	context::{information::InformationChain, GeneralContext, Logical},
	features::{objects::SpecialObject, operations::MathematicalAndBitwise},
	types::{
		generics::{
			contributions::{Contributions, CovariantContribution},
			generic_type_arguments::GenericArguments,
		},
		printing::print_type,
		properties::{get_property_unbound, key_matches, Publicity, SliceArgument},
		GenericChainLink, ObjectNature, TypeStore,
	},
	Constant, Environment, PropertyValue, TypeId,
};

use super::{
	get_constraint, properties::PropertyKey, Constructor, GenericChain, PartiallyAppliedGenerics,
	PolyNature, Type,
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
	(base_type, base_structure_arguments): (TypeId, GenericChain),
	(ty, ty_structure_arguments): (TypeId, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> SubTypeResult {
	// {
	// 	let debug = true;
	// 	crate::utilities::notify!(
	// 		"Checking {} :>= {}",
	// 		print_type(base_type, types, information, debug),
	// 		print_type(ty, types, information, debug)
	// 	);
	// }

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
				(base_type, base_structure_arguments),
				(*left, ty_structure_arguments),
				state,
				information,
				types,
			);

			return if let SubTypeResult::IsSubType = left_result {
				type_is_subtype_with_generics(
					(base_type, base_structure_arguments),
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
		// 	let right = *right;
		// 	let left_result = type_is_subtype_with_generics(
		// 		base_type,
		// 		base_structure_arguments,
		// 		*left,
		// 		ty_structure_arguments,
		// 		state,
		// 		information,
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
		// 			state,
		// 			information,
		// 			types,
		// 			mode,
		// 			already_checked
		// 		)
		// 	};
		// }
		Type::PartiallyAppliedGenerics(..) => {}
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
						(base_type, base_structure_arguments),
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
					(base_type, base_structure_arguments),
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
		_ => (),
	}

	match left_ty {
		Type::FunctionReference(left_func)
		| Type::SpecialObject(SpecialObject::Function(left_func, _)) => subtype_function(
			(*left_func, base_structure_arguments),
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
				(base_type, base_structure_arguments),
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
			crate::utilities::notify!("AND: Checking left and right");
			let left_result = type_is_subtype_with_generics(
				(*left, base_structure_arguments),
				(ty, ty_structure_arguments),
				state,
				information,
				types,
			);

			if let SubTypeResult::IsSubType = left_result {
				type_is_subtype_with_generics(
					(right, base_structure_arguments),
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
				(*left, base_structure_arguments),
				(ty, ty_structure_arguments),
				state,
				information,
				types,
			);

			if let SubTypeResult::IsSubType = left_result {
				if state.contributions.is_some() {
					// only for double generics specialisation. Otherwise short-circuiting is fine
					let _res = type_is_subtype_with_generics(
						(right, base_structure_arguments),
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
					(right, base_structure_arguments),
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
			let base_argument_for_current = base_structure_arguments
				.and_then(|args| args.get_argument(base_type, information, types));

			if let Some(args) = base_argument_for_current {
				// TODO what
				for arg in args {
					let result = type_is_subtype_with_generics(
						(arg, base_structure_arguments),
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
					(constraint, base_structure_arguments),
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
					todo!()
				}
				// TSC intrinsics
				TypeId::STRING_CAPITALIZE
				| TypeId::STRING_UNCAPITALIZE
				| TypeId::STRING_LOWERCASE
				| TypeId::STRING_UPPERCASE => {
					if let Type::Constant(Constant::String(rs)) = right_ty {
						let (matches, value) = slice_matches_type(
							(base_type, base_structure_arguments),
							rs,
							information,
							types,
						);
						if let (Some((to, value)), Some(contributions)) =
							(value, state.contributions.as_mut())
						{
							contributions.staging_contravariant.insert(to, value);
						}
						return if matches {
							SubTypeResult::IsSubType
						} else {
							SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						};
					}
				}
				// Ezno intrinsic
				TypeId::LITERAL_RESTRICTION => {
					let inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
					return if let Type::Constant(rhs_constant) = right_ty {
						type_is_subtype_with_generics(
							(inner, base_structure_arguments),
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
				TypeId::NOINFER => {
					let on = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
					let current_contributing =
						state.contributions.as_ref().map(|c| c.staging_contravariant.len());
					let result = type_is_subtype_with_generics(
						(on, base_structure_arguments),
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
					let value =
						arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
					return match *on {
						TypeId::LESS_THAN => {
							if let (
								Type::Constant(Constant::Number(less_than)),
								Type::Constant(Constant::Number(value)),
							) = (types.get_type_by_id(value), right_ty)
							{
								if less_than < value {
									SubTypeResult::IsSubType
								} else {
									SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
								}
							} else {
								todo!()
							}
						}
						TypeId::GREATER_THAN => {
							if let (
								Type::Constant(Constant::Number(greater_than)),
								Type::Constant(Constant::Number(value)),
							) = (types.get_type_by_id(value), right_ty)
							{
								if greater_than > value {
									SubTypeResult::IsSubType
								} else {
									SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
								}
							} else {
								todo!()
							}
						}
						TypeId::MULTIPLE_OF => {
							if let (
								Type::Constant(Constant::Number(multiple)),
								Type::Constant(Constant::Number(value)),
							) = (types.get_type_by_id(value), right_ty)
							{
								if value % multiple == 0f64 {
									SubTypeResult::IsSubType
								} else {
									SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
								}
							} else {
								todo!()
							}
						}
						_ => unreachable!(),
					};
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
									(*lv, base_structure_arguments),
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
							for value in lookup.calculate_lookup(information, ty) {
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
						(left_arg, base_structure_arguments),
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
					GenericChainLink::append(base_type, base_structure_arguments.as_ref(), &into);

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
					let (matches, value) = slice_matches_type(
						(base_type, base_structure_arguments),
						rs,
						information,
						types,
					);
					if let (Some((to, value)), Some(contributions)) =
						(value, state.contributions.as_mut())
					{
						contributions.staging_contravariant.insert(to, value);
					}
					if matches {
						SubTypeResult::IsSubType
					} else {
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
				truthy_result: _,
				otherwise_result: _,
				result_union: _,
			} => todo!(),
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
				crate::utilities::notify!(
					"base_structure_arguments={:?}, ty_structure_arguments={:?}, *on={:?}",
					base_structure_arguments,
					ty_structure_arguments,
					on
				);

				if let Some(on) =
					base_structure_arguments.and_then(|args| args.get_single_argument(*on))
				{
					let new_under;
					let under = if let PropertyKey::Type(under) = under {
						crate::utilities::notify!(
							"{:?} with {:?}",
							under,
							base_structure_arguments.as_ref()
						);
						new_under = if let Some(under) = base_structure_arguments
							.and_then(|args| args.get_single_argument(*under))
						{
							crate::utilities::notify!("Here 2");
							PropertyKey::from_type(under, types)
						} else {
							PropertyKey::from_type(*under, types)
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
						(on, base_structure_arguments),
						(Publicity::Public, &under, ty_structure_arguments),
						information,
						types,
					);
					if let Ok(property) = property {
						crate::utilities::notify!("Here 3");
						match property {
							Logical::Pure(PropertyValue::Value(property)) => {
								crate::utilities::notify!("Here 4");
								return type_is_subtype_with_generics(
									(property, base_structure_arguments),
									(ty, ty_structure_arguments),
									state,
									information,
									types,
								);
							}
							value => todo!("{:?}", value), // Logical::Or { based_on, left, right } => todo!(),
							                               // Logical::Implies { on, antecedent } => todo!(),
						}
					}
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

				crate::utilities::notify!("Here {:?}", types.get_type_by_id(*on));

				crate::utilities::notify!("Mismatched property");
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			}
			Constructor::Awaited { .. } => todo!(),
			Constructor::KeyOf(on) => {
				if let Type::Constant(crate::Constant::String(s)) = right_ty {
					let get_property_unbound = get_property_unbound(
						(*on, base_structure_arguments),
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
				base_structure_arguments
			} else {
				base_structure_arguments
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
					(base_type, base_structure_arguments),
					right,
					state,
					information,
					types,
				)
			}
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
					(base_type, base_structure_arguments),
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
					// TODO fails if RHS is also OR type :(
					// let right = *right;
					// let left = type_is_subtype2(
					// 	base_type,
					// 	*left,
					// 	ty_arguments.as_deref(),
					// 	state,
					// 	information,
					// 	types,
					// );
					// if let SubTypeResult::IsSubType = left {
					// 	type_is_subtype2(
					// 		base_type,
					// 		right,
					// 		ty_arguments,
					// 		state,
					// 		information,
					// 		types,
					// 	)
					// } else {
					// 	crate::utilities::notify!("Left failed");
					// 	SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					// }
				}
				Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, arguments }) => {
					let into = arguments.clone();
					let append =
						GenericChainLink::append(ty, ty_structure_arguments.as_ref(), &into);
					type_is_subtype_with_generics(
						(base_type, base_structure_arguments),
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
					let arg = base_structure_arguments
						.and_then(|args| args.get_argument(base_type, information, types));

					if let Some(args) = arg {
						for arg in args {
							let result = type_is_subtype_with_generics(
								(arg, base_structure_arguments),
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
							(base_type, base_structure_arguments),
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
				if !lhs_param.is_optional {
					crate::utilities::notify!("Expected parameter, for non optional parameter");
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

	for (publicity, key, lhs_property) in reversed_flattened_properties_on_base {
		// crate::utilities::notify!(
		// 	"key {:?} with base_type_arguments={:?}",
		// 	key,
		// 	base_type_arguments
		// );

		let key = match key {
			PropertyKey::Type(ty) => {
				if let Some(base_type_arguments) = base_type_arguments {
					let ty = base_type_arguments.get_single_argument(*ty).unwrap_or(*ty);
					PropertyKey::from_type(ty, types)
				} else {
					key.clone()
				}
			}
			PropertyKey::String(_) => key.clone(),
		};

		let result = check_lhs_property_is_super_type_of_rhs(
			(*publicity, &key),
			(lhs_property, base_type_arguments),
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
		// TODO type arguments
		if let Some(ref mut object_constraints) = state.object_constraints {
			let base_type =
				if let Some(GenericChainLink::Link { ref from, parent_link, value: _ }) =
					base_type_arguments
				{
					if parent_link.is_none() {
						crate::utilities::notify!("TODO recursive get_from");
					}
					*from
				} else {
					base_type
				};
			object_constraints.push((ty, base_type));
		}

		if let Some(extends) = types.interface_extends.get(&base_type) {
			type_is_subtype_with_generics(
				(*extends, base_type_arguments),
				(ty, right_type_arguments),
				state,
				information,
				types,
			)
		} else {
			SubTypeResult::IsSubType
		}
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
	(lhs_property, base_type_arguments): (&PropertyValue, GenericChain),
	(ty, right_type_arguments): (TypeId, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> Result<(), PropertyError> {
	match lhs_property {
		PropertyValue::Value(lhs_value) => {
			// TODO should all values do this or is it only mapped generic ids
			// let root;
			// let base_type_arguments = if let Some((id, to)) = key.mapped_generic_id(types) {
			// 	// WIP, need to work for ors etc
			// 	// let to = base_type_arguments.and_then(|args| args.get_single_argument(to)).unwrap_or(to);
			// 	let mut map = crate::Map::default();
			// 	map.insert(id, (to, SpanWithSource::NULL));
			// 	root = GenericArguments::ExplicitRestrictions(map);
			// 	Some(GenericChainLink::append_to_link(id, base_type_arguments.as_ref(), &root))
			// } else {
			// 	base_type_arguments
			// };

			// Fix for mapped types and where { [x: string]: ... } etc
			if let PropertyKey::Type(TypeId::STRING_TYPE | TypeId::NUMBER_TYPE) = key {
				if let Type::Object(..) = types.get_type_by_id(ty) {
					let reversed_flattened_properties_on_on = information
						.get_chain_of_info()
						.filter_map(|info| info.current_properties.get(&ty).map(|v| v.iter().rev()))
						.flatten();

					for (_rhs_publicity, rhs_key, rhs_property) in
						reversed_flattened_properties_on_on
					{
						let (key_matches, arguments) = key_matches(
							(key, base_type_arguments),
							(rhs_key, right_type_arguments),
							information,
							types,
						);

						crate::utilities::notify!(
							"Not doing anything with arguments={:?}",
							arguments
						);

						// let root;
						// let base_type_arguments = if let Some(arguments) = arguments {
						// 	todo!()
						// 	// root = GenericArguments::ExplicitRestrictions(map);
						// 	// Some(GenericChainLink::append_to_link(
						// 	// 	id,
						// 	// 	base_type_arguments.as_ref(),
						// 	// 	&root,
						// 	// ))
						// } else {
						// 	base_type_arguments
						// };

						if key_matches {
							let pure = Logical::Pure(rhs_property.clone());
							let res = check_logical_property(
								(*lhs_value, base_type_arguments),
								(pure, right_type_arguments),
								state,
								information,
								types,
							);
							if let SubTypeResult::IsNotSubType(err) = res {
								return Err(PropertyError::Invalid {
									expected: TypeId::UNIMPLEMENTED_ERROR_TYPE,
									found: TypeId::UNIMPLEMENTED_ERROR_TYPE,
									mismatch: err,
								});
							}
						} else {
							crate::utilities::notify!("Here key does not match");
						}
					}
					Ok(())
				} else {
					crate::utilities::notify!("TODO more complex keys");
					Err(PropertyError::Missing)
				}
			} else {
				let property = get_property_unbound(
					(ty, right_type_arguments),
					(publicity, key, base_type_arguments),
					information,
					types,
				);

				{
					let lhs = types.get_type_by_id(*lhs_value);
					crate::utilities::notify!("{:?}", lhs);
				}

				match property {
					Ok(rhs_value) => {
						let res = check_logical_property(
							(*lhs_value, base_type_arguments),
							(rhs_value, right_type_arguments),
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
					Err(..) => Err(PropertyError::Missing),
				}
			}
		}
		PropertyValue::Getter(getter) => {
			let res = get_property_unbound((ty, None), (publicity, key, None), information, types);
			crate::utilities::notify!("looking for {:?} found {:?}", key, res);

			match res {
				Ok(res) => {
					let res = check_logical_property(
						(getter.return_type, base_type_arguments),
						(res, right_type_arguments),
						state,
						information,
						types,
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
		PropertyValue::Setter(_) => {
			let rhs = get_property_unbound((ty, None), (publicity, key, None), information, types);

			match rhs {
				Ok(ok) => {
					crate::utilities::notify!("Set vs {:?}", ok);
					Ok(())
				}
				Err(err) => match err {
					crate::context::MissingOrToCalculate::Missing
					| crate::context::MissingOrToCalculate::Error => {
						// This should be okay? Maybe warning, but I think it is okay
						Ok(())
					}
					crate::context::MissingOrToCalculate::Infer { on: _ } => todo!(),
					crate::context::MissingOrToCalculate::Proxy(_) => todo!(),
				},
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
		PropertyValue::ConditionallyExists { on: _, truthy } => {
			if let PropertyValue::Value(lhs_value) = &**truthy {
				let property = get_property_unbound(
					(ty, right_type_arguments),
					(publicity, key, base_type_arguments),
					information,
					types,
				);
				if let Ok(property) = property {
					// TODO
					let found = if let Logical::Pure(PropertyValue::Value(ref found)) = property {
						*found
					} else {
						TypeId::ERROR_TYPE
					};

					let res = check_logical_property(
						(*lhs_value, base_type_arguments),
						(property, right_type_arguments),
						state,
						information,
						types,
					);

					if let SubTypeResult::IsNotSubType(reason) = res {
						Err(PropertyError::Invalid {
							expected: *lhs_value,
							found,
							mismatch: reason,
						})
					} else {
						Ok(())
					}
				} else {
					// Okay if missing because of the above
					Ok(())
				}
			} else {
				todo!()
			}
		}
	}
}

fn check_logical_property(
	(base, base_type_arguments): (TypeId, GenericChain),
	(rhs_property, right_type_arguments): (Logical<PropertyValue>, GenericChain),
	state: &mut State,
	information: &impl InformationChain,
	types: &TypeStore,
) -> SubTypeResult {
	match rhs_property {
		Logical::Pure(rhs_property) => {
			let rhs_type = rhs_property.as_set_type();
			// crate::utilities::notify!(
			// 	"Checking {} with {}, against {}, left={:?}",
			// 	print_type(key, types, information, true),
			// 	print_type(property, types, information, true),
			// 	print_type(rhs_type, types, information, true),
			// 	base_type_arguments
			// );

			type_is_subtype_with_generics(
				(base, base_type_arguments),
				(rhs_type, right_type_arguments),
				state,
				information,
				types,
			)
		}
		Logical::Or { .. } => todo!(),
		Logical::Implies { on, antecedent } => check_logical_property(
			(base, GenericChainLink::append(base, base_type_arguments.as_ref(), &antecedent)),
			(*on, right_type_arguments),
			state,
			information,
			types,
		),
		Logical::BasedOnKey { .. } => todo!(),
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
			(prop.as_set_type(), property_generics),
			(ty, GenericChain::None),
			state,
			information,
			types,
		),
		Logical::Or { condition: _, left, right } => {
			let left_result = if let Ok(left) = &**left {
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
			} else if let Ok(right) = &**right {
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
		Logical::Implies { on, antecedent } => type_is_subtype_of_property(
			(
				on,
				GenericChainLink::append(
					TypeId::UNIMPLEMENTED_ERROR_TYPE,
					property_generics.as_ref(),
					antecedent,
				),
			),
			ty,
			state,
			information,
			types,
		),
		Logical::BasedOnKey { .. } => todo!(),
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
		}
	}
}

pub(crate) fn slice_matches_type(
	(base, base_type_arguments): (TypeId, Option<super::GenericChainLink>),
	slice: &str,
	information: &impl InformationChain,
	types: &TypeStore,
) -> (bool, Option<SliceArgument>) {
	let key_type = types.get_type_by_id(base);

	match key_type {
		Type::Constant(Constant::String(base_string)) => (base_string == slice, None),
		Type::RootPolyType(PolyNature::MappedGeneric { eager_fixed: to, .. }) => {
			let (res, _) =
				slice_matches_type((*to, base_type_arguments), slice, information, types);

			(res, Some((base, (CovariantContribution::String(slice.to_owned()), 0))))
		}
		Type::RootPolyType(_) => {
			if let Some(argument) = base_type_arguments.and_then(|v| v.get_single_argument(base)) {
				slice_matches_type((argument, base_type_arguments), slice, information, types)
			} else {
				crate::utilities::notify!("No argument in mapped ?");
				(false, None)
			}
		}
		Type::AliasTo { to, .. } => {
			slice_matches_type((*to, base_type_arguments), slice, information, types)
		}
		Type::Or(l, r) => {
			if let (true, value) =
				slice_matches_type((*l, base_type_arguments), slice, information, types)
			{
				(true, value)
			} else {
				slice_matches_type((*r, base_type_arguments), slice, information, types)
			}
		}
		Type::And(l, r) => {
			if let (true, value) =
				slice_matches_type((*l, base_type_arguments), slice, information, types)
			{
				let (result, other) =
					slice_matches_type((*r, base_type_arguments), slice, information, types);
				if value.is_some() && other.is_some() {
					todo!("competing mapped type arguments")
				}
				(result, value.or(other))
			} else {
				(false, None)
			}
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, arguments }) => {
			let matches_constraint = match *on {
				TypeId::STRING_CAPITALIZE => slice.chars().next().map_or(true, char::is_uppercase),
				TypeId::STRING_UNCAPITALIZE => {
					slice.chars().next().map_or(true, char::is_lowercase)
				}
				TypeId::STRING_LOWERCASE => slice.chars().all(char::is_lowercase),
				TypeId::STRING_UPPERCASE => slice.chars().all(char::is_uppercase),
				_ => unreachable!(),
			};

			if matches_constraint {
				let inner = arguments.get_structure_restriction(TypeId::STRING_GENERIC).unwrap();
				slice_matches_type((inner, base_type_arguments), slice, information, types)
			} else {
				(false, None)
			}
		}
		Type::Constructor(super::Constructor::KeyOf(on)) => {
			let var_name =
				(Publicity::Public, &PropertyKey::String(std::borrow::Cow::Borrowed(slice)), None);
			let arg = base_type_arguments
				.as_ref()
				.and_then(|link| link.get_single_argument(*on))
				.unwrap_or(*on);

			let get_property_unbound =
				get_property_unbound((arg, base_type_arguments), var_name, information, types);

			crate::utilities::notify!("Here {:?}", get_property_unbound);
			(get_property_unbound.is_ok(), None)
		}
		Type::Constructor(super::Constructor::BinaryOperator {
			lhs,
			rhs,
			operator: MathematicalAndBitwise::Add,
		}) => {
			if let Type::Constant(Constant::String(prefix)) = types.get_type_by_id(*lhs) {
				if let Some(after) = slice.strip_prefix(prefix) {
					slice_matches_type((*rhs, base_type_arguments), after, information, types)
				} else {
					(false, None)
				}
			} else if let Type::Constant(Constant::String(suffix)) = types.get_type_by_id(*rhs) {
				if let Some(before) = slice.strip_suffix(suffix) {
					slice_matches_type((*lhs, base_type_arguments), before, information, types)
				} else {
					(false, None)
				}
			} else {
				crate::utilities::notify!("More complex type");
				(false, None)
			}
		}
		_ => {
			if base == TypeId::STRING_TYPE || base == TypeId::ANY_TYPE {
				(true, None)
			} else {
				crate::utilities::notify!("Cannot match key {:?}", key_type);
				(false, None)
			}
		}
	}
}
