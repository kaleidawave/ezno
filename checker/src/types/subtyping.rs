//! Type subtype checking. (making sure the RHS type contains all the properties the LHS type requires)

use source_map::{Nullable, SpanWithSource};

use crate::{
	context::{information::InformationChain, Environment, GeneralContext, Logical},
	features::objects::SpecialObjects,
	types::{
		generics::{contributions::Contributions, generic_type_arguments::GenericArguments},
		printing::print_type,
		properties::{get_property_unbound, Publicity},
		GenericChainLink, ObjectNature, TypeStore,
	},
	Constant, PropertyValue, TypeId,
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
	/// Don't allow ERROR_TYPE to pass as everything. This allows using `satisfies` to check that LHS != error
	pub allow_errors: bool,
}

impl Default for SubTypingOptions {
	fn default() -> Self {
		Self { allow_errors: true }
	}
}

impl SubTypingOptions {
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

	let result = type_is_subtype(base_type, ty, &mut state, &environment, types);

	environment.add_object_constraints(state.object_constraints.unwrap().into_iter(), types);
	// TODO environment.add_inferred_constraints(x, types);

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
	environment: &Environment,
	types: &TypeStore,
) -> SubTypeResult {
	type_is_subtype_with_generics(
		(base_type, GenericChain::None),
		(ty, GenericChain::None),
		state,
		environment,
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

/// WIP
impl<'a> State<'a> {
	/// For `or`s, some items might have to be removed if the branch fails
	pub fn produce_save_point(&self) -> StateSavePoint {
		let contributions = self.contributions.as_ref();
		[
			self.already_checked.len() as u16,
			contributions.map_or(0, |c| c.staging_covariant.len().try_into().unwrap()),
			contributions.map_or(0, |c| c.staging_contravariant.len().try_into().unwrap()),
			self.object_constraints.as_ref().map_or(0, |c| c.len().try_into().unwrap()),
		]
	}

	/// For setting the state back to where it was at the point of [Self::produce_save_point]
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
	environment: &Environment,
	types: &TypeStore,
) -> SubTypeResult {
	// {
	// 	let debug = true;
	// 	crate::utilities::notify!(
	// 		"Checking {} :>= {}",
	// 		print_type(base_type, types, environment, debug),
	// 		print_type(ty, types, environment, debug)
	// 	);
	// }

	// (unless specified) treat as subtype as error would have already been thrown
	if state.others.allow_errors && (base_type == TypeId::ERROR_TYPE || ty == TypeId::ERROR_TYPE) {
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
				environment,
				types,
			);

			return if let SubTypeResult::IsSubType = left_result {
				type_is_subtype_with_generics(
					(base_type, base_structure_arguments),
					(right, ty_structure_arguments),
					state,
					environment,
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
		// 			state,
		// 			environment,
		// 			types,
		// 			mode,
		// 			already_checked
		// 		)
		// 	};
		// }
		Type::PartiallyAppliedGenerics(..) => {}
		Type::RootPolyType(..) | Type::Constructor(..) => {
			if let Some(args) =
				ty_structure_arguments.and_then(|tas| tas.get_argument(ty, environment, types))
			{
				// TODO what
				for arg in args {
					let result = type_is_subtype_with_generics(
						(base_type, base_structure_arguments),
						(arg, ty_structure_arguments),
						state,
						environment,
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
					environment,
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
		| Type::SpecialObject(SpecialObjects::Function(left_func, _)) => subtype_function(
			(*left_func, base_structure_arguments),
			(right_ty, ty, ty_structure_arguments),
			state,
			environment,
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
			assert!(matches!(nature, ObjectNature::AnonymousTypeAnnotation));

			subtype_properties(
				(base_type, base_structure_arguments),
				(ty, ty_structure_arguments),
				state,
				environment,
				types,
			)

			// let _left = print_type(base_type, types, environment, true);

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
				environment,
				types,
			);

			if let SubTypeResult::IsSubType = left_result {
				type_is_subtype_with_generics(
					(right, base_structure_arguments),
					(ty, ty_structure_arguments),
					state,
					environment,
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
				environment,
				types,
			);

			if let SubTypeResult::IsSubType = left_result {
				if state.contributions.is_some() {
					// only for double generics specialisation. Otherwise short-circuiting is fine
					let _res = type_is_subtype_with_generics(
						(right, base_structure_arguments),
						(ty, ty_structure_arguments),
						state,
						environment,
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
					environment,
					types,
				)
			}
		}
		Type::RootPolyType(nature) => {
			// TODO little weird, handing two very different cases beside each other. Might introduce bugs.. :(
			let base_argument_for_current = base_structure_arguments
				.and_then(|args| args.get_argument(base_type, environment, types));

			if let Some(args) = base_argument_for_current {
				// TODO what
				for arg in args {
					let result = type_is_subtype_with_generics(
						(arg, base_structure_arguments),
						(ty, ty_structure_arguments),
						state,
						environment,
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
								environment,
								types,
							)
						} else if let Some(constraint) = nature.try_get_constraint() {
							type_is_subtype_with_generics(
								(constraint, GenericChain::None),
								(ty, ty_structure_arguments),
								state,
								environment,
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
					environment,
					types,
				)
			}
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, arguments }) => {
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
									environment,
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
						environment.get_chain_of_info().find_map(|info| info.prototypes.get(&ty));

					crate::utilities::notify!("prototype is {:?}", prototype);

					if prototype.is_some_and(|prototype| prototype == on) {
						for (argument, lookup) in lookup.iter() {
							// TODO no vec
							let backing_type =
								arguments.get_structure_restriction(*argument).unwrap();
							for value in lookup.calculate_lookup(environment, ty) {
								let type_is_subtype =
									type_is_subtype(backing_type, value, state, environment, types);
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
					print_type(backing_type, types, environment, false)
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
					// for value in lookup_restriction.calculate_lookup(environment) {

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
						environment,
						types,
					)
				} else {
					crate::utilities::notify!("Not array-ish {:?}", right_ty);
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			} else {
				let into = arguments.clone().into();

				let base_type_arguments =
					GenericChainLink::append(base_type, base_structure_arguments.as_ref(), &into);

				type_is_subtype_with_generics(
					(*on, base_type_arguments),
					(ty, ty_structure_arguments),
					state,
					environment,
					types,
				)
			}
		}
		Type::Constructor(cst) => match cst {
			// For template literal types
			Constructor::BinaryOperator {
				lhs,
				rhs,
				operator: crate::types::MathematicalAndBitwise::Add,
			} => {
				if let Type::Constant(Constant::String(rs)) = right_ty {
					// TODO abstract
					if let Type::Constant(Constant::String(ls)) = types.get_type_by_id(*lhs) {
						let matches = rs.starts_with(ls);
						if let (true, TypeId::STRING_TYPE) = (matches, *rhs) {
							SubTypeResult::IsSubType
						} else {
							crate::utilities::notify!("TODO more complex {:?}", (matches, rhs));
							SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
						}
					} else {
						crate::utilities::notify!("TODO prefix equality");
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
			Constructor::Property { on, under, result: _, bind_this: _ } => {
				// Ezno custom state
				// TODO might be based of T
				if let Type::Constructor(Constructor::Property {
					on: r_on,
					under: r_under,
					result: _,
					bind_this: _,
				}) = right_ty
				{
					if on == r_on && under == r_under {
						return SubTypeResult::IsSubType;
					}
				}

				// TODO this only seems to work in simple cases. For mapped types
				if let Some(on) =
					base_structure_arguments.and_then(|args| args.get_single_argument(*on))
				{
					crate::utilities::notify!("Here got on");
					if let PropertyKey::Type(under) = under {
						crate::utilities::notify!(
							"{:?} with {:?}",
							under,
							base_structure_arguments.as_ref()
						);
						if let Some(under) = base_structure_arguments
							.and_then(|args| args.get_single_argument(*under))
						{
							crate::utilities::notify!("Here 2");
							let property = get_property_unbound(
								(on, base_structure_arguments),
								(
									Publicity::Public,
									&PropertyKey::Type(under),
									ty_structure_arguments,
								),
								environment,
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
											environment,
											types,
										);
									}
									value => todo!("{:?}", value), // Logical::Or { based_on, left, right } => todo!(),
									                               // Logical::Implies { on, antecedent } => todo!(),
								}
							}
						}
					}
				}

				crate::utilities::notify!("Mismatched property");
				SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
			}
			Constructor::Awaited { .. } => todo!(),
			Constructor::KeyOf(on) => {
				if let Type::Constant(crate::Constant::String(s)) = right_ty {
					let get_property_unbound = &get_property_unbound(
						(*on, base_structure_arguments),
						(
							Publicity::Public,
							&PropertyKey::String(std::borrow::Cow::Borrowed(s)),
							ty_structure_arguments,
						),
						environment,
						types,
					);
					if get_property_unbound.is_ok() {
						SubTypeResult::IsSubType
					} else {
						SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					}
				} else {
					crate::utilities::notify!("TODO");
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			}
		},
		// TODO aliasing might work differently
		Type::AliasTo { to, parameters, name: _ } => {
			if base_type == TypeId::LITERAL_RESTRICTION {
				crate::utilities::notify!("Here");
				return if let Type::Constant(rhs_constant) = right_ty {
					type_is_subtype_with_generics(
						(*to, base_structure_arguments),
						(rhs_constant.get_backing_type_id(), ty_structure_arguments),
						state,
						environment,
						types,
					)
				} else {
					// TODO what about if the rhs == TypeId::CONSTANT_RESTRICTION
					// TODO non-constant error
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				};
			}

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
				environment,
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
					environment.get_chain_of_info().find_map(|info| info.prototypes.get(&ty))
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
				let into = arguments.clone().into();
				let right =
					(*on, GenericChainLink::append(ty, ty_structure_arguments.as_ref(), &into));
				type_is_subtype_with_generics(
					(base_type, base_structure_arguments),
					right,
					state,
					environment,
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
					environment,
					types,
				),
				Type::SpecialObject(SpecialObjects::Function(..)) => {
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
					// 	environment,
					// 	types,
					// );
					// if let SubTypeResult::IsSubType = left {
					// 	type_is_subtype2(
					// 		base_type,
					// 		right,
					// 		ty_arguments,
					// 		state,
					// 		environment,
					// 		types,
					// 	)
					// } else {
					// 	crate::utilities::notify!("Left failed");
					// 	SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
					// }
				}
				Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, arguments }) => {
					let into = arguments.clone().into();
					let append =
						GenericChainLink::append(ty, ty_structure_arguments.as_ref(), &into);
					type_is_subtype_with_generics(
						(base_type, base_structure_arguments),
						(*on, append),
						state,
						environment,
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
						.and_then(|args| args.get_argument(base_type, environment, types));

					if let Some(args) = arg {
						for arg in args {
							let result = type_is_subtype_with_generics(
								(arg, base_structure_arguments),
								(ty, ty_structure_arguments),
								state,
								environment,
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
							environment,
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
	environment: &Environment,
	types: &TypeStore,
) -> SubTypeResult {
	crate::utilities::notify!("Subtyping a function");

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
					environment,
					types,
				);

				if let err @ SubTypeResult::IsNotSubType(_) = result {
					let lhs = print_type(right_param_ty, types, environment, true);
					let rhs = print_type(lhs_param.ty, types, environment, true);
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
			environment,
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
	environment: &Environment,
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
	let reversed_flattened_properties_on_on = environment
		.get_chain_of_info()
		.filter_map(|info| info.current_properties.get(&base_type).map(|v| v.iter().rev()))
		.flatten();

	for (publicity, key, lhs_property) in reversed_flattened_properties_on_on {
		// crate::utilities::notify!(
		// 	"key {:?} with base_type_arguments={:?}",
		// 	key,
		// 	base_type_arguments
		// );

		let key = match key {
			PropertyKey::Type(ty) => {
				if let Some(base_type_arguments) = base_type_arguments {
					PropertyKey::from_type(
						base_type_arguments.get_single_argument(*ty).unwrap_or(*ty),
						types,
					)
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
			environment,
			types,
		);

		if let Err(err) = result {
			property_errors.push((key.into_owned(), err));
		}
	}

	let result = if property_errors.is_empty() {
		// TODO type arguments
		if let Some(ref mut object_constraints) = state.object_constraints {
			let base_type = if let Some(ref arguments) = base_type_arguments {
				if let GenericChainLink::Link { from, parent_link, value: _ } = arguments {
					assert!(parent_link.is_none(), "TODO recursive get_from");
					*from
				} else {
					base_type
				}
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
				environment,
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
	environment: &Environment,
	types: &TypeStore,
) -> Result<(), PropertyError> {
	match lhs_property {
		PropertyValue::Value(lhs_value) => {
			// TODO should all values do this or is it only mapped generic ids
			let root;
			let base_type_arguments = if let Some((id, to)) = key.mapped_generic_id(types) {
				// WIP, need to work for ors etc
				// let to = base_type_arguments.and_then(|args| args.get_single_argument(to)).unwrap_or(to);
				let mut map = crate::Map::default();
				map.insert(id, (to, SpanWithSource::NULL));
				root = GenericArguments::ExplicitRestrictions(map);
				Some(GenericChainLink::append_to_link(id, base_type_arguments.as_ref(), &root))
			} else {
				base_type_arguments
			};

			let res = get_property_unbound(
				(ty, right_type_arguments),
				(publicity, key, base_type_arguments),
				environment,
				types,
			);

			// let key_ty = crate::types::printing::print_property_key(key, types, environment, true);
			// crate::utilities::notify!(
			// 	"looked for {:?} ({:?} with {:?}) found {:?}",
			// 	key,
			// 	key_ty,
			// 	base_type_arguments,
			// 	res
			// );

			// // TODO shouldn't this use substitution
			// let mut v;
			// let base_type_arguments = if let PropertyKey::Type(t) = key {
			// 	crate::utilities::notify!("Setting argument via type. Not sure?");
			// 	// TODO can't create string, don't have mutable types :(
			// 	let from_iter = crate::Map::from_iter(std::iter::once((
			// 		*t,
			// 		(TypeId::HMM_ERROR, <SpanWithSource as source_map::Nullable>::NULL),
			// 	)));
			// 	v = GenericArguments::ExplicitRestrictions(from_iter);
			// 	Some(GenericChainLink::append_to_link(*lhs_value, base_type_arguments.as_ref(), &v))
			// } else {
			// 	base_type_arguments
			// };

			match res {
				Ok(res) => {
					let res = check_logical_property(
						(*lhs_value, base_type_arguments),
						(res, right_type_arguments),
						state,
						environment,
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
		PropertyValue::Getter(getter) => {
			let res = get_property_unbound((ty, None), (publicity, key, None), environment, types);
			crate::utilities::notify!("looking for {:?} found {:?}", key, res);

			match res {
				Ok(res) => {
					let res = check_logical_property(
						(getter.return_type, base_type_arguments),
						(res, right_type_arguments),
						state,
						environment,
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
		PropertyValue::Setter(_) => todo!(),
		PropertyValue::Deleted => {
			// TODO WIP
			let res = get_property_unbound((ty, None), (publicity, key, None), environment, types);
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
					environment,
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
						environment,
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
	environment: &Environment,
	types: &TypeStore,
) -> SubTypeResult {
	match rhs_property {
		Logical::Pure(rhs_property) => {
			let rhs_type = rhs_property.as_set_type();
			// crate::utilities::notify!(
			// 	"Checking {} with {}, against {}, left={:?}",
			// 	print_type(key, types, environment, true),
			// 	print_type(property, types, environment, true),
			// 	print_type(rhs_type, types, environment, true),
			// 	base_type_arguments
			// );

			type_is_subtype_with_generics(
				(base, base_type_arguments),
				(rhs_type, right_type_arguments),
				state,
				environment,
				types,
			)
		}
		Logical::Or { .. } => todo!(),
		Logical::Implies { on, antecedent } => check_logical_property(
			(base, GenericChainLink::append(base, base_type_arguments.as_ref(), &antecedent)),
			(*on, right_type_arguments),
			state,
			environment,
			types,
		),
	}
}

/// TODO integrate `set_restriction`, but it can't create a type ? maybe object restriction should be logically.
/// maybe sub function
pub fn type_is_subtype_of_property(
	(property, property_generics): (&Logical<PropertyValue>, GenericChain),
	ty: TypeId,
	state: &mut State,
	environment: &Environment,
	types: &TypeStore,
) -> SubTypeResult {
	match property {
		Logical::Pure(prop) => type_is_subtype_with_generics(
			(prop.as_set_type(), property_generics),
			(ty, GenericChain::None),
			state,
			environment,
			types,
		),
		Logical::Or { .. } => {
			todo!()
			// let left_result = type_is_subtype_of_property(
			// 	left,
			// 	property_generics,
			// 	ty,
			// 	state,
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
			// 		state,
			// 		environment,
			// 		types,
			// 	)
			// }
		}
		Logical::Implies { on, antecedent } => type_is_subtype_of_property(
			(
				on,
				GenericChainLink::append(TypeId::HMM_ERROR, property_generics.as_ref(), antecedent),
			),
			ty,
			state,
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
