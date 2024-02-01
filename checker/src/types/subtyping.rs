//! Type subtyping / order / subtype checking.

use source_map::SpanWithSource;

use crate::{
	context::{
		information::{get_properties_on_type, get_property_unbound},
		Environment, GeneralContext, Logical,
	},
	types::{
		poly_types::generic_type_arguments::TypeArgumentStore, printing::print_type, TypeStore,
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

/// TODO better place
/// `staging_*` is to get around the fact that intersections cannot be during subtyping (as types
/// is immutable at this stage, rather than mutable)
///
/// `covariant :> contravariant`!!
pub(crate) struct Contributions<'a> {
	pub(crate) existing_covariant: &'a mut map_vec::Map<TypeId, TypeId>,
	/// Only for explicit generic parameters
	pub(crate) staging_covariant: map_vec::Map<TypeId, Vec<(TypeId, u8)>>,
	/// From either explicit generic arguments, or existing parameter checks
	pub(crate) existing_contravariant: &'a map_vec::Map<TypeId, (TypeId, SpanWithSource)>,
	pub(crate) staging_contravariant: map_vec::Map<TypeId, Vec<(TypeId, SpanWithSource)>>,
}

impl<'a> Contributions<'a> {
	pub fn try_set_covariant(
		&mut self,
		on: TypeId,
		argument: TypeId,
		depth: u8,
		environment: &Environment,
		types: &TypeStore,
	) -> SubTypeResult {
		// TODO only need to do this for explicit generics and some types
		// TODO chain
		let current_contravariant = self.existing_contravariant.get(&on).cloned().into_iter();

		// Check that the new argument, satisfies the restriction
		for (restriction, _pos) in current_contravariant {
			if let e @ SubTypeResult::IsNotSubType(..) = type_is_subtype2(
				restriction,
				argument,
				GenericChain::None,
				// TODO
				GenericChain::None,
				self,
				environment,
				types,
				Default::default(),
			) {
				return e;
			}
		}

		self.staging_covariant.entry(on).or_default().push((argument, depth));

		SubTypeResult::IsSubType
	}

	pub fn try_set_contravariant(
		&mut self,
		on: TypeId,
		restriction: TypeId,
		position: SpanWithSource,
		environment: &Environment,
		types: &TypeStore,
	) -> SubTypeResult {
		// TODO chain
		let current_covariant = self.existing_covariant.get(&on).cloned().into_iter();

		// Check that the new argument, satisfies the restriction
		for argument in current_covariant {
			if let e @ SubTypeResult::IsNotSubType(..) = type_is_subtype2(
				restriction,
				argument,
				GenericChain::None,
				GenericChain::None,
				self,
				environment,
				types,
				Default::default(),
			) {
				return e;
			}
		}

		self.staging_contravariant.entry(on).or_default().push((restriction, position));

		SubTypeResult::IsSubType
	}
}

impl<'a> SubTypeBehavior for Contributions<'a> {
	const INFER_GENERICS: bool = true;

	fn add_object_mutation_constraint(&mut self, _on: TypeId, _constraint: TypeId) {
		crate::utils::notify!("TODO");
	}

	fn add_function_restriction(
		&mut self,
		_environment: &mut Environment,
		_function_id: crate::FunctionId,
		_function_type: super::FunctionType,
	) {
		todo!()
	}

	fn set_type_argument(
		&mut self,
		on: TypeId,
		argument: TypeId,
		depth: u8,
		environment: &Environment,
		types: &TypeStore,
	) -> SubTypeResult {
		self.try_set_covariant(on, argument, depth, environment, types)
	}

	fn try_set_contravariant(
		&mut self,
		on: TypeId,
		restriction: TypeId,
		position: SpanWithSource,
		environment: &Environment,
		types: &TypeStore,
	) -> SubTypeResult {
		self.try_set_contravariant(on, restriction, position, environment, types)
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
	type_is_subtype2(
		base_type,
		ty,
		Default::default(),
		Default::default(),
		behavior,
		environment,
		types,
		Default::default(),
	)
}

/// TODO integrate `set_restriction`, but it can't create a type ? maybe object restriction should be logically.
/// maybe sub function
pub fn type_is_subtype_of_property<T: SubTypeBehavior>(
	property: &Logical<PropertyValue>,
	property_generics: GenericChain,
	ty: TypeId,
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
) -> SubTypeResult {
	match property {
		Logical::Pure(prop) => type_is_subtype2(
			prop.as_set_type(),
			ty,
			property_generics,
			Default::default(),
			behavior,
			environment,
			types,
			Default::default(),
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
			property_generics.append(&antecedent.type_arguments),
			ty,
			behavior,
			environment,
			types,
		),
	}
}

#[allow(clippy::too_many_arguments)]
fn type_is_subtype2<T: SubTypeBehavior>(
	base_type: TypeId,
	ty: TypeId,
	base_type_arguments: GenericChain,
	right_type_arguments: GenericChain,
	behavior: &mut T,
	environment: &Environment,
	types: &TypeStore,
	mode: SubTypingMode,
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
			mode,
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
				mode,
			)
		} else {
			left_result
		};
	}

	match left_ty {
		Type::FunctionReference(left_func) | Type::Function(left_func, _) => {
			// TODO this is a mess

			let right_func = if let Type::FunctionReference(right_func)
			| Type::Function(right_func, _) = right_ty
			{
				right_func
			} else if let Some(constraint) = get_constraint(ty, types) {
				// TODO explain why get_constraint early breaks a bunch of tests
				let right_ty = types.get_type_by_id(constraint);
				if let Type::FunctionReference(right_func) | Type::Function(right_func, _) =
					right_ty
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
						let result = type_is_subtype2(
							right_param_ty,
							lhs_param.ty,
							right_type_arguments,
							base_type_arguments,
							behavior,
							environment,
							types,
							// !!!
							SubTypingMode::Covariant { position },
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
						if !lhs_param.optional {
							return SubTypeResult::IsNotSubType(
								NonEqualityReason::MissingParameter,
							);
						}
					}
				}
			}
			// TODO optional and rest parameters

			// `void` return type means anything goes here
			if TypeId::VOID_TYPE == left_func.return_type {
				SubTypeResult::IsSubType
			} else {
				type_is_subtype2(
					left_func.return_type,
					right_func.return_type,
					base_type_arguments,
					right_type_arguments,
					behavior,
					environment,
					types,
					mode,
				)
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
				mode,
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
			let left_result = type_is_subtype2(
				*left,
				ty,
				base_type_arguments,
				right_type_arguments,
				behavior,
				environment,
				types,
				mode,
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
					mode,
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
				mode,
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
					mode,
				)
			}
		}
		Type::RootPolyType(nature) => {
			// TODO little weird, handing two very different cases beside each other. Might introduce bugs.. :(
			if let Some(value) = base_type_arguments.get_arg(base_type) {
				type_is_subtype2(
					value,
					ty,
					base_type_arguments,
					right_type_arguments,
					behavior,
					environment,
					types,
					mode,
				)
			} else {
				if !T::INFER_GENERICS && base_type_arguments.is_empty() {
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

				let constraint = get_constraint(base_type, types).unwrap();
				let type_is_subtype2 = type_is_subtype2(
					constraint,
					ty,
					base_type_arguments,
					right_type_arguments,
					behavior,
					environment,
					types,
					mode,
				);
				if let r @ SubTypeResult::IsNotSubType(..) = type_is_subtype2 {
					crate::utils::notify!("RPT not subtype");
					return r;
				}

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
			// Overwrites for special types with proofs
			if let TypeId::ARRAY_TYPE = *on {
				let backing_type =
					arguments.get_argument(TypeId::T_TYPE).expect("array T argument not set ?");

				// TODO temp fix for general parameters
				if let Type::Object(_) = right_ty {
					for (_publicity, property, value) in get_properties_on_type(ty, environment) {
						// Assume every property on itself is either number or 'length'
						match property {
							PropertyKey::String(a) if a == "length" => {
								continue;
							}
							PropertyKey::String(a) => {
								crate::utils::notify!("looking at property {}", a);
							}
							PropertyKey::Type(_) => (),
						}
						let result = type_is_subtype2(
							backing_type,
							value,
							base_type_arguments.append(&arguments.type_arguments),
							right_type_arguments,
							behavior,
							environment,
							types,
							mode,
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
				} else if let Type::Constructor(Constructor::StructureGenerics(
					StructureGenerics { on: TypeId::ARRAY_TYPE, arguments: right_arguments },
				)) = right_ty
				{
					let left_arg = arguments.get_local_argument(TypeId::T_TYPE).unwrap();
					let right_arg = right_arguments.get_local_argument(TypeId::T_TYPE).unwrap();
					// TODO unsure about arguments here
					type_is_subtype2(
						left_arg,
						right_arg,
						base_type_arguments,
						right_type_arguments,
						behavior,
						environment,
						types,
						mode,
					)
				} else {
					crate::utils::notify!("Not array-ish");
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
			} else {
				type_is_subtype2(
					*on,
					ty,
					base_type_arguments.append(&arguments.type_arguments),
					right_type_arguments,
					behavior,
					environment,
					types,
					mode,
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
			type_is_subtype2(
				*to,
				ty,
				base_type_arguments,
				right_type_arguments,
				behavior,
				environment,
				types,
				mode,
			)
		}
		Type::Interface { nominal: base_type_nominal, .. } => {
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
					mode,
				),
				Type::Function(..) => {
					crate::utils::notify!("TODO implement function checking");
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
				Type::And(_, _) => todo!(),
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
				})) => type_is_subtype2(
					base_type,
					*on,
					base_type_arguments,
					right_type_arguments.append(&arguments.type_arguments),
					behavior,
					environment,
					types,
					mode,
				),
				Type::AliasTo { .. } | Type::Interface { .. } => {
					crate::utils::notify!("lhs={:?} rhs={:?}", left_ty, right_ty);
					// TODO
					SubTypeResult::IsNotSubType(NonEqualityReason::Mismatch)
				}
				Type::Constructor(..) | Type::RootPolyType(..) => {
					if let Some(argument) = base_type_arguments.get_arg(base_type) {
						type_is_subtype2(
							base_type,
							argument,
							base_type_arguments,
							right_type_arguments,
							behavior,
							environment,
							types,
							mode,
						)
					} else {
						let to = get_constraint(ty, types).unwrap();

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
							mode,
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

/// TODO temp

#[allow(clippy::too_many_arguments)]
fn check_properties<T: SubTypeBehavior>(
	base_type: TypeId,
	ty: TypeId,
	types: &TypeStore,
	base_type_arguments: GenericChain,
	right_type_arguments: GenericChain,
	behavior: &mut T,
	environment: &Environment,
	mode: SubTypingMode,
) -> SubTypeResult {
	let mode = mode.one_deeper();

	let mut property_errors = Vec::new();
	for (publicity, key, property) in get_properties_on_type(base_type, environment) {
		let rhs_property = get_property_unbound(ty, publicity, key.clone(), types, environment);

		match rhs_property {
			Some(rhs_property) => {
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

						let result = type_is_subtype2(
							property,
							rhs_type,
							base_type_arguments,
							right_type_arguments,
							behavior,
							environment,
							types,
							mode,
						);

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
		// TODO type arguments
		behavior.add_object_mutation_constraint(ty, base_type);

		SubTypeResult::IsSubType
	} else {
		SubTypeResult::IsNotSubType(NonEqualityReason::PropertiesInvalid {
			errors: property_errors,
		})
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
