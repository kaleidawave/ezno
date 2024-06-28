use source_map::{Nullable, SpanWithSource};

use crate::{
	context::{
		information::InformationChain, CallCheckingBehavior, MissingOrToCalculate, PossibleLogical,
	},
	events::Event,
	features::{functions::ThisValue, objects::Proxy},
	types::{
		generics::generic_type_arguments::GenericArguments, get_constraint, Constructor,
		GenericChain, GenericChainLink, ObjectNature, PartiallyAppliedGenerics, SpecialObject,
		TypeStore,
	},
	Constant, Environment, LocalInformation, Logical, Map, PropertyValue, Type, TypeId,
};

use super::{PropertyKey, PropertyKind, Publicity};

pub(crate) fn get_property_unbound(
	(on, on_type_arguments): (TypeId, GenericChain),
	(publicity, under, under_type_arguments): (Publicity, &PropertyKey, GenericChain),
	info_chain: &impl InformationChain,
	types: &TypeStore,
) -> PossibleLogical<PropertyValue> {
	/// Has to return `Logical` for mapped types
	///
	/// TODO earlier etc
	fn resolver(
		(on, on_type_arguments): (TypeId, GenericChain),
		(publicity, under, under_type_arguments): (Publicity, &PropertyKey, GenericChain),
		info_chain: &impl InformationChain,
		types: &TypeStore,
	) -> Option<Logical<PropertyValue>> {
		// TODO if on == constant string and property == length. Need to be able to create types here
		info_chain.get_chain_of_info().find_map(|info: &LocalInformation| {
			info.current_properties.get(&on).and_then(|properties_on_on| {
				{
					let (on_properties, on_type_arguments) = (properties_on_on, on_type_arguments);
					let (required_publicity, want_key, want_type_arguments) =
						(publicity, under, under_type_arguments);

					// TODO conditional for conditional?
					// let _acc = ();

					// 'rev' is important
					for (publicity, key, value) in on_properties.iter().rev() {
						if *publicity != required_publicity {
							continue;
						}

						let (key_matches, key_arguments) = super::key_matches(
							(key, on_type_arguments),
							(want_key, want_type_arguments),
							info_chain,
							types,
						);

						if key_matches {
							// TODO if conditional then continue to find then logical or
							let pure = Logical::Pure(value.clone());
							return Some(if let Some(key) = key_arguments {
								crate::utilities::notify!("Here BasedOnKey");
								Logical::BasedOnKey { on: Box::new(pure), key }
							} else {
								pure
							});
						}
					}

					None
				}
			})
		})
	}

	if on == TypeId::ERROR_TYPE {
		return Err(MissingOrToCalculate::Error);
	}
	if on == TypeId::ANY_TYPE {
		// TODO any
		return Err(MissingOrToCalculate::Infer { on });
	}

	match types.get_type_by_id(on) {
		Type::SpecialObject(SpecialObject::Function(function_id, _)) => resolver(
			(on, on_type_arguments),
			(publicity, under, under_type_arguments),
			info_chain,
			types,
		)
		.or_else(|| {
			if let (
				true,
				crate::features::functions::FunctionBehavior::Function { prototype, .. },
			) = (under.is_equal_to("prototype"), &types.get_function_from_id(*function_id).behavior)
			{
				Some(Logical::Pure(PropertyValue::Value(*prototype)))
			} else {
				None
			}
		})
		.or_else(|| {
			resolver(
				(TypeId::FUNCTION_TYPE, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
		})
		.ok_or(MissingOrToCalculate::Missing),
		Type::FunctionReference(_) => resolver(
			(TypeId::FUNCTION_TYPE, on_type_arguments),
			(publicity, under, under_type_arguments),
			info_chain,
			types,
		)
		.ok_or(MissingOrToCalculate::Missing),
		Type::AliasTo { to, .. } => {
			get_property_unbound(
				(*to, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
			// TODO why would an alias have a property
			// let property_on_types = info_chain
			// 	.get_chain_of_info()
			// 	.find_map(|info| resolver(info, types, on, on_type_arguments,))
		}
		Type::And(left, right) => get_property_unbound(
			(*left, on_type_arguments),
			(publicity, under, under_type_arguments),
			info_chain,
			types,
		)
		.or_else(|_| {
			get_property_unbound(
				(*right, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
		}),
		Type::Or(left, right) => {
			let left = get_property_unbound(
				(*left, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			);
			let right = get_property_unbound(
				(*right, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			);

			// TODO throwaway if both Missing::None

			Ok(Logical::Or {
				condition: TypeId::BOOLEAN_TYPE,
				left: Box::new(left),
				right: Box::new(right),
			})
		}
		Type::RootPolyType(_nature) => {
			// Can assign to properties on parameters etc
			let aliases = get_constraint(on, types).expect("poly type with no constraint");

			resolver(
				(on, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
			.ok_or(MissingOrToCalculate::Missing)
			.or_else(|_| {
				get_property_unbound(
					(aliases, on_type_arguments),
					(publicity, under, under_type_arguments),
					info_chain,
					types,
				)
			})
		}
		Type::PartiallyAppliedGenerics(crate::types::PartiallyAppliedGenerics {
			on: base,
			arguments,
		}) => {
			let on_sg_type = if let GenericArguments::Closure(_) = arguments {
				resolver(
					(on, on_type_arguments),
					(publicity, under, under_type_arguments),
					info_chain,
					types,
				)
				.ok_or(MissingOrToCalculate::Missing)
			} else {
				Err(MissingOrToCalculate::Missing)
			};

			on_sg_type.or_else(|_| {
				let on_type_arguments = crate::types::GenericChainLink::append(
					on,
					on_type_arguments.as_ref(),
					arguments,
				);

				// crate::utilities::notify!("{:?}", on_type_arguments);

				let get_property_unbound = get_property_unbound(
					(*base, on_type_arguments),
					(publicity, under, under_type_arguments),
					info_chain,
					types,
				);

				get_property_unbound.map(|fact| Logical::Implies {
					on: Box::new(fact),
					antecedent: arguments.clone(),
				})
			})
		}
		Type::Constructor(crate::types::Constructor::ConditionalResult {
			condition,
			truthy_result,
			otherwise_result,
			result_union: _,
		}) => {
			let left = get_property_unbound(
				(*truthy_result, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			);
			let right = get_property_unbound(
				(*otherwise_result, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			);

			// TODO throwaway if both Missing::None

			Ok(Logical::Or { condition: *condition, left: Box::new(left), right: Box::new(right) })
		}
		Type::Constructor(_constructor) => {
			let on_constructor_type = resolver(
				(on, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
			.ok_or(MissingOrToCalculate::Missing);

			let aliases = get_constraint(on, types).expect("no constraint for constructor");

			on_constructor_type.or_else(|_| {
				get_property_unbound(
					(aliases, on_type_arguments),
					(publicity, under, under_type_arguments),
					info_chain,
					types,
				)
			})
		}
		Type::Object(..) => {
			let object_constraint_structure_generics =
				crate::types::get_structure_arguments_based_on_object_constraint(
					on, info_chain, types,
				);

			let prototype =
				info_chain.get_chain_of_info().find_map(|facts| facts.prototypes.get(&on)).copied();

			let generics = if let Some(generics) = object_constraint_structure_generics {
				// TODO clone
				Some(generics.clone())
			} else if prototype
				.is_some_and(|prototype| types.lookup_generic_map.contains_key(&prototype))
			{
				Some(GenericArguments::LookUp { on })
			} else {
				None
			};

			let on_self = resolver(
				(on, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			);

			let result = if let (Some(prototype), None) = (prototype, &on_self) {
				resolver(
					(prototype, on_type_arguments),
					(publicity, under, under_type_arguments),
					info_chain,
					types,
				)
			} else {
				on_self
			};

			result
				.map(|result| {
					if let Some(ref generics) = generics {
						// TODO clone
						Logical::Implies { on: Box::new(result), antecedent: generics.clone() }
					} else {
						result
					}
				})
				.ok_or(MissingOrToCalculate::Missing)
		}
		Type::Interface { .. } => resolver(
			(on, on_type_arguments),
			(publicity, under, under_type_arguments),
			info_chain,
			types,
		)
		.ok_or(MissingOrToCalculate::Missing)
		.or_else(|_| {
			// TODO class and class constructor extends etc
			if let Some(extends) = types.interface_extends.get(&on) {
				get_property_unbound(
					(*extends, on_type_arguments),
					(publicity, under, under_type_arguments),
					info_chain,
					types,
				)
			} else {
				Err(MissingOrToCalculate::Missing)
			}
		}),
		Type::SpecialObject(SpecialObject::ClassConstructor { .. }) | Type::Class { .. } => {
			resolver(
				(on, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
			.ok_or(MissingOrToCalculate::Missing)
			.or_else(|_| {
				if let Some(prototype) =
					info_chain.get_chain_of_info().find_map(|info| info.prototypes.get(&on))
				{
					get_property_unbound(
						(*prototype, on_type_arguments),
						(publicity, under, under_type_arguments),
						info_chain,
						types,
					)
				} else {
					Err(MissingOrToCalculate::Missing)
				}
			})
		}
		Type::Constant(cst) => resolver(
			(on, on_type_arguments),
			(publicity, under, under_type_arguments),
			info_chain,
			types,
		)
		.ok_or(MissingOrToCalculate::Missing)
		.or_else(|_| {
			let backing_type = cst.get_backing_type_id();
			get_property_unbound(
				(backing_type, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
		}),
		Type::SpecialObject(SpecialObject::Promise { .. }) => {
			todo!()
		}
		Type::SpecialObject(SpecialObject::Import(..)) => {
			todo!()
		}
		Type::SpecialObject(SpecialObject::Proxy(proxy)) => {
			Err(MissingOrToCalculate::Proxy(*proxy))
		}
		Type::SpecialObject(SpecialObject::Generator { .. }) => {
			todo!()
		}
		Type::SpecialObject(SpecialObject::RegularExpression(..)) => {
			todo!()
		}
	}
}

pub type Properties = Vec<(Publicity, PropertyKey<'static>, PropertyValue)>;

/// Get properties on a type (for printing and other non-one property uses)
///
/// - TODO prototypes
/// - TODO could this be an iterator
/// - TODO return whether it is fixed
/// - TODO doesn't evaluate properties
pub fn get_properties_on_single_type(
	base: TypeId,
	types: &TypeStore,
	info: &impl InformationChain,
) -> Properties {
	match types.get_type_by_id(base) {
		Type::Interface { .. } | Type::Class { .. } | Type::Object(_) => {
			let reversed_flattened_properties = info
				.get_chain_of_info()
				.filter_map(|info| info.current_properties.get(&base).map(|v| v.iter().rev()))
				.flatten();

			let mut deleted_or_existing_properties =
				std::collections::HashSet::<PropertyKey>::new();

			// This retains ordering here

			let mut properties = Vec::new();
			for (publicity, key, prop) in reversed_flattened_properties {
				if let PropertyValue::Deleted = prop {
					// TODO doesn't cover constants :(
					deleted_or_existing_properties.insert(key.clone());
				} else if deleted_or_existing_properties.insert(key.clone()) {
					properties.push((*publicity, key.to_owned(), prop.clone()));
				}
			}

			properties.reverse();
			properties
		}
		t @ (Type::SpecialObject(_)
		| Type::Constructor(_)
		| Type::RootPolyType(_)
		| Type::Or(..)
		| Type::PartiallyAppliedGenerics(_)
		| Type::Constant(_)
		| Type::AliasTo { .. }
		| Type::FunctionReference(_)
		| Type::And(_, _)) => panic!("Cannot get all properties on {t:?}"),
	}
}

#[derive(Debug, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum AccessMode {
	Regular,
	Optional,
	/// For destructuring
	DoNotBindThis,
}

/// Also evaluates getter and binds `this`
///
/// *be aware this creates a new type every time, bc of this binding. could cache this bound
/// types at some point*
/// TODO: `optional_chain`
#[allow(clippy::too_many_arguments)]
pub(crate) fn get_property<E: CallCheckingBehavior>(
	on: TypeId,
	publicity: Publicity,
	under: &PropertyKey,
	with: Option<TypeId>,
	top_environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
	position: SpanWithSource,
	mode: AccessMode,
) -> Option<(PropertyKind, TypeId)> {
	if on == TypeId::ERROR_TYPE
		|| matches!(under, PropertyKey::Type(under) if *under == TypeId::ERROR_TYPE)
	{
		return Some((PropertyKind::Direct, TypeId::ERROR_TYPE));
	}

	if get_constraint(on, types).is_some() {
		// // TODO temp fix for assigning to a poly type. What about unions etc
		// if let Some(value) = top_environment.info_chain().find_map(|f| {
		// 	f.current_properties.get(&on).and_then(|props| {
		// 		props.iter().find_map(|(_publicity, key, value)| (key == &under).then_some(value))
		// 	})
		// }) {
		// 	return Some((PropertyKind::Direct, value.as_get_type()));
		// }

		evaluate_get_on_poly(
			// constraint,
			on,
			publicity,
			under.clone(),
			with,
			top_environment,
			behavior,
			types,
			position,
			mode,
		)
	} else if let Some(constraint) = top_environment.possibly_mutated_objects.get(&on) {
		evaluate_get_on_poly(
			// TODO ... should be constraint ...
			*constraint,
			publicity,
			under.clone(),
			with,
			top_environment,
			behavior,
			types,
			position,
			mode,
		)
	} else {
		// if environment.get_poly_base(under, types).is_some() {
		// 	todo!()
		// }
		// TODO
		get_from_an_object(on, publicity, under, top_environment, behavior, types, mode)
	}
}

fn get_from_an_object<E: CallCheckingBehavior>(
	on: TypeId,
	publicity: Publicity,
	under: &PropertyKey,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
	mode: AccessMode,
) -> Option<(PropertyKind, TypeId)> {
	/// Generates closure arguments, values of this and more. Runs getters
	fn resolve_property_on_logical<E: CallCheckingBehavior>(
		logical: Logical<PropertyValue>,
		on: TypeId,
		generics: GenericChain,
		environment: &mut Environment,
		types: &mut TypeStore,
		behavior: &mut E,
		mode: AccessMode,
	) -> Option<(PropertyKind, TypeId)> {
		match logical {
			Logical::Pure(property) => {
				match property {
					PropertyValue::Value(value) => {
						let ty = types.get_type_by_id(value);
						match ty {
							Type::SpecialObject(SpecialObject::Function(func, _state)) => {
								let this_value = if let AccessMode::DoNotBindThis = mode {
									// Not `ThisValue::Passed`, but not sure about this
									ThisValue::UseParent
								} else {
									ThisValue::Passed(on)
								};
								let func = types.register_type(Type::SpecialObject(
									SpecialObject::Function(*func, this_value),
								));

								Some((PropertyKind::Direct, func))
							}
							Type::FunctionReference(_) => {
								let ty = if let Some(arguments) =
									generics.and_then(GenericChainLink::get_value).cloned()
								{
									// assert!(chain.parent.is_none());
									types.register_type(Type::PartiallyAppliedGenerics(
										PartiallyAppliedGenerics { on: value, arguments },
									))
								} else {
									value
								};

								Some((PropertyKind::Direct, ty))
							}
							// TODO if uses generics
							Type::SpecialObject(..)
							| Type::Object(..)
							| Type::RootPolyType { .. }
							| Type::Constant(..) => Some((PropertyKind::Direct, value)),
							Type::Class { .. }
							| Type::Interface { .. }
							| Type::And(_, _)
							| Type::Or(_, _) => {
								crate::utilities::notify!(
								    "property was {:?} {:?}, which should be NOT be able to be returned from a function",
								    property, ty
							    );

								let value = types.register_type(Type::RootPolyType(
									crate::types::PolyNature::Open(value),
								));
								Some((PropertyKind::Direct, value))
							}
							// For closures
							Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
								on: sg_on,
								arguments,
							}) => {
								// TODO not great... need less overhead
								if let Type::SpecialObject(SpecialObject::Function(f, _p)) =
									types.get_type_by_id(*sg_on)
								{
									let arguments = arguments.clone();
									let f = types.register_type(Type::SpecialObject(
										SpecialObject::Function(*f, ThisValue::Passed(on)),
									));
									let ty = types.register_type(Type::PartiallyAppliedGenerics(
										PartiallyAppliedGenerics { on: f, arguments },
									));
									Some((PropertyKind::Direct, ty))
								} else {
									Some((PropertyKind::Direct, value))
								}
							}
							Type::Constructor(constructor) => {
								unreachable!("Interesting property on {:?}", constructor);
							}
							Type::AliasTo { to: _, name: _, parameters: _ } => {
								todo!()
							}
						}
					}
					PropertyValue::Getter(getter) => {
						use crate::types::calling::{
							application_result_to_return_type, CalledWithNew, CallingInput,
						};

						let input = CallingInput {
							called_with_new: CalledWithNew::None,
							// TODO
							call_site: source_map::Nullable::NULL,
							// TODO
							max_inline: 0,
						};
						let call = getter.call(
							(
								ThisValue::Passed(on),
								&[],
								None,
								// TODO structure generics
								None,
							),
							input,
							environment,
							behavior,
							types,
						);
						match call {
							Ok(res) => {
								let application_result = application_result_to_return_type(
									res.result,
									environment,
									types,
								);
								Some((PropertyKind::Getter, application_result))
							}
							Err(_) => {
								todo!()
							}
						}
					}
					PropertyValue::Setter(_) => todo!(),
					PropertyValue::Deleted => None,
					PropertyValue::ConditionallyExists { on, truthy } => {
						let (kind, value) = resolve_property_on_logical(
							Logical::Pure(*truthy),
							on,
							generics,
							environment,
							types,
							behavior,
							mode,
						)?;
						Some((kind, types.new_conditional_type(on, value, TypeId::UNDEFINED_TYPE)))
					}
				}
			}
			Logical::Or { left, right, condition } => {
				if let (Ok(lhs), Ok(rhs)) = (*left, *right) {
					let (_, lhs) = resolve_property_on_logical(
						lhs,
						on,
						generics,
						environment,
						types,
						behavior,
						mode,
					)?;
					let (_, rhs) = resolve_property_on_logical(
						rhs,
						on,
						generics,
						environment,
						types,
						behavior,
						mode,
					)?;
					Some((PropertyKind::Direct, types.new_conditional_type(condition, lhs, rhs)))
				} else {
					crate::utilities::notify!("TODO emit some diagnostic about missing");
					None
				}
			}
			Logical::Implies { on: log_on, antecedent } => {
				crate::utilities::notify!("TypeId::UNIMPLEMENTED_ERROR_TYPE here");
				let generics = GenericChainLink::append(
					// TODO
					TypeId::UNIMPLEMENTED_ERROR_TYPE,
					generics.as_ref(),
					&antecedent,
				);
				resolve_property_on_logical(
					*log_on,
					on,
					generics,
					environment,
					types,
					behavior,
					mode,
				)
			}
			Logical::BasedOnKey { on: log_on, key } => {
				let value = key.1 .0.into_type(types);
				let explicit_restrictions = GenericArguments::ExplicitRestrictions(Map::from_iter(
					[(key.0, (value, SpanWithSource::NULL))],
				));

				let generics = GenericChainLink::append(
					TypeId::UNIMPLEMENTED_ERROR_TYPE,
					generics.as_ref(),
					&explicit_restrictions,
				);
				resolve_property_on_logical(
					*log_on,
					on,
					generics,
					environment,
					types,
					behavior,
					mode,
				)
			}
		}
	}

	// TODO explain what happens around non constant strings
	// TODO this breaks for conditionals maybe #120 fixes it?
	if let Type::Constant(Constant::String(s)) = types.get_type_by_id(on) {
		if let Some(n) = under.as_number(types) {
			return s.chars().nth(n).map(|s| {
				(PropertyKind::Direct, types.new_constant_type(Constant::String(s.to_string())))
			});
		}
	}

	let result = get_property_unbound((on, None), (publicity, under, None), environment, types);

	match result {
		Ok(logical) => {
			resolve_property_on_logical(logical, on, None, environment, types, behavior, mode)
		}
		Err(err) => match err {
			MissingOrToCalculate::Missing => None,
			MissingOrToCalculate::Error => {
				// Don't return none because that will raise error!
				Some((PropertyKind::Direct, TypeId::ERROR_TYPE))
			}
			// Can get through set prototype..?
			MissingOrToCalculate::Infer { .. } => {
				crate::utilities::notify!("TODO set infer");
				Some((PropertyKind::Direct, TypeId::ERROR_TYPE))
			}
			MissingOrToCalculate::Proxy(Proxy { .. }) => {
				todo!(); // #33
				 // TODO pass down
			}
		},
	}
}

#[allow(clippy::too_many_arguments)]
#[allow(clippy::needless_pass_by_value)]
fn evaluate_get_on_poly<E: CallCheckingBehavior>(
	on: TypeId,
	publicity: Publicity,
	under: PropertyKey,
	_with: Option<TypeId>,
	top_environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
	position: SpanWithSource,
	mode: AccessMode,
) -> Option<(PropertyKind, TypeId)> {
	/// Also returns the [`Type::Constructor`] -> [`Constructor::Property`]
	fn resolve_logical_with_poly(
		fact: Logical<PropertyValue>,
		on: TypeId,
		under: PropertyKey,
		generics: GenericChain,
		environment: &mut Environment,
		types: &mut TypeStore,
		mode: AccessMode,
	) -> Option<TypeId> {
		match fact {
			Logical::Pure(og) => {
				Some(match og {
					PropertyValue::Value(value) => match types.get_type_by_id(value) {
						t @ (Type::And(_, _)
						| Type::Or(_, _)
						| Type::RootPolyType(_)
						| Type::PartiallyAppliedGenerics(_)
						| Type::Constructor(_)) => {
							let result = if let Some(GenericChainLink::Link {
								value: arguments,
								parent_link,
								..
							}) = generics
							{
								let mut arguments = arguments.into_substitutable();
								if let Some(parent_link) = parent_link {
									parent_link.extend_arguments(&mut arguments);
								}
								crate::types::substitute(value, &arguments, environment, types)
							} else {
								crate::utilities::notify!("Here, getting property on {:?}", t);
								value
							};

							crate::utilities::notify!("If result == constant here");

							types.register_type(Type::Constructor(Constructor::Property {
								on,
								under: under.into_owned(),
								result,
								mode,
							}))
						}
						// Don't need to set this here. It is picked up from `on` during lookup
						Type::SpecialObject(SpecialObject::Function(..))
						| Type::FunctionReference(..)
						| Type::AliasTo { .. }
						| Type::Object(ObjectNature::AnonymousTypeAnnotation)
						| Type::Interface { .. }
						| Type::Class { .. } => types.register_type(Type::Constructor(Constructor::Property {
							on,
							under: under.into_owned(),
							result: value,
							mode,
						})),
						Type::Constant(_)
						| Type::Object(ObjectNature::RealDeal)
						| Type::SpecialObject(..) => value,
					},
					PropertyValue::Getter(getter) => {
						// if is_open_poly {
						// 	crate::utilities::notify!("TODO evaluate getter...");
						// } else {
						// 	crate::utilities::notify!("TODO don't evaluate getter");
						// }
						// TODO : getter.return_type
						types.register_type(Type::Constructor(Constructor::Property {
							on,
							under: under.into_owned(),
							result: getter.return_type,
							mode,
						}))
					}
					PropertyValue::Setter(_) => todo!(),
					// Very important
					PropertyValue::Deleted => return None,
					PropertyValue::ConditionallyExists { on, truthy } => {
						let value = resolve_logical_with_poly(
							Logical::Pure(*truthy),
							on,
							under.clone(),
							generics,
							environment,
							types,
							mode,
						)?;
						types.new_conditional_type(on, value, TypeId::UNDEFINED_TYPE)
					}
				})
			}
			Logical::Or { condition, left, right } => {
				// crate::utilities::notify!("lr = {:?}", (left, right));

				// TODO lots of information (and inference) lost here
				if let (Ok(lhs), Ok(rhs)) = (*left, *right) {
					let lhs = resolve_logical_with_poly(
						lhs,
						on,
						under.clone(),
						generics,
						environment,
						types,
						mode,
					)?;
					let rhs = resolve_logical_with_poly(
						rhs,
						on,
						under,
						generics,
						environment,
						types,
						mode,
					)?;
					Some(types.new_conditional_type(condition, lhs, rhs))
				} else {
					crate::utilities::notify!("TODO emit some diagnostic about missing");
					None
				}
			}
			Logical::Implies { on: log_on, antecedent } => {
				crate::utilities::notify!("TypeId::UNIMPLEMENTED_ERROR_TYPE here");
				let generics = GenericChainLink::append(
					// TODO
					TypeId::UNIMPLEMENTED_ERROR_TYPE,
					generics.as_ref(),
					&antecedent,
				);

				resolve_logical_with_poly(
					*log_on,
					on,
					under.clone(),
					generics,
					environment,
					types,
					mode,
				)
			}
			Logical::BasedOnKey { on: log_on, key } => {
				let value = key.1 .0.into_type(types);
				let explicit_restrictions = GenericArguments::ExplicitRestrictions(Map::from_iter(
					[(key.0, (value, SpanWithSource::NULL))],
				));

				let generics = GenericChainLink::append(
					TypeId::UNIMPLEMENTED_ERROR_TYPE,
					generics.as_ref(),
					&explicit_restrictions,
				);

				resolve_logical_with_poly(
					*log_on,
					on,
					under.clone(),
					generics,
					environment,
					types,
					mode,
				)
			}
		}
	}

	let fact =
		get_property_unbound((on, None), (publicity, &under, None), top_environment, types).ok()?;

	crate::utilities::notify!("unbound is is {:?}", fact);

	let value =
		resolve_logical_with_poly(fact, on, under.clone(), None, top_environment, types, mode)?;

	behavior.get_latest_info(top_environment).events.push(Event::Getter {
		on,
		under: under.into_owned(),
		reflects_dependency: Some(value),
		publicity,
		position,
		mode,
	});

	Some((PropertyKind::Direct, value))
}
