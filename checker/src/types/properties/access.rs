use source_map::{Nullable, SpanWithSource};

use crate::{
	context::{information::InformationChain, CallCheckingBehavior},
	events::Event,
	features::{functions::ThisValue, objects::Proxy},
	types::{
		generics::{
			contributions::CovariantContribution, generic_type_arguments::GenericArguments,
		},
		get_conditional, get_constraint,
		logical::*,
		Constructor, GenericChain, GenericChainLink, ObjectNature, PartiallyAppliedGenerics,
		SliceArguments, SpecialObject, TypeRestrictions, TypeStore,
	},
	Constant, Environment, LocalInformation, PropertyValue, Type, TypeId,
};

use super::{PropertyKey, PropertyKind, Publicity};

pub(crate) fn get_property_unbound(
	(on, on_type_arguments): (TypeId, GenericChain),
	(publicity, under, under_type_arguments): (Publicity, &PropertyKey, GenericChain),
	no_setters: bool,
	info_chain: &impl InformationChain,
	types: &TypeStore,
) -> PossibleLogical<PropertyValue> {
	/// Has to return `Logical` for mapped types
	///
	/// TODO earlier etc
	///
	/// Resolver runs information lookup on **ONE** TypeId
	fn resolver(
		(on, on_type_arguments): (TypeId, GenericChain),
		(publicity, under, under_type_arguments): (Publicity, &PropertyKey, GenericChain),
		no_setters: bool,
		info_chain: &impl InformationChain,
		types: &TypeStore,
	) -> Option<(PropertyValue, Option<SliceArguments>)> {
		// TODO if on == constant string and property == length. Need to be able to create types here
		info_chain.get_chain_of_info().find_map(|info: &LocalInformation| {
			info.current_properties.get(&on).and_then(|properties_on_on| {
				{
					let (on_properties, on_type_arguments) = (properties_on_on, on_type_arguments);
					let (required_publicity, want_key, want_type_arguments) =
						(publicity, under, under_type_arguments);

					// TODO trailing for conditional?
					// let _acc = ();

					// 'rev' is important
					for (publicity, key, value) in on_properties.iter().rev() {
						if *publicity != required_publicity {
							continue;
						}
						if let (true, PropertyValue::Setter(_)) = (no_setters, value.inner_simple())
						{
							continue;
						}

						if let PropertyValue::ConditionallyExists { .. } = value {
							crate::utilities::notify!("TODO trailing");
							// continue;
						}

						let (key_matches, key_arguments) = super::key_matches(
							(key, on_type_arguments),
							(want_key, want_type_arguments),
							info_chain,
							types,
						);

						if key_matches {
							// crate::utilities::notify!("{:?} {:?}", (key, want_key), value);
							// TODO if conditional then continue to find then logical or

							// TODO if key == string | number (through arguments and keyof etc) then make the result conditionally defined

							return Some((value.clone(), key_arguments.into_some()));
						}
					}

					None
				}
			})
		})
	}

	fn wrap(
		(value, slice_arguments): (PropertyValue, Option<SliceArguments>),
	) -> Logical<PropertyValue> {
		let value = Logical::Pure(value);
		if let Some(slice_arguments) = slice_arguments {
			Logical::BasedOnKey(LeftRight::Left {
				value: Box::new(value),
				key_arguments: slice_arguments,
			})
		} else {
			value
		}
	}

	// Intermediate function to avoid cyclic recursion
	// TODO this should return whether the result is on a exclusive type
	fn get_property_on_type_unbound(
		(on, on_type_arguments): (TypeId, GenericChain),
		(publicity, under, under_type_arguments): (Publicity, &PropertyKey, GenericChain),
		no_setters: bool,
		info_chain: &impl InformationChain,
		types: &TypeStore,
	) -> PossibleLogical<PropertyValue> {
		match types.get_type_by_id(on) {
			Type::SpecialObject(SpecialObject::Function(function_id, _)) => resolver(
				(on, on_type_arguments),
				(publicity, under, under_type_arguments),
				no_setters,
				info_chain,
				types,
			)
			.map(wrap)
			.or_else(|| {
				let get_function_from_id = types.get_function_from_id(*function_id);
				let ty = if let (
					true,
					crate::features::functions::FunctionBehavior::Function { prototype, .. },
				) = (under.is_equal_to("prototype"), &get_function_from_id.behavior)
				{
					Some(*prototype)
				} else if under.is_equal_to("name") {
					Some(get_function_from_id.behavior.get_name())
				} else {
					None
				};
				ty.map(PropertyValue::Value).map(Logical::Pure)
			})
			.or_else(|| {
				resolver(
					(TypeId::FUNCTION_TYPE, on_type_arguments),
					(publicity, under, under_type_arguments),
					no_setters,
					info_chain,
					types,
				)
				.map(wrap)
			})
			.map(LogicalOrValid::Logical)
			.ok_or(Invalid(on)),
			Type::FunctionReference(_) => resolver(
				(TypeId::FUNCTION_TYPE, on_type_arguments),
				(publicity, under, under_type_arguments),
				no_setters,
				info_chain,
				types,
			)
			.map(wrap)
			.map(LogicalOrValid::Logical)
			.ok_or(Invalid(on)),
			Type::AliasTo { to, .. } => {
				get_property_unbound(
					(*to, on_type_arguments),
					(publicity, under, under_type_arguments),
					no_setters,
					info_chain,
					types,
				)
				// TODO would an alias have a property?
				// let property_on_types = info_chain
				// 	.get_chain_of_info()
				// 	.find_map(|info| resolver(info, types, on, on_type_arguments,))
			}
			Type::And(left, right) => get_property_unbound(
				(*left, on_type_arguments),
				(publicity, under, under_type_arguments),
				no_setters,
				info_chain,
				types,
			)
			.or_else(|_| {
				get_property_unbound(
					(*right, on_type_arguments),
					(publicity, under, under_type_arguments),
					no_setters,
					info_chain,
					types,
				)
			}),
			Type::RootPolyType(_nature) => {
				if let Some(on) = on_type_arguments.and_then(|args| args.get_single_argument(on)) {
					resolver(
						(on, on_type_arguments),
						(publicity, under, under_type_arguments),
						no_setters,
						info_chain,
						types,
					)
					.map(wrap)
					.map(LogicalOrValid::Logical)
					.ok_or(Invalid(on))
				} else {
					// I think this is for assigning to properties on parameters
					resolver(
						(on, on_type_arguments),
						(publicity, under, under_type_arguments),
						no_setters,
						info_chain,
						types,
					)
					.map(wrap)
					.map(LogicalOrValid::Logical)
					.ok_or(Invalid(on))
					.or_else(|_| {
						// Can assign to properties on parameters etc
						let aliases =
							get_constraint(on, types).expect("poly type with no constraint");

						get_property_unbound(
							(aliases, on_type_arguments),
							(publicity, under, under_type_arguments),
							no_setters,
							info_chain,
							types,
						)
					})
				}
			}
			Type::PartiallyAppliedGenerics(crate::types::PartiallyAppliedGenerics {
				on: base,
				arguments,
			}) => {
				let on_sg_type = if let GenericArguments::Closure(_) = arguments {
					resolver(
						(on, on_type_arguments),
						(publicity, under, under_type_arguments),
						no_setters,
						info_chain,
						types,
					)
					.map(wrap)
					.map(LogicalOrValid::Logical)
					.ok_or(Invalid(on))
				} else {
					Err(Invalid(on))
				};

				on_sg_type.or_else(|_| {
					let on_type_arguments = crate::types::GenericChainLink::append(
						on,
						on_type_arguments.as_ref(),
						arguments,
					);

					crate::utilities::notify!("{:?}", on_type_arguments);

					let property = get_property_unbound(
						(*base, on_type_arguments),
						(publicity, under, under_type_arguments),
						no_setters,
						info_chain,
						types,
					)?;

					Ok(if let LogicalOrValid::Logical(fact) = property {
						Logical::Implies { on: Box::new(fact), antecedent: arguments.clone() }
							.into()
					} else {
						property
					})
				})
			}
			Type::Constructor(crate::types::Constructor::ConditionalResult { .. })
			| Type::Or(..) => {
				let (condition, truthy_result, otherwise_result) =
					get_conditional(on, types).expect("case above != get_conditional");
				let left = get_property_unbound(
					(truthy_result, on_type_arguments),
					(publicity, under, under_type_arguments),
					no_setters,
					info_chain,
					types,
				)?;
				let right = get_property_unbound(
					(otherwise_result, on_type_arguments),
					(publicity, under, under_type_arguments),
					no_setters,
					info_chain,
					types,
				)?;

				Ok(Logical::Or { condition, left: Box::new(left), right: Box::new(right) }.into())
				// if let (Ok(left), Ok(right)) = (left, right) {
				// 	crate::utilities::notify!(
				// 		"Getting property left={:?}, right={:?}",
				// 		left,
				// 		right
				// 	);
				// } else {
				// 	Err(Invalid(on))
				// }
			}
			Type::Constructor(_constructor) => {
				let on_constructor_type = resolver(
					(on, on_type_arguments),
					(publicity, under, under_type_arguments),
					no_setters,
					info_chain,
					types,
				)
				.map(wrap)
				.map(LogicalOrValid::Logical)
				.ok_or(Invalid(on));

				let aliases = get_constraint(on, types).expect("no constraint for constructor");

				on_constructor_type.or_else(|_| {
					get_property_unbound(
						(aliases, on_type_arguments),
						(publicity, under, under_type_arguments),
						no_setters,
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

				let prototype = info_chain
					.get_chain_of_info()
					.find_map(|facts| facts.prototypes.get(&on))
					.copied();

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
					no_setters,
					info_chain,
					types,
				);

				let result = if let (Some(prototype), None) = (prototype, &on_self) {
					resolver(
						(prototype, on_type_arguments),
						(publicity, under, under_type_arguments),
						no_setters,
						info_chain,
						types,
					)
				} else {
					on_self
				};

				// crate::utilities::notify!("result={:?}", result);

				result
					.map(wrap)
					.map(|result| {
						if let Some(ref generics) = generics {
							// TODO clone
							Logical::Implies { on: Box::new(result), antecedent: generics.clone() }
						} else {
							result
						}
					})
					.map(LogicalOrValid::Logical)
					.ok_or(Invalid(on))
			}
			Type::Interface { .. } => resolver(
				(on, on_type_arguments),
				(publicity, under, under_type_arguments),
				no_setters,
				info_chain,
				types,
			)
			.map(wrap)
			.map(LogicalOrValid::Logical)
			.ok_or(Invalid(on))
			.or_else(|_| {
				// TODO class and class constructor extends etc
				if let Some(extends) = types.interface_extends.get(&on) {
					get_property_unbound(
						(*extends, on_type_arguments),
						(publicity, under, under_type_arguments),
						no_setters,
						info_chain,
						types,
					)
				} else {
					Err(Invalid(on))
				}
			}),
			Type::SpecialObject(SpecialObject::Null) => Err(Invalid(on)),
			// Type::SpecialObject(SpecialObject::ClassConstructor { .. }) |
			Type::Class { .. } => resolver(
				(on, on_type_arguments),
				(publicity, under, under_type_arguments),
				no_setters,
				info_chain,
				types,
			)
			.map(wrap)
			.map(LogicalOrValid::Logical)
			.ok_or(Invalid(on))
			.or_else(|_| {
				if let Some(prototype) =
					info_chain.get_chain_of_info().find_map(|info| info.prototypes.get(&on))
				{
					get_property_unbound(
						(*prototype, on_type_arguments),
						(publicity, under, under_type_arguments),
						no_setters,
						info_chain,
						types,
					)
				} else {
					Err(Invalid(on))
				}
			}),
			// Fix for string indexing
			Type::Constant(Constant::String(s)) if under.as_number(types).is_some() => {
				// TODO temp TypeId::STRING_GENERIC for slice member
				let idx: usize = under.as_number(types).unwrap();
				let character = s.chars().nth(idx);
				if let Some(character) = character {
					Ok(Logical::BasedOnKey(LeftRight::Left {
						value: Box::new(Logical::Pure(PropertyValue::Value(
							TypeId::STRING_GENERIC,
						))),
						key_arguments: crate::Map::from_iter([(
							TypeId::STRING_GENERIC,
							(CovariantContribution::String(character.to_string()), 0),
						)]),
					})
					.into())
				} else {
					Err(Invalid(on))
				}
			}
			Type::Constant(cst) => resolver(
				(on, on_type_arguments),
				(publicity, under, under_type_arguments),
				no_setters,
				info_chain,
				types,
			)
			.map(wrap)
			.map(LogicalOrValid::Logical)
			.ok_or(Invalid(on))
			.or_else(|_| {
				let backing_type = cst.get_backing_type_id();
				get_property_unbound(
					(backing_type, on_type_arguments),
					(publicity, under, under_type_arguments),
					no_setters,
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
				Ok(LogicalOrValid::NeedsCalculation(NeedsCalculation::Proxy(*proxy)))
			}
			Type::SpecialObject(SpecialObject::Generator { .. }) => {
				todo!()
			}
			Type::SpecialObject(SpecialObject::RegularExpression { .. }) => get_property_unbound(
				(TypeId::REGEXP_TYPE, None),
				(publicity, under, under_type_arguments),
				no_setters,
				info_chain,
				types,
			),
		}
	}

	// if on == TypeId::ERROR_TYPE {
	// 	return Err(MissingOrToCalculate::Error);
	// }
	// if on == TypeId::ANY_TYPE {
	// 	// TODO any
	// 	return Err(MissingOrToCalculate::Infer { on });
	// }

	if let PropertyKey::Type(key) = under {
		let n = *key;
		// if *key == TypeId::ERROR_TYPE {
		// 	return Err(MissingOrToCalculate::Error);
		// } else
		if let Some((condition, truthy_result, otherwise_result)) = get_conditional(n, types) {
			let left = get_property_unbound(
				(on, on_type_arguments),
				(publicity, &PropertyKey::Type(truthy_result), under_type_arguments),
				no_setters,
				info_chain,
				types,
			)?;
			let right = get_property_unbound(
				(on, on_type_arguments),
				(publicity, &PropertyKey::Type(otherwise_result), under_type_arguments),
				no_setters,
				info_chain,
				types,
			)?;

			Ok(Logical::Or { condition, left: Box::new(left), right: Box::new(right) }.into())
		} else if types.get_type_by_id(n).is_constant() {
			crate::utilities::notify!("Here at constant");
			get_property_on_type_unbound(
				(on, on_type_arguments),
				(publicity, &PropertyKey::Type(n), under_type_arguments),
				no_setters,
				info_chain,
				types,
			)
		} else {
			{
				let n = if let Type::RootPolyType(crate::types::PolyNature::MappedGeneric {
					name,
					eager_fixed,
				}) = types.get_type_by_id(n)
				{
					if let Some(argument) =
						under_type_arguments.and_then(|v| v.get_single_argument(n))
					{
						return get_property_on_type_unbound(
							(on, on_type_arguments),
							(publicity, &PropertyKey::Type(argument), under_type_arguments),
							no_setters,
							info_chain,
							types,
						);
					} else {
						crate::utilities::notify!("No mapped argument");
						n
					}
				// crate::utilities::notify!("{} {:?}", name, types.get_type_by_id(*eager_fixed));
				// let n = under_type_arguments
				// 	.as_ref()
				// 	.and_then(|args| {
				// 		crate::utilities::notify!("{:?}", under_type_arguments);
				// 		args.get_single_argument(*eager_fixed)
				// 	})
				// 	.unwrap_or(n);

				// crate::utilities::notify!("Result {:?}", types.get_type_by_id(n));
				} else {
					n
				};

				// Note uses intermediate function to avoid cyclic recursion
				// TODO could short here
				// let property_is = get_property_on_type_unbound(
				// 	(on, on_type_arguments),
				// 	(publicity, &PropertyKey::Type(n), under_type_arguments),
				// 	no_setters,
				// 	info_chain,
				// 	types,
				// );

				// crate::utilities::notify!("IGNORING VALUE {:?}", property_is);
			}

			// TODO
			// let optional = if let TypeId::STRING_TYPE | TypeId::NUMBER_TYPE = n {
			//  true
			// } else {
			// 	false
			// };

			Ok(Logical::BasedOnKey(LeftRight::Right { on, filter: *key }).into())
		}
	} else {
		get_property_on_type_unbound(
			(on, on_type_arguments),
			(publicity, under, under_type_arguments),
			no_setters,
			info_chain,
			types,
		)
	}
}

pub type Properties = Vec<(Publicity, PropertyKey<'static>, PropertyValue)>;

#[derive(Debug, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum AccessMode {
	Regular,
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
	// TODO remove this
	if on == TypeId::ERROR_TYPE
		|| matches!(under, PropertyKey::Type(under) if *under == TypeId::ERROR_TYPE)
	{
		return Some((PropertyKind::Direct, TypeId::ERROR_TYPE));
	}

	// TODO
	// Ok(new_conditional_context(
	// 	environment,
	// 	(is_lhs_null, lhs.1),
	// 	|env: &mut Environment, data: &mut CheckingData<T, A>| {
	// 		A::synthesise_expression(rhs, TypeId::ANY_TYPE, env, data)
	// 	},
	// 	Some(|_env: &mut Environment, _data: &mut CheckingData<T, A>| lhs.0),
	// 	checking_data,
	// ))

	let (to_index, via) = if let Some(constraint) = get_constraint(on, types) {
		(constraint, Some(on))
	} else if let Some(constraint) = top_environment.possibly_mutated_objects.get(&on).copied() {
		(constraint, Some(on))
	} else {
		(on, None)
	};

	let result = get_property_unbound(
		(to_index, None),
		(publicity, under, None),
		true,
		top_environment,
		types,
	);

	match result {
		Ok(logical) => {
			if let LogicalOrValid::Logical(logical) = logical {
				let (kind, result) = resolve_property_on_logical(
					(logical, None),
					(on, under),
					top_environment,
					types,
					behavior,
					mode,
				)?;

				if let Some(via) = via {
					let constructor =
						types.register_type(Type::Constructor(Constructor::Property {
							on,
							under: under.into_owned(),
							result,
							mode,
						}));
					// TODO if not constant etc
					behavior.get_latest_info(top_environment).events.push(Event::Getter {
						on,
						under: under.into_owned(),
						reflects_dependency: Some(constructor),
						publicity,
						position,
						mode,
					});

					Some((kind, constructor))
				} else {
					Some((kind, result))
				}
			} else {
				// TODO
				None
			}
		}
		Err(err) => None,
		// match err {
		// 	MissingOrToCalculate::Missing => None,
		// 	MissingOrToCalculate::Error => {
		// 		// Don't return none because that will raise error!
		// 		Some((PropertyKind::Direct, TypeId::ERROR_TYPE))
		// 	}
		// 	// Can get through set prototype..?
		// 	MissingOrToCalculate::Infer { .. } => {
		// 		crate::utilities::notify!("TODO set infer");
		// 		Some((PropertyKind::Direct, TypeId::ERROR_TYPE))
		// 	}
		// 	MissingOrToCalculate::Proxy(Proxy { .. }) => {
		// 		todo!(); // #33
		// 		 // TODO pass down
		// 	}
		// },
	}
}

/// Generates closure arguments, values of this and more. Runs getters
///
/// TODO generics
fn resolve_property_on_logical<E: CallCheckingBehavior>(
	(logical, generics): (Logical<PropertyValue>, GenericChain),
	(on, under): (TypeId, &PropertyKey),
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
					// TODO some of these need proper `substitute` I think
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
						Type::Constructor(_) | Type::RootPolyType { .. } => {
							if let Some(generics) = generics {
								let arguments = generics.into_substitutable(types);
								let value =
									crate::types::substitute(value, &arguments, environment, types);
								Some((PropertyKind::Direct, value))
							} else {
								Some((PropertyKind::Direct, value))
							}
							// if let Some(entries) = generics
							// 	.and_then(|args| args.get_argument(value, environment, types))
							// {
							// 	let mut iter = entries.into_iter();
							// 	let mut ty = iter.next().unwrap();
							// 	for other in iter {
							// 		ty = types.new_or_type(ty, other);
							// 	}
							// 	Some((
							// 		PropertyKind::Direct,
							// 		if types.get_type_by_id(ty).is_constant() {
							// 			ty
							// 		} else {
							// 			types.register_type(Type::Constructor(
							// 				Constructor::Property {
							// 					on,
							// 					under: under.into_owned(),
							// 					result: ty,
							// 					mode,
							// 				},
							// 			))
							// 		},
							// 	))
							// } else {
							// }
						}
						Type::SpecialObject(..) | Type::Object(..) | Type::Constant(..) => {
							Some((PropertyKind::Direct, value))
						}
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
							// TODO not great +2 types... need less overhead
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
					let result = getter.call(Vec::new(), input, environment, behavior, types);
					// let getter = types.functions.get(&getter).unwrap().clone();
					// let call = getter.call(
					// 	(
					// 		ThisValue::Passed(on),
					// 		&[],
					// 		None,
					// 		// TODO structure generics
					// 		None,
					// 	),
					// 	input,
					// 	environment,
					// 	behavior,
					// 	types,
					// );
					match result {
						Ok(res) => {
							let application_result =
								application_result_to_return_type(res.result, environment, types);
							Some((PropertyKind::Getter, application_result))
						}
						Err(_) => {
							todo!("getter failed")
						}
					}
				}
				PropertyValue::Setter(_) => {
					crate::utilities::notify!("Found setter");
					None
				}
				PropertyValue::Deleted => None,
				PropertyValue::ConditionallyExists { condition, truthy } => {
					let condition = generics
						.as_ref()
						.and_then(|link| link.get_single_argument(condition))
						.unwrap_or(condition);

					if let TypeId::FALSE = condition {
						None
					} else {
						let (kind, value) = resolve_property_on_logical(
							(Logical::Pure(*truthy), generics),
							(on, under),
							environment,
							types,
							behavior,
							mode,
						)?;
						Some((
							kind,
							types.new_conditional_type(condition, value, TypeId::UNDEFINED_TYPE),
						))
					}
				}
				PropertyValue::Configured { on: value, .. } => resolve_property_on_logical(
					(Logical::Pure(*value), generics),
					(on, under),
					environment,
					types,
					behavior,
					mode,
				),
			}
		}
		Logical::Or { left, right, condition } => {
			// if let (Ok(lhs), Ok(rhs)) = (*left, *right) {
			let (_, left) = if let LogicalOrValid::Logical(left) = *left {
				resolve_property_on_logical(
					(left, generics),
					(on, under),
					environment,
					types,
					behavior,
					mode,
				)?
			} else {
				todo!()
			};
			let (_, right) = if let LogicalOrValid::Logical(right) = *right {
				resolve_property_on_logical(
					(right, generics),
					(on, under),
					environment,
					types,
					behavior,
					mode,
				)?
			} else {
				todo!()
			};
			Some((PropertyKind::Direct, types.new_conditional_type(condition, left, right)))
			// } else {
			// 	crate::utilities::notify!("TODO emit some diagnostic about missing");
			// 	None
			// }
		}
		Logical::Implies { on: log_on, antecedent } => {
			crate::utilities::notify!("from=TypeId::UNIMPLEMENTED_ERROR_TYPE here");
			crate::utilities::notify!("antecedent={:?}", antecedent);
			let generics = GenericChainLink::append(
				// TODO
				TypeId::UNIMPLEMENTED_ERROR_TYPE,
				generics.as_ref(),
				&antecedent,
			);
			resolve_property_on_logical(
				(*log_on, generics),
				(on, under),
				environment,
				types,
				behavior,
				mode,
			)
		}
		Logical::BasedOnKey(kind) => match kind {
			LeftRight::Left { value, key_arguments } => {
				let generics = Some(GenericChainLink::MappedPropertyLink {
					parent_link: generics.as_ref(),
					value: &key_arguments,
				});

				resolve_property_on_logical(
					(*value, generics),
					(on, under),
					environment,
					types,
					behavior,
					mode,
				)
			}
			LeftRight::Right { on, filter } => {
				// todo!()
				crate::utilities::notify!(
					"Here (should do get_properties with filter) {:?} {:?}",
					on,
					filter
				);
				None
				// resolve_property_on_logical(
				// 	(*log_on, generics),
				// 	(on, under),
				// 	environment,
				// 	types,
				// 	behavior,
				// 	mode,
				// )
			}
		},
	}
}
// }

// #[allow(clippy::too_many_arguments)]
// #[allow(clippy::needless_pass_by_value)]
// fn evaluate_get_on_poly<E: CallCheckingBehavior>(
// 	on: TypeId,
// 	publicity: Publicity,
// 	under: PropertyKey,
// 	_with: Option<TypeId>,
// 	top_environment: &mut Environment,
// 	behavior: &mut E,
// 	types: &mut TypeStore,
// 	position: SpanWithSource,
// 	mode: AccessMode,
// ) -> Option<(PropertyKind, TypeId)> {
// 	/// Also returns the [`Type::Constructor`] -> [`Constructor::Property`]
// 	fn resolve_logical_with_poly(
// 		fact: Logical<PropertyValue>,
// 		on: TypeId,
// 		under: PropertyKey,
// 		generics: GenericChain,
// 		environment: &mut Environment,
// 		types: &mut TypeStore,
// 		mode: AccessMode,
// 	) -> Option<TypeId> {
// 		match fact {
// 			Logical::Pure(og) => {
// 				Some(match og {
// 					PropertyValue::Value(value) => match types.get_type_by_id(value) {
// 						t @ (Type::And(_, _)
// 						| Type::Or(_, _)
// 						| Type::RootPolyType(_)
// 						| Type::PartiallyAppliedGenerics(_)
// 						| Type::Constructor(_)
// 						// TODO not sure about these two?
// 						| Type::FunctionReference(..)
// 						| Type::Object(ObjectNature::AnonymousTypeAnnotation)) => {
// 							let result = if let Some(link) = generics {
// 								let arguments = link.into_substitutable(types);
// 								crate::utilities::notify!("arguments={:?}", arguments.arguments);
// 								crate::types::substitute(value, &arguments, environment, types)
// 							} else {
// 								crate::utilities::notify!("Here, getting property on {:?}", t);
// 								value
// 							};

// 							crate::utilities::notify!("If result == constant here");

// 							types.register_type(Type::Constructor(Constructor::Property {
// 								on,
// 								under: under.into_owned(),
// 								result,
// 								mode,
// 							}))
// 						}
// 						// Don't need to set this here. It is picked up from `on` during lookup
// 						Type::SpecialObject(SpecialObject::Function(..))
// 						| Type::AliasTo { .. }
// 						| Type::Interface { .. }
// 						| Type::Class { .. } => types.register_type(Type::Constructor(Constructor::Property {
// 							on,
// 							under: under.into_owned(),
// 							result: value,
// 							mode,
// 						})),
// 						Type::Constant(_)
// 						| Type::Object(ObjectNature::RealDeal)
// 						| Type::SpecialObject(..) => value,
// 					},
// 					PropertyValue::Getter(getter) => {
// 						// if is_open_poly {
// 						// 	crate::utilities::notify!("TODO evaluate getter...");
// 						// } else {
// 						// 	crate::utilities::notify!("TODO don't evaluate getter");
// 						// }
// 						let getter = types.functions.get(&getter).unwrap();
// 						types.register_type(Type::Constructor(Constructor::Property {
// 							on,
// 							under: under.into_owned(),
// 							result: getter.return_type,
// 							mode,
// 						}))
// 					}
// 					PropertyValue::Setter(_) => todo!(),
// 					// Very important
// 					PropertyValue::Deleted => return None,
// 					PropertyValue::ConditionallyExists { on, truthy } => {
// 						let on = generics
// 							.as_ref()
// 							.and_then(|link| link.get_single_argument(on))
// 							.unwrap_or(on);

// 						let value = resolve_logical_with_poly(
// 							Logical::Pure(*truthy),
// 							on,
// 							under.clone(),
// 							generics,
// 							environment,
// 							types,
// 							mode,
// 						)?;
// 						types.new_conditional_type(on, value, TypeId::UNDEFINED_TYPE)
// 					}
// 					PropertyValue::Configured { on: value, .. } => resolve_logical_with_poly(
// 						Logical::Pure(*value),
// 						on,
// 						under.clone(),
// 						generics,
// 						environment,
// 						types,
// 						mode,
// 					)?,
// 				})
// 			}
// 			Logical::Or { condition, left, right } => {
// 				// crate::utilities::notify!("lr = {:?}", (left, right));

// 				// TODO lots of information (and inference) lost here
// 				if let (Ok(lhs), Ok(rhs)) = (*left, *right) {
// 					let lhs = resolve_logical_with_poly(
// 						lhs,
// 						on,
// 						under.clone(),
// 						generics,
// 						environment,
// 						types,
// 						mode,
// 					)?;
// 					let rhs = resolve_logical_with_poly(
// 						rhs,
// 						on,
// 						under,
// 						generics,
// 						environment,
// 						types,
// 						mode,
// 					)?;
// 					Some(types.new_conditional_type(condition, lhs, rhs))
// 				} else {
// 					crate::utilities::notify!("TODO emit some diagnostic about missing");
// 					None
// 				}
// 			}
// 			Logical::Implies { on: log_on, antecedent } => {
// 				crate::utilities::notify!("from=TypeId::UNIMPLEMENTED_ERROR_TYPE here");
// 				let generics = GenericChainLink::append(
// 					// TODO
// 					TypeId::UNIMPLEMENTED_ERROR_TYPE,
// 					generics.as_ref(),
// 					&antecedent,
// 				);

// 				resolve_logical_with_poly(
// 					*log_on,
// 					on,
// 					under.clone(),
// 					generics,
// 					environment,
// 					types,
// 					mode,
// 				)
// 			}
// 			Logical::BasedOnKey { on: log_on, key_arguments } => {
// 				let generics = GenericChainLink::MappedPropertyLink {
// 					parent_link: generics.as_ref(),
// 					value: &key_arguments,
// 				};

// 				resolve_logical_with_poly(
// 					*log_on,
// 					on,
// 					under.clone(),
// 					Some(generics),
// 					environment,
// 					types,
// 					mode,
// 				)
// 			}
// 		}
// 	}

// 	let fact =
// 		get_property_unbound((on, None), (publicity, &under, None), true, top_environment, types)
// 			.ok()?;

// 	crate::utilities::notify!("unbound is is {:?}", fact);

// 	let value =
// 		resolve_logical_with_poly(fact, on, under.clone(), None, top_environment, types, mode)?;

// 	Some((PropertyKind::Direct, value))
// }

pub fn get_properties(
	_base: TypeId,
	_types: &TypeStore,
	_info: &impl InformationChain,
	_filter_enumerable: bool,
) -> Properties {
	todo!()
}

/// Get properties on a type (for printing and other non-one property uses)
///
/// - TODO prototypes?
/// - TODO return whether it is fixed (conditional + conditional enumerable + non string keys)
/// - TODO doesn't evaluate properties
/// - TODO don't have to reverse at end
/// - `filter_enumerable` for printing vs `for in` loops
pub fn get_properties_on_single_type(
	base: TypeId,
	types: &TypeStore,
	info: &impl InformationChain,
	filter_enumerable: bool,
	filter_type: TypeId,
) -> Properties {
	match types.get_type_by_id(base) {
		Type::Interface { .. } | Type::Class { .. } | Type::Object(_) => {
			// Reversed needed for deleted
			let reversed_flattened_properties = info
				.get_chain_of_info()
				.filter_map(|info| info.current_properties.get(&base).map(|v| v.iter().rev()))
				.flatten();

			let mut deleted_or_existing_properties =
				std::collections::HashSet::<PropertyKey>::new();

			// This retains ordering here

			let mut properties = Vec::new();
			let mut numerical_properties = std::collections::BTreeMap::new();

			for (publicity, key, value) in reversed_flattened_properties {
				let new_record = deleted_or_existing_properties.insert(key.clone());

				if let PropertyValue::Configured { on: _, ref descriptor } = value {
					// TODO what about if not `TypeId::TRUE | TypeId::FALSE`
					crate::utilities::notify!("descriptor.enumerable={:?}", descriptor.enumerable);
					if filter_enumerable && !matches!(descriptor.enumerable, TypeId::TRUE) {
						continue;
					}
				}

				if let PropertyValue::Deleted = value {
					// TODO only covers constant keys :(
					continue;
				}

				if !matches!(filter_type, TypeId::ANY_TYPE) {
					let on_type_arguments = None; // TODO
					let (key_matches, key_arguments) = super::key_matches(
						(&PropertyKey::Type(filter_type), on_type_arguments),
						(key, None),
						info,
						types,
					);

					crate::utilities::notify!("key_arguments={:?}", key_arguments);

					if !key_matches {
						continue;
					}
				}

				if new_record {
					let value = (*publicity, key.to_owned(), value.clone());

					if let Some(n) = key.as_number(types) {
						numerical_properties.insert(n, value);
					} else {
						properties.push(value);
					}
				}
			}

			properties.reverse();

			if numerical_properties.is_empty() {
				properties
			} else {
				let mut new: Vec<_> = numerical_properties.into_values().collect();
				new.append(&mut properties);
				new
			}
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

/// WIP TODO remove filter
pub fn get_properties_on_single_type2(
	(base, base_arguments): (TypeId, GenericChain),
	types: &TypeStore,
	info: &impl InformationChain,
	filter_type: TypeId,
) -> Vec<(PropertyKey<'static>, PropertyValue, SliceArguments)> {
	match types.get_type_by_id(base) {
		Type::Interface { .. } | Type::Class { .. } | Type::Object(_) => {
			// Reversed needed for deleted
			let reversed_flattened_properties = info
				.get_chain_of_info()
				.filter_map(|info| info.current_properties.get(&base).map(|v| v.iter().rev()))
				.flatten();

			let mut deleted_or_existing_properties =
				std::collections::HashSet::<PropertyKey>::new();

			// This retains ordering here

			let mut properties = Vec::new();

			for (publicity, key, value) in reversed_flattened_properties {
				let new_record = deleted_or_existing_properties.insert(key.clone());

				let on_type_arguments = None; // TODO
				let (key_matches, key_arguments) = super::key_matches(
					(&PropertyKey::Type(filter_type), on_type_arguments),
					(key, None),
					info,
					types,
				);

				// crate::utilities::notify!("key_arguments={:?}", key_arguments);

				if key_matches {
					properties.push((key.clone(), value.clone(), key_arguments));
				}
			}

			properties
		}
		Type::Constructor(_) | Type::RootPolyType(_) => {
			if let Some(argument) =
				base_arguments.as_ref().and_then(|args| args.get_single_argument(base))
			{
				get_properties_on_single_type2((argument, base_arguments), types, info, filter_type)
			} else {
				todo!("Getting properties on generic")
			}
		}
		t @ (Type::SpecialObject(_)
		| Type::Or(..)
		| Type::PartiallyAppliedGenerics(_)
		| Type::Constant(_)
		| Type::AliasTo { .. }
		| Type::FunctionReference(_)
		| Type::And(_, _)) => panic!("Cannot get all properties on {t:?}"),
	}
}
