use source_map::{Nullable, SpanWithSource};

use crate::{
	context::{CallCheckingBehavior, Environment, InformationChain, LocalInformation},
	events::Event,
	features::objects::Proxy,
	types::{
		calling::{Callable, CallingDiagnostics, ThisValue},
		generics::{
			contributions::CovariantContribution, generic_type_arguments::GenericArguments,
		},
		get_constraint,
		helpers::{get_conditional, is_inferrable_type, is_pseudo_continous},
		logical::{
			BasedOnKey, Invalid, Logical, LogicalOrValid, NeedsCalculation, PossibleLogical,
			PropertyOn,
		},
		Constructor, GenericChain, GenericChainLink, ObjectNature, PartiallyAppliedGenerics,
		SliceArguments, SpecialObject, TypeStore,
	},
	Constant, Type, TypeId,
};

use super::{PropertyKey, PropertyKind, PropertyValue, Publicity};

#[allow(clippy::similar_names)]
fn get_property_from_list(
	(on_properties, on_type_arguments): (&super::Properties, GenericChain),
	(publicity, under, under_type_arguments): (Publicity, &PropertyKey, GenericChain),
	info_chain: &impl InformationChain,
	types: &TypeStore,
) -> Option<(PropertyValue, Option<SliceArguments>, bool)> {
	let (required_publicity, want_key, want_type_arguments) =
		(publicity, under, under_type_arguments);

	// This is fine on the same type!
	let mut trailing_getter = None::<Callable>;
	let mut trailing_setter = None::<Callable>;

	// 'rev' is important
	for (publicity, key, value) in on_properties.iter().rev() {
		if *publicity != required_publicity {
			continue;
		}

		if let PropertyValue::ConditionallyExists { .. } = value {
			crate::utilities::notify!("TODO trailing");
			// continue;
		}

		let (matched, key_arguments) = super::key_matches(
			(key, on_type_arguments),
			(want_key, want_type_arguments),
			info_chain,
			types,
		);

		if matched {
			// crate::utilities::notify!("{:?} {:?}", (key, want_key), value);
			// TODO if conditional then continue to find then logical or

			// TODO should come from key_matches
			let is_key_continous = matches!(key, PropertyKey::Type(ty) if is_pseudo_continous((*ty, on_type_arguments), types));

			match value.inner_simple() {
				PropertyValue::Getter(getter) => {
					if let Some(setter) = trailing_setter {
						return Some((
							PropertyValue::GetterAndSetter { getter: *getter, setter },
							key_arguments.into_some(),
							is_key_continous,
						));
					}
					trailing_getter = Some(*getter);
				}
				PropertyValue::Setter(setter) => {
					if let Some(getter) = trailing_getter {
						return Some((
							PropertyValue::GetterAndSetter { getter, setter: *setter },
							key_arguments.into_some(),
							is_key_continous,
						));
					}
					trailing_setter = Some(*setter);
				}
				_ => {
					return Some((value.clone(), key_arguments.into_some(), is_key_continous));
				}
			}
		}
	}

	#[allow(clippy::manual_map)]
	if let Some(setter) = trailing_setter {
		Some((PropertyValue::Setter(setter), None, false))
	} else if let Some(getter) = trailing_getter {
		Some((PropertyValue::Getter(getter), None, false))
	} else {
		None
	}
}

/// Has to return `Logical` for mapped types
///
/// Resolver runs information lookup on **ONE** `TypeId`
pub(crate) fn resolver(
	(on, on_type_arguments): (TypeId, GenericChain),
	(publicity, under, under_type_arguments): (Publicity, &PropertyKey, GenericChain),
	info_chain: &impl InformationChain,
	types: &TypeStore,
) -> Option<(PropertyValue, Option<SliceArguments>, bool)> {
	// TODO if on == constant string and property == length. Need to be able to create types here
	info_chain.get_chain_of_info().find_map(|info: &LocalInformation| {
		info.current_properties.get(&on).and_then(|on_properties| {
			get_property_from_list(
				(on_properties, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
		})
	})
}

pub(crate) fn get_property_unbound(
	(on, on_type_arguments): (TypeId, GenericChain),
	(publicity, under, under_type_arguments): (Publicity, &PropertyKey, GenericChain),
	require_both_logical: bool,
	info_chain: &impl InformationChain,
	types: &TypeStore,
) -> PossibleLogical<PropertyValue> {
	fn wrap(
		(value, slice_arguments, is_key_continous): (PropertyValue, Option<SliceArguments>, bool),
	) -> Logical<PropertyValue> {
		let value = if is_key_continous {
			crate::utilities::notify!("Value {:?}", value);
			if let PropertyValue::ConditionallyExists { .. } = value {
				// Don't make double conditional
				value
			} else {
				PropertyValue::ConditionallyExists {
					condition: TypeId::OPEN_BOOLEAN_TYPE,
					truthy: Box::new(value),
				}
			}
		} else {
			value
		};
		let logical_value = Logical::Pure(value);
		if let Some(slice_arguments) = slice_arguments {
			Logical::BasedOnKey(BasedOnKey::Left {
				value: Box::new(logical_value),
				key_arguments: slice_arguments,
			})
		} else {
			logical_value
		}
	}

	// Intermediate function to avoid cyclic recursion
	// TODO this should return whether the result is on a exclusive type
	fn get_property_on_type_unbound(
		(on, on_type_arguments): (TypeId, GenericChain),
		(publicity, under, under_type_arguments): (Publicity, &PropertyKey, GenericChain),
		require_both_logical: bool,
		info_chain: &impl InformationChain,
		types: &TypeStore,
	) -> PossibleLogical<PropertyValue> {
		match types.get_type_by_id(on) {
			Type::SpecialObject(SpecialObject::Function(function_id, _)) => resolver(
				(on, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
			.map(wrap)
			.or_else(|| {
				let get_function_from_id = types.get_function_from_id(*function_id);
				let ty = if let (
					true,
					crate::types::functions::FunctionBehavior::Function { prototype, .. }
					| crate::types::functions::FunctionBehavior::Constructor { prototype, .. },
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
				info_chain,
				types,
			)
			.map(wrap)
			.map(LogicalOrValid::Logical)
			.ok_or(Invalid(on)),
			Type::Narrowed { narrowed_to, .. } => {
				// TODO wrap value to mark that this could be unsafe if mutated somehow
				get_property_on_type_unbound(
					(*narrowed_to, on_type_arguments),
					(publicity, under, under_type_arguments),
					require_both_logical,
					info_chain,
					types,
				)
			}
			Type::AliasTo { to, .. } => {
				get_property_on_type_unbound(
					(*to, on_type_arguments),
					(publicity, under, under_type_arguments),
					require_both_logical,
					info_chain,
					types,
				)
				// TODO would an alias have a property?
				// let property_on_types = info_chain
				// 	.get_chain_of_info()
				// 	.find_map(|info| resolver(info, types, on, on_type_arguments,))
			}
			Type::And(left, right) => get_property_on_type_unbound(
				(*left, on_type_arguments),
				(publicity, under, under_type_arguments),
				require_both_logical,
				info_chain,
				types,
			)
			.or_else(|_| {
				get_property_on_type_unbound(
					(*right, on_type_arguments),
					(publicity, under, under_type_arguments),
					require_both_logical,
					info_chain,
					types,
				)
			}),
			Type::RootPolyType(_nature) => {
				if let Some(on) = on_type_arguments.and_then(|args| args.get_single_argument(on)) {
					resolver(
						(on, on_type_arguments),
						(publicity, under, under_type_arguments),
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

						if is_inferrable_type(aliases) {
							Ok(LogicalOrValid::NeedsCalculation(NeedsCalculation::Infer { on }))
						} else {
							get_property_on_type_unbound(
								(aliases, on_type_arguments),
								(publicity, under, under_type_arguments),
								require_both_logical,
								info_chain,
								types,
							)
						}
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

					let property = get_property_on_type_unbound(
						(*base, on_type_arguments),
						(publicity, under, under_type_arguments),
						require_both_logical,
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

				if require_both_logical {
					let left = get_property_on_type_unbound(
						(truthy_result, on_type_arguments),
						(publicity, under, under_type_arguments),
						require_both_logical,
						info_chain,
						types,
					)?;
					let right = get_property_on_type_unbound(
						(otherwise_result, on_type_arguments),
						(publicity, under, under_type_arguments),
						require_both_logical,
						info_chain,
						types,
					)?;

					Ok(Logical::Or { condition, left: Box::new(left), right: Box::new(right) }
						.into())
				} else {
					let left = get_property_on_type_unbound(
						(truthy_result, on_type_arguments),
						(publicity, under, under_type_arguments),
						require_both_logical,
						info_chain,
						types,
					);
					let right = get_property_on_type_unbound(
						(otherwise_result, on_type_arguments),
						(publicity, under, under_type_arguments),
						require_both_logical,
						info_chain,
						types,
					);
					if left.is_err() && right.is_err() {
						Err(Invalid(on))
					} else {
						let left = left.unwrap_or(LogicalOrValid::Logical(Logical::Pure(
							PropertyValue::Deleted,
						)));
						let right = right.unwrap_or(LogicalOrValid::Logical(Logical::Pure(
							PropertyValue::Deleted,
						)));
						Ok(Logical::Or { condition, left: Box::new(left), right: Box::new(right) }
							.into())
					}
				}
			}
			Type::Constructor(_constructor) => {
				let on_constructor_type = resolver(
					(on, on_type_arguments),
					(publicity, under, under_type_arguments),
					info_chain,
					types,
				)
				.map(wrap)
				.map(LogicalOrValid::Logical)
				.ok_or(Invalid(on));

				let aliases = get_constraint(on, types).expect("no constraint for constructor");

				on_constructor_type.or_else(|_| {
					get_property_on_type_unbound(
						(aliases, on_type_arguments),
						(publicity, under, under_type_arguments),
						require_both_logical,
						info_chain,
						types,
					)
				})
			}
			Type::Object(ObjectNature::AnonymousTypeAnnotation(properties)) => {
				get_property_from_list(
					(properties, on_type_arguments),
					(publicity, under, under_type_arguments),
					info_chain,
					types,
				)
				.map(wrap)
				.map(LogicalOrValid::Logical)
				.ok_or(Invalid(on))
			}
			Type::Object(ObjectNature::RealDeal) => {
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
			Type::Interface { extends, .. } => resolver(
				(on, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
			.map(wrap)
			.map(LogicalOrValid::Logical)
			.ok_or(Invalid(on))
			.or_else(|_| {
				if let Some(extends) = extends {
					get_property_on_type_unbound(
						(*extends, on_type_arguments),
						(publicity, under, under_type_arguments),
						require_both_logical,
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
					get_property_on_type_unbound(
						(*prototype, on_type_arguments),
						(publicity, under, under_type_arguments),
						require_both_logical,
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
					Ok(Logical::BasedOnKey(BasedOnKey::Left {
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
			Type::Constant(Constant::String(s)) if under.is_equal_to("length") => {
				// TODO temp TypeId::NUMBER_GENERIC for slice member
				let count = s.chars().count();
				Ok(Logical::BasedOnKey(BasedOnKey::Left {
					value: Box::new(Logical::Pure(PropertyValue::Value(TypeId::NUMBER_GENERIC))),
					key_arguments: crate::Map::from_iter([(
						TypeId::NUMBER_GENERIC,
						(CovariantContribution::Number(count as f64), 0),
					)]),
				})
				.into())
			}
			Type::Constant(cst) => resolver(
				(on, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
			.map(wrap)
			.map(LogicalOrValid::Logical)
			.ok_or(Invalid(on))
			.or_else(|_| {
				let backing_type = cst.get_backing_type_id();
				get_property_on_type_unbound(
					(backing_type, on_type_arguments),
					(publicity, under, under_type_arguments),
					require_both_logical,
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
				Ok(LogicalOrValid::NeedsCalculation(NeedsCalculation::Proxy(*proxy, on)))
			}
			Type::SpecialObject(SpecialObject::Generator { .. }) => {
				todo!()
			}
			Type::SpecialObject(SpecialObject::RegularExpression { .. }) => {
				get_property_on_type_unbound(
					(TypeId::REGEXP_TYPE, None),
					(publicity, under, under_type_arguments),
					require_both_logical,
					info_chain,
					types,
				)
			}
		}
	}

	if let PropertyKey::Type(key) = under {
		let key = *key;
		// if *key == TypeId::ERROR_TYPE {
		// 	return Err(MissingOrToCalculate::Error);
		// } else
		if let Some((condition, truthy_result, otherwise_result)) = get_conditional(key, types) {
			let left = get_property_unbound(
				(on, on_type_arguments),
				(publicity, &PropertyKey::Type(truthy_result), under_type_arguments),
				require_both_logical,
				info_chain,
				types,
			)?;
			let right = get_property_unbound(
				(on, on_type_arguments),
				(publicity, &PropertyKey::Type(otherwise_result), under_type_arguments),
				require_both_logical,
				info_chain,
				types,
			)?;

			Ok(Logical::Or { condition, left: Box::new(left), right: Box::new(right) }.into())
		} else if types.get_type_by_id(key).is_constant() {
			crate::utilities::notify!("Here at constant");
			get_property_on_type_unbound(
				(on, on_type_arguments),
				(publicity, &PropertyKey::Type(key), under_type_arguments),
				require_both_logical,
				info_chain,
				types,
			)
		} else {
			// {
			// 	let debug = true;
			// 	crate::utilities::notify!(
			// 		"Key is {}",
			// 		crate::types::printing::print_type(key, types, info_chain, debug)
			// 	);
			// }

			if let Type::RootPolyType(crate::types::PolyNature::MappedGeneric { .. }) =
				types.get_type_by_id(key)
			{
				if let Some(argument) =
					under_type_arguments.and_then(|v| v.get_single_argument(key))
				{
					return get_property_on_type_unbound(
						(on, on_type_arguments),
						(publicity, &PropertyKey::Type(argument), under_type_arguments),
						require_both_logical,
						info_chain,
						types,
					);
				}

				crate::utilities::notify!("No mapped argument, returning BasedOnKey::Right");
				Ok(Logical::BasedOnKey(BasedOnKey::Right(PropertyOn { on, key })).into())
			} else if get_constraint(on, types).is_some_and(is_inferrable_type)
				|| is_inferrable_type(on)
			{
				// TODO or above temp
				Ok(LogicalOrValid::NeedsCalculation(NeedsCalculation::Infer { on }))
			} else {
				crate::utilities::notify!("{:?}", types.get_type_by_id(on));

				Ok(Logical::BasedOnKey(BasedOnKey::Right(PropertyOn { on, key })).into())
			}
		}
	} else {
		get_property_on_type_unbound(
			(on, on_type_arguments),
			(publicity, under, under_type_arguments),
			require_both_logical,
			info_chain,
			types,
		)
	}
}

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
pub(crate) fn get_property<B: CallCheckingBehavior>(
	on: TypeId,
	publicity: Publicity,
	under: &PropertyKey,
	top_environment: &mut Environment,
	(behavior, diagnostics): (&mut B, &mut CallingDiagnostics),
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

	let require_both_logical = true;
	let result = get_property_unbound(
		(to_index, None),
		(publicity, under, None),
		require_both_logical,
		top_environment,
		types,
	);

	{
		crate::utilities::notify!("Access result {:?}", result);
	}

	match result {
		Ok(LogicalOrValid::Logical(logical)) => {
			let (kind, result) = resolve_property_on_logical(
				(logical, None),
				(on, under),
				top_environment,
				types,
				(behavior, diagnostics),
				mode,
			)?;

			let is_constant = types.get_type_by_id(result).is_constant();

			if let (false, Some(_via)) = (is_constant, via) {
				let constructor = types.register_type(Type::Constructor(Constructor::Property {
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
		}
		Ok(LogicalOrValid::NeedsCalculation(NeedsCalculation::Proxy(proxy, proxy_ty))) => {
			proxy_access((proxy, proxy_ty), under, (behavior, diagnostics), top_environment, types)
		}
		Ok(LogicalOrValid::NeedsCalculation(NeedsCalculation::Infer { .. })) => {
			crate::utilities::notify!("Here infer constraint");
			let constructor = types.register_type(Type::Constructor(Constructor::Property {
				on,
				under: under.into_owned(),
				result: TypeId::ANY_TO_INFER_TYPE,
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

			Some((PropertyKind::Direct, constructor))
		}
		Err(_err) => None,
	}
}

/// Generates closure arguments, values of this and more. Runs getters
///
/// TODO generics
fn resolve_property_on_logical<B: CallCheckingBehavior>(
	(logical, generics): (Logical<PropertyValue>, GenericChain),
	(on, under): (TypeId, &PropertyKey),
	environment: &mut Environment,
	types: &mut TypeStore,
	(behavior, diagnostics): (&mut B, &mut CallingDiagnostics),
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
						Type::Narrowed { .. }
						| Type::Constructor(_)
						| Type::RootPolyType { .. } => {
							if let Some(generics) = generics {
								let arguments = generics.build_substitutable(types);
								let value =
									crate::types::substitute(value, &arguments, environment, types);

								if value == TypeId::NEVER_TYPE {
									crate::utilities::notify!("Here");
								}
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
						| Type::AliasTo { to: _, name: _, parameters: _ }
						| Type::Or(_, _) => {
							crate::utilities::notify!(
							    "property was {:?} {:?}, which should be NOT be able to be returned from a function",
							    property, ty
						    );

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
					}
				}
				PropertyValue::GetterAndSetter { getter, setter: _ }
				| PropertyValue::Getter(getter) => {
					use crate::types::calling::{
						application_result_to_return_type, CalledWithNew, CallingInput,
					};

					let input = CallingInput {
						called_with_new: CalledWithNew::GetterOrSetter { this_type: on },
						// TODO
						call_site: source_map::Nullable::NULL,
						// TODO
						max_inline: 0,
					};
					let result =
						getter.call(Vec::new(), input, environment, (behavior, diagnostics), types);
					if let Ok(res) = result {
						let application_result =
							application_result_to_return_type(res.result, environment, types);
						Some((PropertyKind::Getter, application_result))
					} else {
						crate::utilities::notify!("TODO merge calling");
						Some((PropertyKind::Getter, TypeId::ERROR_TYPE))
					}
				}
				PropertyValue::Setter(_) => {
					crate::utilities::notify!("Found setter. TODO warning");
					None
				}
				PropertyValue::Deleted => None,
				PropertyValue::ConditionallyExists { condition, truthy } => {
					crate::utilities::notify!("{:?} {:?}", condition, generics);

					let condition = if let TypeId::NON_OPTIONAL_KEY_ARGUMENT = condition {
						// `unwrap_or(TypeId::TRUE)` temp fix for argument not passed along
						generics
							.as_ref()
							.and_then(|link| link.get_single_argument(condition))
							.unwrap_or(TypeId::TRUE)
					} else {
						generics
							.as_ref()
							.and_then(|link| link.get_single_argument(condition))
							.unwrap_or(condition)
					};

					if let TypeId::FALSE = condition {
						None
					} else {
						let (kind, value) = resolve_property_on_logical(
							(Logical::Pure(*truthy), generics),
							(on, under),
							environment,
							types,
							(behavior, diagnostics),
							mode,
						)?;
						let value = if let TypeId::TRUE = condition {
							value
						} else {
							types.new_conditional_type(condition, value, TypeId::UNDEFINED_TYPE)
						};
						Some((kind, value))
					}
				}
				PropertyValue::Configured { on: value, .. } => resolve_property_on_logical(
					(Logical::Pure(*value), generics),
					(on, under),
					environment,
					types,
					(behavior, diagnostics),
					mode,
				),
			}
		}
		Logical::Or { condition, left, right } => {
			// WIP!
			{
				// TODO get_chain_of_info with behavior??
				if let Some(narrowed_value) = environment.get_narrowed(condition) {
					if let crate::Decidable::Known(condition) =
						crate::types::is_type_truthy_falsy(narrowed_value, types)
					{
						let to_evaluate = if condition { left } else { right };
						return if let LogicalOrValid::Logical(to_evaluate) = *to_evaluate {
							resolve_property_on_logical(
								(to_evaluate, generics),
								(on, under),
								environment,
								types,
								(behavior, diagnostics),
								mode,
							)
						} else {
							todo!()
						};
					}
				}
			}

			// if let (Ok(lhs), Ok(rhs)) = (*left, *right) {
			let (_, left) = if let LogicalOrValid::Logical(left) = *left {
				resolve_property_on_logical(
					(left, generics),
					(on, under),
					environment,
					types,
					(behavior, diagnostics),
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
					(behavior, diagnostics),
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
				(behavior, diagnostics),
				mode,
			)
		}
		Logical::BasedOnKey(kind) => match kind {
			BasedOnKey::Left { value, key_arguments } => {
				let generics = Some(GenericChainLink::MappedPropertyLink {
					parent_link: generics.as_ref(),
					value: &key_arguments,
				});

				resolve_property_on_logical(
					(*value, generics),
					(on, under),
					environment,
					types,
					(behavior, diagnostics),
					mode,
				)
			}
			BasedOnKey::Right(property_on) => {
				crate::utilities::notify!("{:?}", generics);
				let result = property_on.get_on(generics, environment, types)?;

				// {
				// 	let constructor = types.register_type(Type::Constructor(Constructor::Property {
				// 		on,
				// 		under: under.into_owned(),
				// 		result,
				// 		mode,
				// 	}));
				// 	// TODO if not constant etc
				// 	behavior.get_latest_info(environment).events.push(Event::Getter {
				// 		on,
				// 		under: under.into_owned(),
				// 		reflects_dependency: Some(constructor),
				// 		// always this
				// 		publicity: Publicity::Public,
				// 		// TODO
				// 		position: SpanWithSource::NULL,
				// 		mode,
				// 	});
				// 	Some((PropertyKind::Direct, constructor))
				// }

				Some((PropertyKind::Direct, result))
			}
		},
	}
}

pub(crate) fn proxy_access<B: CallCheckingBehavior>(
	(Proxy { handler, over }, resolver): (Proxy, TypeId),
	under: &PropertyKey,
	(behavior, diagnostics): (&mut B, &mut CallingDiagnostics),
	environment: &mut Environment,
	types: &mut TypeStore,
) -> Option<(PropertyKind, TypeId)> {
	use crate::types::calling::{
		application_result_to_return_type, CalledWithNew, CallingInput, SynthesisedArgument,
	};

	// TODO pass down
	let position = SpanWithSource::NULL;
	let property_key = PropertyKey::String(std::borrow::Cow::Borrowed("get"));
	let result = get_property(
		handler,
		Publicity::Public,
		&property_key,
		environment,
		(behavior, diagnostics),
		types,
		position,
		AccessMode::DoNotBindThis,
	);

	if let Some((_, get_trap)) = result {
		let key_to_pass_to_function = under.into_type(types);
		// TODO receiver
		let arguments = vec![
			SynthesisedArgument { spread: false, value: over, position },
			SynthesisedArgument { spread: false, value: key_to_pass_to_function, position },
			SynthesisedArgument { spread: false, value: resolver, position },
		];
		let input = CallingInput {
			// TOOD special
			called_with_new: CalledWithNew::GetterOrSetter { this_type: handler },
			// TODO
			call_site: source_map::Nullable::NULL,
			// TODO
			max_inline: 0,
		};
		let result = crate::types::calling::Callable::Type(get_trap).call(
			arguments,
			input,
			environment,
			(behavior, diagnostics),
			types,
		);
		if let Ok(res) = result {
			let application_result =
				application_result_to_return_type(res.result, environment, types);
			Some((PropertyKind::Getter, application_result))
		} else {
			crate::utilities::notify!("TODO merge calling");
			Some((PropertyKind::Getter, TypeId::ERROR_TYPE))
		}
	} else {
		get_property(
			over,
			Publicity::Public,
			under,
			environment,
			(behavior, diagnostics),
			types,
			position,
			AccessMode::DoNotBindThis,
		)
	}
}
