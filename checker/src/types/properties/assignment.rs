use super::{get_property_unbound, Descriptor, PropertyKey, PropertyValue, Publicity};

use crate::{
	context::CallCheckingBehavior,
	diagnostics::{PropertyKeyRepresentation, TypeStringRepresentation},
	events::Event,
	features::objects::Proxy,
	subtyping::{State, SubTypeResult},
	types::{
		calling::{CallingDiagnostics, CallingOutput, SynthesisedArgument},
		get_constraint,
		helpers::tuple_like,
		logical::{BasedOnKey, Logical, LogicalOrValid, NeedsCalculation},
		Constructor, GenericChain, PartiallyAppliedGenerics, TypeStore,
	},
	Environment, Type, TypeId,
};

use source_map::SpanWithSource;

pub enum SetPropertyError {
	/// Both readonly readonly modifier (TS), readonly property (TS), frozen and not writable
	NotWriteable {
		property: PropertyKeyRepresentation,
		position: SpanWithSource,
	},
	DoesNotMeetConstraint {
		property_constraint: TypeStringRepresentation,
		value_type: TypeStringRepresentation,
		reason: crate::types::subtyping::NonEqualityReason,
		position: SpanWithSource,
	},
	/// I  Has no effect but doesn't throw an error
	///
	/// [Although only a strict mode issue][https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Errors/Getter_only] it can break the type syetem
	AssigningToGetter {
		property: PropertyKeyRepresentation,
		position: SpanWithSource,
	},
	// TODO #170
	// AssigningToTuple,
	AssigningToNonExistent {
		property: PropertyKeyRepresentation,
		position: SpanWithSource,
	},
}

pub type SetPropertyResult = Result<(), SetPropertyError>;

/// Aka a assignment to a property, **INCLUDING initialization of a new one**
/// - Evaluates setters
/// - This handles both objects and poly types
/// - Only handles `PropertyValue`. See info.register for conditionals, getters etc
pub fn set_property<B: CallCheckingBehavior>(
	on: TypeId,
	(publicity, under, new): (Publicity, &PropertyKey, TypeId),
	position: SpanWithSource,
	environment: &mut Environment,
	(behavior, diagnostics): (&mut B, &mut CallingDiagnostics),
	types: &mut TypeStore,
) -> SetPropertyResult {
	// Frozen checks
	{
		if environment.info.frozen.contains(&on) {
			return Err(SetPropertyError::NotWriteable {
				property: PropertyKeyRepresentation::new(under, environment, types),
				position,
			});
		}
	}

	// Doing this regardless of E::CHECK_PARAMETERS is how #18 works
	{
		let object_constraint =
			environment.get_object_constraint(on).or_else(|| get_constraint(on, types));

		if let Some(object_constraint) = object_constraint {
			// crate::utilities::notify!("constraint={:?}", types.get_type_by_id(constraint));

			if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
				on: TypeId::READONLY_RESTRICTION,
				..
			}) = types.get_type_by_id(object_constraint)
			{
				return Err(SetPropertyError::NotWriteable {
					property: PropertyKeyRepresentation::new(under, environment, types),
					position,
				});
			}

			let property_constraint = get_property_unbound(
				(object_constraint, None),
				(publicity, under, None),
				false,
				environment,
				types,
			);

			// crate::utilities::notify!("Property constraint .is_some() {:?}", property_constraint.is_some());

			// crate::utilities::notify!(
			// 	"Re-assignment constraint {}, prop={} {:?}",
			// 	print_type(constraint, types, environment, true),
			// 	print_type(under, types, environment, true),
			// 	property_constraint
			// );

			if let Ok(LogicalOrValid::Logical(property_constraint)) = property_constraint {
				// TODO property value is readonly
				// TODO state is writeable etc?
				// TODO document difference with context.writeable

				// match new {
				// 	PropertyValue::Value(value) => {
				// TODO ...?
				let mut state = State {
					already_checked: Default::default(),
					mode: Default::default(),
					contributions: Default::default(),
					others: Default::default(),
					object_constraints: Default::default(),
				};
				let result = crate::subtyping::type_is_subtype_of_property(
					(&property_constraint, None),
					new,
					&mut state,
					environment,
					types,
				);
				if let SubTypeResult::IsNotSubType(reason) = result {
					let is_modifying_tuple_length = under.is_equal_to("length")
						&& tuple_like(object_constraint, types, environment);

					crate::utilities::notify!(
						"is_modifying_tuple_length={:?}",
						is_modifying_tuple_length
					);

					let property_constraint = TypeStringRepresentation::from_property_constraint(
						property_constraint,
						None,
						environment,
						types,
						false,
					);

					let value_type =
						TypeStringRepresentation::from_type_id(new, environment, types, false);

					return Err(SetPropertyError::DoesNotMeetConstraint {
						property_constraint,
						value_type,
						reason,
						position,
					});
				}
				// if get_constraint(on, types).is_none() {
				// 	crate::utilities::notify!("Here");
				// 	return Err(SetPropertyError::AssigningToNonExistent {
				// 		property: PropertyKeyRepresentation::new(under, environment, types),
				// 		position,
				// 	});
				// }
				crate::utilities::notify!("TODO assigning to non existent");

				// PropertyValue::Getter(_)
				// | PropertyValue::Setter(_)
				// | PropertyValue::GetterAndSetter { .. }
				// | PropertyValue::Deleted => {}
				// PropertyValue::ConditionallyExists { truthy: ref _truthy, .. } => {
				// 	crate::utilities::notify!("Here assigning to conditional. TODO recursive");
				// }
				// PropertyValue::Configured { descriptor, .. } => {
				// 	crate::utilities::notify!("TODO nested");
				// 	// TODO doesn't account for other types here
				// 	if !matches!(descriptor.writable, TypeId::TRUE) {
				// 		return Err(SetPropertyError::NotWriteable {
				// 			property: PropertyKeyRepresentation::new(under, environment, types),
				// 			position,
				// 		});
				// 	}
				// }
			}
		}
	}

	// crate::utilities::notify!(
	// 	"setting {:?} {:?} {:?}",
	// 	crate::types::printing::print_type(types, on, environment, true),
	// 	crate::types::printing::print_type(types, under, environment, true),
	// 	crate::types::printing::print_type(types, new.as_get_type(), environment, true),
	// );

	// crate::utilities::notify!("(2) Made it here assigning to {:?}", types.get_type_by_id(on));

	// Cascade if it is a union (unsure tho)
	if let Type::Constructor(Constructor::ConditionalResult {
		truthy_result,
		otherwise_result,
		condition: _,
		result_union: _,
	}) = types.get_type_by_id(on)
	{
		crate::utilities::notify!("Cascading assigment bc of conditional result");
		let truthy = *truthy_result;
		let otherwise_result = *otherwise_result;

		// TODO under conditionals for side effected setter
		set_property(
			truthy,
			(publicity, under, new),
			position,
			environment,
			(behavior, diagnostics),
			types,
		)?;
		return set_property(
			otherwise_result,
			(publicity, under, new),
			position,
			environment,
			(behavior, diagnostics),
			types,
		);
	}

	// IMPORTANT: THIS ALSO CAPTURES POLY CONSTRAINTS
	let current_property =
		get_property_unbound((on, None), (publicity, under, None), false, environment, types);

	if let Ok(value) = current_property {
		crate::utilities::notify!("Got {:?}", value);
		match value {
			LogicalOrValid::Logical(fact) => set_on_logical(
				fact,
				(behavior, diagnostics),
				environment,
				(on, None),
				(publicity, under, new),
				types,
				position,
			),
			LogicalOrValid::NeedsCalculation(NeedsCalculation::Infer { on }) => {
				crate::utilities::notify!("TODO add assignment request on {:?}", on);
				behavior.get_latest_info(environment).events.push(Event::Setter {
					on,
					new,
					under: under.into_owned(),
					publicity,
					position,
				});
				Ok(())
			}
			LogicalOrValid::NeedsCalculation(NeedsCalculation::Proxy(proxy, on)) => proxy_assign(
				(proxy, on),
				under,
				new,
				position,
				(behavior, diagnostics),
				environment,
				types,
			),
		}
	} else if get_constraint(on, types).is_some() {
		Err(SetPropertyError::AssigningToNonExistent {
			property: PropertyKeyRepresentation::new(under, environment, types),
			position,
		})
	} else {
		crate::utilities::notify!("No property on object, assigning anyway");
		let info = behavior.get_latest_info(environment);
		info.register_property(
			on,
			publicity,
			under.into_owned(),
			PropertyValue::Value(new),
			position,
		);
		Ok(())
	}
}

fn set_on_logical<B: CallCheckingBehavior>(
	existing: Logical<PropertyValue>,
	(behavior, diagnostics): (&mut B, &mut CallingDiagnostics),
	environment: &mut Environment,
	(on, generics): (TypeId, GenericChain),
	(publicity, under, new): (Publicity, &PropertyKey, TypeId),
	types: &mut TypeStore,
	position: SpanWithSource,
) -> SetPropertyResult {
	match existing {
		Logical::Pure(existing) => run_setter_on_object(
			(existing, None),
			(behavior, diagnostics),
			environment,
			(on, generics),
			(publicity, under, new),
			types,
			position,
		),
		Logical::Or { .. } => {
			if types.get_type_by_id(on).is_dependent() {
				{
					let info = behavior.get_latest_info(environment);
					// TODO cannot do because if substituted property is getter then the following is not true
					// info.current_properties.entry(on).or_default().push((
					// 	publicity,
					// 	under.into_owned(),
					// 	new.clone(),
					// ));
					info.events.push(Event::Setter {
						on,
						new,
						under: under.into_owned(),
						publicity,
						position,
					});
				}
			} else {
				crate::utilities::notify!("TODO ");
			}
			Ok(())
		}
		Logical::Implies { on: og, antecedent } => {
			crate::utilities::notify!("antecedent={:?}", antecedent);
			let generics = crate::types::GenericChainLink::FunctionRoot {
				parent_arguments: Some(&antecedent),
				call_site_type_arguments: None,
				// TODO weird?
				type_arguments: &crate::Map::default(),
			};
			set_on_logical(
				*og,
				(behavior, diagnostics),
				environment,
				(on, Some(generics)),
				(publicity, under, new),
				types,
				position,
			)
		}
		Logical::BasedOnKey(kind) => {
			if let BasedOnKey::Left { value, key_arguments } = kind {
				let generics = crate::types::GenericChainLink::MappedPropertyLink {
					parent_link: generics.as_ref(),
					value: &key_arguments,
				};
				set_on_logical(
					*value,
					(behavior, diagnostics),
					environment,
					(on, Some(generics)),
					(publicity, under, new),
					types,
					position,
				)
			} else {
				crate::utilities::notify!("Here {:?}", kind);

				{
					let info = behavior.get_latest_info(environment);
					info.current_properties.entry(on).or_default().push((
						publicity,
						under.into_owned(),
						PropertyValue::Value(new),
					));
					info.events.push(Event::Setter {
						on,
						new,
						under: under.into_owned(),
						publicity,
						position,
					});
				}

				// set_on_logical(
				// 	*og,
				// 	behavior,
				// 	environment,
				// 	(on, None),
				// 	(publicity, under, new),
				// 	types,
				// 	position,
				// )
				Ok(())
			}
		}
	}
}

/// `Vec<FunctionCallingError>` from calling setter
///
/// `og` = current last value, `position` = assignment position
#[allow(clippy::too_many_arguments)]
fn run_setter_on_object<B: CallCheckingBehavior>(
	(existing, descriptor): (PropertyValue, Option<Descriptor>),
	(behavior, diagnostics): (&mut B, &mut CallingDiagnostics),
	environment: &mut Environment,
	(on, generics): (TypeId, GenericChain),
	(publicity, under, new): (Publicity, &PropertyKey<'_>, TypeId),
	types: &mut TypeStore,
	position: SpanWithSource,
) -> SetPropertyResult {
	match existing {
		PropertyValue::Deleted | PropertyValue::Value(..) => {
			if let (Some(_), PropertyValue::Value(constraint_for_new)) =
				(get_constraint(on, types), existing)
			{
				// TODO ...?
				let mut state = State {
					already_checked: Default::default(),
					mode: Default::default(),
					contributions: Default::default(),
					others: Default::default(),
					object_constraints: Default::default(),
				};
				let result = crate::subtyping::type_is_subtype_with_generics(
					(constraint_for_new, generics),
					(new, None),
					&mut state,
					environment,
					types,
				);
				if let SubTypeResult::IsNotSubType(reason) = result {
					{
						crate::utilities::notify!(
							"{} {:?}",
							crate::types::printing::print_type(
								constraint_for_new,
								types,
								environment,
								true
							),
							generics
						);
					}

					let property_constraint = TypeStringRepresentation::from_property_constraint(
						Logical::Pure(PropertyValue::Value(constraint_for_new)),
						generics,
						environment,
						types,
						false,
					);
					let value_type =
						TypeStringRepresentation::from_type_id(new, environment, types, false);

					// TOOD generics
					return Err(SetPropertyError::DoesNotMeetConstraint {
						property_constraint,
						value_type,
						reason,
						position,
					});
				}
			}

			let value = if let Some(descriptor) = descriptor {
				PropertyValue::Configured { on: Box::new(PropertyValue::Value(new)), descriptor }
			} else {
				PropertyValue::Value(new)
			};

			let info = behavior.get_latest_info(environment);
			info.current_properties.entry(on).or_default().push((
				publicity,
				under.into_owned(),
				value,
			));
			info.events.push(Event::Setter {
				on,
				new,
				under: under.into_owned(),
				publicity,
				position,
			});
			Ok(())
		}
		PropertyValue::Getter(_) => Err(SetPropertyError::AssigningToGetter {
			property: PropertyKeyRepresentation::new(under, environment, types),
			position,
		}),
		PropertyValue::GetterAndSetter { setter, getter: _ } | PropertyValue::Setter(setter) => {
			use crate::types::calling::{CalledWithNew, CallingInput};

			let arg = SynthesisedArgument { position, spread: false, value: new };
			let this_type = generics.and_then(|arg| arg.get_origin()).unwrap_or(on);
			let input = CallingInput {
				called_with_new: CalledWithNew::GetterOrSetter { this_type },
				call_site: position,
				// TODO
				max_inline: 0,
			};

			let result = setter.call(vec![arg], input, environment, (behavior, diagnostics), types);

			// TODO add event if dependent

			match result {
				Ok(CallingOutput {
					result: _,
					called: _,
					special: _,
					result_was_const_computation: _,
				}) => Ok(()),
				Err(_errors) => Ok(()),
			}
		}
		PropertyValue::ConditionallyExists { condition: _condition, truthy } => {
			crate::utilities::notify!("TODO conditionally");

			run_setter_on_object(
				(*truthy, descriptor),
				(behavior, diagnostics),
				environment,
				(on, generics),
				(publicity, under, new),
				types,
				position,
			)
		}
		PropertyValue::Configured { on: existing_value, descriptor } => {
			crate::utilities::notify!("descriptor={:?} {:?}", descriptor, generics);

			let writable = generics
				.as_ref()
				.and_then(|link| link.get_single_argument(descriptor.writable))
				.unwrap_or(descriptor.writable);

			if !matches!(writable, TypeId::TRUE) {
				crate::utilities::notify!("{:?}", writable);
				return Err(SetPropertyError::NotWriteable {
					property: PropertyKeyRepresentation::new(under, environment, types),
					position,
				});
			}

			// let new = PropertyValue::Configured { on: Box::new(new), descriptor };

			run_setter_on_object(
				(*existing_value, Some(descriptor)),
				(behavior, diagnostics),
				environment,
				(on, generics),
				(publicity, under, new),
				types,
				position,
			)
		}
	}
}

pub(crate) fn proxy_assign<B: CallCheckingBehavior>(
	(Proxy { handler, over }, resolver): (Proxy, TypeId),
	under: &PropertyKey,
	new: TypeId,
	position: SpanWithSource,
	(behavior, diagnostics): (&mut B, &mut CallingDiagnostics),
	environment: &mut Environment,
	types: &mut TypeStore,
) -> SetPropertyResult {
	use crate::types::calling::{CalledWithNew, CallingInput, SynthesisedArgument};
	use crate::types::properties::{get_property, AccessMode};

	let property_key = PropertyKey::String(std::borrow::Cow::Borrowed("set"));
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

	if let Some((_, set_trap)) = result {
		let key_to_pass_to_function = under.into_type(types);
		// TODO receiver
		let arguments = vec![
			SynthesisedArgument { spread: false, value: over, position },
			SynthesisedArgument { spread: false, value: key_to_pass_to_function, position },
			SynthesisedArgument { spread: false, value: new, position },
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
		let result = crate::types::calling::Callable::Type(set_trap).call(
			arguments,
			input,
			environment,
			(behavior, diagnostics),
			types,
		);
		if let Ok(_res) = result {
			Ok(())
		} else {
			crate::utilities::notify!("TODO Proxy.set failed but returning Ok() (as difference captured in CallingDiagnostics)");
			Ok(())
		}
	} else {
		set_property(
			over,
			(Publicity::Public, under, new),
			position,
			environment,
			(behavior, diagnostics),
			types,
		)
	}
}
