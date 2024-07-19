use super::{get_property_unbound, PropertyKey, PropertyValue, Publicity};

use crate::{
	context::{CallCheckingBehavior, SetPropertyError},
	diagnostics::{PropertyKeyRepresentation, TypeStringRepresentation},
	events::Event,
	features::{constant_functions::CallSiteTypeArguments, functions::ThisValue},
	subtyping::{State, SubTypeResult},
	types::{
		calling::FunctionCallingError, generics::generic_type_arguments::GenericArguments,
		get_constraint, tuple_like, Constructor, GenericChain, PartiallyAppliedGenerics,
		SynthesisedArgument, TypeStore,
	},
	Environment, Logical, Type, TypeId,
};

use source_map::SpanWithSource;

/// Aka a assignment to a property, **INCLUDING initialization of a new one**
///
/// Evaluates setters
///
/// This handles both poly objects and
pub fn set_property<E: CallCheckingBehavior>(
	on: TypeId,
	(publicity, under, new): (Publicity, &PropertyKey, PropertyValue),
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
	setter_position: SpanWithSource,
) -> Result<Option<TypeId>, SetPropertyError> {
	// Frozen checks
	{
		if environment.info.frozen.contains(&on) {
			return Err(SetPropertyError::NotWriteable {
				property: PropertyKeyRepresentation::new(under, environment, types),
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

			if let Ok(property_constraint) = property_constraint {
				// TODO ...?
				let mut state = State {
					already_checked: Default::default(),
					mode: Default::default(),
					contributions: Default::default(),
					others: Default::default(),
					object_constraints: Default::default(),
				};

				// TODO property value is readonly
				// TODO state is writeable etc?
				// TODO document difference with context.writeable

				match new {
					PropertyValue::Value(value) => {
						let result = crate::subtyping::type_is_subtype_of_property(
							(&property_constraint, None),
							value,
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

							let property_constraint =
								TypeStringRepresentation::from_property_constraint(
									property_constraint,
									None,
									environment,
									types,
									false,
								);
							return Err(SetPropertyError::DoesNotMeetConstraint {
								property_constraint,
								reason,
							});
						}
					}
					PropertyValue::Getter(_) => todo!(),
					PropertyValue::Setter(_) => todo!(),
					PropertyValue::Deleted => todo!(),
					PropertyValue::ConditionallyExists { truthy: ref _truthy, .. } => {
						crate::utilities::notify!("Here assigning to conditional. TODO recursive");
					}
					PropertyValue::Configured { descriptor, .. } => {
						crate::utilities::notify!("TODO nested");
						if !matches!(descriptor.writable, TypeId::TRUE) {
							return Err(SetPropertyError::NotWriteable {
								property: PropertyKeyRepresentation::new(under, environment, types),
							});
						}
					}
				}
			} else {
				// TODO does not exist warning
				// return Err(SetPropertyError::DoesNotMeetConstraint(
				// 	new.as_get_type(),
				// 	todo!("no property"),
				// ));
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
		let truthy = *truthy_result;
		let otherwise_result = *otherwise_result;

		// TODO under conditionals
		set_property(
			truthy,
			(publicity, under, new.clone()),
			environment,
			behavior,
			types,
			setter_position,
		)?;
		return set_property(
			otherwise_result,
			(publicity, under, new),
			environment,
			behavior,
			types,
			setter_position,
		);
	}

	// IMPORTANT: THIS ALSO CAPTURES POLY CONSTRAINTS
	let current_property =
		get_property_unbound((on, None), (publicity, under, None), false, environment, types);

	if let Ok(fact) = current_property {
		if let Some(value) = set_on_logical(
			fact,
			behavior,
			environment,
			(on, None),
			(publicity, under, new),
			types,
			setter_position,
		) {
			return value;
		}
	} else {
		// TODO abstract
		// TODO only if dependent?
		let register_setter_event = true;
		behavior.get_latest_info(environment).register_property(
			on,
			publicity,
			under.into_owned(),
			new,
			register_setter_event,
			setter_position,
		);
	}
	Ok(None)
}

fn set_on_logical<E: CallCheckingBehavior>(
	fact: Logical<PropertyValue>,
	behavior: &mut E,
	environment: &mut Environment,
	(on, generics): (TypeId, GenericChain),
	(publicity, under, new): (Publicity, &PropertyKey, PropertyValue),
	types: &mut TypeStore,
	setter_position: SpanWithSource,
) -> Option<Result<Option<TypeId>, SetPropertyError>> {
	match fact {
		Logical::Pure(og) => {
			let result = run_setter_on_object(
				og,
				behavior,
				environment,
				(on, generics),
				(publicity, under, new),
				types,
				setter_position,
			);
			if let Err(result) = result {
				Some(Err(produce_set_property_error(result, under, environment, types)))
			} else {
				None
			}
		}
		Logical::Or { .. } => todo!(),
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
				behavior,
				environment,
				(on, Some(generics)),
				(publicity, under, new),
				types,
				setter_position,
			)
		}
		Logical::BasedOnKey { on: og, key_arguments } => {
			let generics = crate::types::GenericChainLink::MappedPropertyLink {
				parent_link: None,
				value: &key_arguments,
			};
			set_on_logical(
				*og,
				behavior,
				environment,
				(on, Some(generics)),
				(publicity, under, new),
				types,
				setter_position,
			)
		}
	}
}

fn produce_set_property_error(
	result: SetterResult,
	under: &PropertyKey,
	environment: &mut crate::context::Context<crate::context::Syntax>,
	types: &mut TypeStore,
) -> SetPropertyError {
	match result {
		SetterResult::NotWriteable => SetPropertyError::NotWriteable {
			property: PropertyKeyRepresentation::new(under, environment, types),
		},
		SetterResult::SetterErrors(errors) => {
			// TODO temp
			for error in errors {
				match error {
					FunctionCallingError::InvalidArgumentType {
						parameter_type,
						argument_type: _,
						argument_position: _,
						parameter_position: _,
						restriction: _,
					} => {
						return SetPropertyError::DoesNotMeetConstraint {
							property_constraint: parameter_type,
							reason: crate::subtyping::NonEqualityReason::Mismatch,
						}
					}
					FunctionCallingError::DeleteConstraint { constraint: _, .. } => todo!(),
					FunctionCallingError::NeedsToBeCalledWithNewKeyword(_)
					| FunctionCallingError::NoLogicForIdentifier(..)
					| FunctionCallingError::NotCallable { .. }
					| FunctionCallingError::ExcessArguments { .. }
					| FunctionCallingError::ExcessTypeArguments { .. }
					| FunctionCallingError::MissingArgument { .. } => unreachable!(),
					FunctionCallingError::ReferenceRestrictionDoesNotMatch { .. } => {
						todo!()
					}
					FunctionCallingError::CyclicRecursion(_, _) => todo!(),
					FunctionCallingError::TDZ { .. } => todo!(),
					FunctionCallingError::SetPropertyConstraint { .. } => todo!(),
					FunctionCallingError::MismatchedThis { .. } => {
						todo!()
					}
					FunctionCallingError::CannotCatch { .. } => todo!(),
				}
			}
			unreachable!()
		}
	}
}

pub enum SetterResult {
	NotWriteable,
	SetterErrors(Vec<FunctionCallingError>),
}

/// `Vec<FunctionCallingError>` from calling setter
///
/// `og` = current last value
#[allow(clippy::too_many_arguments)]
fn run_setter_on_object<E: CallCheckingBehavior>(
	og: PropertyValue,
	behavior: &mut E,
	environment: &mut Environment,
	(on, generics): (TypeId, GenericChain),
	(publicity, under, new): (Publicity, &PropertyKey<'_>, PropertyValue),
	types: &mut TypeStore,
	setter_position: SpanWithSource,
) -> Result<(), SetterResult> {
	match og {
		PropertyValue::Deleted | PropertyValue::Value(..) => {
			let info = behavior.get_latest_info(environment);
			info.current_properties.entry(on).or_default().push((
				publicity,
				under.into_owned(),
				new.clone(),
			));
			info.events.push(Event::Setter {
				on,
				new,
				under: under.into_owned(),
				publicity,
				initialisation: false,
				position: setter_position,
			});

			Ok(())
		}
		PropertyValue::Getter(_) => todo!(),
		PropertyValue::Setter(setter) => {
			use crate::types::calling::{CalledWithNew, CallingInput};

			let value = match new {
				PropertyValue::Value(type_id) => type_id,
				_ => todo!(),
			};
			let arg = SynthesisedArgument { position: setter_position, spread: false, value };
			let input = CallingInput {
				called_with_new: CalledWithNew::None,
				call_site: setter_position,
				// TODO
				max_inline: 0,
			};
			let setter = types.functions.get(&setter).unwrap().clone();
			let result = setter.call(
				(
					ThisValue::Passed(on),
					&[arg],
					None::<CallSiteTypeArguments>,
					// TODO structure generics
					None::<GenericArguments>,
				),
				input,
				environment,
				behavior,
				types,
			);

			match result {
				// Ignore the returned result of the setter
				Ok(_ok) => Ok(()),
				Err(res) => Err(SetterResult::SetterErrors(res.errors)),
			}
		}
		PropertyValue::ConditionallyExists { on: _condition, truthy } => {
			crate::utilities::notify!("TODO conditionally");

			run_setter_on_object(
				*truthy,
				behavior,
				environment,
				(on, generics),
				(publicity, under, new),
				types,
				setter_position,
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
				return Err(SetterResult::NotWriteable);
			}

			crate::utilities::notify!("Carrying property");
			let new = PropertyValue::Configured { on: Box::new(new), descriptor };

			run_setter_on_object(
				*existing_value,
				behavior,
				environment,
				(on, generics),
				(publicity, under, new),
				types,
				setter_position,
			)
		}
	}
}
