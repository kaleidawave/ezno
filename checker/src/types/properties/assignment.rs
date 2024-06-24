use super::{get_property_unbound, PropertyKey, PropertyValue, Publicity};

use crate::{
	context::{CallCheckingBehavior, SetPropertyError},
	diagnostics::TypeStringRepresentation,
	events::Event,
	features::{constant_functions::CallSiteTypeArguments, functions::ThisValue},
	subtyping::{State, SubTypeResult},
	types::{
		calling::FunctionCallingError, generics::generic_type_arguments::GenericArguments,
		get_constraint, tuple_like, Constructor, PartiallyAppliedGenerics, SynthesisedArgument,
		TypeStore,
	},
	Environment, Logical, Type, TypeId,
};

use source_map::SpanWithSource;

/// Aka a assignment to a property, **INCLUDING initialization of a new one**
///
/// Evaluates setters
pub fn set_property<E: CallCheckingBehavior>(
	on: TypeId,
	(publicity, under, new): (Publicity, &PropertyKey, PropertyValue),
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
	setter_position: SpanWithSource,
) -> Result<Option<TypeId>, SetPropertyError> {
	// TODO
	// if environment.is_not_writeable(on, under) {
	// 	return Err(SetPropertyError::NotWriteable);
	// }

	// if E::CHECK_PARAMETERS {
	let constraint = environment.get_object_constraint(on).or_else(|| get_constraint(on, types));
	if let Some(constraint) = constraint {
		// crate::utilities::notify!("constraint={:?}", types.get_type_by_id(constraint));

		if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::READONLY_RESTRICTION,
			..
		}) = types.get_type_by_id(constraint)
		{
			return Err(SetPropertyError::NotWriteable);
		}

		let property_constraint =
			get_property_unbound((constraint, None), (publicity, under, None), environment, types);

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
							&& tuple_like(constraint, types, environment);

						crate::utilities::notify!(
							"is_modifying_tuple_length={:?}",
							is_modifying_tuple_length
						);

						return Err(SetPropertyError::DoesNotMeetConstraint {
							property_constraint: TypeStringRepresentation::from_property_constraint(
								property_constraint,
								None,
								environment,
								types,
								false,
							),
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
			}
		} else {
			// TODO does not exist warning
			// return Err(SetPropertyError::DoesNotMeetConstraint(
			// 	new.as_get_type(),
			// 	todo!("no property"),
			// ));
		}
	}

	// crate::utilities::notify!(
	// 	"setting {:?} {:?} {:?}",
	// 	crate::types::printing::print_type(types, on, environment, true),
	// 	crate::types::printing::print_type(types, under, environment, true),
	// 	crate::types::printing::print_type(types, new.as_get_type(), environment, true),
	// );

	let current_property =
		get_property_unbound((on, None), (publicity, under, None), environment, types);

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

	if let Ok(fact) = current_property {
		match fact {
			Logical::Pure(og) => {
				let result = run_setter_on_object(
					og,
					behavior,
					environment,
					on,
					publicity,
					under,
					new,
					types,
					setter_position,
				);
				if let Err(result) = result {
					// TODO temp
					for error in result {
						match error {
							FunctionCallingError::InvalidArgumentType {
								parameter_type,
								argument_type: _,
								argument_position: _,
								parameter_position: _,
								restriction: _,
							} => {
								return Err(SetPropertyError::DoesNotMeetConstraint {
									property_constraint: parameter_type,
									reason: crate::subtyping::NonEqualityReason::Mismatch,
								})
							}
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
				}
			}
			Logical::Or { .. } => todo!(),
			Logical::Implies { on: _implies_on, antecedent: _ } => {
				crate::utilities::notify!("Check that `implies_on` could be a setter here");
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
					initialization: false,
					position: setter_position,
				});
			}
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

/// `Vec<FunctionCallingError>` from calling setter
#[allow(clippy::too_many_arguments)]
fn run_setter_on_object<E: CallCheckingBehavior>(
	og: PropertyValue,
	behavior: &mut E,
	environment: &mut Environment,
	on: TypeId,
	publicity: Publicity,
	under: &PropertyKey<'_>,
	new: PropertyValue,
	types: &mut TypeStore,
	setter_position: SpanWithSource,
) -> Result<(), Vec<FunctionCallingError>> {
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
				initialization: false,
				position: setter_position,
			});

			Ok(())
		}
		PropertyValue::Getter(_) => todo!(),
		PropertyValue::Setter(setter) => {
			use crate::types::calling::*;

			let arg = SynthesisedArgument {
				position: setter_position,
				spread: false,
				value: match new {
					PropertyValue::Value(type_id) => type_id,
					_ => todo!(),
				},
			};
			let input = CallingInput {
				called_with_new: CalledWithNew::None,
				call_site: setter_position,
				// TODO
				max_inline: 0,
			};
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
				// Ignore the result
				Ok(_ok) => Ok(()),
				Err(res) => Err(res.errors),
			}
		}
		PropertyValue::ConditionallyExists { .. } => todo!(),
	}
}
