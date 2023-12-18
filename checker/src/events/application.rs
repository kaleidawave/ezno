use super::{CallingTiming, Event, EventResult, PrototypeArgument, RootReference};

use crate::{
	behavior::{
		functions::ThisValue,
		iteration::{self, IterationKind},
	},
	context::{calling::Target, get_value_of_variable, CallCheckingBehavior, SetPropertyError},
	diagnostics::{TypeStringRepresentation, TDZ},
	types::{
		curry_arguments,
		functions::SynthesisedArgument,
		get_constraint, is_type_truthy_falsy,
		poly_types::{generic_type_arguments::TypeArgumentStore, FunctionTypeArguments},
		properties::{get_property, set_property, PropertyValue},
		substitute, Constructor, StructureGenerics, TypeId, TypeStore,
	},
	Decidable, Environment, Type,
};

#[must_use]
pub(crate) fn apply_event(
	event: Event,
	this_value: ThisValue,
	type_arguments: &mut FunctionTypeArguments,
	environment: &mut Environment,
	target: &mut Target,
	types: &mut TypeStore,
	// TODO WIP
	errors: &mut Vec<crate::types::calling::FunctionCallingError>,
) -> Option<EventResult> {
	match event {
		Event::ReadsReference { reference, reflects_dependency, position } => {
			if let Some(id) = reflects_dependency {
				let value = match reference {
					RootReference::Variable(id) => {
						let value = get_value_of_variable(
							environment.facts_chain(),
							id,
							Some(&*type_arguments),
						);
						if let Some(ty) = value {
							ty
						} else {
							errors.push(crate::types::calling::FunctionCallingError::TDZ {
								error: TDZ {
									variable_name: environment.get_variable_name(id).to_owned(),
									position,
								},
								call_site: None,
							});
							TypeId::ERROR_TYPE
						}
					}
					RootReference::This => this_value.get(environment, types, &position),
				};
				type_arguments.set_id_from_reference(id, value);
			}
		}
		Event::SetsVariable(variable, value, position) => {
			let new_value = substitute(value, type_arguments, environment, types);

			// if not closed over!!
			// TODO temp assigns to many contexts, which is bad
			let facts = target.get_latest_facts(environment);
			for closure_id in type_arguments
				.closure_id
				.iter()
				.chain(type_arguments.structure_arguments.iter().flat_map(|s| s.closures.iter()))
			{
				facts
					.closure_current_values
					.insert((*closure_id, RootReference::Variable(variable)), new_value);
			}

			facts.events.push(Event::SetsVariable(variable, new_value, position));
			facts.variable_current_value.insert(variable, new_value);
		}
		Event::Getter { on, under, reflects_dependency, publicity, position } => {
			let on = substitute(on, type_arguments, environment, types);
			let under = match under {
				crate::types::properties::PropertyKey::Type(under) => {
					let ty = substitute(under, type_arguments, environment, types);
					crate::types::properties::PropertyKey::from_type(ty, types)
				}
				under @ crate::types::properties::PropertyKey::String(_) => under,
			};

			let (_, value) =
				get_property(on, publicity, under, None, environment, target, types, position)
					.expect(
						"Inferred or checking failed, could not get property when getting property",
					);

			if let Some(id) = reflects_dependency {
				type_arguments.set_id_from_reference(id, value);
			}
		}
		Event::Setter { on, under, new, initialization, publicity, position } => {
			let was = on;
			let on = substitute(on, type_arguments, environment, types);

			crate::utils::notify!("was {:?} now {:?}", was, on);

			let under = match under {
				crate::types::properties::PropertyKey::Type(under) => {
					let ty = substitute(under, type_arguments, environment, types);
					crate::types::properties::PropertyKey::from_type(ty, types)
				}
				under @ crate::types::properties::PropertyKey::String(_) => under,
			};

			let new = match new {
				PropertyValue::Value(new) => {
					PropertyValue::Value(substitute(new, type_arguments, environment, types))
				}
				// For declare property
				PropertyValue::Getter(_) => todo!(),
				PropertyValue::Setter(_) => todo!(),
				// TODO this might be a different thing at some point
				PropertyValue::Deleted => {
					environment.delete_property(on, &under);
					return None;
				}
			};

			let gc = environment.as_general_context();

			// crate::utils::notify!(
			// 	"[Event::Setter] {}[{}] = {}",
			// 	crate::types::printing::print_type(on, types, &gc, true),
			// 	crate::types::printing::print_type(under, types, &gc, true),
			// 	if let Property::Value(new) = new {
			// 		crate::types::printing::print_type(new, types, &gc, true)
			// 	} else {
			// 		format!("{:#?}", new)
			// 	}
			// );

			if initialization {
				// TODO temp fix for closures
				let on =
					if let Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
						on,
						arguments: _,
					})) = types.get_type_by_id(on)
					{
						*on
					} else {
						on
					};
				target
					.get_latest_facts(environment)
					.register_property(on, publicity, under, new, true, position);
			} else {
				let result =
					set_property(on, publicity, &under, &new, environment, target, types, position);

				if let Err(err) = result {
					if let SetPropertyError::DoesNotMeetConstraint { property_constraint, reason } =
						err
					{
						let value_type = if let PropertyValue::Value(id) = new {
							TypeStringRepresentation::from_type_id(
								id,
								&environment.as_general_context(),
								types,
								false,
							)
						} else {
							todo!()
						};

						errors.push(
							crate::types::calling::FunctionCallingError::SetPropertyConstraint {
								property_type: property_constraint,
								value_type,
								assignment_position: position.unwrap(),
								call_site: None,
							},
						);
					} else {
						unreachable!()
					}
				}
			}
		}
		Event::CallsType { on, with, reflects_dependency, timing, called_with_new, position } => {
			let on = substitute(on, type_arguments, environment, types);

			let with = with
				.iter()
				.map(|argument| match argument {
					SynthesisedArgument::NonSpread { ty, position } => {
						let ty = substitute(*ty, type_arguments, environment, types);
						SynthesisedArgument::NonSpread { ty, position: *position }
					}
				})
				.collect::<Vec<_>>();

			match timing {
				CallingTiming::Synchronous => {
					let result = crate::types::calling::call_type(
						on,
						crate::types::calling::CallingInput {
							called_with_new,
							this_value: Default::default(),
							call_site_type_arguments: None,
							// TODO:
							call_site: source_map::SpanWithSource::NULL_SPAN,
						},
						with,
						environment,
						target,
						types,
					);
					match result {
						Ok(result) => {
							if let Some(reflects_dependency) = reflects_dependency {
								type_arguments.set_id_from_reference(
									reflects_dependency,
									result.returned_type,
								);
							}
						}
						Err(mut calling_errors) => {
							crate::utils::notify!("inference and or checking failed at function");
							errors.append(&mut calling_errors);
						}
					}
				}
				// TODO different
				CallingTiming::QueueTask | CallingTiming::AtSomePointManyTimes => {
					todo!()
					// TODO not sure whether need function id here
					// if let Some(Constant::FunctionReference(function)) =
					// 	environment.get_constant_type(on)
					// {
					// 	match function {
					// 		FunctionPointer::Function(function_id) => {
					// 			environment.tasks_to_run.push((on, *function_id));
					// 		}
					// 		FunctionPointer::AutoConstructor(..)
					// 		| FunctionPointer::Internal(..) => {
					// 			todo!()
					// 		}
					// 	}
					// } else {
					// 	unreachable!("calling something that isn't a function... ?")
					// }
				}
			}
		}
		Event::Throw(thrown, position) => {
			let substituted_thrown = substitute(thrown, type_arguments, environment, types);

			target.get_latest_facts(environment).throw_value(substituted_thrown, position);

			// TODO write down why result isn't added here
			return Some(EventResult::Throw);
		}
		// TODO extract
		Event::Conditionally { condition, events_if_truthy, else_events, position } => {
			let condition = substitute(condition, type_arguments, environment, types);

			let result = is_type_truthy_falsy(condition, types);
			// crate::utils::notify!("Condition {:?}", result);

			if let Decidable::Known(result) = result {
				let to_evaluate = if result { events_if_truthy } else { else_events };
				for event in to_evaluate.iter().cloned() {
					if let Some(early) = apply_event(
						event,
						this_value,
						type_arguments,
						environment,
						target,
						types,
						errors,
					) {
						return Some(early);
					}
				}
			} else {
				// TODO early returns

				// TODO could inject proofs but probably already worked out
				let (truthy_facts, _early_return) =
					target.new_conditional_target(|target: &mut Target| {
						for event in events_if_truthy.into_vec() {
							if let Some(early) = apply_event(
								event,
								this_value,
								type_arguments,
								environment,
								target,
								types,
								errors,
							) {
								return Some(early);
							}
						}
						None
					});

				let (mut else_facts, _early_return) =
					target.new_conditional_target(|target: &mut Target| {
						for event in else_events.into_vec() {
							if let Some(early) = apply_event(
								event,
								this_value,
								type_arguments,
								environment,
								target,
								types,
								errors,
							) {
								return Some(early);
							}
						}
						None
					});

				// TODO early return

				// crate::utils::notify!("TF {:?}\n EF {:?}", truthy_facts, else_facts);

				// TODO all things that are
				// - variable and property values (these aren't read from events)
				// - immutable, mutable, prototypes etc
				// }
				let facts = target.get_latest_facts(environment);
				for (var, truth) in truthy_facts.variable_current_value {
					let entry = facts.variable_current_value.entry(var);
					entry.and_modify(|existing| {
						*existing = types.new_conditional_type(
							condition,
							truth,
							else_facts.variable_current_value.remove(&var).unwrap_or(*existing),
						);
					});
				}

				target.get_latest_facts(environment).events.push(Event::Conditionally {
					condition,
					events_if_truthy: truthy_facts.events.into_boxed_slice(),
					else_events: else_facts.events.into_boxed_slice(),
					position,
				});
			}
		}
		Event::Return { returned, returned_position } => {
			let substituted_returned = substitute(returned, type_arguments, environment, types);
			return Some(EventResult::Return(substituted_returned, returned_position));
		}
		// TODO Needs a position (or not?)
		Event::CreateObject { referenced_in_scope_as, prototype, position, is_function_this } => {
			// TODO
			let is_under_dyn = true;

			let new_object_id = match prototype {
				PrototypeArgument::Yeah(prototype) => {
					let prototype = substitute(prototype, type_arguments, environment, types);
					target.get_latest_facts(environment).new_object(
						Some(prototype),
						types,
						is_under_dyn,
						is_function_this,
					)
				}
				PrototypeArgument::None => target.get_latest_facts(environment).new_object(
					None,
					types,
					is_under_dyn,
					is_function_this,
				),
				PrototypeArgument::Function(id) => {
					types.register_type(crate::Type::Function(id, this_value))
				}
			};

			// TODO conditionally if any properties are structurally generic
			// let new_object_id_with_curried_arguments =
			// 	curry_arguments(type_arguments, types, new_object_id);

			// crate::utils::notify!(
			// 	"Setting {:?} to {:?}",
			// 	referenced_in_scope_as,
			// 	new_object_id_with_curried_arguments
			// );

			type_arguments.set_id_from_reference(referenced_in_scope_as, new_object_id);
		}
		Event::Break { position, carry } => return Some(EventResult::Break { carry }),
		Event::Continue { position, carry } => return Some(EventResult::Continue { carry }),
		Event::Iterate { kind, iterate_over, initial } => {
			// TODO this might clash
			let initial = initial
				.into_iter()
				.map(|(id, value)| (id, substitute(value, type_arguments, environment, types)))
				.collect();

			let kind = match kind {
				IterationKind::Condition { under, postfix_condition } => IterationKind::Condition {
					under: under.map(|under| under.specialise(type_arguments, environment, types)),
					postfix_condition,
				},
				IterationKind::Properties(on) => {
					IterationKind::Properties(substitute(on, type_arguments, environment, types))
				}
				IterationKind::Iterator(on) => {
					IterationKind::Iterator(substitute(on, type_arguments, environment, types))
				}
			};

			iteration::run_iteration_block(
				kind,
				iterate_over.to_vec(),
				iteration::InitialVariablesInput::Calculated(initial),
				type_arguments,
				environment,
				target,
				types,
			)?;
		}
	}
	None
}

// /// For loops and recursion
// pub(crate) fn apply_event_unknown(
// 	event: Event,
// 	this_value: ThisValue,
// 	type_arguments: &mut FunctionTypeArguments,
// 	environment: &mut Environment,
// 	target: &mut Target,
// 	types: &mut TypeStore,
// ) {
// 	match event {
// 		// TODO maybe mark as read
// 		Event::ReadsReference { .. } => {}
// 		Event::Getter { on, under, reflects_dependency, publicity, position } => {
// 			crate::utils::notify!("Run getters");
// 		}
// 		Event::SetsVariable(variable, value, _) => {
// 			let new_value = get_constraint(value, types)
// 				.map(|value| {
// 					types.register_type(Type::RootPolyType(crate::types::PolyNature::Open(value)))
// 				})
// 				.unwrap_or(value);
// 			environment.facts.variable_current_value.insert(variable, new_value);
// 		}
// 		Event::Setter { on, under, new, initialization, publicity, position } => {
// 			let on = substitute(on, type_arguments, environment, types);
// 			let new_value = match new {
// 				PropertyValue::Value(new) => {
// 					let new = get_constraint(new, types)
// 						.map(|value| {
// 							types.register_type(Type::RootPolyType(crate::types::PolyNature::Open(
// 								value,
// 							)))
// 						})
// 						.unwrap_or(new);
// 					PropertyValue::Value(new)
// 				}
// 				PropertyValue::Getter(_) | PropertyValue::Setter(_) | PropertyValue::Deleted => new,
// 			};
// 			match under {
// 				crate::types::properties::PropertyKey::String(_) => {
// 					environment
// 						.facts
// 						.register_property(on, publicity, under, new_value, false, position);
// 				}
// 				crate::types::properties::PropertyKey::Type(_) => todo!(),
// 			}
// 		}
// 		Event::CallsType { on, with, reflects_dependency, timing, called_with_new, position } => {
// 			todo!()
// 		}
// 		Event::Throw(_, _) => todo!(),
// 		Event::Conditionally { condition, events_if_truthy, else_events, position } => {
// 			// TODO think this is correct...?
// 			for event in events_if_truthy.into_vec() {
// 				apply_event_unknown(event, this_value, type_arguments, environment, target, types)
// 			}
// 			for event in else_events.into_vec() {
// 				apply_event_unknown(event, this_value, type_arguments, environment, target, types)
// 			}
// 		}
// 		Event::Return { returned, returned_position } => todo!(),
// 		Event::CreateObject { prototype, referenced_in_scope_as, position, is_function_this } => {
// 			todo!()
// 		}
// 		Event::Break { position, carry } => {
// 			// TODO conditionally
// 		}
// 		Event::Continue { position, carry } => {
// 			// TODO conditionally
// 		}
// 		Event::Iterate { .. } => todo!(),
// 	}
// }
