use super::{CallingTiming, Event, FinalEvent, PrototypeArgument, RootReference};

use crate::{
	context::{
		get_value_of_variable, invocation::InvocationContext, CallCheckingBehavior,
		SetPropertyError,
	},
	diagnostics::{TypeStringRepresentation, TDZ},
	features::{
		functions::ThisValue,
		iteration::{self, IterationKind},
	},
	types::{
		functions::SynthesisedArgument,
		is_type_truthy_falsy,
		poly_types::FunctionTypeArguments,
		printing::print_type,
		properties::{get_property, set_property, PropertyKey, PropertyValue},
		substitute, Constructor, StructureGenerics, TypeId, TypeStore,
	},
	Decidable, Environment, Type,
};

#[derive(Default)]
pub struct ErrorsAndInfo {
	pub errors: Vec<crate::types::calling::FunctionCallingError>,
	pub warnings: Vec<crate::types::calling::InfoDiagnostic>,
}

#[must_use]
pub(crate) fn apply_event(
	event: Event,
	this_value: ThisValue,
	type_arguments: &mut FunctionTypeArguments,
	environment: &mut Environment,
	target: &mut InvocationContext,
	types: &mut TypeStore,
	errors: &mut ErrorsAndInfo,
) -> Option<FinalEvent> {
	match event {
		Event::ReadsReference { reference, reflects_dependency, position } => {
			if let Some(id) = reflects_dependency {
				let value = match reference {
					RootReference::Variable(id) => {
						let value = get_value_of_variable(environment, id, Some(&*type_arguments));
						if let Some(ty) = value {
							ty
						} else {
							errors.errors.push(crate::types::calling::FunctionCallingError::TDZ {
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
				type_arguments.set_id_from_event_application(id, value);
			}
		}
		Event::SetsVariable(variable, value, position) => {
			let new_value = substitute(value, type_arguments, environment, types);

			// TODO temp assigns to many contexts, which is bad.
			// Closures should have an indicator of what they close over #56
			let info = target.get_latest_info(environment);
			for closure_id in type_arguments.closure_id.iter() {
				info.closure_current_values
					.insert((*closure_id, RootReference::Variable(variable)), new_value);
			}

			info.events.push(Event::SetsVariable(variable, new_value, position));
			info.variable_current_value.insert(variable, new_value);
		}
		Event::Getter { on, under, reflects_dependency, publicity, position } => {
			// let was = on;
			let on = substitute(on, type_arguments, environment, types);

			// crate::utils::notify!("was {:?} now {:?}", was, on);

			let under = match under {
				crate::types::properties::PropertyKey::Type(under) => {
					let ty = substitute(under, type_arguments, environment, types);
					crate::types::properties::PropertyKey::from_type(ty, types)
				}
				under @ crate::types::properties::PropertyKey::String(_) => under,
			};

			let Some((_, value)) = get_property(
				on,
				publicity,
				under.clone(),
				None,
				environment,
				target,
				types,
				position,
			) else {
				panic!(
					"could not get property {under:?} at {position:?} on {}, (inference or some checking failed)",
					print_type(on, types, environment, true)
				);
			};

			if let Some(id) = reflects_dependency {
				type_arguments.set_id_from_event_application(id, value);
			}
		}
		Event::Setter { on, under, new, initialization, publicity, position } => {
			// let was = on;
			let on = substitute(on, type_arguments, environment, types);
			// crate::utils::notify!("was {:?} now {:?}", was, on);

			let under = match under {
				PropertyKey::Type(under) => {
					let ty = substitute(under, type_arguments, environment, types);
					PropertyKey::from_type(ty, types)
				}
				under @ PropertyKey::String(_) => under,
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

			let _gc = environment.as_general_context();

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
					.get_latest_info(environment)
					.register_property(on, publicity, under, new, true, position);
			} else {
				let result = set_property(
					on,
					publicity,
					&under,
					new.clone(),
					environment,
					target,
					types,
					position,
				);

				if let Err(err) = result {
					if let SetPropertyError::DoesNotMeetConstraint {
						property_constraint,
						reason: _,
					} = err
					{
						let value_type = if let PropertyValue::Value(id) = new {
							TypeStringRepresentation::from_type_id(id, environment, types, false)
						} else {
							todo!()
						};

						errors.errors.push(
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
		Event::CallsType {
			on,
			with,
			reflects_dependency,
			timing,
			called_with_new,
			position: _,
		} => {
			let _was = on;
			let on = substitute(on, type_arguments, environment, types);

			// crate::utils::notify!("was {:?} now {:?}", was, on);

			let with = with
				.iter()
				.map(|argument| SynthesisedArgument {
					value: substitute(argument.value, type_arguments, environment, types),
					position: argument.position,
					spread: argument.spread,
				})
				.collect::<Vec<_>>();

			match timing {
				CallingTiming::Synchronous => {
					let result = crate::types::calling::call_type(
						on,
						with,
						crate::types::calling::CallingInput {
							called_with_new,
							this_value: Default::default(),
							call_site_type_arguments: None,
							// TODO:
							call_site: source_map::Nullable::NULL,
						},
						environment,
						target,
						types,
					);
					match result {
						Ok(mut result) => {
							errors.warnings.append(&mut result.warnings);
							if let Some(reflects_dependency) = reflects_dependency {
								type_arguments.set_id_from_event_application(
									reflects_dependency,
									result.returned_type,
								);
							}
						}
						Err(mut calling_errors) => {
							crate::utils::notify!("inference and or checking failed at function");
							errors.errors.append(&mut calling_errors);
							if let Some(reflects_dependency) = reflects_dependency {
								type_arguments.set_id_from_event_application(
									reflects_dependency,
									TypeId::ERROR_TYPE,
								);
							}
						}
					}
				}
				// TODO different
				CallingTiming::QueueTask | CallingTiming::AtSomePointManyTimes => {
					todo!()
					// TODO unsure whether need function id here
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
		// TODO extract
		Event::Conditionally {
			condition,
			true_events: events_if_truthy,
			else_events,
			position,
		} => {
			let condition = substitute(condition, type_arguments, environment, types);

			let result = is_type_truthy_falsy(condition, types);
			// crate::utils::notify!("Condition {:?} {:?}", types.get_type_by_id(condition), result);

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
				let (mut truthy_info, truthy_early_return) =
					target.new_conditional_target(|target: &mut InvocationContext| {
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

				let (mut else_info, else_early_return) =
					target.new_conditional_target(|target: &mut InvocationContext| {
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

				// TODO what about two early returns?
				// crate::utils::notify!("TER {:?}, EER {:?}", truthy_early_return, else_early_return);

				if let Some(truthy_early_return) = truthy_early_return {
					truthy_info.events.push(truthy_early_return.into());
				}

				if let Some(else_early_return) = else_early_return {
					else_info.events.push(else_early_return.into());
				}

				// TODO all things that are
				// - variable and property values (these aren't read from events)
				// - immutable, mutable, prototypes etc
				// }
				let info = target.get_latest_info(environment);

				// Merge variable current values conditionally. TODO other info...?
				for (var, truth) in truthy_info.variable_current_value {
					let entry = info.variable_current_value.entry(var);
					entry.and_modify(|existing| {
						let else_result =
							else_info.variable_current_value.remove(&var).unwrap_or(*existing);

						*existing = types.new_conditional_type(condition, truth, else_result);
					});
				}

				info.events.push(Event::Conditionally {
					condition,
					true_events: truthy_info.events.into_boxed_slice(),
					else_events: else_info.events.into_boxed_slice(),
					position,
				});
			}
		}
		Event::FinalEvent(final_event) => {
			return Some(match final_event {
				e @ (FinalEvent::Break { carry: 0, position: _ }
				| FinalEvent::Continue { carry: 0, position: _ }) => e,
				FinalEvent::Break { carry, position } => {
					FinalEvent::Break { carry: carry - target.get_iteration_depth(), position }
				}
				FinalEvent::Continue { carry, position } => {
					FinalEvent::Continue { carry: carry - target.get_iteration_depth(), position }
				}
				FinalEvent::Throw { thrown, position } => {
					let substituted_thrown = substitute(thrown, type_arguments, environment, types);
					FinalEvent::Throw { thrown: substituted_thrown, position }
				}
				FinalEvent::Return { returned, returned_position } => {
					let substituted_returned =
						substitute(returned, type_arguments, environment, types);
					FinalEvent::Return { returned: substituted_returned, returned_position }
				}
			});
		}
		// TODO Needs a position (or not?)
		Event::CreateObject {
			referenced_in_scope_as,
			prototype,
			position: _,
			is_function_this,
		} => {
			// TODO
			let is_under_dyn = true;

			let new_object_id = match prototype {
				PrototypeArgument::Yeah(prototype) => {
					let prototype = substitute(prototype, type_arguments, environment, types);
					target.get_latest_info(environment).new_object(
						Some(prototype),
						types,
						is_under_dyn,
						is_function_this,
					)
				}
				PrototypeArgument::None => target.get_latest_info(environment).new_object(
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

			type_arguments.set_id_from_event_application(referenced_in_scope_as, new_object_id);
		}
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
				IterationKind::Properties { on, variable } => IterationKind::Properties {
					on: substitute(on, type_arguments, environment, types),
					variable,
				},
				IterationKind::Iterator { on, variable } => IterationKind::Iterator {
					on: substitute(on, type_arguments, environment, types),
					variable,
				},
			};

			let early_result = iteration::run_iteration_block(
				kind,
				iterate_over.to_vec(),
				iteration::InitialVariablesInput::Calculated(initial),
				type_arguments,
				environment,
				target,
				errors,
				types,
			);

			if let Some(early_result) = early_result {
				// crate::utils::notify!("got out {:?}", early_result);
				return Some(early_result);
			}
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
// 			environment.info.variable_current_value.insert(variable, new_value);
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
// 						.info
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
