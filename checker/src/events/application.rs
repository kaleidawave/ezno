use source_map::SpanWithSource;

use super::{CallingTiming, Event, FinalEvent, PrototypeArgument, RootReference};

use crate::{
	context::{get_value_of_variable, invocation::InvocationContext, CallCheckingBehavior},
	diagnostics::{TypeStringRepresentation, TDZ},
	events::ApplicationResult,
	features::{
		functions::ThisValue,
		iteration::{self, IterationKind},
		objects::SpecialObject,
		CannotDeleteFromError,
	},
	subtyping::type_is_subtype,
	types::{
		calling::{self, CallingDiagnostics, FunctionCallingError, SynthesisedArgument},
		generics::substitution::SubstitutionArguments,
		is_type_truthy_falsy,
		printing::print_type,
		properties::{assignment::SetPropertyError, get_property, set_property, PropertyValue},
		substitute, PartiallyAppliedGenerics, TypeId, TypeStore,
	},
	Decidable, Environment, Type,
};

pub(crate) struct ApplicationInput {
	pub(crate) this_value: ThisValue,
	pub(crate) call_site: SpanWithSource,
	/// From above, not log
	pub(crate) max_inline: u16,
}

/// `type_arguments` are mutable to add new ones during lookup etc
///
/// because `events` are flat the iteration here is a little bit unintuitive
#[must_use]
#[allow(clippy::too_many_arguments)]
pub(crate) fn apply_events(
	events: &[Event],
	input: &ApplicationInput,
	mut type_arguments: &mut SubstitutionArguments,
	top_environment: &mut Environment,
	target: &mut InvocationContext,
	types: &mut TypeStore,
	mut diagnostics: &mut CallingDiagnostics,
) -> Option<ApplicationResult> {
	let mut trailing = None::<(TypeId, ApplicationResult)>;

	// TODO this could be done better
	let unknown_mode = target.in_unknown();

	// crate::utilities::notify!("Running {:?}", events);

	let mut idx = 0;
	while idx < events.len() {
		let event = &events[idx];

		// 	crate::utilities::notify!("Running single {:?}", event);

		match &event {
			Event::ReadsReference { reference, reflects_dependency, position } => {
				// if B::CHECK_PARAMETERS {
				// TODO check free variables from inference
				// }

				if let Some(id) = reflects_dependency {
					if unknown_mode {
						if let RootReference::Variable(variable) = reference {
							// TODO this is okay for loops, not sure about other cases of this function
							crate::utilities::notify!(
								"Setting loop variable here {:?}",
								reflects_dependency
							);
							target
								.get_latest_info(top_environment)
								.variable_current_value
								.insert(*variable, *id);
						}

						// Think this is correct
						type_arguments.set_during_application(*id, *id);
					} else {
						let value = match reference {
							RootReference::Variable(id) => {
								//  Some(&*type_arguments)
								let value = get_value_of_variable(
									top_environment,
									*id,
									Some(type_arguments),
								);
								if let Some(ty) = value {
									ty
								} else {
									diagnostics.errors.push(
										crate::types::calling::FunctionCallingError::TDZ {
											error: TDZ {
												variable_name: top_environment
													.get_variable_name(*id)
													.to_owned(),
												position: *position,
											},
											call_site: input.call_site,
										},
									);
									TypeId::ERROR_TYPE
								}
							}
							RootReference::This => {
								input.this_value.get(top_environment, types, *position)
							}
						};
						type_arguments.set_during_application(*id, value);
					}
				}
			}
			Event::SetsVariable(variable, value, position) => {
				if unknown_mode {
					// TODO Top environment?
					// TODO maybe needs to be returned back up, rather than set here?
					top_environment.possibly_mutated_variables.insert(*variable);
				} else {
					let new_value = substitute(*value, type_arguments, top_environment, types);

					// There is probably a way to speed this up. For example `Environment` could have a range?
					// This also doesn't work around conditionals
					let in_above_environment =
						top_environment.variables.values().any(|var| var.get_id() == *variable);

					// TODO temp assigns to many contexts, which is bad.
					// Closures should have an indicator of what they close over #56
					let info = target.get_latest_info(top_environment);
					for id in &type_arguments.closures {
						info.closure_current_values
							.insert((*id, RootReference::Variable(*variable)), new_value);
					}

					if !in_above_environment {
						info.events.push(Event::SetsVariable(*variable, new_value, *position));
					}

					info.variable_current_value.insert(*variable, new_value);
				}
			}
			Event::Getter { on, under, reflects_dependency, publicity, position, mode } => {
				// let was = on;
				let on = substitute(*on, type_arguments, top_environment, types);

				// crate::utilities::notify!("was {:?} now {:?}", was, on);

				let under = under.substitute(type_arguments, top_environment, types);

				let Some((_, value)) = get_property(
					on,
					*publicity,
					&under,
					None,
					top_environment,
					(target, diagnostics),
					types,
					*position,
					*mode,
				) else {
					// TODO getters can fail here
					panic!(
						"Could not get property {under:?} at {position:?} on {}, (inference or some checking failed)",
						print_type(on, types, top_environment, true)
					);
				};

				if let Some(id) = reflects_dependency {
					type_arguments.set_during_application(*id, value);

					// {
					// 	let value = print_type(value, types, top_environment, true);
					// 	crate::utilities::notify!("Setting {:?} to {:?}", id, value);
					// }
				}
			}
			Event::Setter { on, under, new, publicity, position } => {
				let on = substitute(*on, type_arguments, top_environment, types);

				if unknown_mode {
					// TODO constraint something?
					// TODO maybe needs to be returned back up, rather than set here?
					top_environment.possibly_mutated_objects.insert(on, TypeId::ANY_TYPE);
				} else {
					let under = under.substitute(type_arguments, top_environment, types);
					let new = substitute(*new, type_arguments, top_environment, types);

					{
						crate::utilities::notify!(
							"[Event::Setter] ({})[{:?}] = {:?}",
							crate::types::printing::print_type(on, types, top_environment, true),
							under,
							new
						);
					}

					let result = set_property(
						on,
						(*publicity, &under, new.clone()),
						*position,
						top_environment,
						(target, diagnostics),
						types,
					);

					match result {
						Ok(()) => {}
						Err(err) => {
							if let SetPropertyError::DoesNotMeetConstraint {
								property_constraint,
								value_type,
								reason: _,
								position: _,
							} = err
							{
								diagnostics.errors.push(
									crate::types::calling::FunctionCallingError::SetPropertyConstraint {
										property_type: property_constraint,
										value_type,
										assignment_position: *position,
										call_site: input.call_site,
									},
								);
							} else {
								unreachable!("set property check failed")
							}
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
				possibly_thrown: _,
				call_site,
			} => {
				let on = match on {
					calling::Callable::Fixed(fixed, this_value) => {
						crate::utilities::notify!("TODO specialise this?");
						calling::Callable::Fixed(*fixed, *this_value)
					}
					calling::Callable::Type(on) => calling::Callable::Type(substitute(
						*on,
						type_arguments,
						top_environment,
						types,
					)),
				};

				// crate::utilities::notify!("was {:?} now {:?}", was, on);

				let with = with
					.iter()
					.map(|argument| SynthesisedArgument {
						value: substitute(argument.value, type_arguments, top_environment, types),
						position: argument.position,
						spread: argument.spread,
					})
					.collect::<Vec<_>>();

				match timing {
					CallingTiming::Synchronous => {
						let input = crate::types::calling::CallingInput {
							called_with_new: *called_with_new,
							// TODO:
							max_inline: input.max_inline,
							call_site: *call_site,
						};
						let result =
							on.call(with, input, top_environment, (target, diagnostics), types);
						match result {
							Ok(result) => {
								if let Some(ApplicationResult::Throw { .. }) = result.result {
									// TODO conditional here
									return result.result;
								}

								if let Some(reflects_dependency) = reflects_dependency {
									let as_type = calling::application_result_to_return_type(
										result.result,
										top_environment,
										types,
									);
									type_arguments
										.set_during_application(*reflects_dependency, as_type);
								}

								// if result.thrown_type != TypeId::NEVER_TYPE {
								// 	// TODO
								// 	// return FinalEvent::Throw {
								// 	// 	thrown: result.thrown_type,
								// 	// 	position: source_map::Nullable::NULL,
								// 	// }
								// 	// .into();
								// }
							}
							Err(_) => {
								crate::utilities::notify!(
									"inference and or checking failed at function"
								);
								if let Some(reflects_dependency) = reflects_dependency {
									type_arguments.set_during_application(
										*reflects_dependency,
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
			Event::Conditionally { condition, truthy_events, otherwise_events, position } => {
				let condition = substitute(*condition, type_arguments, top_environment, types);

				let fixed_result = is_type_truthy_falsy(condition, types);
				// crate::utilities::notify!("Condition {:?} {:?}", types.get_type_by_id(condition), result);

				let (truthy_events, otherwise_events) =
					(*truthy_events as usize, *otherwise_events as usize);
				let offset = idx + 1;

				match fixed_result {
					Decidable::Known(result) => {
						let (start, end) = if result {
							(offset, offset + truthy_events)
						} else {
							(offset + truthy_events, offset + truthy_events + otherwise_events)
						};
						let events_to_run = &events[start..end];
						// crate::utilities::notify!(
						// 	"(start, end) = {:?}, {:?}",
						// 	(start, end),
						// 	events_to_run
						// );
						let result =
							target.new_unconditional_target(|target: &mut InvocationContext| {
								apply_events(
									events_to_run,
									input,
									type_arguments,
									top_environment,
									target,
									types,
									diagnostics,
								)
							});

						if result.is_some() {
							crate::utilities::notify!("Here {:?}", result);
							return result;
						}
					}
					Decidable::Unknown(condition) => {
						// TODO early returns

						// TODO could inject proofs but probably already worked out
						let truthy_events_slice = &events[offset..(offset + truthy_events)];
						let otherwise_events_slice = &events
							[(offset + truthy_events)..(offset + truthy_events + otherwise_events)];

						let (data, (truthy_result, otherwise_result)) = target
							.evaluate_conditionally(
								top_environment,
								types,
								(condition, *position),
								(truthy_events_slice, otherwise_events_slice),
								(type_arguments, diagnostics),
								|top_environment, types, target, events, data| {
									let (type_arguments, diagnostics) = data;
									apply_events(
										events,
										input,
										type_arguments,
										top_environment,
										target,
										types,
										diagnostics,
									)
								},
							);

						(type_arguments, diagnostics) = data;

						match (truthy_result, otherwise_result) {
							(Some(truthy_result), Some(otherwise_result)) => {
								return Some(ApplicationResult::Or {
									on: condition,
									truthy_result: Box::new(truthy_result),
									otherwise_result: Box::new(otherwise_result),
								});
							}
							(None, Some(right)) => {
								let negated_condition = types.new_logical_negation_type(condition);
								trailing = Some(match trailing {
									Some((existing_condition, existing)) => (
										negated_condition,
										ApplicationResult::Or {
											on: existing_condition,
											truthy_result: Box::new(existing),
											otherwise_result: Box::new(right),
										},
									),
									None => (negated_condition, right),
								});
							}
							(Some(left), None) => {
								trailing = Some(match trailing {
									Some((existing_condition, existing)) => (
										condition,
										ApplicationResult::Or {
											on: existing_condition,
											truthy_result: Box::new(existing),
											otherwise_result: Box::new(left),
										},
									),
									None => (condition, left),
								});
							}
							(None, None) => {}
						}
					}
				}
				// Don't run condition again
				// TODO there must be better way
				idx += truthy_events + otherwise_events + 1;
			}
			Event::CreateObject { referenced_in_scope_as, prototype, position } => {
				// TODO
				let is_under_dyn = true;

				let new_object_ty = match prototype {
					PrototypeArgument::Yeah(prototype) => {
						let prototype =
							substitute(*prototype, type_arguments, top_environment, types);
						target.get_latest_info(top_environment).new_object(
							Some(prototype),
							types,
							*position,
							is_under_dyn,
						)
					}
					PrototypeArgument::None => target.get_latest_info(top_environment).new_object(
						None,
						types,
						*position,
						is_under_dyn,
					),
					PrototypeArgument::Function(id) => types.register_type(
						crate::Type::SpecialObject(SpecialObject::Function(*id, input.this_value)),
					),
				};

				// TODO conditionally if any properties are structurally generic
				// let new_object_ty_with_curried_arguments =
				// 	curry_arguments(type_arguments, types, new_object_ty);

				// crate::utilities::notify!(
				// 	"Setting {:?} to {:?}",
				// 	referenced_in_scope_as,
				// 	new_object_ty_with_curried_arguments
				// );

				if let Some(object_constraint) =
					top_environment.get_object_constraint(*referenced_in_scope_as)
				{
					top_environment.add_object_constraints(
						std::iter::once((new_object_ty, object_constraint)),
						types,
					);
				}

				type_arguments.set_during_application(*referenced_in_scope_as, new_object_ty);
			}
			Event::Iterate { kind, iterate_over, initial } => {
				let closure_id = types.new_closure_id();
				type_arguments.closures.push(closure_id);

				crate::utilities::notify!("Setting closure variables");

				// Set closed over values
				// TODO `this`
				for (variable, value) in initial.0.iter() {
					let value = substitute(*value, type_arguments, top_environment, types);
					let info = target.get_latest_info(top_environment);
					info.closure_current_values
						.insert((closure_id, RootReference::Variable(*variable)), value);

					crate::utilities::notify!(
						"in {:?} set {:?} to {:?}",
						closure_id,
						variable,
						value
					);
				}

				let kind = match kind {
					IterationKind::Condition { under, postfix_condition } => {
						IterationKind::Condition {
							under: under.map(|under| {
								under.specialise(type_arguments, top_environment, types)
							}),
							postfix_condition: *postfix_condition,
						}
					}
					IterationKind::Properties { on, variable } => IterationKind::Properties {
						on: substitute(*on, type_arguments, top_environment, types),
						variable: *variable,
					},
					IterationKind::Iterator { on, variable } => IterationKind::Iterator {
						on: substitute(*on, type_arguments, top_environment, types),
						variable: *variable,
					},
				};

				let offset = idx + 1;

				let result = target.new_loop_iteration(|target| {
					let events = &events[offset..(offset + *iterate_over as usize)];
					// crate::utilities::notify!("Running in iteration {:?}", events);
					iteration::run_iteration_block(
						kind,
						events,
						input,
						iteration::RunBehavior::Run,
						type_arguments,
						top_environment,
						target,
						diagnostics,
						types,
					)
				});

				crate::utilities::notify!("Inner loop returned {:?}", result);

				if result.is_some() {
					return result;
				}
				idx += *iterate_over as usize + 1;
			}
			Event::ExceptionTrap { investigate, handle, finally, trapped_type_id } => {
				let (investigate, handle, finally) =
					(*investigate as usize, *handle as usize, *finally as usize);
				let total = investigate + handle + finally;
				let offset = idx + 1;

				let inner_result = apply_events(
					&events[offset..(offset + investigate)],
					input,
					type_arguments,
					top_environment,
					target,
					types,
					diagnostics,
				);

				if let Some(result) = inner_result {
					// TODO finally + what if no catch (handle)

					match result {
						ApplicationResult::Or { .. } => {
							todo!()
						}
						ApplicationResult::Yield { .. } => {
							todo!()
						}
						ApplicationResult::Return { .. }
						| ApplicationResult::Break { .. }
						| ApplicationResult::Continue { .. } => return Some(result),
						ApplicationResult::Throw { thrown, position } => {
							if let Some(trap) = trapped_type_id {
								type_arguments.set_during_application(trap.generic_type, thrown);

								if let Some(constraint) = trap.constrained {
									crate::utilities::notify!("TODO check using function");

									let mut state = crate::subtyping::State {
										already_checked: Default::default(),
										mode: crate::subtyping::SubTypingMode::default(),
										contributions: None,
										object_constraints: None,
										others: crate::subtyping::SubTypingOptions::default(),
									};

									let result = type_is_subtype(
										constraint,
										thrown,
										&mut state,
										top_environment,
										types,
									);

									if let crate::subtyping::SubTypeResult::IsNotSubType(_reason) =
										result
									{
										diagnostics.errors.push(
											FunctionCallingError::CannotCatch {
												catch: TypeStringRepresentation::from_type_id(
													constraint,
													top_environment,
													types,
													false,
												),
												thrown: TypeStringRepresentation::from_type_id(
													thrown,
													top_environment,
													types,
													false,
												),
												thrown_position: position,
											},
										);
									}
								}
							}

							let handler =
								&events[(offset + investigate)..(offset + investigate + handle)];

							let result = apply_events(
								handler,
								input,
								type_arguments,
								top_environment,
								target,
								types,
								diagnostics,
							);

							if result.is_some() {
								return result;
							}
						}
					}
				} else {
					crate::utilities::notify!("todo {:?}", inner_result);
				}

				idx += total;
			}
			Event::RegisterVariable { .. } => {}
			Event::EndOfControlFlow { .. } => {
				crate::utilities::notify!("Shouldn't be applying `Event::EndOfControlFlow`");
			}
			Event::Miscellaneous(misc) => match misc {
				super::MiscellaneousEvents::Has { on, publicity, under, into } => {
					let on = substitute(*on, type_arguments, top_environment, types);
					let under = under.substitute(type_arguments, top_environment, types);

					let has = crate::features::in_operator(
						(*publicity, &under),
						on,
						top_environment,
						types,
					);

					type_arguments.arguments.insert(*into, has);
				}
				super::MiscellaneousEvents::Delete { on, publicity, under, into, position } => {
					let on = substitute(*on, type_arguments, top_environment, types);
					let under = under.substitute(type_arguments, top_environment, types);

					match crate::features::delete_operator(
						(*publicity, under),
						on,
						*position,
						top_environment,
						types,
					) {
						Ok(result) => {
							if let Some(into) = into {
								type_arguments.arguments.insert(*into, result);
							}
						}
						Err(err) => {
							diagnostics.errors.push(match err {
								CannotDeleteFromError::Constraint { constraint, position } => {
									FunctionCallingError::DeleteConstraint {
										constraint,
										delete_position: position,
										call_site: input.call_site,
									}
								}
								_ => todo!(),
							});

							if let Some(into) = into {
								type_arguments.arguments.insert(*into, TypeId::ERROR_TYPE);
							}
						}
					}
				}
				super::MiscellaneousEvents::RegisterProperty {
					on,
					publicity,
					under,
					value,
					position,
				} => {
					let on = substitute(*on, type_arguments, top_environment, types);

					// TODO temp fix for closures
					let on = if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
						on,
						arguments: _,
					}) = types.get_type_by_id(on)
					{
						*on
					} else {
						on
					};

					let under = under.substitute(type_arguments, top_environment, types);
					let value = match value {
						PropertyValue::Value(value) => PropertyValue::Value(substitute(
							*value,
							type_arguments,
							top_environment,
							types,
						)),
						value => {
							crate::utilities::notify!("TODO value {:?}", value);
							value.clone()
						}
					};

					target.get_latest_info(top_environment).register_property(
						on,
						*publicity,
						under.clone(),
						value.clone(),
						*position,
					);
				}
				super::MiscellaneousEvents::CreateConstructor {
					referenced_in_scope_as,
					function,
				} => {
					let new_function_ty = types.register_type(Type::SpecialObject(
						SpecialObject::Function(*function, Default::default()),
					));
					type_arguments.set_during_application(*referenced_in_scope_as, new_function_ty);
				}
			},
			Event::FinalEvent(final_event) => {
				// I think this is okay
				if !unknown_mode {
					let application_result = match final_event {
						FinalEvent::Break { carry, position } => ApplicationResult::Break {
							// TODO is this correct?
							carry: carry.saturating_sub(target.get_iteration_depth()),
							position: *position,
						},
						FinalEvent::Continue { carry, position } => ApplicationResult::Continue {
							// TODO is this correct?
							carry: carry.saturating_sub(target.get_iteration_depth()),
							position: *position,
						},
						FinalEvent::Throw { thrown, position } => {
							let substituted_thrown =
								substitute(*thrown, type_arguments, top_environment, types);
							if target.in_unconditional() {
								let value = TypeStringRepresentation::from_type_id(
									substituted_thrown,
									// TODO is this okay?
									top_environment,
									types,
									false,
								);
								let warning =
									crate::diagnostics::TypeCheckWarning::ConditionalExceptionInvoked {
										value,
										call_site: input.call_site,
									};
								diagnostics.warnings.push(warning);
							}
							ApplicationResult::Throw {
								thrown: substituted_thrown,
								position: *position,
							}
						}
						FinalEvent::Return { returned, position } => {
							let substituted_returned =
								substitute(*returned, type_arguments, top_environment, types);
							ApplicationResult::Return {
								returned: substituted_returned,
								position: *position,
							}
						}
					};
					return Some(if let Some((on, trailing)) = trailing {
						ApplicationResult::Or {
							on,
							truthy_result: Box::new(trailing),
							otherwise_result: Box::new(application_result),
						}
					} else {
						application_result
					});
				}
			}
		}
		idx += 1;
	}
	None
}
