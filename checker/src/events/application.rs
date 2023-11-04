use super::{CallingTiming, EarlyReturn, Event, PrototypeArgument, RootReference};

use crate::{
	behavior::functions::ThisValue,
	context::{calling::Target, get_value_of_variable, CallCheckingBehavior},
	types::{
		curry_arguments,
		functions::SynthesisedArgument,
		is_type_truthy_falsy,
		poly_types::FunctionTypeArguments,
		properties::{get_property, set_property, Property},
		substitute, Constructor, StructureGenerics, TypeId, TypeStore,
	},
	Environment, TruthyFalsy, Type,
};

pub(crate) fn apply_event(
	event: Event,
	this_value: ThisValue,
	type_arguments: &mut FunctionTypeArguments,
	environment: &mut Environment,
	target: &mut Target,
	types: &mut TypeStore,
) -> EarlyReturn {
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
						match value {
							Some(ty) => ty,
							None => {
								todo!("tdz error");
								TypeId::ERROR_TYPE
							}
						}
					}
					RootReference::This => this_value.get(environment, types, position),
				};
				type_arguments.set_id_from_reference(id, value, types);
			}
		}
		Event::SetsVariable(variable, value, position) => {
			let new_value = substitute(value, type_arguments, environment, types);

			// if not closed over!!
			// TODO temp assigns to many contexts, which is bad
			let facts = target.get_top_level_facts(environment);
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
			let property = substitute(under, type_arguments, environment, types);

			let (_, value) =
				get_property(on, under, publicity, None, environment, target, types, position)
					.expect("Inferred constraints and checking failed");

			if let Some(id) = reflects_dependency {
				type_arguments.set_id_from_reference(id, value, types);
			}
		}
		Event::Setter { on, under, new, reflects_dependency, initialization, publicity } => {
			let on = substitute(on, type_arguments, environment, types);
			let under = substitute(under, type_arguments, environment, types);

			let new = match new {
				Property::Value(new) => {
					Property::Value(substitute(new, type_arguments, environment, types))
				}
				// For declare property
				Property::Getter(_) => todo!(),
				Property::Setter(_) => todo!(),
				Property::GetterAndSetter(_, _) => todo!(),
				Property::Deleted => todo!(),
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
					.get_top_level_facts(environment)
					.register_property(on, under, new, true, publicity);
			} else {
				let returned =
					set_property(on, under, publicity, new, environment, target, types).unwrap();

				if let Some(id) = reflects_dependency {
					type_arguments.set_id_from_reference(
						id,
						returned.unwrap_or(TypeId::UNDEFINED_TYPE),
						types,
					);
				}
			}
		}
		Event::CallsType { on, with, reflects_dependency, timing, called_with_new } => {
			let on = substitute(on, type_arguments, environment, types);

			let with = with
				.iter()
				.map(|argument| match argument {
					SynthesisedArgument::NonSpread { ty, position: pos } => {
						let ty = substitute(*ty, type_arguments, environment, types);
						SynthesisedArgument::NonSpread { ty, position: pos.clone() }
					}
				})
				.collect::<Vec<_>>();

			match timing {
				CallingTiming::Synchronous => {
					let result = crate::types::calling::call_type(
						on,
						called_with_new,
						Default::default(),
						None,
						with,
						// TODO
						source_map::SpanWithSource::NULL_SPAN,
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
									types,
								);
							}
						}
						Err(_) => todo!("inference and or checking failed at function"),
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
		Event::Throw(thrown) => {
			let substituted_thrown = substitute(thrown, type_arguments, environment, types);

			target.get_top_level_facts(environment).throw_value(substituted_thrown);

			if substituted_thrown != TypeId::ERROR_TYPE {
				return None;
			}
		}
		// TODO extract
		Event::Conditionally { condition, events_if_truthy, else_events } => {
			let condition = substitute(condition, type_arguments, environment, types);

			if let TruthyFalsy::Decidable(result) = is_type_truthy_falsy(condition, types) {
				let to_evaluate = if result { events_if_truthy } else { else_events };
				for event in to_evaluate.iter().cloned() {
					apply_event(event, this_value, type_arguments, environment, target, types);
				}
			} else {
				// TODO early returns

				// TODO could inject proofs but probably already worked out
				let truthy_facts = target.new_conditional_target(|target: &mut Target| {
					for event in events_if_truthy.into_vec() {
						apply_event(event, this_value, type_arguments, environment, target, types);
					}
				});

				let mut else_facts = target.new_conditional_target(|target: &mut Target| {
					for event in else_events.into_vec() {
						apply_event(event, this_value, type_arguments, environment, target, types);
					}
				});

				// crate::utils::notify!("TF {:?}\n EF {:?}", truthy_facts, else_facts);

				// TODO all things that are
				// - variable and property values (these aren't read from events)
				// - immutable, mutable, prototypes etc
				// }
				let facts = target.get_top_level_facts(environment);
				for (var, truth) in truthy_facts.variable_current_value {
					let entry = facts.variable_current_value.entry(var);
					entry.and_modify(|existing| {
						*existing = types.new_conditional_type(
							condition,
							truth,
							else_facts.variable_current_value.remove(&var).unwrap_or(*existing),
						)
					});
				}

				target.get_top_level_facts(environment).events.push(Event::Conditionally {
					condition,
					events_if_truthy: truthy_facts.events.into_boxed_slice(),
					else_events: else_facts.events.into_boxed_slice(),
				});
			}
		}
		Event::Return { returned, returned_position } => {
			let substituted_returned = substitute(returned, type_arguments, environment, types);

			if substituted_returned != TypeId::ERROR_TYPE {
				return Some(substituted_returned);
			} else {
				crate::utils::notify!("event returned error so skipped");
			}
		}
		Event::CreateObject { referenced_in_scope_as, prototype } => {
			// TODO only if exposed via set

			// TODO
			let is_under_dyn = true;

			let new_object_id = match prototype {
				PrototypeArgument::Yeah(prototype) => {
					let prototype = substitute(prototype, type_arguments, environment, types);
					target.get_top_level_facts(environment).new_object(
						Some(prototype),
						types,
						is_under_dyn,
					)
				}
				PrototypeArgument::None => {
					target.get_top_level_facts(environment).new_object(None, types, is_under_dyn)
				}
				PrototypeArgument::Function(id) => {
					types.register_type(crate::Type::Function(id, this_value))
				}
			};

			// let new_object_id_with_curried_arguments =  new_object_id;
			// TODO conditionally
			let new_object_id_with_curried_arguments =
				curry_arguments(type_arguments, types, new_object_id);

			type_arguments.set_id_from_reference(
				referenced_in_scope_as,
				new_object_id_with_curried_arguments,
				types,
			);
		}
	}
	None
}
