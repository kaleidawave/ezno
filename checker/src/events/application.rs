use super::{CallingTiming, EarlyReturn, Event, PrototypeArgument, RootReference};

use crate::{
	behavior::functions::ThisValue,
	context::{calling::Target, get_value_of_variable, CallCheckingBehavior},
	types::{
		curry_arguments,
		functions::SynthesizedArgument,
		is_type_truthy_falsy,
		poly_types::FunctionTypeArguments,
		properties::{get_property, set_property, Property},
		specialize, Constructor, StructureGenerics, TypeId, TypeStore,
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
		Event::ReadsReference { reference, reflects_dependency } => {
			if let Some(id) = reflects_dependency {
				let value = match reference {
					RootReference::Variable(id) => {
						get_value_of_variable(environment.facts_chain(), id, Some(&*type_arguments))
							.expect("variable has no value")
					}
					RootReference::This => this_value.get(environment, types),
				};
				type_arguments.set_id(id, value, types);
			}
		}
		Event::SetsVariable(variable, value) => {
			let new_value = specialize(value, type_arguments, environment, types);

			// if not closed over!!
			// TODO temp, might need to set something else. Doesn't work deep
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

			facts.events.push(Event::SetsVariable(variable.clone(), new_value));
			facts.variable_current_value.insert(variable, new_value);
		}
		Event::Getter { on, under, reflects_dependency } => {
			let on = specialize(on, type_arguments, environment, types);
			let property = specialize(under, type_arguments, environment, types);

			let (_, value) = get_property(on, under, None, environment, target, types)
				.expect("Inferred constraints and checking failed");

			if let Some(id) = reflects_dependency {
				type_arguments.set_id(id, value, types);
			}
		}
		Event::Setter { on, under, new, reflects_dependency, initialization } => {
			let on = specialize(on, type_arguments, environment, types);
			let under = specialize(under, type_arguments, environment, types);

			let new = match new {
				Property::Value(new) => {
					Property::Value(specialize(new, type_arguments, environment, types))
				}
				// For declare property
				Property::Getter(_) => todo!(),
				Property::Setter(_) => todo!(),
				Property::GetterAndSetter(_, _) => todo!(),
			};

			let gc = environment.into_general_context();

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
				target.get_top_level_facts(environment).register_property(on, under, new, true);
			} else {
				let returned = set_property(on, under, new, environment, target, types).unwrap();

				if let Some(id) = reflects_dependency {
					type_arguments.set_id(id, returned.unwrap_or(TypeId::UNDEFINED_TYPE), types);
				}
			}
		}
		Event::CallsType { on, with, reflects_dependency, timing, called_with_new } => {
			let on = specialize(on, type_arguments, environment, types);

			let with = with
				.into_iter()
				.map(|argument| match argument {
					SynthesizedArgument::NonSpread { ty, position: pos } => {
						let ty = specialize(*ty, type_arguments, environment, types);
						SynthesizedArgument::NonSpread { ty, position: pos.clone() }
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
						source_map::Span::NULL_SPAN,
						environment,
						target,
						types,
					);
					match result {
						Ok(result) => {
							if let Some(reflects_dependency) = reflects_dependency {
								type_arguments.set_id(
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
			let specialized_thrown = specialize(thrown, type_arguments, environment, types);

			target.get_top_level_facts(environment).throw_value(specialized_thrown);

			if specialized_thrown != TypeId::ERROR_TYPE {
				return None;
			}
		}
		// TODO extract
		Event::Conditionally { condition, events_if_truthy, else_events } => {
			let condition = specialize(condition, type_arguments, environment, types);

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
		Event::Return { returned } => {
			let specialized_returned = specialize(returned, type_arguments, environment, types);

			if specialized_returned != TypeId::ERROR_TYPE {
				return Some(specialized_returned);
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
					let prototype = specialize(prototype, type_arguments, environment, types);
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
					types.register_type(crate::Type::Function(id, this_value.clone()))
				}
			};

			// let new_object_id_with_curried_arguments =  new_object_id;
			// TODO conditionally
			let new_object_id_with_curried_arguments =
				curry_arguments(type_arguments, types, new_object_id);

			type_arguments.set_id(
				referenced_in_scope_as,
				new_object_id_with_curried_arguments,
				types,
			);
		}
		Event::Repeatedly { n, with } => {
			todo!()
		}
	}
	None
}
