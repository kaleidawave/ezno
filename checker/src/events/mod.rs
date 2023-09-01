//! Side effects which are not described in return types... Such as variable reassignment, function calling etc
//!
//! Events is the general name for the IR. Effect = Events of a function

use crate::{
	context::{calling::Target, get_value_of_variable, CallCheckingBehavior},
	types::{
		is_type_truthy_falsy,
		printing::print_type,
		properties::{get_property, set_property, Property},
		TypeStore,
	},
	TruthyFalsy, VariableId,
};

mod function_calling;
pub(crate) mod helpers;
pub use function_calling::*;

use crate::{
	types::functions::SynthesizedArgument,
	types::{poly_types::TypeArguments, specialize, TypeId},
	Environment,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, binary_serialize_derive::BinarySerializable)]
pub enum RootReference {
	VariableId(VariableId),
	This,
}

impl RootReference {
	pub fn get_name(self, environment: &Environment) -> String {
		match self {
			Self::VariableId(id) => environment.get_variable_name(&id),
			Self::This => "this".to_owned(),
		}
	}
}

/// Events which happen
///
/// Used for getting values and states
///
/// `reflects_dependency` means the result goes into the type argument map. This corresponds to the
/// type id (of constructor) it goes under
///
/// TODO store positions?
#[derive(Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub enum Event {
	/// Reads variable
	///
	/// Can be used for DCE reasons, or finding variables in context
	ReadsReference {
		reference: RootReference,
		reflects_dependency: Option<TypeId>,
	},
	/// Also used for DCE
	SetsVariable(VariableId, TypeId),

	/// Mostly trivial, sometimes can call a function :(
	Getter {
		on: TypeId,
		under: TypeId,
		reflects_dependency: Option<TypeId>,
	},

	/// All changes to the value of a property
	Setter {
		on: TypeId,
		under: TypeId,
		// Can be a getter through define property
		new: Property,
		reflects_dependency: Option<TypeId>,
		/// THIS DOES NOT CALL SETTERS, JUST SETS VALUE!
		/// TODO this is [define] property
		/// see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes/Public_class_fields
		initialization: bool,
	},

	/// This includes closed over variables, anything dependent
	CallsType {
		on: TypeId,
		with: Box<[SynthesizedArgument]>,
		reflects_dependency: Option<TypeId>,
		timing: CallingTiming,
		called_with_new: CalledWithNew,
	},

	/// From a `throw ***` statement (or expression)
	Throw(TypeId),

	Conditionally {
		condition: TypeId,
		events_if_truthy: Box<[Event]>,
		else_events: Box<[Event]>,
	},

	Repeatedly {
		n: TypeId,
		with: Box<[Event]>,
	},

	/// TODO not sure but whatever
	Return {
		returned: TypeId,
	},

	/// *lil bit magic*, handles:
	/// - Creating objects `{}`
	/// - Creating objects with prototypes:
	/// 	- Arrays
	/// 	- Map & Sets
	/// 	- HTMLElement and derivatives
	///
	/// ```typescript
	/// function x() {
	/// 	return {}
	/// }
	/// ```
	///
	/// TODO this is only need if exposed by return typed. It may get added but needs some way of ignoring it if
	/// not in externally referenced set or something
	CreateObject {
		prototype: Option<TypeId>,
		/// This is the id referencing a [Type::AliasTo] that is created
		///
		/// This is also for the specialization (somehow)
		referenced_in_scope_as: TypeId,
	},
	CreatesClosure {
		id: TypeId,
	},
	// Registration(Registration),
}

#[derive(Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub enum CallingTiming {
	Synchronous,
	QueueTask,
	/// TODO could use above mechanism at some point
	AtSomePointManyTimes,
}

pub(crate) type EarlyReturn = Option<TypeId>;

pub(crate) fn apply_event(
	event: Event,
	this_argument: Option<TypeId>,
	type_arguments: &mut TypeArguments,
	environment: &mut Environment,
	target: &mut Target,
	types: &mut TypeStore,
) -> EarlyReturn {
	match event {
		Event::ReadsReference { reference, reflects_dependency } => {
			if let Some(id) = reflects_dependency {
				let value = match reference {
					RootReference::VariableId(id) => {
						get_value_of_variable(environment.facts_chain(), id)
							.expect("variable has no value")
					}
					RootReference::This => {
						this_argument.unwrap_or_else(|| environment.get_value_of_this(types))
					}
				};
				type_arguments.set_id(id, value, types);
			}
		}
		Event::SetsVariable(variable, value) => {
			let new_value =
				specialize(value, type_arguments, &environment.into_general_context(), types);

			{
				let facts = target.get_top_level_facts(environment);
				facts.events.push(Event::SetsVariable(variable.clone(), new_value));
				facts.variable_current_value.insert(variable, new_value);
			}
		}
		Event::Getter { on, under, reflects_dependency } => {
			let gc = environment.into_general_context();

			let on = specialize(on, type_arguments, &gc, types);
			let property = specialize(under, type_arguments, &gc, types);

			let value = get_property(on, under, None, environment, target, types)
				.expect("Inferred constraints and checking failed");

			if let Some(id) = reflects_dependency {
				type_arguments.set_id(id, value.into(), types);
			}
		}
		Event::Setter { on, under, new, reflects_dependency, initialization } => {
			let gc = environment.into_general_context();
			let on = specialize(on, type_arguments, &gc, types);
			let under = specialize(under, type_arguments, &gc, types);

			let new = match new {
				Property::Value(new) => {
					Property::Value(specialize(new, type_arguments, &gc, types))
				}
				// For declare property
				Property::Getter(_) => todo!(),
				Property::Setter(_) => todo!(),
				Property::GetterAndSetter(_, _) => todo!(),
			};

			let gc = environment.into_general_context();

			crate::utils::notify!(
				"[Event::Setter] {}[{}] = {}",
				print_type(on, types, &gc, true),
				print_type(under, types, &gc, true),
				if let Property::Value(new) = new {
					print_type(new, types, &gc, true)
				} else {
					format!("{:?}", new)
				}
			);

			if initialization {
				target.get_top_level_facts(environment).register_property(on, under, new, true);
			} else {
				let returned = set_property(on, under, new, environment, target, types).unwrap();

				if let Some(id) = reflects_dependency {
					type_arguments.set_id(id, returned.unwrap_or(TypeId::UNDEFINED_TYPE), types);
				}
			}
		}
		Event::CallsType { on, with, reflects_dependency, timing, called_with_new } => {
			let on = specialize(on, type_arguments, &environment.into_general_context(), types);

			let with = with
				.into_iter()
				.map(|argument| match argument {
					SynthesizedArgument::NonSpread { ty, position: pos } => {
						SynthesizedArgument::NonSpread {
							ty: specialize(
								*ty,
								type_arguments,
								&environment.into_general_context(),
								types,
							),
							position: pos.clone(),
						}
					}
				})
				.collect::<Vec<_>>();

			match timing {
				CallingTiming::Synchronous => {
					let result = crate::types::calling::call_type(
						on,
						called_with_new,
						None,
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
			let specialized_thrown =
				specialize(thrown, type_arguments, &environment.into_general_context(), types);

			target.get_top_level_facts(environment).throw_value(specialized_thrown);

			if specialized_thrown != TypeId::ERROR_TYPE {
				return None;
			}
		}
		// TODO extract
		Event::Conditionally { condition, events_if_truthy, else_events } => {
			let condition =
				specialize(condition, type_arguments, &environment.into_general_context(), types);

			if let TruthyFalsy::Decidable(result) = is_type_truthy_falsy(condition, types) {
				let to_evaluate = if result { events_if_truthy } else { else_events };
				for event in to_evaluate.iter().cloned() {
					apply_event(event, this_argument, type_arguments, environment, target, types);
				}
			} else {
				// TODO early returns

				// TODO could inject proofs but probably already worked out
				let truthy_facts = target.new_conditional_target(|target: &mut Target| {
					for event in events_if_truthy.into_vec() {
						apply_event(
							event,
							this_argument,
							type_arguments,
							environment,
							target,
							types,
						);
					}
				});

				let mut else_facts = target.new_conditional_target(|target: &mut Target| {
					for event in else_events.into_vec() {
						apply_event(
							event,
							this_argument,
							type_arguments,
							environment,
							target,
							types,
						);
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
			let specialized_returned =
				specialize(returned, type_arguments, &environment.into_general_context(), types);

			if specialized_returned != TypeId::ERROR_TYPE {
				return Some(specialized_returned);
			} else {
				crate::utils::notify!("event returned error so skipped");
			}
		}
		Event::CreateObject { referenced_in_scope_as, prototype } => {
			// TODO only if exposed via set

			let prototype = prototype.map(|prototype| {
				specialize(prototype, type_arguments, &environment.into_general_context(), types)
			});

			crate::utils::notify!(
				"Event::CreateObject: Check whether creating a function for closed over variables"
			);

			// TODO
			let is_under_dyn = true;
			let new_id =
				target.get_top_level_facts(environment).new_object(prototype, types, is_under_dyn);
			type_arguments.set_id(referenced_in_scope_as, new_id, types);
		}
		Event::Repeatedly { n, with } => {
			todo!()
		}
		Event::CreatesClosure { id } => {}
	}
	None
}
