//! Side effects which are not described in return types... Such as variable reassignment, function calling etc
//!
//! Events is the general name for the IR. Effect = Events of a function

use crate::{context::VariableId, types::TypeStore};

mod function_calling;
pub use function_calling::*;

use crate::{
	structures::functions::SynthesizedArgument,
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

	/// All changes to the value of a property
	Setter {
		on: TypeId,
		under: TypeId,
		new: TypeId,
		reflects_dependency: Option<TypeId>,
		/// THIS DOES NOT CALL SETTERS, JUST SETS VALUE!
		/// TODO this is [define] property
		/// see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes/Public_class_fields
		initialization: bool,
	},

	// TODO
	Getter {
		on: TypeId,
		under: TypeId,
		reflects_dependency: Option<TypeId>,
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
		on: TypeId,
		true_res: Box<[Event]>,
		false_res: Box<[Event]>,
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
	environment: &mut Environment,
	this_argument: Option<TypeId>,
	type_arguments: &mut TypeArguments,
	types: &mut TypeStore,
) -> EarlyReturn {
	match event {
		Event::ReadsReference { reference: variable, reflects_dependency } => {
			if let Some(id) = reflects_dependency {
				// TODO checking constraints if inferred
				let value = match variable {
					RootReference::VariableId(variable) => {
						environment.get_value_of_variable(variable)
					}
					RootReference::This => {
						this_argument.unwrap_or_else(|| environment.get_value_of_this(types))
					}
				};

				type_arguments.set_id(id, value, types);
			}
		}
		Event::SetsVariable(variable, value) => {
			let new_value = specialize(value, type_arguments, environment, types);

			// TODO re-push event
			environment.context_type.events.push(Event::SetsVariable(variable.clone(), new_value));
			environment.variable_current_value.insert(variable, new_value);
		}
		Event::Setter { on, under, new, reflects_dependency, initialization } => {
			let on = specialize(on, type_arguments, environment, types);
			let under = specialize(under, type_arguments, environment, types);

			let new = specialize(new, type_arguments, environment, types);
			// crate::utils::notify!(
			// 	"[Event::Setter] {}[{}] = {}",
			// 	environment.debug_type(under),
			// 	environment.debug_type(on),
			// 	environment.debug_type(new)
			// );

			if initialization {
				environment.properties.entry(on).or_default().push((under, new));
				environment.context_type.events.push(Event::Setter {
					on,
					new,
					under,
					reflects_dependency: None,
					initialization: true,
				});
			} else {
				let returned = environment.set_property(on, under, new, types).unwrap();

				if let Some(id) = reflects_dependency {
					type_arguments.set_id(id, returned.unwrap_or(TypeId::UNDEFINED_TYPE), types);
				}
			}
		}
		Event::Getter { on, under: property, reflects_dependency } => {
			if let Some(id) = reflects_dependency {
				let on = specialize(on, type_arguments, environment, types);
				let property = specialize(property, type_arguments, environment, types);

				let value = environment
					.get_property(on, property, types, None)
					.expect("Inferred constraints and checking failed");

				type_arguments.set_id(id, value.into(), types);
			}
		}
		Event::CallsType { on, with, reflects_dependency, timing, called_with_new } => {
			let on = specialize(on, type_arguments, environment, types);

			let with = with
				.into_iter()
				.map(|argument| match argument {
					SynthesizedArgument::NonSpread { ty, position: pos } => {
						SynthesizedArgument::NonSpread {
							ty: specialize(*ty, type_arguments, environment, types),
							position: pos.clone(),
						}
					}
				})
				.collect::<Vec<_>>();

			match timing {
				CallingTiming::Synchronous => {
					let result = crate::types::calling::call_type(
						on,
						with,
						None,
						None,
						environment,
						types,
						called_with_new,
					)
					.expect("Inference and/or checking failed");

					if let Some(reflects_dependency) = reflects_dependency {
						type_arguments.set_id(reflects_dependency, result.returned_type, types);
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
			environment.throw_value(specialized_thrown);

			if specialized_thrown != TypeId::ERROR_TYPE {
				return None;
			}
		}
		// TODO extract
		Event::Conditionally { on, true_res, false_res } => {
			let on = specialize(on, type_arguments, environment, types);

			// TODO cast to boolean
			if on == TypeId::TRUE {
				for event in true_res.iter().cloned() {
					apply_event(event, environment, this_argument, type_arguments, types);
				}
			} else if on == TypeId::FALSE {
				for event in false_res.iter().cloned() {
					apply_event(event, environment, this_argument, type_arguments, types);
				}
			} else {
				crate::utils::notify!("TODO merge events, skipping here");
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

			crate::utils::notify!(
				"Event::CreateObject: Check whether creating a function for closed over variables"
			);

			let new_id = environment.new_object(types, prototype);
			type_arguments.set_id(referenced_in_scope_as, new_id, types);
		}
		Event::Repeatedly { n, with } => {
			todo!()
		}
	}
	None
}
