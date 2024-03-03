//! Side effects which are not described in return types... Such as variable reassignment, function calling etc
//!
//! Events is the general name for the IR. Effect = Events of a function

use crate::{
	context::{get_on_ctx, information::Publicity},
	features::iteration::IterationKind,
	types::{
		calling::CalledWithNew,
		properties::{PropertyKey, PropertyValue},
		store::TypeStore,
	},
	FunctionId, GeneralContext, SpanWithSource, VariableId,
};

pub(crate) mod application;
pub(crate) use application::apply_event;

use crate::{types::functions::SynthesisedArgument, types::TypeId};

#[derive(Debug, Clone, PartialEq, Eq, Hash, binary_serialize_derive::BinarySerializable)]
pub enum RootReference {
	Variable(VariableId),
	This,
}

impl RootReference {
	#[must_use]
	pub fn get_name<'a>(&self, ctx: &'a GeneralContext) -> &'a str {
		match self {
			Self::Variable(id) => get_on_ctx!(ctx.get_variable_name(*id)),
			Self::This => "this",
		}
	}
}

/// For iterations. TODO up for debate
pub type InitialVariables = map_vec::Map<VariableId, TypeId>;

/// Events which happen
///
/// Used for getting values and states
///
/// `reflects_dependency` means the result goes into the type argument map. This corresponds to the
/// type id (of constructor) it goes under
#[derive(Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub enum Event {
	/// Reads a reference (as a free variable or `this`)
	///
	/// Can be used for DCE reasons, or finding variables in context
	ReadsReference {
		reference: RootReference,
		reflects_dependency: Option<TypeId>,
		position: SpanWithSource,
	},
	/// Also used for DCE
	SetsVariable(VariableId, TypeId, SpanWithSource),
	/// Mostly trivial, sometimes can call a function :(
	Getter {
		on: TypeId,
		under: PropertyKey<'static>,
		reflects_dependency: Option<TypeId>,
		publicity: Publicity,
		position: SpanWithSource,
	},
	/// All changes to the value of a property
	Setter {
		on: TypeId,
		under: PropertyKey<'static>,
		// Can be a getter through define property
		new: PropertyValue,
		/// THIS DOES NOT CALL SETTERS, JUST SETS VALUE!
		/// TODO this is [define] property
		/// see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes/Public_class_fields
		initialization: bool,
		publicity: Publicity,
		position: Option<SpanWithSource>,
	},
	/// This includes closed over variables, anything dependent
	CallsType {
		on: TypeId,
		with: Box<[SynthesisedArgument]>,
		reflects_dependency: Option<TypeId>,
		timing: CallingTiming,
		called_with_new: CalledWithNew,
		position: SpanWithSource,
	},
	/// Run events conditionally
	Conditionally {
		condition: TypeId,
		true_events: Box<[Event]>,
		else_events: Box<[Event]>,
		position: Option<SpanWithSource>,
	},
	/// Run events multiple times
	Iterate {
		kind: IterationKind,
		/// Contains initial values that the iteration runs over. Without, initial iterations can't access anything...?
		initial: InitialVariables,
		/// TODO for of and in variants here:
		// condition: TypeId,
		iterate_over: Box<[Event]>,
	},

	/// *lil bit magic*, handles:
	/// - Creating objects `{}`
	/// - Creating objects with prototypes:
	///     - Arrays
	///     - Map & Sets
	///     - HTMLElement and derivatives
	///
	/// ```typescript
	/// function x() {
	///     return {}
	/// }
	/// ```
	///
	/// TODO this is only need if exposed by return typed. It may get added but needs some way of ignoring it if
	/// not in externally referenced set or something
	CreateObject {
		prototype: PrototypeArgument,
		/// This is the id referencing a [Type::AliasTo] that is created
		///
		/// This is also for the specialisation (somehow)
		referenced_in_scope_as: TypeId,
		position: Option<SpanWithSource>,
		/// Debug only
		is_function_this: bool,
	},
	FinalEvent(FinalEvent),
}

impl From<FinalEvent> for Event {
	fn from(value: FinalEvent) -> Self {
		Event::FinalEvent(value)
	}
}

/// Nothing runs after this event
#[derive(Debug, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum FinalEvent {
	Return {
		returned: TypeId,
		returned_position: SpanWithSource,
	},
	/// From a `throw ***` statement (or expression)
	Throw {
		thrown: TypeId,
		position: SpanWithSource,
	},
	Break {
		carry: u8,
		position: Option<SpanWithSource>,
	},
	/// TODO explain why this can't be done with just (or at least label makes it more difficult)
	Continue {
		carry: u8,
		position: Option<SpanWithSource>,
	},
}

impl FinalEvent {
	#[must_use]
	pub fn returns(self) -> Option<TypeId> {
		match self {
			FinalEvent::Return { returned, .. } => Some(returned),
			FinalEvent::Throw { .. } => Some(TypeId::NEVER_TYPE),
			FinalEvent::Break { .. } | FinalEvent::Continue { .. } => None,
		}
	}

	#[must_use]
	pub fn throws(self) -> Option<TypeId> {
		match self {
			FinalEvent::Throw { thrown, .. } => Some(thrown),
			FinalEvent::Return { .. } | FinalEvent::Break { .. } | FinalEvent::Continue { .. } => {
				None
			}
		}
	}
}

/// TODO WIP
#[derive(Debug)]
pub enum ApplicationResult {
	Completed,
	Interrupt(FinalEvent),
	/// This is a blend
	Conditionally {
		on: TypeId,
		truthy: Box<ApplicationResult>,
		otherwise: Box<ApplicationResult>,
	},
}

impl From<FinalEvent> for ApplicationResult {
	fn from(value: FinalEvent) -> Self {
		ApplicationResult::Interrupt(value)
	}
}

impl ApplicationResult {
	pub(crate) fn is_it_so_over(&self) -> bool {
		match self {
			ApplicationResult::Completed => false,
			ApplicationResult::Interrupt(_) => true,
			ApplicationResult::Conditionally { on: _, truthy, otherwise } => {
				truthy.is_it_so_over() && otherwise.is_it_so_over()
			}
		}
	}

	pub(crate) fn append_termination(&mut self, result: impl Into<ApplicationResult>) {
		match self {
			ApplicationResult::Completed => *self = result.into(),
			ApplicationResult::Interrupt(_) => {
				crate::utils::notify!("Should be unreachable, result already failed");
			}
			ApplicationResult::Conditionally { on: _, truthy, otherwise } => {
				if truthy.is_it_so_over() {
					otherwise.append_termination(result);
				} else {
					truthy.append_termination(result);
				}
			}
		}
	}

	pub(crate) fn new_from_unknown_condition(
		on: TypeId,
		truthy: ApplicationResult,
		otherwise: ApplicationResult,
		// types: &mut TypeStore,
	) -> Self {
		match (truthy, otherwise) {
			(ApplicationResult::Completed, ApplicationResult::Completed) => {
				ApplicationResult::Completed
			}
			// (ApplicationResult::Interrupt(FinalEvent::Return { returned, returned_position }), ApplicationResult::Interrupt(FinalEvent::Return { returned, returned_position }))
			// => ApplicationResult::Completed,
			(truthy, otherwise) => ApplicationResult::Conditionally {
				on,
				truthy: Box::new(truthy),
				otherwise: Box::new(otherwise),
			},
		}
	}

	pub(crate) fn returned_type(self, types: &mut TypeStore) -> TypeId {
		match self {
			ApplicationResult::Completed => TypeId::UNDEFINED_TYPE,
			ApplicationResult::Interrupt(int) => int.returns().expect("returning continue ??"),
			ApplicationResult::Conditionally { on, truthy, otherwise } => {
				let truthy_result = truthy.returned_type(types);
				let otherwise_result = otherwise.returned_type(types);
				types.new_conditional_type(on, truthy_result, otherwise_result)
			}
		}
	}

	// TODO Option?
	pub(crate) fn throw_type(&self, types: &mut TypeStore) -> TypeId {
		match self {
			ApplicationResult::Completed => TypeId::NEVER_TYPE,
			ApplicationResult::Interrupt(int) => int.throws().expect("no throw?"),
			ApplicationResult::Conditionally { on, truthy, otherwise } => {
				let truthy_result = truthy.throw_type(types);
				let otherwise_result = otherwise.throw_type(types);
				types.new_conditional_type(*on, truthy_result, otherwise_result)
			}
		}
	}
}

impl From<Option<FinalEvent>> for ApplicationResult {
	fn from(value: Option<FinalEvent>) -> Self {
		value.map_or(Self::Completed, Self::Interrupt)
	}
}

#[derive(Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub enum PrototypeArgument {
	Yeah(TypeId),
	None,
	Function(FunctionId),
}

#[derive(Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub enum CallingTiming {
	Synchronous,
	QueueTask,
	/// TODO could use above mechanism at some point
	AtSomePointManyTimes,
}
