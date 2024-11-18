//! Side effects which are not described in return types... Such as variable reassignment, function calling etc
//!
//! Events is the general name for the IR (intermediate representation) of impure operations. Effect = Events of a function

pub(crate) mod application;
pub mod printing;

use crate::{
	context::get_on_ctx,
	features::{functions::ClosedOverVariables, iteration::IterationKind},
	types::{
		calling::{Callable, CalledWithNew, SynthesisedArgument},
		properties::{AccessMode, PropertyKey, PropertyValue, Publicity},
		TypeId,
	},
	FunctionId, GeneralContext, SpanWithSource, VariableId,
};

pub(crate) use application::apply_events;

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
pub type InitialVariables = crate::Map<VariableId, TypeId>;

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
		mode: AccessMode,
	},
	/// All assignments to the value of a property. TODO explain others for defining getters etc
	Setter {
		on: TypeId,
		under: PropertyKey<'static>,
		new: TypeId,
		publicity: Publicity,
		position: SpanWithSource,
	},
	/// This includes closed over variables, anything dependent
	CallsType {
		on: Callable,
		with: Box<[SynthesisedArgument]>,
		reflects_dependency: Option<TypeId>,
		timing: CallingTiming,
		called_with_new: CalledWithNew,
		possibly_thrown: Option<TypeId>,
		call_site: SpanWithSource,
	},
	/// Run events conditionally
	Conditionally {
		condition: TypeId,
		truthy_events: u32,
		otherwise_events: u32,
		position: SpanWithSource,
	},
	/// Run events multiple times
	Iterate {
		kind: IterationKind,
		/// Contains initial values that the iteration runs over. Without, initial iterations can't access anything...?
		initial: ClosedOverVariables,
		/// TODO for of and in variants here:
		// condition: TypeId,
		iterate_over: u32,
	},
	/// *lil bit magic*, handles:
	/// - Creating objects `{}`
	/// - Creating objects with prototypes:
	///     - Arrays
	///     - Map & Sets
	///     - `HTMLElement` and derivatives
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
		/// This is the id referencing a [`Type::AliasTo`] that is created
		///
		/// This is also for the specialisation (somehow)
		referenced_in_scope_as: TypeId,
		position: SpanWithSource,
	},
	/// aka try-catch-finally
	ExceptionTrap {
		/// events in the try block
		investigate: u32,
		/// This can run subtyping
		trapped_type_id: Option<Trapped>,
		/// events in catch block
		handle: u32,
		/// run `Immediately before a control-flow statement (return, throw, break, continue) is executed in the try block or catch block`
		finally: u32,
	},
	// TODO block trap...?
	FinalEvent(FinalEvent),

	/// **doesn't affect type checking**
	/// useful for linting WIP
	RegisterVariable {
		name: String,
		position: SpanWithSource,
		/// `None` for `let x;`
		initial_value: Option<TypeId>,
	},
	Miscellaneous(MiscellaneousEvents),
	/// TODO was trying to avoid
	EndOfControlFlow(u32),
}

#[derive(Debug, Copy, Clone, binary_serialize_derive::BinarySerializable)]
pub struct Trapped {
	pub generic_type: TypeId,
	/// To check thrown type against
	pub constrained: Option<TypeId>,
}

impl From<FinalEvent> for Event {
	fn from(value: FinalEvent) -> Self {
		Event::FinalEvent(value)
	}
}

/// Some of these are [`crate::features::objects::Proxy`] traps
#[derive(Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub enum MiscellaneousEvents {
	/// Also for <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy/Proxy/has>
	Has { on: TypeId, publicity: Publicity, under: PropertyKey<'static>, into: TypeId },
	/// Very similar to [`MiscellaneousEvents::Has`] but deletes the properties
	/// Also for <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy/Proxy/deleteProperty>
	Delete {
		on: TypeId,
		publicity: Publicity,
		under: PropertyKey<'static>,
		into: Option<TypeId>,
		position: SpanWithSource,
	},
	/// :/
	RegisterProperty {
		on: TypeId,
		publicity: Publicity,
		under: PropertyKey<'static>,
		value: PropertyValue,
		position: SpanWithSource,
	},
	/// Creates a new function or class
	CreateConstructor { referenced_in_scope_as: TypeId, function: FunctionId },
	/// While [`Event::CallsType`] marks callbacks (and parameters) as being called, it does not mark regular function as being called
	/// this event has no effect but does add it to the global `HashSet` of called types
	MarkFunctionAsCalled(FunctionId),
}

/// A break in application
#[derive(Debug, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum FinalEvent {
	Return {
		returned: TypeId,
		position: SpanWithSource,
	},
	/// From a `throw ***` statement (or expression)
	Throw {
		thrown: TypeId,
		position: SpanWithSource,
	},
	Break {
		carry: u8,
		position: SpanWithSource,
	},
	/// TODO explain why this can't be done with just (or at least label makes it more difficult)
	Continue {
		carry: u8,
		position: SpanWithSource,
	},
	// Yield {
	// 	value: TypeId,
	// 	returns: TypeId,
	// 	position: SpanWithSource,
	// }
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

/// This doesn't cover unconditional thrown from internal functions
///
/// Similar to [`FinalEvent`] but includes different information + or
/// `break` and `continue` don't apply for function returns (but do for iteration and conditionals)
#[derive(Debug)]
pub enum ApplicationResult {
	Return {
		returned: TypeId,
		position: SpanWithSource,
	},
	/// From a `throw ***` statement (or expression).
	Throw {
		thrown: TypeId,
		position: SpanWithSource,
	},
	/// TODO state
	Yield {},
	Break {
		carry: u8,
		position: SpanWithSource,
	},
	/// TODO explain why this can't be done with just (or at least label makes it more difficult)
	Continue {
		carry: u8,
		position: SpanWithSource,
	},
	/// One of these is `Some`.
	Or {
		on: TypeId,
		truthy_result: Box<ApplicationResult>,
		otherwise_result: Box<ApplicationResult>,
	},
}
