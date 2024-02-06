//! Side effects which are not described in return types... Such as variable reassignment, function calling etc
//!
//! Events is the general name for the IR. Effect = Events of a function

use crate::{
	context::{get_on_ctx, information::Publicity},
	features::iteration::IterationKind,
	types::{
		calling::CalledWithNew,
		properties::{PropertyKey, PropertyValue},
	},
	FunctionId, GeneralContext, SpanWithSource, VariableId,
};

pub(crate) mod application;
pub(crate) mod helpers;
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
#[derive(Debug, Clone, binary_serialize_derive::BinarySerializable)]
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
