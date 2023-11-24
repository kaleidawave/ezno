//! Side effects which are not described in return types... Such as variable reassignment, function calling etc
//!
//! Events is the general name for the IR. Effect = Events of a function

use crate::{
	behavior::functions::ThisValue,
	context::{calling::Target, facts::Publicity, get_on_ctx, CallCheckingBehavior},
	types::{
		calling::CalledWithNew,
		properties::{PropertyKey, PropertyValue},
	},
	FunctionId, GeneralContext, SpanWithSource, VariableId,
};

pub(crate) mod application;
pub(crate) mod helpers;
pub(crate) use application::apply_event;
use source_map::Span;

use crate::{
	types::functions::SynthesisedArgument,
	types::{poly_types::FunctionTypeArguments, TypeId},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, binary_serialize_derive::BinarySerializable)]
pub enum RootReference {
	Variable(VariableId),
	This,
}

impl RootReference {
	pub fn get_name<'a>(&self, ctx: &'a GeneralContext) -> &'a str {
		match self {
			Self::Variable(id) => get_on_ctx!(ctx.get_variable_name(id)),
			Self::This => "this",
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
		reflects_dependency: Option<TypeId>,
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
	/// From a `throw ***` statement (or expression)
	Throw(TypeId, SpanWithSource),
	/// Run events conditionally
	Conditionally {
		condition: TypeId,
		events_if_truthy: Box<[Event]>,
		else_events: Box<[Event]>,
		position: Option<SpanWithSource>,
	},
	/// TODO not sure but whatever
	Return { returned: TypeId, returned_position: SpanWithSource },
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

pub(crate) type EarlyReturn = Option<TypeId>;
