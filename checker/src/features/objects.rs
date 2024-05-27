use source_map::SpanWithSource;

use crate::{
	context::Environment,
	types::{
		properties::{PropertyKey, PropertyValue, Publicity},
		TypeStore,
	},
	FunctionId, LocalInformation, TypeId,
};

use super::functions::ThisValue;

/// Helper for building objects easy
// TODO slice indexes
pub struct ObjectBuilder {
	pub object: TypeId,
}

impl ObjectBuilder {
	pub fn new(
		prototype: Option<TypeId>,
		types: &mut TypeStore,
		info: &mut LocalInformation,
	) -> Self {
		// TODO is_under_dyn bad
		let is_under_dyn = true;
		Self { object: info.new_object(prototype, types, is_under_dyn) }
	}

	pub fn append(
		&mut self,
		environment: &mut Environment,
		publicity: Publicity,
		under: PropertyKey<'static>,
		value: PropertyValue,
		position: Option<SpanWithSource>,
	) {
		environment.info.register_property(self.object, publicity, under, value, true, position);
	}

	#[must_use]
	pub fn build_object(self) -> TypeId {
		self.object
	}
}

/// These are objects (`typeof * = "object"`) but have special behavior
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum SpecialObjects {
	/// Hold state of the runtime
	Promise { events: () },
	/// Hold state of the runtime
	Generator { position: () },
	/// Needs overrides for calling, getting etc
	Proxy(Proxy),
	/// Not a [Constant] as `typeof /hi/ === "object"` and it has state
	RegularExpression(String),
	/// This cannot be a regular object because of is because of let mutations
	Import(super::modules::Exported),
	/// Yeah here
	Function(FunctionId, ThisValue),
	/// Mainly for printing
	ClassConstructor {
		name: String,
		constructor: FunctionId,
		/// For `instanceof` thing
		prototype: TypeId,
	},
}

/// Properties of handler called (`over` passed as first argument)
#[derive(Copy, Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct Proxy {
	pub over: TypeId,
	pub handler: TypeId,
}
