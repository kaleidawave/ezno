use source_map::SpanWithSource;

use crate::{
	context::LocalInformation,
	types::{
		calling::ThisValue,
		properties::{PropertyKey, PropertyValue, Publicity},
		TypeStore,
	},
	FunctionId, TypeId,
};

/// Helper for building objects easy
// TODO slice indexes
pub struct ObjectBuilder {
	pub object: TypeId,
}

impl ObjectBuilder {
	pub fn new(
		prototype: Option<TypeId>,
		types: &mut TypeStore,
		position: SpanWithSource,
		info: &mut LocalInformation,
	) -> Self {
		// TODO is_under_dyn bad
		let is_under_dyn = true;
		Self { object: info.new_object(prototype, types, position, is_under_dyn) }
	}

	pub fn append(
		&mut self,
		publicity: Publicity,
		under: PropertyKey<'static>,
		value: PropertyValue,
		position: SpanWithSource,
		info: &mut LocalInformation,
	) {
		info.register_property(self.object, publicity, under, value, position);
	}

	#[must_use]
	pub fn build_object(self) -> TypeId {
		self.object
	}
}

/// These are objects (`typeof * = "object"`) but have special behavior
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum SpecialObject {
	/// Hold state of the runtime
	Promise {
		from: FunctionId,
		position: TypeId,
	},
	/// Hold state of the runtime
	Generator {
		from: FunctionId,
		position: TypeId,
	},
	/// Needs overrides for calling, getting etc
	Proxy(Proxy),
	/// Not a [Constant] as `typeof /hi/ === "object"` and it has state
	RegularExpression {
		content: TypeId,
		// groups: Option<TypeId>,
	},
	/// This cannot be a regular object because of is because of let mutations
	Import(super::modules::Exported),
	/// Yeah here. Also for classes
	/// TODO not all functions have `ThisValue`
	Function(FunctionId, ThisValue),
	Null,
}

/// Properties of handler called (`over` passed as first argument)
///
/// Has traps for `getPrototypeOf()`, `setPrototypeOf()`, `isExtensible()`,
/// `preventExtensions()`, `getOwnPropertyDescriptor()`, `defineProperty()`, `has()`,
/// `get()`, `set()`, `deleteProperty()`, `ownKeys()` and function methods
/// `apply()` and `construct()`
#[derive(Copy, Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct Proxy {
	pub over: TypeId,
	pub handler: TypeId,
}
