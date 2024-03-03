use source_map::SpanWithSource;

use crate::{
	context::{
		information::{LocalInformation, Publicity},
		Environment,
	},
	types::{
		properties::{PropertyKey, PropertyValue},
		TypeStore,
	},
	FunctionId, TypeId,
};

use super::functions::ThisValue;

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
		Self { object: info.new_object(prototype, types, is_under_dyn, false) }
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

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum SpecialObjects {
	Promise {
		events: (),
	},
	Generator {
		position: (),
	},
	Proxy {
		over: TypeId,
		handler: TypeId,
	},
	/// Not a [Constant] as `typeof /hi/ === "object"` and it has state
	Regexp(String),
	/// This cannot be a regular object because of is because of let mutations
	Import(super::modules::Exported),
	/// Yeah here
	Function(FunctionId, ThisValue),
}
