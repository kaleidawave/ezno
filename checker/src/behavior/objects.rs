use source_map::SpanWithSource;

use crate::{
	context::{
		facts::{Facts, PublicityKind},
		Environment,
	},
	types::{
		properties::{PropertyKey, PropertyValue},
		TypeStore,
	},
	TypeId,
};

// TODO slice indexes
pub struct ObjectBuilder {
	pub object: TypeId,
}

impl ObjectBuilder {
	pub fn new(prototype: Option<TypeId>, types: &mut TypeStore, facts: &mut Facts) -> Self {
		// TODO is_under_dyn bad
		let is_under_dyn = true;
		Self { object: facts.new_object(prototype, types, is_under_dyn) }
	}

	pub fn append(
		&mut self,
		environment: &mut Environment,
		publicity: PublicityKind,
		under: PropertyKey<'static>,
		value: PropertyValue,
		position: Option<SpanWithSource>,
	) {
		environment.facts.register_property(self.object, publicity, under, value, true, position)
	}

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
	/// This cannot be a regular object because of is because of let mutations
	Import(super::modules::Exported),
}
