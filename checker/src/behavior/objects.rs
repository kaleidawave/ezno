use crate::{
	context::{
		facts::{Facts, PublicityKind},
		Environment,
	},
	types::{properties::Property, TypeStore},
	TypeId,
};

// TODO slice indexes
pub struct ObjectBuilder {
	pub(crate) object: TypeId,
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
		under: TypeId,
		value: Property,
		property: PublicityKind,
	) {
		environment.facts.register_property(self.object, under, value, true, property)
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
		handler: (),
		over: (),
	},
	/// This cannot be a regular object because of is because of let mutations
	Import(super::modules::Exported),
}
