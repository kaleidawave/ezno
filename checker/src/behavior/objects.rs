use crate::{
	context::{facts::Facts, Environment},
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

	pub fn append(&mut self, environment: &mut Environment, under: TypeId, value: Property) {
		environment.facts.register_property(self.object, under, value, true)
	}

	pub fn build_object(self) -> TypeId {
		self.object
	}
}

pub enum SpecialObjects {
	Promise { events: () },
	Generator { position: () },
	Proxy { handler: (), over: () },
}
