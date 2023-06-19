use crate::{
	context::Environment,
	types::{properties::Property, TypeStore},
	TypeId,
};

// TODO slice indexes
pub struct ObjectBuilder {
	pub(crate) object: TypeId,
}

impl ObjectBuilder {
	pub fn new(
		prototype: Option<TypeId>,
		types: &mut TypeStore,
		environment: &mut Environment,
	) -> Self {
		let object = environment.new_object(types, prototype);
		Self { object }
	}

	pub fn append(&mut self, environment: &mut Environment, under: TypeId, value: Property) {
		environment.register_property(self.object, under, value)
	}

	pub fn build_object(self) -> TypeId {
		self.object
	}
}
