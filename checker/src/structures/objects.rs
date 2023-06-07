use crate::{context::Environment, events::Event, types::TypeStore, TypeId};

// TODO slice indexes
pub struct ObjectBuilder {
	object: TypeId,
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

	pub fn append(&mut self, environment: &mut Environment, under: TypeId, value: TypeId) {
		// TODO combine
		environment.properties.entry(self.object).or_default().push((under, value));

		environment.context_type.events.push(Event::Setter {
			on: self.object,
			new: value,
			under,
			reflects_dependency: None,
			initialization: true,
		});
	}

	pub fn build_object(self) -> TypeId {
		self.object
	}
}
