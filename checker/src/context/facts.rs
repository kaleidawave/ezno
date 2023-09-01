use std::collections::HashMap;

use crate::{events::Event, Property, Type, TypeId, VariableId};

/// Things that are currently true or have happened
#[derive(Debug, Default)]
pub struct Facts {
	pub(crate) events: Vec<Event>,
	/// TODO think about tasks. These are things that may happen at next stop point
	pub(crate) queued_events: Vec<Event>,

	/// This can be not have a value if not defined
	pub(crate) variable_current_value: HashMap<VariableId, TypeId>,
	pub(crate) current_properties: HashMap<TypeId, Vec<(TypeId, Property)>>,
	pub(crate) prototypes: HashMap<TypeId, TypeId>,

	pub(crate) configurable: HashMap<(TypeId, TypeId), TypeId>,
	pub(crate) enumerable: HashMap<(TypeId, TypeId), TypeId>,
	pub(crate) writable: HashMap<(TypeId, TypeId), TypeId>,
	pub(crate) frozen: HashMap<TypeId, TypeId>,
}

impl Facts {
	/// TODO temp
	pub fn register_property(
		&mut self,
		on: TypeId,
		under: TypeId,
		to: Property,
		register_setter_event: bool,
	) {
		// crate::utils::notify!("Registering {:?} {:?} {:?}", on, under, to);
		self.current_properties.entry(on).or_default().push((under, to.clone()));
		if register_setter_event {
			self.events.push(Event::Setter {
				on,
				under,
				new: to,
				reflects_dependency: None,
				initialization: true,
			});
		}
	}

	pub(crate) fn throw_value(&mut self, value: TypeId) {
		self.events.push(Event::Throw(value));
	}

	pub(crate) fn new_object(
		&mut self,
		prototype: Option<TypeId>,
		types: &mut crate::types::TypeStore,
		is_under_dyn: bool,
	) -> TypeId {
		let ty = types.register_type(Type::Object(crate::types::ObjectNature::RealDeal));

		if let Some(prototype) = prototype {
			self.prototypes.insert(ty, prototype);
		}

		if is_under_dyn {
			// TODO maybe register the environment if function ...
			// TODO register properties
			let value = Event::CreateObject { referenced_in_scope_as: ty, prototype };
			self.events.push(value);
		}

		ty
	}
}
