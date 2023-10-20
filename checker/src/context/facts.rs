use std::collections::HashMap;

use crate::{
	behavior::functions::ClosureId,
	events::{Event, RootReference},
	Property, Type, TypeId, VariableId,
};

/// TODO explain usage
#[derive(Debug, Clone, Copy, PartialEq, Eq, binary_serialize_derive::BinarySerializable)]
pub enum PublicityKind {
	Private,
	Public,
}

/// Things that are currently true or have happened
#[derive(Debug, Default)]
pub struct Facts {
	pub(crate) events: Vec<Event>,
	/// TODO think about tasks. These are things that may happen at next stop point
	pub(crate) queued_events: Vec<Event>,

	/// This can be not have a value if not defined
	pub(crate) variable_current_value: HashMap<VariableId, TypeId>,
	pub(crate) current_properties: HashMap<TypeId, Vec<(TypeId, PublicityKind, Property)>>,
	pub(crate) prototypes: HashMap<TypeId, TypeId>,

	pub(crate) closure_current_values: HashMap<(ClosureId, RootReference), TypeId>,

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
		publicity: PublicityKind,
	) {
		// crate::utils::notify!("Registering {:?} {:?} {:?}", on, under, to);
		self.current_properties.entry(on).or_default().push((under, publicity, to.clone()));
		if register_setter_event {
			self.events.push(Event::Setter {
				on,
				under,
				new: to,
				reflects_dependency: None,
				initialization: true,
				publicity,
			});
		}
	}

	pub(crate) fn throw_value(&mut self, value: TypeId) {
		self.events.push(Event::Throw(value));
	}

	pub fn get_events(&self) -> &[Event] {
		&self.events
	}

	pub(crate) fn new_object(
		&mut self,
		prototype: Option<TypeId>,
		types: &mut crate::types::TypeStore,
		is_under_dyn: bool,
	) -> TypeId {
		let ty = types.register_type(Type::Object(crate::types::ObjectNature::RealDeal));
		// crate::utils::notify!("New object created under {:?}", ty);

		if let Some(prototype) = prototype {
			self.prototypes.insert(ty, prototype);
		}

		if is_under_dyn {
			let prototype = match prototype {
				Some(id) => crate::events::PrototypeArgument::Yeah(id),
				None => crate::events::PrototypeArgument::None,
			};
			// TODO maybe register the environment if function ...
			// TODO register properties
			let value = Event::CreateObject { referenced_in_scope_as: ty, prototype };
			self.events.push(value);
		}

		ty
	}

	pub fn get_properties_on_type(
		&self,
		ty: TypeId,
	) -> Option<&Vec<(TypeId, PublicityKind, Property)>> {
		self.current_properties.get(&ty)
	}

	pub(crate) fn extend(&mut self, other: Facts, condition: Option<TypeId>) {
		self.events.extend(other.events);
		self.queued_events.extend(other.queued_events);
		self.variable_current_value.extend(other.variable_current_value);
		self.current_properties.extend(other.current_properties);
		self.prototypes.extend(other.prototypes);
		self.closure_current_values.extend(other.closure_current_values);
		self.configurable.extend(other.configurable);
		self.enumerable.extend(other.enumerable);
		self.writable.extend(other.writable);
		self.frozen.extend(other.frozen);
	}
}
