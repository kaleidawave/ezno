use std::{borrow::Cow, collections::HashMap};

use source_map::SpanWithSource;

use crate::{
	behavior::functions::{ClosureId, ThisValue},
	events::{Event, RootReference},
	types::properties::PropertyKey,
	PropertyValue, Type, TypeId, VariableId,
};

/// TODO explain usage
#[derive(Debug, Clone, Copy, PartialEq, Eq, binary_serialize_derive::BinarySerializable)]
pub enum Publicity {
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
	pub(crate) current_properties:
		HashMap<TypeId, Vec<(Publicity, PropertyKey<'static>, PropertyValue)>>,
	pub(crate) prototypes: HashMap<TypeId, TypeId>,

	pub(crate) closure_current_values: HashMap<(ClosureId, RootReference), TypeId>,

	pub(crate) configurable: HashMap<(TypeId, TypeId), TypeId>,
	pub(crate) enumerable: HashMap<(TypeId, TypeId), TypeId>,
	pub(crate) writable: HashMap<(TypeId, TypeId), TypeId>,
	pub(crate) frozen: HashMap<TypeId, TypeId>,

	/// For super calls etc
	///
	/// TODO not great that this has to be Option to satisfy Default
	pub(crate) value_of_this: ThisValue,
}

impl Facts {
	/// TODO temp
	pub fn register_property(
		&mut self,
		on: TypeId,
		publicity: Publicity,
		under: PropertyKey<'static>,
		to: PropertyValue,
		register_setter_event: bool,
		position: Option<SpanWithSource>,
	) {
		// crate::utils::notify!("Registering {:?} {:?} {:?}", on, under, to);
		self.current_properties.entry(on).or_default().push((publicity, under.clone(), to.clone()));
		if register_setter_event {
			self.events.push(Event::Setter {
				on,
				under,
				new: to,
				reflects_dependency: None,
				initialization: true,
				publicity,
				position,
			});
		}
	}

	pub(crate) fn throw_value(&mut self, value: TypeId, position: SpanWithSource) {
		self.events.push(Event::Throw(value, position));
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
			// TODO Needs a position (or not?)
			let value =
				Event::CreateObject { referenced_in_scope_as: ty, prototype, position: None };
			self.events.push(value);
		}

		ty
	}

	pub fn get_properties_on_type_for_this_level(
		&self,
		ty: TypeId,
	) -> Option<&Vec<(Publicity, PropertyKey, PropertyValue)>> {
		self.current_properties.get(&ty)
	}

	pub(crate) fn extend(&mut self, other: Facts, condition: Option<TypeId>) {
		if condition.is_some() {
			todo!()
		}
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

	pub(crate) fn extend_ref(&mut self, other: &Facts) {
		self.events.extend(other.events.iter().cloned());
		self.queued_events.extend(other.queued_events.iter().cloned());
		self.variable_current_value.extend(other.variable_current_value.iter().to_owned());
		self.prototypes.extend(other.prototypes.iter().to_owned());
		self.current_properties
			.extend(other.current_properties.iter().map(|(l, r)| (*l, r.clone())));
		self.closure_current_values
			.extend(other.closure_current_values.iter().map(|(l, r)| (l.clone(), *r)));
		self.configurable.extend(other.configurable.iter().to_owned());
		self.enumerable.extend(other.enumerable.iter().to_owned());
		self.writable.extend(other.writable.iter().to_owned());
		self.frozen.extend(other.frozen.iter().to_owned());
	}
}
