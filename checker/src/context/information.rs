use source_map::SpanWithSource;
use std::collections::HashMap;

use crate::{
	events::{Event, RootReference},
	features::functions::{ClosureId, ThisValue},
	types::{
		properties::{PropertyKey, Publicity},
		TypeStore,
	},
	PropertyValue, Type, TypeId, VariableId,
};

/// Things that are currently true or have happened
#[derive(Debug, Default, binary_serialize_derive::BinarySerializable)]
pub struct LocalInformation {
	pub(crate) events: Vec<Event>,
	/// TODO think about tasks. These are things that may happen at next stop point
	pub(crate) queued_events: Vec<Event>,

	/// This can be not have a value if not defined
	pub(crate) variable_current_value: HashMap<VariableId, TypeId>,
	pub(crate) current_properties:
		HashMap<TypeId, Vec<(Publicity, PropertyKey<'static>, PropertyValue)>>,

	/// Can be modified (unfortunately) so here
	pub(crate) prototypes: HashMap<TypeId, TypeId>,

	pub(crate) closure_current_values: HashMap<(ClosureId, RootReference), TypeId>,

	pub(crate) configurable: HashMap<(TypeId, TypeId), TypeId>,
	pub(crate) enumerable: HashMap<(TypeId, TypeId), TypeId>,
	pub(crate) writable: HashMap<(TypeId, TypeId), TypeId>,
	pub(crate) frozen: HashMap<TypeId, TypeId>,

	/// Object type (LHS), must always be RHS
	///
	/// *not quite the best place, but used in [`InformationChain`]*
	pub(crate) object_constraints: HashMap<TypeId, TypeId>,

	/// For super calls etc
	///
	/// TODO not great that this has to be Option to satisfy Default
	pub(crate) value_of_this: ThisValue,
}

impl LocalInformation {
	/// TODO temp
	pub fn register_property(
		&mut self,
		on: TypeId,
		publicity: Publicity,
		under: PropertyKey<'static>,
		to: PropertyValue,
		register_setter_event: bool,
		position: SpanWithSource,
	) {
		// crate::utilities::notify!("Registering {:?} {:?} {:?}", on, under, to);
		self.current_properties.entry(on).or_default().push((publicity, under.clone(), to.clone()));
		if register_setter_event {
			self.events.push(Event::Setter {
				on,
				under,
				new: to,
				initialization: true,
				publicity,
				position,
			});
		}
	}

	// /// This is how invocation contexts register throws...
	// pub(crate) fn throw_value_in_info(&mut self, value: TypeId, position: SpanWithSource) {
	// 	self.events.push(crate::events::FinalEvent::Throw { thrown: value, position }.into());
	// }

	#[must_use]
	pub fn get_events(&self) -> &[Event] {
		&self.events
	}

	pub(crate) fn new_object(
		&mut self,
		prototype: Option<TypeId>,
		types: &mut crate::types::TypeStore,
		position: SpanWithSource,
		// TODO if this on environment instead it could be worked out?
		is_under_dyn: bool,
	) -> TypeId {
		let ty = types.register_type(Type::Object(crate::types::ObjectNature::RealDeal));
		// crate::utilities::notify!("New object created under {:?}", ty);

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
			let value = Event::CreateObject { referenced_in_scope_as: ty, prototype, position };
			self.events.push(value);
		}

		ty
	}

	#[must_use]
	pub fn get_properties_on_type_for_this_level(
		&self,
		ty: TypeId,
	) -> Option<&Vec<(Publicity, PropertyKey, PropertyValue)>> {
		self.current_properties.get(&ty)
	}

	pub(crate) fn extend(&mut self, other: LocalInformation, condition: Option<TypeId>) {
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

	/// TODO explain when `ref`
	pub(crate) fn extend_ref(&mut self, other: &LocalInformation) {
		self.events.extend(other.events.iter().cloned());
		self.queued_events.extend(other.queued_events.iter().cloned());
		self.variable_current_value.extend(other.variable_current_value.iter().clone());
		self.prototypes.extend(other.prototypes.iter().clone());
		self.current_properties
			.extend(other.current_properties.iter().map(|(l, r)| (*l, r.clone())));
		self.closure_current_values
			.extend(other.closure_current_values.iter().map(|(l, r)| (l.clone(), *r)));
		self.configurable.extend(other.configurable.iter().clone());
		self.enumerable.extend(other.enumerable.iter().clone());
		self.writable.extend(other.writable.iter().clone());
		self.frozen.extend(other.frozen.iter().clone());
	}

	#[must_use]
	pub fn is_halted(&self) -> bool {
		if let Some(last) = self.events.last() {
			if let Event::FinalEvent(_) = last {
				true
			} else {
				// crate::utilities::notify!("TODO ifs others");
				false
			}
		} else {
			false
		}
	}
}

pub trait InformationChain {
	fn get_chain_of_info(&self) -> impl Iterator<Item = &'_ LocalInformation>;
}

impl InformationChain for LocalInformation {
	fn get_chain_of_info(&self) -> impl Iterator<Item = &'_ LocalInformation> {
		std::iter::once(self)
	}
}

pub struct ModuleInformation<'a> {
	pub top: &'a LocalInformation,
	pub module: &'a LocalInformation,
}

impl<'a> InformationChain for ModuleInformation<'a> {
	fn get_chain_of_info(&self) -> impl Iterator<Item = &'_ LocalInformation> {
		IntoIterator::into_iter([self.top, self.module])
	}
}

pub(crate) fn get_value_of_constant_import_variable(
	variable: VariableId,
	info: &impl InformationChain,
) -> TypeId {
	info.get_chain_of_info()
		.find_map(|info| info.variable_current_value.get(&variable).copied())
		.unwrap()
}

pub fn merge_info(
	parents: &impl InformationChain,
	onto: &mut LocalInformation,
	condition: TypeId,
	mut truthy: LocalInformation,
	mut otherwise: Option<LocalInformation>,
	types: &mut TypeStore,
	position: SpanWithSource,
) {
	let truthy_events = truthy.events.len() as u32;
	let otherwise_events = otherwise.as_ref().map_or(0, |f| f.events.len() as u32);
	onto.events.push(Event::Conditionally { condition, truthy_events, otherwise_events, position });

	onto.events.append(&mut truthy.events);
	if let Some(ref mut otherwise) = otherwise {
		crate::utilities::notify!("truthy {:?} otherwise {:?}", truthy.events, otherwise.events);
		onto.events.append(&mut otherwise.events);
	}

	onto.events.push(Event::EndOfControlFlow(truthy_events + otherwise_events));

	// TODO don't need to do above some scope
	for (var, true_value) in truthy.variable_current_value {
		crate::utilities::notify!("{:?} {:?}", var, true_value);
		// TODO don't get value above certain scope...
		let otherwise_value = otherwise
			.as_mut()
			// Remove is important here
			.and_then(|otherwise| otherwise.variable_current_value.remove(&var))
			.or_else(|| onto.variable_current_value.get(&var).copied())
			.or_else(|| {
				parents
					.get_chain_of_info()
					.find_map(|info| info.variable_current_value.get(&var))
					.copied()
			})
			.unwrap_or(TypeId::ERROR_TYPE);

		let new = types.new_conditional_type(condition, true_value, otherwise_value);

		onto.variable_current_value.insert(var, new);
	}

	// TODO temp fix for `... ? { ... } : { ... }`. Breaks for the fact that property
	// properties might be targeting something above the current condition (e.g. `x ? (y.a = 2) : false`);
	onto.current_properties.extend(truthy.current_properties.drain());
	if let Some(ref mut otherwise) = otherwise {
		onto.current_properties.extend(otherwise.current_properties.drain());
	}

	// TODO set more information? an exit condition?
}
