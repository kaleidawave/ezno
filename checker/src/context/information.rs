use std::{collections::HashMap, mem};

use source_map::SpanWithSource;

use crate::{
	context::PossibleLogical,
	events::{Event, RootReference},
	features::functions::{ClosureId, ThisValue},
	types::{get_constraint, properties::PropertyKey, GenericChain, TypeStore},
	Constant, PropertyValue, Type, TypeId, VariableId,
};

/// TODO explain usage
#[derive(Debug, Clone, Copy, PartialEq, Eq, binary_serialize_derive::BinarySerializable)]
pub enum Publicity {
	Private,
	Public,
}

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
		position: Option<SpanWithSource>,
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
		// TODO if this on environment instead it could be worked out?
		is_under_dyn: bool,
		is_function_this: bool,
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
			// TODO Needs a position (or not?)
			let value = Event::CreateObject {
				referenced_in_scope_as: ty,
				prototype,
				position: None,
				is_function_this,
			};
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
}

pub trait InformationChain {
	fn get_chain_of_info(&self) -> impl Iterator<Item = &'_ LocalInformation>;
}

impl InformationChain for LocalInformation {
	fn get_chain_of_info(&self) -> impl Iterator<Item = &'_ LocalInformation> {
		std::iter::once(self)
	}
}

/// Get all properties on a type (for printing and other non-one property uses)
///
/// - TODO make aware of ands and aliases
/// - TODO prototypes
/// - TODO could this be an iterator
/// - TODO return whether it is fixed
/// - TODO doesn't evaluate properties
pub fn get_properties_on_type(
	base: TypeId,
	_types: &TypeStore,
	info: &impl InformationChain,
) -> Vec<(Publicity, PropertyKey<'static>, TypeId)> {
	let reversed_flattened_properties = info
		.get_chain_of_info()
		.filter_map(|info| info.current_properties.get(&base).map(|v| v.iter().rev()))
		.flatten();

	let mut deleted_or_existing_properties = std::collections::HashSet::<PropertyKey>::new();

	let mut properties = Vec::new();
	for (publicity, key, prop) in reversed_flattened_properties {
		if let PropertyValue::Deleted = prop {
			// TODO doesn't cover constants :(
			deleted_or_existing_properties.insert(key.clone());
		} else if deleted_or_existing_properties.insert(key.clone()) {
			properties.push((*publicity, key.to_owned(), prop.as_get_type()));
		}
	}

	properties.reverse();
	properties
}

pub(crate) fn get_value_of_constant_import_variable(
	variable: VariableId,
	info: &impl InformationChain,
) -> TypeId {
	info.get_chain_of_info()
		.find_map(|info| info.variable_current_value.get(&variable).copied())
		.unwrap()
}

pub(crate) fn get_property_unbound(
	on: TypeId,
	publicity: Publicity,
	under: &PropertyKey,
	types: &TypeStore,
	info: &impl InformationChain,
) -> PossibleLogical<PropertyValue> {
	fn get_property(
		info: &LocalInformation,
		types: &TypeStore,
		on: TypeId,
		on_type_arguments: GenericChain,
		under: (Publicity, &PropertyKey),
	) -> Option<PropertyValue> {
		info.current_properties
			.get(&on)
			.and_then(|properties| get_property_under(properties, under, on_type_arguments, types))
	}

	// let under = match under {
	// 	PropertyKey::Type(t) => PropertyKey::Type(get_constraint(t, types).unwrap_or(t)),
	// 	under @ PropertyKey::String(_) => under,
	// };

	types.get_fact_about_type(info, on, None, &get_property, (publicity, under))
}

fn get_property_under(
	properties: &[(Publicity, PropertyKey<'_>, PropertyValue)],
	(want_publicity, want_key): (Publicity, &PropertyKey<'_>),
	key_type_arguments: GenericChain,
	types: &TypeStore,
) -> Option<PropertyValue> {
	// 'rev' is important
	properties.iter().rev().find_map(move |(publicity, key, value)| {
		if *publicity != want_publicity {
			return None;
		}

		match key {
			PropertyKey::String(string) => {
				if let PropertyKey::String(want) = want_key {
					(string == want).then_some(value.clone())
				} else {
					// TODO
					None
				}
			}
			PropertyKey::Type(key) => {
				key_matches(*key, key_type_arguments, want_key, types).then(|| value.clone())
			}
		}
	})
}

/// TODO contributions for `P`
#[allow(clippy::if_same_then_else)]
fn key_matches(
	key: TypeId,
	key_type_arguments: GenericChain,
	want_key: &PropertyKey<'_>,
	types: &TypeStore,
) -> bool {
	let key = if let Some(on_type_arguments) = key_type_arguments {
		on_type_arguments.get_single_argument(key).unwrap_or(key)
	} else {
		key
	};
	if let Type::Or(lhs, rhs) = types.get_type_by_id(key) {
		key_matches(*lhs, key_type_arguments, want_key, types)
			|| key_matches(*rhs, key_type_arguments, want_key, types)
	} else {
		match want_key {
			PropertyKey::Type(want) => {
				crate::utilities::notify!("want {:?} key {:?}", want, key);
				let want = get_constraint(*want, types).unwrap_or(*want);
				crate::utilities::notify!("want {:?} key {:?}", want, key);
				key == want
			}
			PropertyKey::String(s) => {
				// TODO WIP
				if key == TypeId::ANY_TYPE {
					true
				} else if key == TypeId::NUMBER_TYPE && s.parse::<usize>().is_ok() {
					true
				} else if key == TypeId::STRING_TYPE && s.parse::<usize>().is_err() {
					true
				} else if let Type::Constant(Constant::String(ks)) = types.get_type_by_id(key) {
					ks == s
				} else {
					false
				}
			}
		}
	}
}

pub fn merge_info(
	parents: &impl InformationChain,
	onto: &mut LocalInformation,
	condition: TypeId,
	truthy: LocalInformation,
	mut falsy: Option<LocalInformation>,
	types: &mut TypeStore,
) {
	onto.events.push(Event::Conditionally {
		condition,
		true_events: truthy.events.into_boxed_slice(),
		else_events: falsy
			.as_mut()
			.map(|falsy| mem::take(&mut falsy.events).into_boxed_slice())
			.unwrap_or_default(),
		position: None,
	});

	// TODO don't need to do above some scope
	for (var, true_value) in truthy.variable_current_value {
		crate::utilities::notify!("{:?} {:?}", var, true_value);
		// TODO don't get value above certain scope...
		let falsy_value = falsy
			.as_mut()
			// Remove is important here
			.and_then(|falsy| falsy.variable_current_value.remove(&var))
			.or_else(|| onto.variable_current_value.get(&var).copied())
			.or_else(|| {
				parents
					.get_chain_of_info()
					.find_map(|info| info.variable_current_value.get(&var))
					.copied()
			})
			.unwrap_or(TypeId::ERROR_TYPE);

		let new = types.new_conditional_type(condition, true_value, falsy_value);

		onto.variable_current_value.insert(var, new);
	}
}
