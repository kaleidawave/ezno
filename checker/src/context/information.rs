use std::{collections::HashMap, mem};

use source_map::SpanWithSource;

use crate::{
	context::PossibleLogical,
	events::{Event, RootReference},
	features::{
		functions::{ClosureId, ThisValue},
		objects::SpecialObjects,
	},
	types::{
		generics::generic_type_arguments::StructureGenericArguments, get_constraint,
		properties::PropertyKey, GenericChain, TypeStore,
	},
	Constant, Logical, PropertyValue, Type, TypeId, VariableId,
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
	(on, on_type_arguments): (TypeId, GenericChain),
	(publicity, under): (Publicity, &PropertyKey),
	info_chain: &impl InformationChain,
	types: &TypeStore,
) -> PossibleLogical<PropertyValue> {
	fn resolver(
		(publicity, under, on, on_type_arguments): (Publicity, &PropertyKey, TypeId, GenericChain),
		info: &LocalInformation,
		types: &TypeStore,
	) -> Option<PropertyValue> {
		// TODO if on == constant string and property == length. Need to be able to create types here

		info.current_properties.get(&on).and_then(|properties| {
			get_property_under(properties, (publicity, under), on_type_arguments, types)
		})
	}

	if on == TypeId::ERROR_TYPE {
		return Err(crate::context::MissingOrToCalculate::Error);
	}
	if on == TypeId::ANY_TYPE {
		// TODO any
		return Err(crate::context::MissingOrToCalculate::Infer { on });
	}

	match types.get_type_by_id(on) {
		Type::SpecialObject(SpecialObjects::Function(..)) => info_chain
			.get_chain_of_info()
			.find_map(|info| {
				resolver((publicity, under, on, on_type_arguments), info, types).or_else(|| {
					resolver(
						(publicity, under, TypeId::FUNCTION_TYPE, on_type_arguments),
						info,
						types,
					)
				})
			})
			.map(Logical::Pure)
			.ok_or(crate::context::MissingOrToCalculate::Missing),
		Type::FunctionReference(_) => info_chain
			.get_chain_of_info()
			.find_map(|info| {
				resolver((publicity, under, TypeId::FUNCTION_TYPE, on_type_arguments), info, types)
			})
			.map(Logical::Pure)
			.ok_or(crate::context::MissingOrToCalculate::Missing),
		Type::AliasTo { to, .. } => {
			get_property_unbound((*to, on_type_arguments), (publicity, under), info_chain, types)
			// TODO why would an alias have a property
			// let property_on_types = info_chain
			// 	.get_chain_of_info()
			// 	.find_map(|info| resolver(info, types, on, on_type_arguments,))
			// 	.map(Logical::Pure);
		}
		Type::And(left, right) => {
			get_property_unbound((*left, on_type_arguments), (publicity, under), info_chain, types)
				.or_else(|_| {
					get_property_unbound(
						(*right, on_type_arguments),
						(publicity, under),
						info_chain,
						types,
					)
				})
		}
		Type::Or(left, right) => {
			let left = get_property_unbound(
				(*left, on_type_arguments),
				(publicity, under),
				info_chain,
				types,
			);
			let right = get_property_unbound(
				(*right, on_type_arguments),
				(publicity, under),
				info_chain,
				types,
			);

			// TODO throwaway if both Missing::None

			Ok(Logical::Or {
				based_on: TypeId::BOOLEAN_TYPE,
				left: Box::new(left),
				right: Box::new(right),
			})
		}
		Type::RootPolyType(_nature) => {
			// Can assign to properties on parameters etc
			let aliases = get_constraint(on, types).expect("poly type with no constraint");

			info_chain
				.get_chain_of_info()
				.find_map(|info| resolver((publicity, under, on, on_type_arguments), info, types))
				.map(Logical::Pure)
				.ok_or(crate::context::MissingOrToCalculate::Missing)
				.or_else(|_| {
					get_property_unbound(
						(aliases, on_type_arguments),
						(publicity, under),
						info_chain,
						types,
					)
				})
		}
		Type::Constructor(crate::types::Constructor::StructureGenerics(
			crate::types::StructureGenerics { on: base, arguments },
		)) => {
			let on_sg_type = if let StructureGenericArguments::Closure(_) = arguments {
				info_chain
					.get_chain_of_info()
					.find_map(|info| {
						resolver((publicity, under, on, on_type_arguments), info, types)
					})
					.map(Logical::Pure)
					.ok_or(crate::context::MissingOrToCalculate::Missing)
			} else {
				Err(crate::context::MissingOrToCalculate::Missing)
			};

			on_sg_type.or_else(|_| {
				let on_type_arguments =
					crate::types::GenericChainLink::append(on_type_arguments.as_ref(), &arguments);

				get_property_unbound(
					(*base, on_type_arguments),
					(publicity, under),
					info_chain,
					types,
				)
				.map(|fact| Logical::Implies {
					on: Box::new(fact),
					antecedent: arguments.clone().into(),
				})
			})
		}
		Type::Constructor(crate::types::Constructor::ConditionalResult {
			condition,
			truthy_result,
			otherwise_result,
			result_union: _,
		}) => {
			let left = get_property_unbound(
				(*truthy_result, on_type_arguments),
				(publicity, under),
				info_chain,
				types,
			);
			let right = get_property_unbound(
				(*otherwise_result, on_type_arguments),
				(publicity, under),
				info_chain,
				types,
			);

			// TODO throwaway if both Missing::None

			Ok(Logical::Or { based_on: *condition, left: Box::new(left), right: Box::new(right) })
		}
		Type::Constructor(_constructor) => {
			let on_constructor_type = info_chain
				.get_chain_of_info()
				.find_map(|info| resolver((publicity, under, on, on_type_arguments), info, types))
				.map(Logical::Pure)
				.ok_or(crate::context::MissingOrToCalculate::Missing);

			let aliases = get_constraint(on, types).expect("no constraint for constructor");

			on_constructor_type.or_else(|_| {
				get_property_unbound(
					(aliases, on_type_arguments),
					(publicity, under),
					info_chain,
					types,
				)
			})
		}
		Type::Object(..) => {
			let object_constraint_structure_generics =
				crate::types::get_structure_arguments_based_on_object_constraint(
					on, info_chain, types,
				);

			let prototype =
				info_chain.get_chain_of_info().find_map(|facts| facts.prototypes.get(&on)).copied();

			let generics = if let Some(generics) = object_constraint_structure_generics {
				// TODO clone
				Some(generics.clone())
			} else if prototype
				.is_some_and(|prototype| types.lookup_generic_map.contains_key(&prototype))
			{
				crate::utilities::notify!("Registering lookup");
				Some(StructureGenericArguments::LookUp { on })
			} else {
				None
			};

			info_chain
				.get_chain_of_info()
				.find_map(|info| {
					let on_self = resolver((publicity, under, on, on_type_arguments), info, types);

					let result = if let (Some(prototype), None) = (prototype, &on_self) {
						resolver((publicity, under, prototype, on_type_arguments), info, types)
					} else {
						on_self
					};

					result.map(|result| {
						let pure = Logical::Pure(result);
						if let Some(ref generics) = generics {
							// TODO clone
							Logical::Implies { on: Box::new(pure), antecedent: generics.clone() }
						} else {
							pure
						}
					})
				})
				.ok_or(crate::context::MissingOrToCalculate::Missing)
		}
		Type::Interface { .. } => info_chain
			.get_chain_of_info()
			.find_map(|env| resolver((publicity, under, on, on_type_arguments), env, types))
			.map(Logical::Pure)
			.ok_or(crate::context::MissingOrToCalculate::Missing)
			.or_else(|_| {
				// TODO class and class constructor extends etc
				if let Some(extends) = types.interface_extends.get(&on) {
					get_property_unbound(
						(*extends, on_type_arguments),
						(publicity, under),
						info_chain,
						types,
					)
				} else {
					Err(crate::context::MissingOrToCalculate::Missing)
				}
			}),
		Type::SpecialObject(SpecialObjects::ClassConstructor { .. }) | Type::Class { .. } => {
			info_chain
				.get_chain_of_info()
				.find_map(|env| resolver((publicity, under, on, on_type_arguments), env, types))
				.map(Logical::Pure)
				.ok_or(crate::context::MissingOrToCalculate::Missing)
				.or_else(|_| {
					if let Some(prototype) =
						info_chain.get_chain_of_info().find_map(|info| info.prototypes.get(&on))
					{
						get_property_unbound(
							(*prototype, on_type_arguments),
							(publicity, under),
							info_chain,
							types,
						)
					} else {
						Err(crate::context::MissingOrToCalculate::Missing)
					}
				})
		}
		Type::Constant(cst) => info_chain
			.get_chain_of_info()
			.find_map(|env| resolver((publicity, under, on, on_type_arguments), env, types))
			.map(Logical::Pure)
			.ok_or(crate::context::MissingOrToCalculate::Missing)
			.or_else(|_| {
				let backing_type = cst.get_backing_type_id();
				get_property_unbound(
					(backing_type, on_type_arguments),
					(publicity, under),
					info_chain,
					types,
				)
			}),
		Type::SpecialObject(SpecialObjects::Promise { .. }) => {
			todo!()
		}
		Type::SpecialObject(SpecialObjects::Import(..)) => {
			todo!()
		}
		Type::SpecialObject(SpecialObjects::Proxy(proxy)) => {
			Err(crate::context::MissingOrToCalculate::Proxy(*proxy))
		}
		Type::SpecialObject(SpecialObjects::Generator { .. }) => {
			todo!()
		}
		Type::SpecialObject(SpecialObjects::RegularExpression(..)) => {
			todo!()
		}
	}
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
