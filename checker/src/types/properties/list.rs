use super::{Properties, PropertyKey, PropertyValue};
use crate::{
	context::InformationChain,
	types::{GenericChain, ObjectNature, SliceArguments},
	Type, TypeId, TypeStore,
};
use std::collections::{BTreeMap, HashMap};

/// Get properties on a type (for printing and other non-one property uses)
///
/// - TODO prototypes?
/// - TODO return whether it is fixed (conditional + conditional enumerable + non string keys)
/// - TODO doesn't evaluate properties
/// - TODO don't have to reverse at end
/// - `filter_enumerable` for printing vs `for in` loops
pub fn get_properties_on_single_type(
	base: TypeId,
	types: &TypeStore,
	info: &impl InformationChain,
	filter_enumerable: bool,
	filter_type: TypeId,
) -> Properties {
	match types.get_type_by_id(base) {
		Type::Object(ObjectNature::AnonymousTypeAnnotation(properties)) => properties.clone(),
		Type::Interface { .. } | Type::Class { .. } | Type::Object(ObjectNature::RealDeal) => {
			// Reversed needed for deleted
			let flattened_properties = info
				.get_chain_of_info()
				.filter_map(|info| info.current_properties.get(&base).map(|v| v.iter()))
				.flatten();

			let mut existing_properties = HashMap::<PropertyKey, usize>::new();

			// This retains ordering here

			let mut properties = Vec::new();
			let mut numerical_properties = BTreeMap::new();

			for (publicity, key, value) in flattened_properties {
				if let PropertyValue::Configured { on: _, ref descriptor } = value {
					// TODO what about if not `TypeId::TRUE | TypeId::FALSE`
					crate::utilities::notify!("descriptor.enumerable={:?}", descriptor.enumerable);
					if filter_enumerable && !matches!(descriptor.enumerable, TypeId::TRUE) {
						continue;
					}
				}

				let existing = existing_properties.insert(key.clone(), properties.len());
				if let PropertyValue::Deleted = value {
					if let Some(existing) = existing {
						properties.remove(existing);
					}
					// TODO only covers constant keys :(
					continue;
				}

				if !matches!(filter_type, TypeId::ANY_TYPE) {
					let on_type_arguments = None; // TODO
					let (key_matches, key_arguments) = super::key_matches(
						(&PropertyKey::Type(filter_type), on_type_arguments),
						(key, None),
						info,
						types,
					);

					crate::utilities::notify!("key_arguments={:?}", key_arguments);

					if !key_matches {
						if let Some(existing) = existing {
							properties.remove(existing);
						}
						continue;
					}
				}

				if let Some(idx) = existing {
					let value = (*publicity, key.to_owned(), value.clone());

					if let Some(n) = key.as_number(types) {
						numerical_properties.insert(n, value);
					} else {
						// TODO temp fix
						if idx >= properties.len() {
							crate::utilities::notify!("Here! {:?}", properties);
							continue;
						}
						properties[idx] = value;
					}
				} else {
					// crate::utilities::notify!("Here just with {:?}", key);

					let value = (*publicity, key.to_owned(), value.clone());

					if let Some(n) = key.as_number(types) {
						numerical_properties.insert(n, value);
					} else {
						properties.push(value);
					}
				}
			}

			if numerical_properties.is_empty() {
				properties
			} else {
				numerical_properties.into_values().chain(properties).collect()
			}
		}
		Type::Narrowed { narrowed_to: to, .. } | Type::AliasTo { to, .. } => {
			get_properties_on_single_type(*to, types, info, filter_enumerable, filter_type)
		}
		Type::Constant(c) => get_properties_on_single_type(
			c.get_backing_type_id(),
			types,
			info,
			filter_enumerable,
			filter_type,
		),
		Type::SpecialObject(crate::types::SpecialObject::Function(..))
		| Type::FunctionReference(_) => get_properties_on_single_type(
			TypeId::FUNCTION_TYPE,
			types,
			info,
			filter_enumerable,
			filter_type,
		),
		t @ (Type::SpecialObject(_)
		| Type::Constructor(_)
		| Type::RootPolyType(_)
		| Type::PartiallyAppliedGenerics(_)
		| Type::Or(..)
		| Type::And(_, _)) => {
			crate::utilities::notify!("Cannot get all properties on {:?}", t);
			Default::default()
		}
	}
}

/// WIP TODO remove filter
/// Slightly different to regular `get_properties_on_single_type`
/// - appends key argument
/// - no numerical sorting
/// - no enumerable
pub fn get_properties_on_single_type2(
	(base, base_arguments): (TypeId, GenericChain),
	types: &TypeStore,
	info: &impl InformationChain,
	filter_type: TypeId,
) -> Vec<(PropertyKey<'static>, PropertyValue, SliceArguments)> {
	match types.get_type_by_id(base) {
		Type::Object(ObjectNature::AnonymousTypeAnnotation(on_properties)) => {
			let mut existing_properties = HashMap::<PropertyKey, usize>::new();
			let mut properties = Vec::new();

			for (_publicity, key, value) in on_properties {
				let existing = existing_properties.insert(key.clone(), properties.len());
				if let PropertyValue::Deleted = value {
					if let Some(existing) = existing {
						properties.remove(existing);
					}
					// TODO only covers constant keys :(
					continue;
				}

				// if !matches!(filter_type, TypeId::ANY_TYPE) {
				let on_type_arguments = None; // TODO
				let (key_matches, key_arguments) = super::key_matches(
					(&PropertyKey::Type(filter_type), on_type_arguments),
					(key, None),
					info,
					types,
				);

				// crate::utilities::notify!("key_arguments={:?}", key_arguments);

				if !key_matches {
					continue;
				}
				// }

				if let Some(idx) = existing {
					let value = (key.to_owned(), value.clone(), key_arguments);
					properties[idx] = value;
				} else {
					let value = (key.to_owned(), value.clone(), key_arguments);
					properties.push(value);
				}
			}

			properties
		}
		Type::Interface { .. } | Type::Class { .. } | Type::Object(_) => {
			// Reversed needed for deleted
			let flattened_properties = info
				.get_chain_of_info()
				.filter_map(|info| info.current_properties.get(&base).map(|v| v.iter()))
				.flatten();

			let mut existing_properties = HashMap::<PropertyKey, usize>::new();

			// This retains ordering here

			let mut properties = Vec::new();

			for (_publicity, key, value) in flattened_properties {
				let existing = existing_properties.insert(key.clone(), properties.len());
				if let PropertyValue::Deleted = value {
					if let Some(existing) = existing {
						properties.remove(existing);
					}
					// TODO only covers constant keys :(
					continue;
				}

				// if !matches!(filter_type, TypeId::ANY_TYPE) {
				let on_type_arguments = None; // TODO
				let (key_matches, key_arguments) = super::key_matches(
					(&PropertyKey::Type(filter_type), on_type_arguments),
					(key, None),
					info,
					types,
				);

				// crate::utilities::notify!("key_arguments={:?}", key_arguments);

				if !key_matches {
					continue;
				}
				// }

				if let Some(idx) = existing {
					let value = (key.to_owned(), value.clone(), key_arguments);
					properties[idx] = value;
				} else {
					let value = (key.to_owned(), value.clone(), key_arguments);
					properties.push(value);
				}
			}

			properties
			// if numerical_properties.is_empty() {
			// } else {
			// 	todo!()
			// 	// numerical_properties.into_values().chain(properties).collect()
			// }
		}
		Type::Constructor(_) | Type::RootPolyType(_) => {
			if let Some(argument) =
				base_arguments.as_ref().and_then(|args| args.get_single_argument(base))
			{
				get_properties_on_single_type2((argument, base_arguments), types, info, filter_type)
			} else {
				let backing = crate::types::get_constraint(base, types).unwrap();
				get_properties_on_single_type2((backing, base_arguments), types, info, filter_type)
			}
		}
		Type::PartiallyAppliedGenerics(crate::types::PartiallyAppliedGenerics {
			on,
			arguments,
		}) => {
			// Temp fix
			if *on == TypeId::ARRAY_TYPE {
				let value = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
				vec![(
					PropertyKey::Type(TypeId::NUMBER_TYPE),
					PropertyValue::Value(value),
					Default::default(),
				)]
			} else {
				// let result = super::access::get_property_unbound(
				// 	(base, base_arguments),
				// 	(Publicity::Public, &PropertyKey::Type(filter_type), None),
				// 	false,
				// 	info,
				// 	types,
				// );
				crate::utilities::notify!("Cannot get all properties on {:?}", base);
				Default::default()
			}
		}
		Type::Narrowed { narrowed_to: to, .. } | Type::AliasTo { to, .. } => {
			get_properties_on_single_type2((*to, base_arguments), types, info, filter_type)
		}
		Type::Constant(c) => get_properties_on_single_type2(
			(c.get_backing_type_id(), base_arguments),
			types,
			info,
			filter_type,
		),
		Type::SpecialObject(crate::types::SpecialObject::Function(..))
		| Type::FunctionReference(_) => get_properties_on_single_type2(
			(TypeId::FUNCTION_TYPE, base_arguments),
			types,
			info,
			filter_type,
		),
		t @ (Type::SpecialObject(_) | Type::Or(..) | Type::And(_, _)) => {
			crate::utilities::notify!("Cannot get all properties on {:?}", t);
			Default::default()
		}
	}
}

pub fn get_property_key_names_on_a_single_type(
	base: TypeId,
	types: &TypeStore,
	environment: &mut crate::Environment,
) -> Vec<String> {
	let is_special = matches!(
		types.get_type_by_id(base),
		Type::SpecialObject(_)
			| Type::Constructor(_)
			| Type::RootPolyType(_)
			| Type::Or(..)
			| Type::PartiallyAppliedGenerics(_)
			| Type::Constant(_)
			| Type::AliasTo { .. }
			| Type::FunctionReference(_)
			| Type::And(_, _)
	);
	if is_special {
		return vec![];
	}

	get_properties_on_single_type(base, types, environment, false, TypeId::ANY_TYPE)
		.into_iter()
		.map(|property| super::get_property_as_string(&property.1, types, environment))
		.collect()
}
