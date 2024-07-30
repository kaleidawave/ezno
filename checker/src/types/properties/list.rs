use super::{PropertyKey, PropertyValue};
use crate::{
	context::{InformationChain, Properties},
	types::{GenericChain, SliceArguments},
	Type, TypeId, TypeStore,
};
use std::collections::{BTreeMap, HashSet};

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
		Type::Interface { .. } | Type::Class { .. } | Type::Object(_) => {
			// Reversed needed for deleted
			let reversed_flattened_properties = info
				.get_chain_of_info()
				.filter_map(|info| info.current_properties.get(&base).map(|v| v.iter().rev()))
				.flatten();

			let mut deleted_or_existing_properties = HashSet::<PropertyKey>::new();

			// This retains ordering here

			let mut properties = Vec::new();
			let mut numerical_properties = BTreeMap::new();

			for (publicity, key, value) in reversed_flattened_properties {
				let new_record = deleted_or_existing_properties.insert(key.clone());

				if let PropertyValue::Configured { on: _, ref descriptor } = value {
					// TODO what about if not `TypeId::TRUE | TypeId::FALSE`
					crate::utilities::notify!("descriptor.enumerable={:?}", descriptor.enumerable);
					if filter_enumerable && !matches!(descriptor.enumerable, TypeId::TRUE) {
						continue;
					}
				}

				if let PropertyValue::Deleted = value {
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
						continue;
					}
				}

				if new_record {
					let value = (*publicity, key.to_owned(), value.clone());

					if let Some(n) = key.as_number(types) {
						numerical_properties.insert(n, value);
					} else {
						properties.push(value);
					}
				}
			}

			properties.reverse();

			if numerical_properties.is_empty() {
				properties
			} else {
				let mut new: Vec<_> = numerical_properties.into_values().collect();
				new.append(&mut properties);
				new
			}
		}
		t @ (Type::SpecialObject(_)
		| Type::Constructor(_)
		| Type::RootPolyType(_)
		| Type::Or(..)
		| Type::PartiallyAppliedGenerics(_)
		| Type::Constant(_)
		| Type::AliasTo { .. }
		| Type::FunctionReference(_)
		| Type::And(_, _)) => panic!("Cannot get all properties on {t:?}"),
	}
}

/// WIP TODO remove filter
pub fn get_properties_on_single_type2(
	(base, base_arguments): (TypeId, GenericChain),
	types: &TypeStore,
	info: &impl InformationChain,
	filter_type: TypeId,
) -> Vec<(PropertyKey<'static>, PropertyValue, SliceArguments)> {
	match types.get_type_by_id(base) {
		Type::Interface { .. } | Type::Class { .. } | Type::Object(_) => {
			// Reversed needed for deleted
			let reversed_flattened_properties = info
				.get_chain_of_info()
				.filter_map(|info| info.current_properties.get(&base).map(|v| v.iter().rev()))
				.flatten();

			let mut deleted_or_existing_properties = HashSet::<PropertyKey>::new();

			// This retains ordering here

			let mut properties = Vec::new();

			for (publicity, key, value) in reversed_flattened_properties {
				let new_record = deleted_or_existing_properties.insert(key.clone());

				let on_type_arguments = None; // TODO
				let (key_matches, key_arguments) = super::key_matches(
					(&PropertyKey::Type(filter_type), on_type_arguments),
					(key, None),
					info,
					types,
				);

				// crate::utilities::notify!("key_arguments={:?}", key_arguments);

				if key_matches {
					properties.push((key.clone(), value.clone(), key_arguments));
				}
			}

			properties
		}
		Type::Constructor(_) | Type::RootPolyType(_) => {
			if let Some(argument) =
				base_arguments.as_ref().and_then(|args| args.get_single_argument(base))
			{
				get_properties_on_single_type2((argument, base_arguments), types, info, filter_type)
			} else {
				todo!("Getting properties on generic")
			}
		}
		t @ (Type::SpecialObject(_)
		| Type::Or(..)
		| Type::PartiallyAppliedGenerics(_)
		| Type::Constant(_)
		| Type::AliasTo { .. }
		| Type::FunctionReference(_)
		| Type::And(_, _)) => panic!("Cannot get all properties on {t:?}"),
	}
}
