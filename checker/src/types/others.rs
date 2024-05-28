// Types to runtime behavior

use crate::{
	features::objects::{ObjectBuilder, SpecialObjects},
	types::properties::{get_properties_on_type, Publicity},
	Constant, Environment, Type, TypeId,
};

use super::{properties::PropertyKey, TypeStore};

#[allow(unused)]
pub(crate) fn create_object_for_type(
	ty: TypeId,
	environment: &mut Environment,
	// &mut to create new objects
	types: &mut TypeStore,
) -> TypeId {
	let mut obj = ObjectBuilder::new(None, types, &mut environment.info);
	match types.get_type_by_id(ty) {
		Type::AliasTo { to: _, name: _, parameters: _ } => todo!(),
		ty @ (Type::And(left, right) | Type::Or(left, right)) => {
			let kind = if matches!(ty, Type::And(..)) { "and" } else { "or" };
			let (left, right) = (*left, *right);

			// TODO: Do we need positions for the following appends?
			obj.append(
				environment,
				Publicity::Public,
				PropertyKey::String("kind".into()),
				crate::PropertyValue::Value(types.new_constant_type(Constant::String(kind.into()))),
				None,
			);
			let left = create_object_for_type(left, environment, types);
			let right = create_object_for_type(right, environment, types);
			obj.append(
				environment,
				Publicity::Public,
				PropertyKey::String("left".into()),
				crate::PropertyValue::Value(left),
				None,
			);
			obj.append(
				environment,
				Publicity::Public,
				PropertyKey::String("right".into()),
				crate::PropertyValue::Value(right),
				None,
			);
		}
		Type::RootPolyType(_) => todo!(),
		Type::Constructor(_) => todo!(),
		Type::Interface { name, parameters: _, nominal: _ } => {
			let name = name.clone();

			// TODO: Do we need positions for the following appends?
			obj.append(
				environment,
				Publicity::Public,
				PropertyKey::String("name".into()),
				crate::PropertyValue::Value(types.new_constant_type(Constant::String(name))),
				None,
			);

			if !matches!(ty, TypeId::BOOLEAN_TYPE | TypeId::STRING_TYPE | TypeId::NUMBER_TYPE) {
				// TODO array
				let mut inner_object = ObjectBuilder::new(None, types, &mut environment.info);

				// let properties = env.create_array();
				for (_, key, property) in get_properties_on_type(ty, types, environment) {
					let value = create_object_for_type(property, environment, types);
					inner_object.append(
						environment,
						Publicity::Public,
						key,
						crate::PropertyValue::Value(value),
						None,
					);
				}

				obj.append(
					environment,
					Publicity::Public,
					PropertyKey::String("properties".into()),
					crate::PropertyValue::Value(inner_object.build_object()),
					None,
				);
			}
		}
		Type::Constant(_) => {
			obj.append(
				environment,
				Publicity::Public,
				PropertyKey::String("constant".into()),
				crate::PropertyValue::Value(ty),
				None,
			);
		}
		Type::SpecialObject(SpecialObjects::Function(..)) => todo!(),
		Type::FunctionReference(..) => todo!(),
		Type::Object(_) => {
			let value = crate::PropertyValue::Value(
				types.new_constant_type(Constant::String("anonymous object".into())),
			);
			obj.append(
				environment,
				Publicity::Public,
				PropertyKey::String("kind".into()),
				value,
				None,
			);

			// TODO array
			let mut inner_object = ObjectBuilder::new(None, types, &mut environment.info);

			// let properties = env.create_array();
			for (_, key, property) in get_properties_on_type(ty, types, environment) {
				let value = create_object_for_type(property, environment, types);
				inner_object.append(
					environment,
					Publicity::Public,
					key,
					crate::PropertyValue::Value(value),
					None,
				);
			}

			obj.append(
				environment,
				Publicity::Public,
				PropertyKey::String("properties".into()),
				crate::PropertyValue::Value(inner_object.build_object()),
				None,
			);
		}
		Type::SpecialObject(_) => todo!(),
		Type::Class { name, parameters } => todo!(),
		Type::PartiallyAppliedGenerics(..) => todo!(),
	}
	obj.build_object()
}
