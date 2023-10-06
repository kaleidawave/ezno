// Types to runtime behavior

use crate::{behavior::objects::ObjectBuilder, Constant, Environment, Type, TypeId};

use super::TypeStore;

pub(crate) fn create_object_for_type(
	ty: TypeId,
	environment: &mut Environment,
	// &mut to create new objects
	types: &mut TypeStore,
) -> TypeId {
	let mut obj = ObjectBuilder::new(None, types, &mut environment.facts); // env.facts.new_object(None, types, false);
	match types.get_type_by_id(ty) {
		Type::AliasTo { to, name, parameters } => todo!(),
		ty @ Type::And(left, right) | ty @ Type::Or(left, right) => {
			let kind = if matches!(ty, Type::And(..)) { "and" } else { "or" };
			let (left, right) = (*left, *right);
			obj.append(
				environment,
				types.new_constant_type(Constant::String("kind".into())),
				crate::Property::Value(types.new_constant_type(Constant::String(kind.into()))),
			);
			let left = create_object_for_type(left, environment, types);
			let right = create_object_for_type(right, environment, types);
			obj.append(
				environment,
				types.new_constant_type(Constant::String("left".into())),
				crate::Property::Value(left),
			);
			obj.append(
				environment,
				types.new_constant_type(Constant::String("right".into())),
				crate::Property::Value(right),
			);
		}
		Type::RootPolyType(_) => todo!(),
		Type::Constructor(_) => todo!(),
		Type::NamedRooted { name, parameters } => {
			let name = name.clone();
			obj.append(
				environment,
				types.new_constant_type(Constant::String("name".into())),
				crate::Property::Value(types.new_constant_type(Constant::String(name))),
			);

			if !matches!(ty, TypeId::BOOLEAN_TYPE | TypeId::STRING_TYPE | TypeId::NUMBER_TYPE) {
				// TODO array
				let mut inner_object = ObjectBuilder::new(None, types, &mut environment.facts);

				// let properties = env.create_array();
				for (key, property) in environment.get_properties_on_type(ty) {
					let value = create_object_for_type(property, environment, types);
					inner_object.append(environment, key, crate::Property::Value(value));
				}

				obj.append(
					environment,
					types.new_constant_type(Constant::String("properties".into())),
					crate::Property::Value(inner_object.build_object()),
				);
			}
		}
		Type::Constant(_) => {
			obj.append(
				environment,
				types.new_constant_type(Constant::String("constant".into())),
				crate::Property::Value(ty),
			);
		}
		Type::Function(_, _) => todo!(),
		Type::FunctionReference(_, _) => todo!(),
		Type::Class(_) => todo!(),
		Type::Object(_) => {
			obj.append(
				environment,
				types.new_constant_type(Constant::String("kind".into())),
				crate::Property::Value(
					types.new_constant_type(Constant::String("anonymous object".into())),
				),
			);

			// TODO array
			let mut inner_object = ObjectBuilder::new(None, types, &mut environment.facts);

			// let properties = env.create_array();
			for (key, property) in environment.get_properties_on_type(ty) {
				let value = create_object_for_type(property, environment, types);
				inner_object.append(environment, key, crate::Property::Value(value));
			}

			obj.append(
				environment,
				types.new_constant_type(Constant::String("properties".into())),
				crate::Property::Value(inner_object.build_object()),
			);
		}
	}
	obj.build_object()
}
