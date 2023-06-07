use parser::{
	types::interface::{InterfaceDeclaration, InterfaceMember, InterfaceMemberBody},
	Decorated,
};

use crate::{
	context::{
		Environment, {Context, ContextType},
	},
	structures::functions::FunctionNature,
	synthesis::property_key_as_type,
	types::Type,
	types::{poly_types::type_generic_type_constraints, PolyNature},
	CheckingData, TypeId,
};

use super::functions::type_function_reference;

pub(crate) fn hoist_interface_name<T: crate::FSResolver, U: ContextType>(
	interface: &parser::declarations::InterfaceDeclaration,
	environment: &mut Context<U>,
	checking_data: &mut CheckingData<T>,
) -> (parser::TypeId, TypeId) {
	let name = interface.name.clone();
	if let Some(id) = environment.get_type_from_name(&name) {
		(interface.type_id, id)
	} else {
		let parameters = if let Some(ref parameters) = interface.type_parameters {
			// TODO not great
			let values = parameters
				.into_iter()
				.map(|param| {
					// TODO TypeId that reflects something is recursive here?
					let poly_nature = PolyNature::Generic {
						name: param.name().to_owned(),
						eager_fixed: crate::types::PolyPointer::Fixed(TypeId::ERROR_TYPE),
					};

					checking_data.types.new_type(Type::RootPolyType(poly_nature))
				})
				.collect();
			Some(values)
		} else {
			None
		};
		let ty = Type::NamedRooted { name, parameters };
		let ty_id = checking_data.types.new_type(ty);
		if interface.extends.is_some() {
			todo!();
		}
		checking_data.type_mappings.types_to_types.insert(interface.type_id, ty_id);

		let nominal = interface.nominal_keyword.is_some();

		(interface.type_id, ty_id)
	}
}

/// TODO synthesize interface declaration ...?
pub(crate) fn type_interface_declaration<T: crate::FSResolver, S: ContextType>(
	interface: &Decorated<InterfaceDeclaration>,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T>,
) {
	let Decorated { on: interface, decorators } = interface;

	let interface_type =
		*checking_data.type_mappings.types_to_types.get(&interface.type_id).unwrap();

	{
		// let InterfaceDeclarationMetadata { is_this, .. } =
		// 	InterfaceDeclarationMetadata::from_decorators(decorators);

		// if let Some(html_element_name) = html_element_names {
		// 	for name in html_element_name {
		// 		let tag_mapping = TagNamedMapping::Inbuilt(interface_type);
		// 		let tag_name = checking_data.types.new_constant_type(Constant::String(name.clone()));
		// 		// environment.proofs.tag_names_to_elements.insert(tag_name, tag_mapping);
		// 	}
		// }
	}

	environment.new_lexical_environment_fold_into_parent(
		crate::context::Scope::InterfaceEnvironment { this_constraint: interface_type },
		checking_data,
		|environment, checking_data| {
			if let Some(parameters) =
				checking_data.types.get_type_by_id(interface_type).get_parameters()
			{
				let generic_parameters = type_generic_type_constraints(
					interface.type_parameters.as_ref().unwrap(),
					environment,
					checking_data,
					Some(parameters),
				);
			};

			get_extends(interface, environment, checking_data, interface_type);

			for member in interface.members.iter() {
				type_interface_member(member, environment, checking_data, interface_type);
			}
		},
	);

	// let extends = if let Some(extends_type_references) = &interface.extends {
	// } else {
	//     ExtendsType::None
	// };

	// let mut properties = HashMap::new();

	// let interface_type = InterfaceType {
	//     name: Some(interface.name.clone()),
	//     generic_type_parameters: crate::types::poly_types::NonGeneric,
	//     properties,
	//     extends,
	// };
	// checking_data
	//     .memory
	//     .interface_types
	//     .insert(interface_type_identifier.try_into().unwrap(), interface_type);
	// }
}

fn get_extends<T: crate::FSResolver>(
	interface: &InterfaceDeclaration,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	interface_type: TypeId,
) {
	if let Some([reference, others @ ..]) = interface.extends.as_deref() {
		let mut ty = environment.get_type_handle_errors(reference, checking_data);
		for reference in others {
			let rhs = environment.get_type_handle_errors(reference, checking_data);
			ty = checking_data.types.new_type(Type::And(ty, rhs));
		}

		environment.bases.connect_extends(interface_type, ty);
	}
}

/// TODO overloads
pub(crate) fn type_interface_member<T: crate::FSResolver, S: ContextType>(
	member: &Decorated<InterfaceMember>,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T>,
	interface_type: TypeId,
) {
	let Decorated { decorators, on: member } = member;

	match member {
		InterfaceMember::Method {
			name,
			type_parameters,
			parameters,
			body,
			return_type,
			is_optional,
			position,
		} => {
			let mut function = type_function_reference(
				type_parameters,
				parameters,
				return_type.as_ref(),
				environment,
				checking_data,
				position.clone(),
				FunctionNature::Arrow,
			);

			if let Some(InterfaceMemberBody { body, condition, .. }) = body {
				if let Some(condition) = condition {}
				// let result = synthesize_block(body, environment, checking_data, chain);
			}

			let key_ty = property_key_as_type(name, environment, &mut checking_data.types);

			todo!();

			// let func_ty = checking_data.types.new_type(Type::AliasTo {
			// 	to: TypeId::FUNCTION_TYPE,
			// 	name: None,
			// 	parameters: None,
			// });
			// environment.functions_on_type.insert(func_ty, function);
			// // func_ty;

			// let existing_property =
			// 	crate::utils::add_property(environment, interface_type, key_ty, func_ty);

			// if let Some(existing_property) = existing_property {
			// 	panic!("{:?} declared twice", name);
			// }

			// if existing.is_some() {
			// 	crate::utils::notify!("Overwrote interface key");
			// }
		}
		InterfaceMember::Property { name, type_reference, is_readonly, is_optional, .. } => {
			let value_ty = environment.get_type_handle_errors(type_reference, checking_data);

			let property_ty = if *is_optional {
				checking_data.types.new_type(Type::Or(value_ty, TypeId::UNDEFINED_TYPE))
			} else {
				value_ty
			};

			let key_ty = property_key_as_type(name, environment, &mut checking_data.types);

			// if environment.get_property_truths(interface_type, name_as_type).is_some() {
			// 	// Handling for overload
			// 	todo!()
			// }

			// TODO crate::utils::notify!("Extract property add logic:");

			let existing_property = environment
				.properties
				.entry(interface_type)
				.or_default()
				.push((key_ty, property_ty));

			// if let Some(existing_property) = existing_property {
			// 	panic!("{:?} declared twice", name);
			// }
		}
		InterfaceMember::Indexer { indexer_type, return_type, is_readonly, .. } => {
			todo!()
			// if *is_readonly {
			//     unimplemented!();
			// }
			// let indexer_type = environment.get_type_else_any(
			//     indexer_type,
			//     checking_data,
			//     &mut GetTypeFromReferenceSettings::Default,
			// );
			// let return_type = environment.get_type_else_any(
			//     return_type,
			//     checking_data,
			//     &mut GetTypeFromReferenceSettings::Default,
			// );
			// *indexer.borrow_mut() = Some((indexer_type.clone(), return_type.clone()));
		}
		InterfaceMember::Constructor {
			parameters,
			type_parameters,
			return_type,
			is_readonly,
			position,
		} => {
			let mut function = crate::synthesis::functions::type_function_reference(
				type_parameters,
				parameters,
				return_type.as_ref(),
				environment,
				checking_data,
				position.clone(),
				FunctionNature::Arrow,
			);

			todo!()

			// let func_ty = environment.new_type(Type::AliasTo {
			// 	to: TypeId::FUNCTION_TYPE,
			// 	name: None,
			// 	parameters: None,
			// });

			// // TODO not a great place
			// function.nature = FunctionNature::ClassConstructor {
			// 	class_prototype: interface_type,
			// 	class_constructor: todo!(),
			// };

			// environment.functions_on_type.insert(func_ty, function);
		}
		InterfaceMember::Comment(_comment) => {}
		InterfaceMember::Caller { .. } => todo!(),
		InterfaceMember::Rule { .. } => todo!(),
	}
}
