use parser::{
	types::interface::{InterfaceDeclaration, InterfaceMember},
	Decorated, PropertyKey,
};

use crate::{
	context::{
		Environment, {Context, ContextType},
	},
	synthesis::property_key_as_type,
	types::FunctionKind,
	types::{properties::Property, FunctionType, Type},
	CheckingData, TypeId,
};

use super::{
	classes::type_generic_type_constraints, functions::type_function_reference,
	type_annotations::synthesize_type_annotation,
};

/// TODO synthesize interface declaration ...?
pub(super) fn type_interface_declaration<T: crate::FSResolver, S: ContextType>(
	interface: &Decorated<InterfaceDeclaration>,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T>,
) {
	let Decorated { on: interface, decorators } = interface;

	let interface_type = todo!();
	// 	*checking_data.type_mappings.types_to_types.get(&interface.type_id).unwrap();

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

	// let extends = if let Some(extends_type_annotations) = &interface.extends {
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
		let mut ty = synthesize_type_annotation(reference, environment, checking_data);
		for reference in others {
			let rhs = synthesize_type_annotation(reference, environment, checking_data);
			ty = checking_data.types.register_type(Type::And(ty, rhs));
		}

		environment.bases.connect_extends(interface_type, ty);
	}
}

pub(crate) trait SynthesizeInterfaceBehavior {
	fn register<T: crate::FSResolver, S: ContextType>(
		&mut self,
		key: PropertyOrType,
		value: InterfaceValue,
		checking_data: &mut CheckingData<T>,
		environment: &mut Context<S>,
	);

	fn interface_type(&self) -> Option<TypeId>;
}

pub(crate) enum InterfaceValue {
	Function { function: FunctionType, constructor: bool },
	Value(TypeId),
}

pub(crate) enum PropertyOrType<'a> {
	ClassProperty(&'a PropertyKey<parser::property_key::PublicOrPrivate>),
	ObjectProperty(&'a PropertyKey<parser::property_key::AlwaysPublic>),
	Type(TypeId),
}

pub(crate) struct OnToType(pub(crate) TypeId);

impl SynthesizeInterfaceBehavior for OnToType {
	fn register<T: crate::FSResolver, S: ContextType>(
		&mut self,
		key: PropertyOrType,
		value: InterfaceValue,
		checking_data: &mut CheckingData<T>,
		environment: &mut Context<S>,
	) {
		let under = match key {
			PropertyOrType::ClassProperty(key) => {
				property_key_as_type(key, environment, &mut checking_data.types)
			}
			PropertyOrType::ObjectProperty(key) => {
				property_key_as_type(key, environment, &mut checking_data.types)
			}
			PropertyOrType::Type(ty) => ty,
		};
		let ty = match value {
			InterfaceValue::Function { function, constructor } => {
				// TODO constructor
				let ty = Type::Function(function, crate::types::FunctionNature::Reference);
				checking_data.types.register_type(ty)
			}
			InterfaceValue::Value(value) => value,
		};
		environment.facts.register_property(self.0, under, Property::Value(ty), false)
	}

	fn interface_type(&self) -> Option<TypeId> {
		Some(self.0)
	}
}

pub(super) fn synthesize_signatures<
	T: crate::FSResolver,
	S: ContextType,
	B: SynthesizeInterfaceBehavior,
>(
	signatures: &[Decorated<InterfaceMember>],
	mut behavior: B,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T>,
) -> B {
	for signature in signatures {
		match &signature.on {
			InterfaceMember::Method {
				name,
				type_parameters,
				parameters,
				return_type,
				is_optional,
				performs,
				position,
			} => {
				let function = type_function_reference(
					type_parameters,
					parameters,
					return_type.as_ref(),
					environment,
					checking_data,
					performs.as_ref().into(),
					position.clone(),
					FunctionKind::Arrow,
					behavior.interface_type(),
				);
				behavior.register(
					PropertyOrType::ClassProperty(name),
					InterfaceValue::Function { function, constructor: false },
					checking_data,
					environment,
				)
			}
			InterfaceMember::Property {
				name,
				type_annotation,
				is_readonly,
				is_optional,
				position,
			} => {
				let value = synthesize_type_annotation(type_annotation, environment, checking_data);
				behavior.register(
					PropertyOrType::ClassProperty(name),
					InterfaceValue::Value(value),
					checking_data,
					environment,
				)
			}
			InterfaceMember::Indexer { name, indexer_type, return_type, is_readonly, position } => {
				// TODO think this is okay
				let key = synthesize_type_annotation(indexer_type, environment, checking_data);
				let value = synthesize_type_annotation(return_type, environment, checking_data);
				behavior.register(
					PropertyOrType::Type(key),
					InterfaceValue::Value(value),
					checking_data,
					environment,
				)
			}
			InterfaceMember::Constructor {
				parameters,
				type_parameters,
				return_type,
				is_readonly,
				position,
			} => checking_data.raise_unimplemented_error("interface constructor", position.clone()),
			InterfaceMember::Caller {
				parameters,
				type_parameters,
				return_type,
				is_readonly,
				position,
			} => checking_data.raise_unimplemented_error("interface caller", position.clone()),
			InterfaceMember::Rule {
				parameter,
				rule,
				matching_type,
				optionality,
				is_readonly,
				output_type,
				position,
			} => checking_data.raise_unimplemented_error("interface rule", position.clone()),
			InterfaceMember::Comment(_) => {}
		}
	}

	behavior
}

/// TODO overloads
pub(super) fn type_interface_member<T: crate::FSResolver, S: ContextType>(
	member: &Decorated<InterfaceMember>,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T>,
	interface_type: TypeId,
) {
	todo!();
	// let Decorated { decorators, on: member } = member;

	// match member {
	// 	InterfaceMember::Method {
	// 		name,
	// 		type_parameters,
	// 		parameters,
	// 		performs,
	// 		return_type,
	// 		is_optional,
	// 		position,
	// 	} => {
	// 		let mut function = type_function_reference(
	// 			type_parameters,
	// 			parameters,
	// 			return_type.as_ref(),
	// 			environment,
	// 			checking_data,
	// 			None,
	// 			// performs.as_ref().map(|p| &p.performs),
	// 			position.clone(),
	// 			FunctionKind::Arrow { get_set: crate::GetSetGeneratorOrNone::None },
	// 		);

	// 		// if let Some(InterfaceMemberBody { body, condition, .. }) = body {
	// 		// 	if let Some(condition) = condition {}
	// 		// 	// let result = synthesize_block(body, environment, checking_data);
	// 		// }

	// 		let key_ty = property_key_as_type(name, environment, &mut checking_data.types);

	// 		// let func_ty = checking_data.types.register_type(Type::AliasTo {
	// 		// 	to: TypeId::FUNCTION_TYPE,
	// 		// 	name: None,
	// 		// 	parameters: None,
	// 		// });
	// 		// environment.functions_on_type.insert(func_ty, function);
	// 		// // func_ty;

	// 		// let existing_property =
	// 		// 	crate::utils::add_property(environment, interface_type, key_ty, func_ty);

	// 		// if let Some(existing_property) = existing_property {
	// 		// 	panic!("{:?} declared twice", name);
	// 		// }

	// 		// if existing.is_some() {
	// 		// 	crate::utils::notify!("Overwrote interface key");
	// 		// }
	// 	}
	// 	InterfaceMember::Property { name, type_annotation, is_readonly, is_optional, .. } => {
	// 		let value_ty = synthesize_type_annotation(type_annotation, environment, checking_data);

	// 		let property_ty = if *is_optional {
	// 			checking_data.types.register_type(Type::Or(value_ty, TypeId::UNDEFINED_TYPE))
	// 		} else {
	// 			value_ty
	// 		};

	// 		let key_ty = property_key_as_type(name, environment, &mut checking_data.types);

	// 		// if environment.get_property_truths(interface_type, name_as_type).is_some() {
	// 		// 	// Handling for overload
	// 		// 	todo!()
	// 		// }

	// 		// TODO crate::utils::notify!("Extract property add logic:");

	// 		let existing_property = environment
	// 			.properties
	// 			.entry(interface_type)
	// 			.or_default()
	// 			.push((key_ty, Property::Value(property_ty)));

	// 		// if let Some(existing_property) = existing_property {
	// 		// 	panic!("{:?} declared twice", name);
	// 		// }
	// 	}
	// 	InterfaceMember::Indexer { indexer_type, return_type, is_readonly, .. } => {
	// 		todo!()
	// 		// if *is_readonly {
	// 		//     unimplemented!();
	// 		// }
	// 		// let indexer_type = environment.get_type_else_any(
	// 		//     indexer_type,
	// 		//     checking_data,
	// 		//     &mut GetTypeFromReferenceSettings::Default,
	// 		// );
	// 		// let return_type = environment.get_type_else_any(
	// 		//     return_type,
	// 		//     checking_data,
	// 		//     &mut GetTypeFromReferenceSettings::Default,
	// 		// );
	// 		// *indexer.borrow_mut() = Some((indexer_type.clone(), return_type.clone()));
	// 	}
	// 	InterfaceMember::Constructor {
	// 		parameters,
	// 		type_parameters,
	// 		return_type,
	// 		is_readonly,
	// 		position,
	// 	} => {
	// 		let mut function = crate::synthesis::functions::type_function_reference(
	// 			type_parameters,
	// 			parameters,
	// 			return_type.as_ref(),
	// 			environment,
	// 			checking_data,
	// 			None,
	// 			position.clone(),
	// 			FunctionKind::ClassConstructor {
	// 				class_prototype: todo!(),
	// 				class_constructor: todo!(),
	// 			},
	// 		);

	// 		todo!()

	// 		// let func_ty = environment.register_type(Type::AliasTo {
	// 		// 	to: TypeId::FUNCTION_TYPE,
	// 		// 	name: None,
	// 		// 	parameters: None,
	// 		// });

	// 		// // TODO not a great place
	// 		// function.nature = FunctionKind::ClassConstructor {
	// 		// 	class_prototype: interface_type,
	// 		// 	class_constructor: todo!(),
	// 		// };

	// 		// environment.functions_on_type.insert(func_ty, function);
	// 	}
	// 	InterfaceMember::Comment(_comment) => {}
	// 	InterfaceMember::Caller { .. } => todo!(),
	// 	InterfaceMember::Rule { .. } => todo!(),
	// }
}
