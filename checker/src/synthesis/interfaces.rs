use parser::{
	types::interface::{InterfaceDeclaration, InterfaceMember},
	Decorated, PropertyKey,
};

use crate::{
	context::{
		facts::PublicityKind,
		Environment, {Context, ContextType},
	},
	synthesis::property_key_as_type,
	types::FunctionKind,
	types::{poly_types::GenericTypeParameter, properties::Property, FunctionType, Type},
	CheckingData, TypeId,
};

use super::{
	classes::type_generic_type_constraints, functions::type_function_reference,
	type_annotations::synthesise_type_annotation,
};

fn get_extends<T: crate::ReadFromFS>(
	interface: &InterfaceDeclaration,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	interface_type: TypeId,
) {
	if let Some([reference, others @ ..]) = interface.extends.as_deref() {
		let mut ty = synthesise_type_annotation(reference, environment, checking_data);
		for reference in others {
			let rhs = synthesise_type_annotation(reference, environment, checking_data);
			ty = checking_data.types.register_type(Type::And(ty, rhs));
		}

		environment.bases.connect_extends(interface_type, ty);
	}
}

pub(crate) trait SynthesiseInterfaceBehavior {
	fn register<T: crate::ReadFromFS, S: ContextType>(
		&mut self,
		key: PropertyOrType,
		value: InterfaceValue,
		checking_data: &mut CheckingData<T, super::EznoParser>,
		environment: &mut Context<S>,
	);

	fn interface_type(&self) -> Option<TypeId>;
}

pub(crate) enum InterfaceValue {
	Function(FunctionType),
	Value(TypeId),
}

pub(crate) enum PropertyOrType<'a> {
	ClassProperty(&'a PropertyKey<parser::property_key::PublicOrPrivate>),
	ObjectProperty(&'a PropertyKey<parser::property_key::AlwaysPublic>),
	Type(TypeId),
}

pub(crate) struct OnToType(pub(crate) TypeId);

impl SynthesiseInterfaceBehavior for OnToType {
	fn register<T: crate::ReadFromFS, S: ContextType>(
		&mut self,
		key: PropertyOrType,
		value: InterfaceValue,
		checking_data: &mut CheckingData<T, super::EznoParser>,
		environment: &mut Context<S>,
	) {
		let (under, publicity) = match key {
			PropertyOrType::ClassProperty(key) => {
				let publicity = if matches!(key, parser::PropertyKey::Ident(_, _, true)) {
					PublicityKind::Private
				} else {
					PublicityKind::Public
				};
				(property_key_as_type(key, environment, &mut checking_data.types), publicity)
			}
			PropertyOrType::ObjectProperty(key) => (
				property_key_as_type(key, environment, &mut checking_data.types),
				PublicityKind::Public,
			),
			PropertyOrType::Type(ty) => (ty, PublicityKind::Public),
		};
		let ty = match value {
			InterfaceValue::Function(function) => {
				// TODO constructor
				if let FunctionKind::Method { get_set: Some(get_set) } = function.kind {
					match get_set {
						crate::types::GetSet::Get => Property::Getter(Box::new(function)),
						crate::types::GetSet::Set => Property::Setter(Box::new(function)),
					}
				} else {
					let ty = Type::FunctionReference(
						function.id,
						crate::behavior::functions::ThisValue::UseParent,
					);
					checking_data.types.functions.insert(function.id, function);
					Property::Value(checking_data.types.register_type(ty))
				}
			}
			InterfaceValue::Value(value) => Property::Value(value),
		};

		// TODO: `None` position passed
		environment.facts.register_property(self.0, under, ty, false, publicity, None)
	}

	fn interface_type(&self) -> Option<TypeId> {
		Some(self.0)
	}
}

pub(super) fn synthesise_signatures<T: crate::ReadFromFS, B: SynthesiseInterfaceBehavior>(
	type_parameters: Option<&[parser::GenericTypeConstraint]>,
	signatures: &[Decorated<InterfaceMember>],
	mut behavior: B,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> B {
	/// TODO check members declared before
	fn synthesize_members<T: crate::ReadFromFS, B: SynthesiseInterfaceBehavior>(
		members: &[Decorated<InterfaceMember>],
		environment: &mut Context<crate::context::environment::Syntax<'_>>,
		checking_data: &mut CheckingData<T, super::EznoParser>,
		behavior: &mut B,
	) {
		for member in members {
			match &member.on {
				InterfaceMember::Method {
					kind,
					name,
					type_parameters,
					parameters,
					return_type,
					is_optional,
					performs,
					position,
				} => {
					let kind = FunctionKind::Method {
						get_set: kind.as_ref().map(|kind| match kind {
							parser::MethodHeader::Get(_) => crate::types::GetSet::Get,
							parser::MethodHeader::Set(_) => crate::types::GetSet::Set,
							parser::MethodHeader::GeneratorStar(_, _)
							| parser::MethodHeader::Generator(_, _) => todo!(),
							parser::MethodHeader::Async(_) => todo!(),
						}),
					};
					let function = type_function_reference(
						type_parameters,
						parameters,
						return_type.as_ref(),
						environment,
						checking_data,
						performs.as_ref().into(),
						position.clone().with_source(environment.get_source()),
						kind,
						behavior.interface_type(),
					);
					behavior.register(
						PropertyOrType::ClassProperty(name),
						InterfaceValue::Function(function),
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
					let value =
						synthesise_type_annotation(type_annotation, environment, checking_data);
					behavior.register(
						PropertyOrType::ClassProperty(name),
						InterfaceValue::Value(value),
						checking_data,
						environment,
					)
				}
				InterfaceMember::Indexer {
					name,
					indexer_type,
					return_type,
					is_readonly,
					position,
				} => {
					// TODO think this is okay
					let key = synthesise_type_annotation(indexer_type, environment, checking_data);
					crate::utils::notify!("Indexer {:?}", key);
					let value = synthesise_type_annotation(return_type, environment, checking_data);
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
				} => checking_data.raise_unimplemented_error(
					"interface constructor",
					position.clone().with_source(environment.get_source()),
				),
				InterfaceMember::Caller {
					parameters,
					type_parameters,
					return_type,
					is_readonly,
					position,
				} => checking_data.raise_unimplemented_error(
					"interface caller",
					position.clone().with_source(environment.get_source()),
				),
				InterfaceMember::Rule {
					parameter,
					rule,
					matching_type,
					optionality,
					is_readonly,
					output_type,
					position,
				} => checking_data.raise_unimplemented_error(
					"interface rule",
					position.clone().with_source(environment.get_source()),
				),
				InterfaceMember::Comment(..) => {}
			}
		}
	}

	if let Some(parameters) = type_parameters {
		let parameter_types = behavior
			.interface_type()
			.map(|id| checking_data.types.get_type_by_id(id).get_parameters())
			.unwrap();

		environment.new_lexical_environment_fold_into_parent(
			crate::Scope::InterfaceEnvironment { this_constraint: TypeId::ERROR_TYPE },
			checking_data,
			|environment, checking_data| {
				for (parameter, ty) in parameters.iter().zip(parameter_types.iter().flatten()) {
					// TODO set constraint by modifying type
					environment.named_types.insert(parameter.name().to_owned(), *ty);
				}
				synthesize_members(signatures, environment, checking_data, &mut behavior);
			},
		);

		behavior
	} else {
		synthesize_members(signatures, environment, checking_data, &mut behavior);
		behavior
	}
}

/// TODO overloads
pub(super) fn type_interface_member<T: crate::ReadFromFS, S: ContextType>(
	member: &Decorated<InterfaceMember>,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T, super::EznoParser>,
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
	// 		// 	// let result = synthesise_block(body, environment, checking_data);
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
	// 		let value_ty = synthesise_type_annotation(type_annotation, environment, checking_data);

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
