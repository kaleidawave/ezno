use parser::{
	types::interface::{InterfaceDeclaration, InterfaceMember},
	Decorated, PropertyKey as ParserPropertyKey, WithComment,
};

use crate::{
	context::{information::Publicity, Context, Environment},
	features::functions::{self, GetterSetter},
	synthesis::parser_property_key_to_checker_property_key,
	types::{
		properties::{PropertyKey, PropertyValue},
		FunctionType, Type,
	},
	CheckingData, TypeId,
};

use super::{
	functions::synthesise_function_annotation, type_annotations::synthesise_type_annotation,
};

fn _get_extends<T: crate::ReadFromFS>(
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
	fn register<T: crate::ReadFromFS>(
		&mut self,
		key: ParserPropertyKeyType,
		value: InterfaceValue,
		checking_data: &mut CheckingData<T, super::EznoParser>,
		environment: &mut Environment,
	);

	fn interface_type(&self) -> Option<TypeId>;
}

pub(crate) enum InterfaceValue {
	Function(FunctionType, GetterSetter),
	Value(TypeId),
	Optional(TypeId),
}

pub(crate) enum ParserPropertyKeyType<'a> {
	ClassProperty(&'a ParserPropertyKey<parser::property_key::PublicOrPrivate>),
	// ObjectProperty(&'a ParserPropertyKey<parser::property_key::AlwaysPublic>),
	Type(TypeId),
}

pub(crate) struct OnToType(pub(crate) TypeId);

impl SynthesiseInterfaceBehavior for OnToType {
	fn register<T: crate::ReadFromFS>(
		&mut self,
		key: ParserPropertyKeyType,
		value: InterfaceValue,
		checking_data: &mut CheckingData<T, super::EznoParser>,
		environment: &mut Environment,
	) {
		let (publicity, under) = match key {
			ParserPropertyKeyType::ClassProperty(key) => {
				// TODO
				let perform_side_effect_computed = true;
				(
					if matches!(
						key,
						parser::PropertyKey::Ident(
							_,
							_,
							parser::property_key::PublicOrPrivate::Private
						)
					) {
						Publicity::Private
					} else {
						Publicity::Public
					},
					parser_property_key_to_checker_property_key(
						key,
						environment,
						checking_data,
						perform_side_effect_computed,
					),
				)
			}
			// ParserPropertyKeyType::ObjectProperty(key) => (
			// 	Publicity::Public,
			// 	parser_property_key_to_checker_property_key(key, environment, checking_data),
			// ),
			ParserPropertyKeyType::Type(ty) => (Publicity::Public, PropertyKey::Type(ty)),
		};
		let ty = match value {
			InterfaceValue::Function(function, getter_setter) => match getter_setter {
				GetterSetter::Getter => PropertyValue::Getter(Box::new(function)),
				GetterSetter::Setter => PropertyValue::Setter(Box::new(function)),
				GetterSetter::None => {
					let function_id = function.id;
					checking_data.types.functions.insert(function.id, function);
					let ty = Type::FunctionReference(function_id);
					PropertyValue::Value(checking_data.types.register_type(ty))
				}
			},
			InterfaceValue::Value(value) => PropertyValue::Value(value),
			// optional properties (`?:`) is implemented here:
			InterfaceValue::Optional(value) => PropertyValue::Dependent {
				on: TypeId::BOOLEAN_TYPE,
				truthy: PropertyValue::Value(value).into(),
				otherwise: PropertyValue::Deleted.into(),
			},
		};

		// None position should be fine here
		environment.info.register_property(self.0, publicity, under, ty, false, None);
	}

	fn interface_type(&self) -> Option<TypeId> {
		Some(self.0)
	}
}

pub(super) fn synthesise_signatures<T: crate::ReadFromFS, B: SynthesiseInterfaceBehavior>(
	type_parameters: Option<&[parser::TypeParameter]>,
	extends: Option<&[parser::TypeAnnotation]>,
	signatures: &[WithComment<Decorated<InterfaceMember>>],
	mut behavior: B,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> B {
	/// TODO check members declared before
	fn synthesise_members<T: crate::ReadFromFS, B: SynthesiseInterfaceBehavior>(
		members: &[WithComment<Decorated<InterfaceMember>>],
		environment: &mut Context<crate::context::environment::Syntax<'_>>,
		checking_data: &mut CheckingData<T, super::EznoParser>,
		interface_register_behavior: &mut B,
	) {
		for member in members {
			let member = member.get_ast_ref();
			match &member.on {
				InterfaceMember::Method {
					header,
					name,
					type_parameters,
					parameters,
					return_type,
					is_optional: _,
					position,
				} => {
					// Fix for performing const annotations. TODO want to do better
					let behavior = if member
						.decorators
						.iter()
						.any(|a| a.name.first().cloned().as_deref() == Some("DoNotIncludeThis"))
					{
						functions::FunctionBehavior::ArrowFunction { is_async: header.is_async() }
					} else {
						functions::FunctionBehavior::Method {
							is_async: header.is_async(),
							is_generator: header.is_generator(),
							// TODO ...
							free_this_id: TypeId::ERROR_TYPE,
						}
					};
					let getter = match header {
						parser::functions::MethodHeader::Get => GetterSetter::Getter,
						parser::functions::MethodHeader::Set => GetterSetter::Setter,
						parser::functions::MethodHeader::Regular { .. } => GetterSetter::None,
					};

					let function = synthesise_function_annotation(
						type_parameters,
						parameters,
						return_type.as_ref(),
						environment,
						checking_data,
						&position.with_source(environment.get_source()),
						behavior,
					);

					interface_register_behavior.register(
						ParserPropertyKeyType::ClassProperty(name),
						InterfaceValue::Function(function, getter),
						checking_data,
						environment,
					);
				}
				InterfaceMember::Property {
					name,
					type_annotation,
					is_readonly,
					is_optional,
					position,
				} => {
					if *is_readonly {
						checking_data.raise_unimplemented_error(
							"readonly items",
							position.with_source(environment.get_source()),
						);
					}

					let value =
						synthesise_type_annotation(type_annotation, environment, checking_data);

					let value = if *is_optional {
						InterfaceValue::Optional(value)
					} else {
						InterfaceValue::Value(value)
					};

					interface_register_behavior.register(
						ParserPropertyKeyType::ClassProperty(name),
						value,
						checking_data,
						environment,
					);
				}
				InterfaceMember::Indexer {
					name: _,
					indexer_type,
					return_type,
					is_readonly: _,
					position: _,
				} => {
					// TODO think this is okay
					let key = synthesise_type_annotation(indexer_type, environment, checking_data);
					let value = synthesise_type_annotation(return_type, environment, checking_data);
					interface_register_behavior.register(
						ParserPropertyKeyType::Type(key),
						InterfaceValue::Value(value),
						checking_data,
						environment,
					);
				}
				InterfaceMember::Constructor {
					parameters: _,
					type_parameters: _,
					return_type: _,
					is_readonly: _,
					position,
				} => checking_data.raise_unimplemented_error(
					"interface constructor",
					position.with_source(environment.get_source()),
				),
				InterfaceMember::Caller {
					parameters: _,
					type_parameters: _,
					return_type: _,
					is_readonly: _,
					position,
				} => checking_data.raise_unimplemented_error(
					"interface caller",
					position.with_source(environment.get_source()),
				),
				InterfaceMember::Rule {
					parameter,
					rule,
					matching_type,
					as_type: _,
					optionality: _,
					is_readonly: _,
					output_type,
					position: _,
				} => {
					// TODO WIP
					let to = match rule {
						parser::types::interface::TypeRule::In => {
							crate::utilities::notify!("TODO");
							TypeId::ANY_TYPE
						}
						parser::types::interface::TypeRule::InKeyOf => todo!(),
					};

					// TODO
					let _parameter = checking_data.types.register_type(Type::RootPolyType(
						crate::types::PolyNature::FunctionGeneric {
							name: parameter.clone(),
							eager_fixed: to,
						},
					));

					let key = synthesise_type_annotation(matching_type, environment, checking_data);
					let value = synthesise_type_annotation(output_type, environment, checking_data);

					interface_register_behavior.register(
						ParserPropertyKeyType::Type(key),
						InterfaceValue::Value(value),
						checking_data,
						environment,
					);
				}
				InterfaceMember::Comment { .. } => {}
			}
		}
	}

	if type_parameters.is_some() || extends.is_some() {
		let interface_type = behavior.interface_type().unwrap();

		environment.new_lexical_environment_fold_into_parent(
			crate::Scope::InterfaceEnvironment { this_constraint: TypeId::ERROR_TYPE },
			checking_data,
			|environment, checking_data| {
				let parameter_types =
					checking_data.types.get_type_by_id(interface_type).get_parameters();

				if let Some(parameters) = type_parameters {
					for (parameter, ty) in parameters.iter().zip(parameter_types.iter().flatten()) {
						if let Some(ref extends) = parameter.extends {
							let extends =
								synthesise_type_annotation(extends, environment, checking_data);

							checking_data
								.types
								.modify_interface_type_parameter_constraint(*ty, extends);
						}

						// TODO set constraint by modifying type
						environment.named_types.insert(parameter.name.clone(), *ty);
					}
				}

				// Afterwards so have access to generics
				if let Some(extends) = extends {
					let mut iter = extends.iter();

					// TODO generics
					let mut extends = synthesise_type_annotation(
						iter.next().unwrap(),
						environment,
						checking_data,
					);

					for ta in iter {
						let ty = synthesise_type_annotation(ta, environment, checking_data);
						extends = checking_data.types.new_and_type(extends, ty).unwrap();
					}

					checking_data.types.set_extends_on_interface(interface_type, extends);
				}

				synthesise_members(signatures, environment, checking_data, &mut behavior);
			},
		);

		behavior
	} else {
		synthesise_members(signatures, environment, checking_data, &mut behavior);
		behavior
	}
}
