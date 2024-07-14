use parser::{
	types::interface::InterfaceMember, Decorated, PropertyKey as ParserPropertyKey, WithComment,
};
use source_map::SpanWithSource;

use crate::{
	context::{Context, Environment},
	features::functions::{self, GetterSetter},
	synthesis::parser_property_key_to_checker_property_key,
	types::{
		properties::{PropertyKey, PropertyValue, Publicity},
		FunctionType, Type,
	},
	CheckingData, Scope, TypeId,
};

use super::{
	functions::synthesise_function_annotation, type_annotations::synthesise_type_annotation,
};

pub(crate) trait SynthesiseInterfaceBehavior {
	fn register<T: crate::ReadFromFS>(
		&mut self,
		key: InterfaceKey,
		value: InterfaceValue,
		checking_data: &mut CheckingData<T, super::EznoParser>,
		environment: &mut Environment,
		position: SpanWithSource,
	);

	fn interface_type(&self) -> Option<TypeId>;
}

pub(crate) enum InterfaceKey<'a> {
	ClassProperty(&'a ParserPropertyKey<parser::property_key::PublicOrPrivate>),
	// ObjectProperty(&'a ParserPropertyKey<parser::property_key::AlwaysPublic>),
	Type(TypeId),
}

pub(crate) enum InterfaceValue {
	Function(FunctionType, GetterSetter),
	Value(TypeId),
	Optional(TypeId),
}

pub(crate) struct OnToType(pub(crate) TypeId);

impl SynthesiseInterfaceBehavior for OnToType {
	fn register<T: crate::ReadFromFS>(
		&mut self,
		key: InterfaceKey,
		value: InterfaceValue,
		checking_data: &mut CheckingData<T, super::EznoParser>,
		environment: &mut Environment,
		position: SpanWithSource,
	) {
		let (publicity, under) = match key {
			InterfaceKey::ClassProperty(key) => {
				// TODO
				let perform_side_effect_computed = true;
				(
					if key.is_private() { Publicity::Private } else { Publicity::Public },
					parser_property_key_to_checker_property_key(
						key,
						environment,
						checking_data,
						perform_side_effect_computed,
					),
				)
			}
			InterfaceKey::Type(ty) => (Publicity::Public, PropertyKey::Type(ty)),
		};
		let ty = match value {
			InterfaceValue::Function(function, getter_setter) => match getter_setter {
				GetterSetter::Getter => {
					let id = function.id;
					checking_data.types.functions.insert(id, function);
					PropertyValue::Getter(id)
				}
				GetterSetter::Setter => {
					let id = function.id;
					checking_data.types.functions.insert(id, function);
					PropertyValue::Setter(id)
				}
				GetterSetter::None => {
					let function_id = function.id;
					checking_data.types.functions.insert(function.id, function);
					let ty = Type::FunctionReference(function_id);
					PropertyValue::Value(checking_data.types.register_type(ty))
				}
			},
			InterfaceValue::Value(value) => PropertyValue::Value(value),
			// optional properties (`?:`) is implemented here:
			InterfaceValue::Optional(value) => PropertyValue::ConditionallyExists {
				on: TypeId::OPEN_BOOLEAN_TYPE,
				truthy: PropertyValue::Value(value).into(),
			},
		};

		// None position should be fine here
		environment.info.register_property(self.0, publicity, under, ty, false, position);
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
					is_optional,
					position,
				} => {
					if *is_optional {
						checking_data.raise_unimplemented_error(
							"is_optional interface method",
							position.with_source(environment.get_source()),
						);
					}

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

					let position_with_source = position.with_source(environment.get_source());

					let function = synthesise_function_annotation(
						type_parameters,
						parameters,
						return_type.as_ref(),
						environment,
						checking_data,
						&position_with_source,
						behavior,
					);

					interface_register_behavior.register(
						InterfaceKey::ClassProperty(name),
						InterfaceValue::Function(function, getter),
						checking_data,
						environment,
						position_with_source,
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
							"readonly interface property",
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
						InterfaceKey::ClassProperty(name),
						value,
						checking_data,
						environment,
						position.with_source(environment.get_source()),
					);
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
					let value = synthesise_type_annotation(return_type, environment, checking_data);

					if *is_readonly {
						checking_data.raise_unimplemented_error(
							"readonly interface index",
							position.with_source(environment.get_source()),
						);
					}

					// TODO WIP
					let value = if matches!(key, TypeId::NUMBER_TYPE | TypeId::STRING_TYPE) {
						crate::utilities::notify!(
							"Special case for [number] or [string], making optional {}",
							name
						);
						InterfaceValue::Optional(value)
					} else {
						// Not doing for generics + Symbols
						InterfaceValue::Value(value)
					};

					interface_register_behavior.register(
						InterfaceKey::Type(key),
						value,
						checking_data,
						environment,
						position.with_source(environment.get_source()),
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
					matching_type,
					as_type,
					optionality,
					is_readonly,
					output_type,
					position,
				} => {
					// For mapped types: https://www.typescriptlang.org/docs/handbook/2/mapped-types.html

					let matching_type =
						synthesise_type_annotation(matching_type, environment, checking_data);

					let (key, value) = {
						// TODO special scope here
						let mut sub_environment =
							environment.new_lexical_environment(Scope::Block {});
						let parameter_type = checking_data.types.register_type(Type::RootPolyType(
							crate::types::PolyNature::MappedGeneric {
								name: parameter.clone(),
								eager_fixed: matching_type,
							},
						));
						sub_environment.named_types.insert(parameter.clone(), parameter_type);

						let key = if let Some(as_type) = as_type {
							synthesise_type_annotation(as_type, &mut sub_environment, checking_data)
						} else {
							parameter_type
						};

						let value = synthesise_type_annotation(
							output_type,
							&mut sub_environment,
							checking_data,
						);

						environment
							.info
							.current_properties
							.extend(sub_environment.info.current_properties);

						(key, value)
					};

					let value = match optionality {
						parser::types::interface::Optionality::Default => {
							InterfaceValue::Value(value)
						}
						parser::types::interface::Optionality::Optional => {
							InterfaceValue::Optional(value)
						}
						parser::types::interface::Optionality::Required => {
							let position = position.with_source(environment.get_source());
							checking_data
								.raise_unimplemented_error("mapped type required", position);
							InterfaceValue::Value(value)
						}
					};

					match is_readonly {
						parser::types::interface::MappedReadonlyKind::Negated => {
							// Hmm this one is tricky
							let position = position.with_source(environment.get_source());
							checking_data.raise_unimplemented_error(
								"mapped type negated readonly",
								position,
							);
						}
						parser::types::interface::MappedReadonlyKind::Always => {
							let position = position.with_source(environment.get_source());
							checking_data
								.raise_unimplemented_error("mapped type readonly", position);
						}
						parser::types::interface::MappedReadonlyKind::False => {}
					}

					interface_register_behavior.register(
						InterfaceKey::Type(key),
						value,
						checking_data,
						environment,
						position.with_source(environment.get_source()),
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
