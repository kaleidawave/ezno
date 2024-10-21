use parser::{
	types::interface::InterfaceMember, Decorated, PropertyKey as ParserPropertyKey, WithComment,
};
use source_map::SpanWithSource;

use crate::{
	context::{Context, Environment},
	features::functions::GetterSetter,
	synthesis::parser_property_key_to_checker_property_key,
	types::{
		calling::Callable,
		helpers::references_key_of,
		properties::{Descriptor, PropertyKey, PropertyValue, Publicity},
		FunctionType, Type,
	},
	CheckingData, Scope, TypeId,
};

use super::{
	functions::synthesise_function_annotation, type_annotations::synthesise_type_annotation,
};

/// inverse of readonly. Closer to JS semantics
pub struct Writable(pub TypeId);

impl Writable {
	fn from_readonly(is_readonly: bool) -> Self {
		Self(if is_readonly { TypeId::FALSE } else { TypeId::TRUE })
	}
}

/// inverse of optional. Closer to implementation
pub struct IsDefined(pub TypeId);

impl IsDefined {
	fn from_optionality(is_optional: bool) -> Self {
		Self(if is_optional { TypeId::OPEN_BOOLEAN_TYPE } else { TypeId::TRUE })
	}
}

pub(crate) trait SynthesiseInterfaceBehavior {
	fn register<T: crate::ReadFromFS>(
		&mut self,
		key: InterfaceKey,
		value: (InterfaceValue, IsDefined, Writable),
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
	Function(Box<FunctionType>, Option<GetterSetter>),
	Value(TypeId),
}

pub(crate) struct OnToType(pub(crate) TypeId);

fn register<T: crate::ReadFromFS>(
	key: &InterfaceKey,
	(value, always_defined, writable): (InterfaceValue, IsDefined, Writable),
	checking_data: &mut CheckingData<T, super::EznoParser>,
	environment: &mut Environment,
	_position: SpanWithSource,
) -> (Publicity, PropertyKey<'static>, PropertyValue) {
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
		InterfaceKey::Type(ty) => (Publicity::Public, PropertyKey::Type(*ty)),
	};
	let value = match value {
		InterfaceValue::Function(function, getter_setter) => match getter_setter {
			Some(GetterSetter::Getter) => PropertyValue::Getter(Callable::new_from_function(
				*function,
				&mut checking_data.types,
			)),
			Some(GetterSetter::Setter) => PropertyValue::Setter(Callable::new_from_function(
				*function,
				&mut checking_data.types,
			)),
			None => {
				let function_id = function.id;
				checking_data.types.functions.insert(function.id, *function);
				let ty = Type::FunctionReference(function_id);
				PropertyValue::Value(checking_data.types.register_type(ty))
			}
		},
		InterfaceValue::Value(value) => PropertyValue::Value(value),
	};
	let value = if let Writable(TypeId::TRUE) = writable {
		value
	} else {
		let descriptor = Descriptor {
			writable: writable.0,
			enumerable: TypeId::TRUE,
			configurable: TypeId::TRUE,
		};
		PropertyValue::Configured { on: Box::new(value), descriptor }
	};
	// optional properties (`?:`) is implemented here:
	let value = if let IsDefined(TypeId::TRUE) = always_defined {
		value
	} else {
		// crate::utilities::notify!("always_defined.0 {:?}", always_defined.0);
		PropertyValue::ConditionallyExists { condition: always_defined.0, truthy: Box::new(value) }
	};
	(publicity, under, value)
}

impl SynthesiseInterfaceBehavior for OnToType {
	fn register<T: crate::ReadFromFS>(
		&mut self,
		key: InterfaceKey,
		(value, always_defined, writable): (InterfaceValue, IsDefined, Writable),
		checking_data: &mut CheckingData<T, super::EznoParser>,
		environment: &mut Environment,
		position: SpanWithSource,
	) {
		let (publicity, under, value) =
			register(&key, (value, always_defined, writable), checking_data, environment, position);
		environment.info.register_property_on_type(self.0, publicity, under, value);
	}

	fn interface_type(&self) -> Option<TypeId> {
		Some(self.0)
	}
}

pub struct PropertiesList(pub crate::types::properties::Properties);

impl SynthesiseInterfaceBehavior for PropertiesList {
	fn register<T: crate::ReadFromFS>(
		&mut self,
		key: InterfaceKey,
		(value, always_defined, writable): (InterfaceValue, IsDefined, Writable),
		checking_data: &mut CheckingData<T, super::EznoParser>,
		environment: &mut Environment,
		position: SpanWithSource,
	) {
		let (publicity, under, value) =
			register(&key, (value, always_defined, writable), checking_data, environment, position);
		self.0.push((publicity, under, value));
	}

	fn interface_type(&self) -> Option<TypeId> {
		None
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
					// Fix for performing const annotations. TODO want to do better
					let behavior = if member
						.decorators
						.iter()
						.any(|a| a.name.first().cloned().as_deref() == Some("DoNotIncludeThis"))
					{
						crate::types::functions::FunctionBehavior::ArrowFunction {
							is_async: header.is_async(),
						}
					} else {
						crate::types::functions::FunctionBehavior::Method {
							is_async: header.is_async(),
							is_generator: header.is_generator(),
							// TODO ...
							free_this_id: TypeId::UNIMPLEMENTED_ERROR_TYPE,
							name: TypeId::EMPTY_STRING,
						}
					};
					let getter = match header {
						parser::functions::MethodHeader::Get => Some(GetterSetter::Getter),
						parser::functions::MethodHeader::Set => Some(GetterSetter::Setter),
						parser::functions::MethodHeader::Regular { .. } => None,
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
						(
							InterfaceValue::Function(Box::new(function), getter),
							IsDefined::from_optionality(*is_optional),
							Writable::from_readonly(false),
						),
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
					let value =
						synthesise_type_annotation(type_annotation, environment, checking_data);

					interface_register_behavior.register(
						InterfaceKey::ClassProperty(name),
						(
							InterfaceValue::Value(value),
							IsDefined::from_optionality(*is_optional),
							Writable::from_readonly(*is_readonly),
						),
						checking_data,
						environment,
						position.with_source(environment.get_source()),
					);
				}
				InterfaceMember::Indexer {
					name: _,
					indexer_type,
					return_type,
					is_readonly,
					position,
				} => {
					// TODO think this is okay
					let key = synthesise_type_annotation(indexer_type, environment, checking_data);
					let value = synthesise_type_annotation(return_type, environment, checking_data);

					let value = InterfaceValue::Value(value);

					interface_register_behavior.register(
						InterfaceKey::Type(key),
						(
							value,
							IsDefined::from_optionality(false),
							Writable::from_readonly(*is_readonly),
						),
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
								extends: matching_type,
							},
						));
						sub_environment.named_types.insert(parameter.clone(), parameter_type);

						let key = if let Some(as_type) = as_type {
							synthesise_type_annotation(as_type, &mut sub_environment, checking_data)
						} else {
							parameter_type
						};

						// crate::utilities::notify!("output_type {:?}", output_type);

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

					// wrg to `references_key_of`, it is TSC behavior for the conditionality and
					// writable of the property to be based on the argument from keyof.
					// if keyof is not present this argument is not set and so breaks things.
					// This `keyof` could be collected during synthesising but doing here as easier
					// + edge cases around alias and generics

					let always_defined = match optionality {
						parser::types::interface::Optionality::Default => {
							if references_key_of(key, &checking_data.types) {
								IsDefined(TypeId::NON_OPTIONAL_KEY_ARGUMENT)
							} else {
								IsDefined::from_optionality(false)
							}
						}
						parser::types::interface::Optionality::Optional => {
							IsDefined::from_optionality(true)
						}
						parser::types::interface::Optionality::Required => {
							IsDefined::from_optionality(false)
						}
					};

					let writable = match is_readonly {
						parser::types::interface::MappedReadonlyKind::Negated => {
							Writable::from_readonly(false)
						}
						parser::types::interface::MappedReadonlyKind::Always => {
							Writable::from_readonly(true)
						}
						parser::types::interface::MappedReadonlyKind::False => {
							if references_key_of(key, &checking_data.types) {
								crate::utilities::notify!("Here");
								Writable(TypeId::WRITABLE_KEY_ARGUMENT)
							} else {
								Writable::from_readonly(false)
							}
						}
					};

					interface_register_behavior.register(
						InterfaceKey::Type(key),
						(InterfaceValue::Value(value), always_defined, writable),
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
			crate::Scope::InterfaceEnvironment {
				this_constraint: TypeId::UNIMPLEMENTED_ERROR_TYPE,
			},
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
						extends = checking_data.types.new_and_type(extends, ty);
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
