use parser::{
	types::interface::{InterfaceDeclaration, InterfaceMember},
	Decorated, PropertyKey as ParserPropertyKey, WithComment,
};

use crate::{
	context::{facts::Publicity, Context, Environment},
	features::functions::{self, GetterSetter, ThisValue},
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
			ParserPropertyKeyType::ClassProperty(key) => (
				if matches!(key, parser::PropertyKey::Ident(_, _, true)) {
					Publicity::Private
				} else {
					Publicity::Public
				},
				parser_property_key_to_checker_property_key(key, environment, checking_data),
			),
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
					let ty = Type::FunctionReference(function_id, ThisValue::UseParent);
					PropertyValue::Value(checking_data.types.register_type(ty))
				}
			},
			InterfaceValue::Value(value) => PropertyValue::Value(value),
		};

		// TODO: `None` position passed
		environment.facts.register_property(self.0, publicity, under, ty, false, None);
	}

	fn interface_type(&self) -> Option<TypeId> {
		Some(self.0)
	}
}

pub(super) fn synthesise_signatures<T: crate::ReadFromFS, B: SynthesiseInterfaceBehavior>(
	type_parameters: Option<&[parser::GenericTypeConstraint]>,
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
					performs,
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
						parser::functions::MethodHeader::Get(_) => GetterSetter::Getter,
						parser::functions::MethodHeader::Set(_) => GetterSetter::Setter,
						parser::functions::MethodHeader::Regular { .. } => GetterSetter::None,
					};
					let function = synthesise_function_annotation(
						type_parameters,
						parameters,
						return_type.as_ref(),
						environment,
						checking_data,
						performs.as_ref().into(),
						&position.with_source(environment.get_source()),
						behavior,
						interface_register_behavior.interface_type(),
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
					is_readonly: _,
					is_optional: _,
					position: _,
				} => {
					let value =
						synthesise_type_annotation(type_annotation, environment, checking_data);
					interface_register_behavior.register(
						ParserPropertyKeyType::ClassProperty(name),
						InterfaceValue::Value(value),
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
					performs: _,
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
					parameter: _,
					rule: _,
					matching_type: _,
					optionality: _,
					is_readonly: _,
					output_type: _,
					position,
				} => checking_data.raise_unimplemented_error(
					"interface rule",
					position.with_source(environment.get_source()),
				),
				InterfaceMember::Comment(_, _) => {}
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
				synthesise_members(signatures, environment, checking_data, &mut behavior);
			},
		);

		behavior
	} else {
		synthesise_members(signatures, environment, checking_data, &mut behavior);
		behavior
	}
}
