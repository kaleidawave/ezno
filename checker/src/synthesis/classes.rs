use std::iter;

use parser::{
	declarations::{classes::ClassMember, ClassDeclaration},
	property_key::PublicOrPrivate,
	Decorated, Expression, GenericTypeConstraint, MethodHeader, TypeAnnotation,
};

use crate::{
	behavior::functions::FunctionRegisterBehavior,
	context::{
		environment,
		facts::PublicityKind,
		Environment, {Context, ContextType},
	},
	synthesis::{
		parser_property_key_to_checker_property_key, type_annotations::synthesise_type_annotation,
	},
	types::{
		classes::{PropertyFunctionProperty, PropertyOnClass},
		poly_types::GenericTypeParameters,
		FunctionType, SynthesisedParameters,
	},
	CheckingData, PropertyValue, Scope, Type, TypeId,
};

use super::{block::synthesise_block, expressions::synthesise_expression, EznoParser};

/// Doesn't have any metadata yet
///
/// Returns the constructor
pub(super) fn synthesise_class_declaration<
	T: crate::ReadFromFS,
	P: parser::ExpressionOrStatementPosition,
>(
	class: &ClassDeclaration<P>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> TypeId {
	// TODO type needs to be hoisted
	let parameters =
		if let Some(ref type_parameters) = class.type_parameters { todo!() } else { None };

	// TODO what about no name
	let name = P::as_option_str(&class.name).unwrap().to_owned();

	let nominal = true;
	let ty = Type::NamedRooted { name, parameters, nominal };

	let class_type = checking_data.types.register_type(ty);
	let extends = if let Some(ref extends) = class.extends {
		// TODO temp
		// let expecting = TypeId::ANY_TYPE;
		Some(synthesise_expression(&**extends, environment, checking_data, TypeId::ANY_TYPE))
	} else {
		None
	};

	let mut class_constructor = class.members.iter().find_map(|member| {
		if let ClassMember::Constructor(c) = &member.on {
			Some(c)
		} else {
			None
		}
	});

	// TODO also synthesise type restriction
	let property_keys = class
		.members
		.iter()
		.map(|member| match &member.on {
			ClassMember::StaticBlock(_)
			| ClassMember::Comment(..)
			| ClassMember::Constructor(_) => None,
			ClassMember::Method(_, method) => Some(parser_property_key_to_checker_property_key(
				method.name.get_ast_ref(),
				environment,
				checking_data,
			)),
			ClassMember::Property(_, property) => {
				Some(parser_property_key_to_checker_property_key(
					property.key.get_ast_ref(),
					environment,
					checking_data,
				))
			}
		})
		.collect::<Vec<_>>();

	let not_static_properties = class
		.members
		.iter()
		.zip(property_keys.iter().cloned())
		.flat_map(|(member, property_key)| match &member.on {
			ClassMember::Method(None, method) => {
				let publicity_kind = match method.name.get_ast_ref() {
					parser::PropertyKey::Ident(_, _, true) => PublicityKind::Private,
					_ => PublicityKind::Public,
				};
				let property = match &method.header {
					Some(MethodHeader::Get(_)) => PropertyFunctionProperty::Get,
					Some(MethodHeader::Set(_)) => PropertyFunctionProperty::Set,
					Some(
						MethodHeader::Generator(is_async, _)
						| MethodHeader::GeneratorStar(is_async, _),
					) => PropertyFunctionProperty::Standard {
						is_async: is_async.is_some(),
						is_generator: true,
					},
					Some(MethodHeader::Async(_)) => {
						PropertyFunctionProperty::Standard { is_async: true, is_generator: false }
					}
					None => {
						PropertyFunctionProperty::Standard { is_async: false, is_generator: false }
					}
				};
				Some((
					publicity_kind,
					property_key.unwrap(),
					PropertyOnClass::Function { method, property },
				))
			}
			ClassMember::Property(None, property) => {
				let publicity_kind = match property.key.get_ast_ref() {
					parser::PropertyKey::Ident(_, _, true) => PublicityKind::Private,
					_ => PublicityKind::Public,
				};
				Some((
					publicity_kind,
					property_key.unwrap(),
					PropertyOnClass::Expression(property.value.as_ref().map(|a| &**a)),
				))
			}
			_ => None,
		})
		.collect::<Vec<_>>();

	// TODO abstract
	let function = if let Some(constructor) = class_constructor {
		let behavior = FunctionRegisterBehavior::Constructor {
			prototype: TypeId::ERROR_TYPE,
			super_type: extends,
			properties: not_static_properties,
		};
		environment.new_function(checking_data, constructor, behavior)
	} else {
		FunctionType::new_auto_constructor(
			TypeId::ERROR_TYPE,
			not_static_properties,
			environment,
			checking_data,
		)
	};

	let class_type = checking_data.types.new_function_type(function);

	// Static items and blocks
	class.members.iter().zip(property_keys.iter().cloned()).for_each(|(member, property_key)| {
		match &member.on {
			ClassMember::Method(Some(_), method) => {
				let publicity_kind = match method.name.get_ast_ref() {
					parser::PropertyKey::Ident(_, _, true) => PublicityKind::Private,
					_ => PublicityKind::Public,
				};
				let (is_async, is_generator) = match &method.header {
					None | Some(MethodHeader::Set(_)) | Some(MethodHeader::Get(_)) => {
						(false, false)
					}
					Some(
						MethodHeader::Generator(is_async, _)
						| MethodHeader::GeneratorStar(is_async, _),
					) => (is_async.is_some(), true),
					Some(MethodHeader::Async(_)) => (true, false),
				};
				let behavior = FunctionRegisterBehavior::ClassMethod {
					is_async,
					is_generator,
					// TODO
					super_type: None,
				};
				let function = environment.new_function(checking_data, method, behavior);

				let value = match method.header {
					Some(MethodHeader::Get(_)) => PropertyValue::Getter(Box::new(function)),
					Some(MethodHeader::Set(_)) => PropertyValue::Setter(Box::new(function)),
					_ => PropertyValue::Value(checking_data.types.new_function_type(function)),
				};

				crate::utils::notify!("Here");

				// (publicity_kind, property_key, PropertyOnClass::Function { method, property })
				environment.facts.register_property(
					class_type,
					publicity_kind,
					property_key.unwrap(),
					value,
					// TODO
					true,
					// TODO not needed right?
					None,
				);
			}
			ClassMember::Property(Some(_), property) => {
				let publicity_kind = match property.key.get_ast_ref() {
					parser::PropertyKey::Ident(_, _, true) => PublicityKind::Private,
					_ => PublicityKind::Public,
				};
				let value = if let Some(ref value) = property.value {
					let expecting = TypeId::ANY_TYPE;
					synthesise_expression(&**value, environment, checking_data, expecting)
				} else {
					TypeId::UNDEFINED_TYPE
				};
				environment.facts.register_property(
					class_type,
					publicity_kind,
					property_key.unwrap(),
					PropertyValue::Value(value),
					// TODO
					true,
					// TODO not needed right?
					None,
				);
			}
			ClassMember::StaticBlock(block) => {
				environment.new_lexical_environment_fold_into_parent(
					Scope::StaticBlock {},
					checking_data,
					|environment, checking_data| {
						synthesise_block(&block.0, environment, checking_data)
					},
				);
			}
			_ => {}
		}
	});

	class_type
}
