use parser::{
	declarations::{classes::ClassMember, ClassDeclaration},
	functions::MethodHeader,
	PropertyKey as ParserPropertyKey,
};

use crate::{
	context::{facts::Publicity, Environment},
	features::functions::{
		function_to_property, ClassPropertiesToRegister, FunctionRegisterBehavior, GetterSetter,
	},
	synthesis::parser_property_key_to_checker_property_key,
	types::{classes::ClassValue, properties::PropertyKey, FunctionType},
	CheckingData, PropertyValue, Scope, Type, TypeId,
};

use super::{block::synthesise_block, expressions::synthesise_expression};

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
	{
		// TODO what about no name
		let _name = P::as_option_str(&class.name).unwrap().to_owned();
		// TODO type needs to be hoisted
		// let parameters =
		// 	if let Some(ref type_parameters) = class.type_parameters { todo!() } else { None };
		// TODO
		// let nominal = true;
		// let ty = Type::NamedRooted { name, parameters, nominal };
		// let class_type = checking_data.types.register_type(ty);
	}

	let extends = class.extends.as_ref().map(|extends| {
		synthesise_expression(extends, environment, checking_data, TypeId::ANY_TYPE)
	});

	// TODO prototype of extends
	let class_prototype =
		checking_data.types.register_type(Type::Object(crate::types::ObjectNature::RealDeal));

	let class_constructor = class.members.iter().find_map(|member| {
		if let ClassMember::Constructor(c) = &member.on {
			Some(c)
		} else {
			None
		}
	});

	let mut properties = Vec::new();

	// Property keys on `static` items
	let mut static_property_keys: Vec<PropertyKey<'static>> = Vec::new();

	for member in &class.members {
		match &member.on {
			ClassMember::Method(false, method) => {
				let publicity = match method.name.get_ast_ref() {
					ParserPropertyKey::Ident(_, _, true) => Publicity::Private,
					_ => Publicity::Public,
				};
				let property_key = parser_property_key_to_checker_property_key(
					method.name.get_ast_ref(),
					environment,
					checking_data,
				);

				// TODO abstract
				let (getter_setter, is_async, is_generator) = match &method.header {
					MethodHeader::Get => (GetterSetter::Getter, false, false),
					MethodHeader::Set => (GetterSetter::Setter, false, false),
					MethodHeader::Regular { is_async, generator } => {
						(GetterSetter::None, *is_async, generator.is_some())
					}
				};

				let method_ty = environment.new_function(
					checking_data,
					method,
					FunctionRegisterBehavior::ClassMethod {
						is_async,
						is_generator,
						// TODO
						super_type: None,
						// TODO
						expecting: TypeId::ANY_TYPE,
					},
				);

				let property =
					function_to_property(&getter_setter, method_ty, &mut checking_data.types);

				let position = Some(method.position.with_source(environment.get_source()));
				environment.facts.register_property(
					class_prototype,
					publicity,
					property_key,
					property,
					// Is dynamic environment
					true,
					position,
				);
			}
			ClassMember::Property(false, property) => {
				let publicity = match property.key.get_ast_ref() {
					ParserPropertyKey::Ident(_, _, true) => Publicity::Private,
					_ => Publicity::Public,
				};
				let key = parser_property_key_to_checker_property_key(
					property.key.get_ast_ref(),
					environment,
					checking_data,
				);
				// TODO restriction
				properties.push(ClassValue { publicity, key, value: property.value.as_deref() });
			}
			ClassMember::Property(true, property) => {
				let value = parser_property_key_to_checker_property_key(
					property.key.get_ast_ref(),
					environment,
					checking_data,
				);
				static_property_keys.push(value);
			}
			_ => {}
		}
	}

	// TODO abstract
	let function = if let Some(constructor) = class_constructor {
		let behavior = FunctionRegisterBehavior::Constructor {
			prototype: class_prototype,
			super_type: extends,
			properties: ClassPropertiesToRegister(properties),
		};
		environment.new_function(checking_data, constructor, behavior)
	} else {
		FunctionType::new_auto_constructor(
			class_prototype,
			ClassPropertiesToRegister(properties),
			environment,
			checking_data,
		)
	};

	let class_type = checking_data.types.new_function_type(function);

	// Static items and blocks

	// TODO ...
	static_property_keys.reverse();

	for member in &class.members {
		match &member.on {
			ClassMember::Method(true, method) => {
				let publicity_kind = match method.name.get_ast_ref() {
					ParserPropertyKey::Ident(_, _, true) => Publicity::Private,
					_ => Publicity::Public,
				};
				let behavior = FunctionRegisterBehavior::ClassMethod {
					is_async: method.header.is_async(),
					is_generator: method.header.is_generator(),
					// TODO
					super_type: None,
					// TODO
					expecting: TypeId::ANY_TYPE,
				};
				let function = environment.new_function(checking_data, method, behavior);

				let value = match method.header {
					MethodHeader::Get => PropertyValue::Getter(Box::new(function)),
					MethodHeader::Set => PropertyValue::Setter(Box::new(function)),
					MethodHeader::Regular { .. } => {
						PropertyValue::Value(checking_data.types.new_function_type(function))
					}
				};

				// (publicity_kind, property_key, PropertyOnClass::Function { method, property })
				environment.facts.register_property(
					class_type,
					publicity_kind,
					static_property_keys.pop().unwrap(),
					value,
					// TODO
					true,
					// TODO not needed right?
					None,
				);
			}
			ClassMember::Property(true, property) => {
				let publicity_kind = match property.key.get_ast_ref() {
					ParserPropertyKey::Ident(_, _, true) => Publicity::Private,
					_ => Publicity::Public,
				};
				let value = if let Some(ref value) = property.value {
					let expecting = TypeId::ANY_TYPE;
					synthesise_expression(value, environment, checking_data, expecting)
				} else {
					TypeId::UNDEFINED_TYPE
				};
				environment.facts.register_property(
					class_type,
					publicity_kind,
					static_property_keys.pop().unwrap(),
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
						synthesise_block(&block.0, environment, checking_data);
					},
				);
			}
			_ => {}
		}
	}

	class_type
}
