use parser::{
	declarations::{classes::ClassMember, ClassDeclaration},
	functions::MethodHeader,
	ASTNode, Expression, PropertyKey as ParserPropertyKey, StatementPosition,
};

use crate::{
	context::{information::InformationChain, Environment, VariableRegisterArguments},
	diagnostics::TypeCheckError,
	features::functions::{
		function_to_property, synthesise_function, ClassPropertiesToRegister, FunctionBehavior,
		FunctionRegisterBehavior, GetterSetter, SynthesisableFunction,
	},
	synthesis::{
		definitions::get_internal_function_effect_from_decorators,
		functions::{build_overloaded_function, synthesise_shape},
		parser_property_key_to_checker_property_key,
		type_annotations::synthesise_type_annotation,
		variables::register_variable_identifier,
	},
	types::{
		classes::ClassValue,
		properties::{PropertyKey, Publicity},
		FunctionType, PolyNature,
	},
	CheckingData, FunctionId, PropertyValue, Scope, Type, TypeId,
};

use super::{block::synthesise_block, expressions::synthesise_expression};

/// Doesn't have any metadata yet
///
/// Returns the constructor for expressions!
///
/// TODO this duplicates work done during lifting
pub(super) fn synthesise_class_declaration<
	T: crate::ReadFromFS,
	P: parser::ExpressionOrStatementPosition + super::StatementOrExpressionVariable,
>(
	class: &ClassDeclaration<P>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> TypeId {
	let is_declare = class.name.is_declare();
	let name = P::as_option_str(&class.name).map_or_else(String::new, str::to_owned);

	{
		// TODO what about no name
		// TODO type needs to be hoisted
		// let parameters =
		// 	if let Some(ref type_parameters) = class.type_parameters { todo!() } else { None };
		// TODO
		// let nominal = true;
		// let ty = Type::NamedRooted { name, parameters, nominal };
		// let class_type = checking_data.types.register_type(ty);
	}

	// crate::utilities::notify!("hmm {:?}", (&checking_data.local_type_mappings.types_to_types, class.position));

	let existing_id =
		checking_data.local_type_mappings.types_to_types.get_exact(class.position).copied();

	// Will leak hoisted properties on existing class ...?
	let class_prototype = if let Some(existing_id) = existing_id {
		existing_id
	} else {
		// For classes in expression position
		crate::utilities::notify!("TODO class expression parameters");
		checking_data.types.register_type(Type::Class { name: name.clone(), parameters: None })
	};

	let extends = class.extends.as_ref().map(|extends| {
		let ty = synthesise_expression(extends, environment, checking_data, TypeId::ANY_TYPE);

		if let TypeId::NULL_TYPE = ty {
			checking_data.raise_unimplemented_error(
				"extends `null` edge case",
				extends.get_position().with_source(environment.get_source()),
			);
		}
		ty
	});

	let class_constructor = class.members.iter().find_map(|member| {
		if let ClassMember::Constructor(c) = &member.on {
			Some((&member.decorators, c))
		} else {
			None
		}
	});

	// From table here https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes/extends#description
	// TODO explain that no prototype => prototype = Function.prototype
	if let Some(extends) = extends {
		environment.info.prototypes.insert(class_prototype, extends);
		let copied =
			environment.get_chain_of_info().find_map(|info| info.prototypes.get(&extends)).copied();
		if let Some(extends_prototype) = copied {
			environment.info.prototypes.insert(class_prototype, extends_prototype);
		}
	}

	let mut properties = Vec::new();

	// Property keys on `static` items
	let mut static_property_keys: Vec<PropertyKey<'static>> = Vec::new();

	for member in &class.members {
		match &member.on {
			ClassMember::Method(false, method) => {
				let publicity = match method.name.get_ast_ref() {
					ParserPropertyKey::Identifier(
						_,
						_,
						parser::property_key::PublicOrPrivate::Private,
					) => Publicity::Private,
					_ => Publicity::Public,
				};

				let property_key = parser_property_key_to_checker_property_key(
					method.name.get_ast_ref(),
					environment,
					checking_data,
					true,
				);

				// TODO abstract
				let (getter_setter, is_async, is_generator) = match &method.header {
					MethodHeader::Get => (GetterSetter::Getter, false, false),
					MethodHeader::Set => (GetterSetter::Setter, false, false),
					MethodHeader::Regular { is_async, generator } => {
						(GetterSetter::None, *is_async, generator.is_some())
					}
				};

				let internal_marker = if let (true, ParserPropertyKey::Identifier(name, _, _)) =
					(is_declare, method.name.get_ast_ref())
				{
					get_internal_function_effect_from_decorators(
						&member.decorators,
						name,
						environment,
					)
				} else {
					None
				};

				let behavior = FunctionRegisterBehavior::ClassMethod {
					is_async,
					is_generator,
					// TODO
					super_type: None,
					// TODO
					expecting: TypeId::ANY_TYPE,
					internal_marker,
					this_shape: class_prototype,
				};

				let function = synthesise_function(method, behavior, environment, checking_data);

				let property = function_to_property(
					getter_setter,
					function,
					&mut checking_data.types,
					is_declare,
				);

				let position = method.position.with_source(environment.get_source());

				environment.info.register_property(
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
					ParserPropertyKey::Identifier(
						_,
						_,
						parser::property_key::PublicOrPrivate::Private,
					) => Publicity::Private,
					_ => Publicity::Public,
				};
				let key = parser_property_key_to_checker_property_key(
					property.key.get_ast_ref(),
					environment,
					checking_data,
					true,
				);
				// TODO restriction
				properties.push(ClassValue { publicity, key, value: property.value.as_deref() });
			}
			ClassMember::Property(true, property) => {
				let key = parser_property_key_to_checker_property_key(
					property.key.get_ast_ref(),
					environment,
					checking_data,
					true,
				);
				static_property_keys.push(key);
			}
			ClassMember::Method(true, method) => {
				let key = parser_property_key_to_checker_property_key(
					method.name.get_ast_ref(),
					environment,
					checking_data,
					true,
				);
				static_property_keys.push(key);
			}
			_ => {}
		}
	}

	// TODO abstract
	let constructor = if let Some((decorators, constructor)) = class_constructor {
		let internal_marker = if is_declare {
			get_internal_function_effect_from_decorators(decorators, "TODO", environment)
		} else {
			None
		};

		let behavior = FunctionRegisterBehavior::Constructor {
			prototype: class_prototype,
			super_type: extends,
			properties: ClassPropertiesToRegister { properties },
			internal_marker,
		};
		synthesise_function(constructor, behavior, environment, checking_data)
	} else {
		let function_id = FunctionId(environment.get_source(), class.position.start);
		FunctionType::new_auto_constructor(
			function_id,
			class_prototype,
			extends,
			ClassPropertiesToRegister { properties },
			environment,
			checking_data,
			class.position.with_source(environment.get_source()),
		)
	};

	let class_variable_type =
		checking_data.types.new_class_constructor_type(name, constructor, class_prototype);

	{
		// Static items and blocks
		static_property_keys.reverse();

		for member in &class.members {
			match &member.on {
				ClassMember::Method(true, method) => {
					let publicity_kind = match method.name.get_ast_ref() {
						ParserPropertyKey::Identifier(
							_,
							_,
							parser::property_key::PublicOrPrivate::Private,
						) => Publicity::Private,
						_ => Publicity::Public,
					};

					let internal_marker = if let (true, ParserPropertyKey::Identifier(name, _, _)) =
						(is_declare, method.name.get_ast_ref())
					{
						get_internal_function_effect_from_decorators(
							&member.decorators,
							name,
							environment,
						)
					} else {
						None
					};

					let (getter_setter, is_async, is_generator) = match &method.header {
						MethodHeader::Get => (GetterSetter::Getter, false, false),
						MethodHeader::Set => (GetterSetter::Setter, false, false),
						MethodHeader::Regular { is_async, generator } => {
							(GetterSetter::None, *is_async, generator.is_some())
						}
					};

					let behavior = FunctionRegisterBehavior::ClassMethod {
						is_async,
						is_generator,
						// TODO
						super_type: None,
						// TODO
						expecting: TypeId::ANY_TYPE,
						// Important that it points to the marker
						this_shape: class_variable_type,
						internal_marker,
					};

					let function =
						synthesise_function(method, behavior, environment, checking_data);

					let property = function_to_property(
						getter_setter,
						function,
						&mut checking_data.types,
						is_declare,
					);

					let key = static_property_keys.pop().unwrap();

					environment.info.register_property(
						class_variable_type,
						publicity_kind,
						key,
						property,
						// TODO
						true,
						// TODO not needed right?
						method.position.with_source(environment.get_source()),
					);
				}
				ClassMember::Property(true, property) => {
					let publicity_kind = match property.key.get_ast_ref() {
						ParserPropertyKey::Identifier(
							_,
							_,
							parser::property_key::PublicOrPrivate::Private,
						) => Publicity::Private,
						_ => Publicity::Public,
					};

					let value = if let Some(ref value) = property.value {
						let expecting = TypeId::ANY_TYPE;
						synthesise_expression(value, environment, checking_data, expecting)
					} else if is_declare {
						if let Some(ref type_annotation) = property.type_annotation {
							synthesise_type_annotation(type_annotation, environment, checking_data)
						} else {
							crate::utilities::notify!("Declare without type annotation");
							TypeId::ERROR_TYPE
						}
					} else {
						TypeId::UNDEFINED_TYPE
					};

					environment.info.register_property(
						class_variable_type,
						publicity_kind,
						static_property_keys.pop().unwrap(),
						PropertyValue::Value(value),
						// TODO
						true,
						// TODO not needed right?
						property.position.with_source(environment.get_source()),
					);
				}
				ClassMember::StaticBlock(block) => {
					environment.new_lexical_environment_fold_into_parent(
						Scope::StaticBlock { this_type: class_variable_type },
						checking_data,
						|environment, checking_data| {
							synthesise_block(&block.0, environment, checking_data);
						},
					);
				}
				_ => {}
			}
		}
	}

	if let Some(variable) = class.name.get_variable_id(environment.get_source()) {
		environment.info.variable_current_value.insert(variable, class_variable_type);
	}

	class_variable_type
}

/// Also sets variable for hoisting
///
/// Builds the type of the class
pub(super) fn register_statement_class_with_members<T: crate::ReadFromFS>(
	class: &ClassDeclaration<StatementPosition>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	{
		const CLASS_VARIABLE_CONSTANT: bool = true;
		// crate::utilities::notify!("registering {:?}", class.name.identifier);

		register_variable_identifier(
			&class.name.identifier,
			environment,
			checking_data,
			VariableRegisterArguments {
				constant: CLASS_VARIABLE_CONSTANT,
				// No value yet, classes are equiv to `const`
				initial_value: None,
				space: None,
			},
		);
	}

	let class_type = *checking_data
		.local_type_mappings
		.types_to_types
		.get_exact(class.get_position())
		.expect("class type not lifted");

	if let Some(ref extends) = class.extends {
		let extends = get_extends_as_simple_type(extends, environment, checking_data);

		crate::utilities::notify!("Hoisting class with extends {:?}", extends);
		if let Some(ty) = extends {
			environment.info.prototypes.insert(class_type, ty);
		}
	}

	let get_type_by_id = checking_data.types.get_type_by_id(class_type);

	let Type::Class { name: _, parameters } = get_type_by_id else {
		unreachable!("expected class type {:?}", get_type_by_id)
	};

	// TODO also remove
	if let Some(parameters) = parameters {
		for parameter in parameters {
			let parameter_ty = checking_data.types.get_type_by_id(*parameter);
			let Type::RootPolyType(PolyNature::StructureGeneric { name, constrained: _ }) =
				parameter_ty
			else {
				unreachable!("{parameter_ty:?}")
			};

			environment.named_types.insert(name.clone(), *parameter);
		}
	}

	// Set the class type, should be okay
	// checking_data.local_type_mappings.types_to_types.push(class.position, class_type);

	let mut members_iter = class.members.iter().peekable();
	while let Some(member) = members_iter.next() {
		match &member.on {
			ClassMember::Method(initial_is_static, method) => {
				// TODO refactor. Maybe do in reverse?
				let (overloads, actual) = if method.body.0.is_none() {
					let mut overloads = Vec::new();
					let shape = synthesise_shape(method, environment, checking_data);
					overloads.push(shape);

					// Read declarations until
					while let Some(overload_declaration) = members_iter.next_if(|t| {
						matches!(
							&t.on, 
							ClassMember::Method(is_static, m) 
							if initial_is_static == is_static 
							&& m.name == method.name 
							&& !m.has_body())
					}) {
						let ClassMember::Method(_, method) = &overload_declaration.on else {
							unreachable!()
						};
						let shape = synthesise_shape(method, environment, checking_data);
						overloads.push(shape);
					}

					let upcoming = members_iter.peek().and_then(|next| {
						matches!(
							&next.on,
							ClassMember::Method(is_static, m)
							if initial_is_static == is_static
							&& m.name == method.name
							&& m.has_body()
						)
						.then_some(&next.on)
					});

					if let Some(ClassMember::Method(_, method)) = upcoming {
						let actual = synthesise_shape(method, environment, checking_data);
						(overloads, actual)
					} else if class.name.declare {
						let actual = overloads.pop().unwrap();
						(overloads, actual)
					} else {
						todo!("error that missing body")
					}
				} else {
					let actual = synthesise_shape(method, environment, checking_data);
					(Vec::new(), actual)
				};

				let value = build_overloaded_function(
					FunctionId(environment.get_source(), method.position.start),
					FunctionBehavior::Method {
						free_this_id: TypeId::ANY_TYPE,
						is_async: method.header.is_async(),
						is_generator: method.header.is_generator(),
					},
					overloads,
					actual,
					environment,
					&mut checking_data.types,
					&mut checking_data.diagnostics_container,
				);

				let under = crate::synthesis::parser_property_key_to_checker_property_key(
					method.name.get_ast_ref(),
					environment,
					checking_data,
					false,
				);

				environment.info.register_property(
					class_type,
					Publicity::Public,
					under,
					PropertyValue::Value(value),
					false,
					method.position.with_source(environment.get_source()),
				);
			}
			ClassMember::Property(_is_static, property) => {
				let under = crate::synthesis::parser_property_key_to_checker_property_key(
					property.key.get_ast_ref(),
					environment,
					checking_data,
					false,
				);
				let value = if let Some(ref type_annotation) = property.type_annotation {
					synthesise_type_annotation(type_annotation, environment, checking_data)
				} else {
					TypeId::ANY_TYPE
				};
				environment.info.register_property(
					class_type,
					Publicity::Public,
					under,
					PropertyValue::Value(value),
					false,
					property.position.with_source(environment.get_source()),
				);
			}
			ClassMember::Indexer {
				name: _,
				indexer_type,
				return_type,
				is_readonly: _,
				position,
			} => {
				crate::utilities::notify!("Warn if not declare");
				// TODO think this is okay
				let key = synthesise_type_annotation(indexer_type, environment, checking_data);
				let value = synthesise_type_annotation(return_type, environment, checking_data);
				environment.info.register_property(
					class_type,
					Publicity::Public,
					PropertyKey::Type(key),
					PropertyValue::Value(value),
					false,
					position.with_source(environment.get_source()),
				);
			}
			ClassMember::Constructor(c) => {
				if !c.has_body() {
					crate::utilities::notify!("TODO possible constructor overloading");
				}
			}
			ClassMember::StaticBlock(_) | ClassMember::Comment(_, _, _) => {}
		}
	}
}

/// For hoisting using the class as a type annotation
/// Don't want to evaluate side effects during this stage
fn get_extends_as_simple_type<T: crate::ReadFromFS>(
	extends: &Expression,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> Option<TypeId> {
	if let Expression::VariableReference(name, pos) = extends {
		if let Some(ty) = environment.get_type_from_name(name) {
			// Warn if it requires parameters. e.g. Array
			Some(if checking_data.types.get_type_by_id(ty).get_parameters().is_some() {
				// TODO check defaults...
				checking_data.diagnostics_container.add_error(
					TypeCheckError::TypeNeedsTypeArguments(
						name,
						pos.with_source(environment.get_source()),
					),
				);
				TypeId::ERROR_TYPE
			} else {
				ty
			})
		} else {
			None
			// checking_data.diagnostics_container.add_error(TypeCheckError::CannotFindType(
			// 	name,
			// 	pos.with_source(environment.get_source()),
			// ));
			// TypeId::ERROR_TYPE
		}
	} else {
		None
	}
}
