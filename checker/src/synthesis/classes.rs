use parser::{
	declarations::{classes::ClassMember, ClassDeclaration},
	functions::MethodHeader,
	ASTNode, Expression, PropertyKey as ParserPropertyKey, StatementPosition,
};

use crate::{
	context::{Environment, InformationChain, LocalInformation},
	diagnostics::TypeCheckError,
	features::functions::{
		function_to_property, synthesise_function, ClassPropertiesToRegister,
		FunctionRegisterBehavior, GetterSetter, SynthesisableFunction, class_generics_to_function_generics, ReturnType
	},
	types::{
		classes::ClassValue,
		properties::{PropertyKey, Publicity},
		FunctionType, PolyNature, SynthesisedParameters
	},
	CheckingData, FunctionId, PropertyValue, Scope, Type, TypeId,
};

use super::{
	block::synthesise_block,
	definitions::get_internal_function_effect_from_decorators,
	expressions::synthesise_expression,
	functions::{build_overloaded_function, synthesise_shape},
	parser_property_key_to_checker_property_key,
	type_annotations::synthesise_type_annotation,
};

/// Doesn't have any metadata yet
///
/// Returns the constructor for expressions!
///
/// TODO this duplicates work done during lifting
/// returns the new constructor
#[must_use]
pub(super) fn synthesise_class_declaration<
	T: crate::ReadFromFS,
	P: parser::ExpressionOrStatementPosition + super::StatementOrExpressionVariable,
>(
	class: &ClassDeclaration<P>,
	existing_id: Option<TypeId>,
	expected: TypeId,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> TypeId {
	// crate::utilities::notify!("hmm {:?}", (&checking_data.local_type_mappings.types_to_types, class.position));

	// Will leak hoisted properties on existing class ...?
	if let Some(class_type) = existing_id {
		let class_type2 = checking_data.types.get_type_by_id(class_type);

		let Type::Class { name: _, type_parameters } = class_type2 else {
			unreachable!("expecting class type {:?}", class_type2)
		};

		if let Some(type_parameters) = type_parameters {
			let mut sub_environment = environment.new_lexical_environment(Scope::TypeAlias);
			for parameter in type_parameters {
				let parameter_ty = checking_data.types.get_type_by_id(*parameter);
				let Type::RootPolyType(PolyNature::StructureGeneric { name, extends: _ }) =
					parameter_ty
				else {
					unreachable!("{parameter_ty:?}")
				};

				sub_environment.named_types.insert(name.clone(), *parameter);
			}
			let result = synthesise_class_declaration_extends_and_members(
				class,
				(class_type, expected),
				&mut sub_environment,
				checking_data,
			);
			{
				let LocalInformation { current_properties, prototypes, mut events, .. } =
					sub_environment.info;
				environment.info.events.append(&mut events);
				environment.info.current_properties.extend(current_properties);
				environment.info.prototypes.extend(prototypes);
			}
			result
		} else {
			synthesise_class_declaration_extends_and_members(
				class,
				(class_type, expected),
				environment,
				checking_data,
			)
		}
	} else {
		// For classes in expression position
		crate::utilities::notify!("TODO class expression type parameters");
		let name =
			P::as_option_str(&class.name).map_or_else(|| "(anonymous)".to_owned(), str::to_owned);

		let class_type = checking_data
			.types
			.register_type(Type::Class { name: name.clone(), type_parameters: None });

		synthesise_class_declaration_extends_and_members(
			class,
			(class_type, expected),
			environment,
			checking_data,
		)
	}
}

/// `expected` for name
/// returns the new constructor
#[must_use]
fn synthesise_class_declaration_extends_and_members<
	T: crate::ReadFromFS,
	P: parser::ExpressionOrStatementPosition + super::StatementOrExpressionVariable,
>(
	class: &ClassDeclaration<P>,
	(class_type, expected): (TypeId, TypeId),
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> TypeId {
	let is_declare = class.name.is_declare();
	let _name = P::as_option_str(&class.name).map_or_else(String::new, str::to_owned);
	let class_prototype = class_type;

	crate::utilities::notify!("At start {:?}", environment.context_type.free_variables);

	let extends = class.extends.as_ref().map(|extends_expression| {
		let extends =
			synthesise_expression(extends_expression, environment, checking_data, TypeId::ANY_TYPE);

		crate::utilities::notify!("{:?}", (extends, checking_data.types.get_type_by_id(extends)));

		if let TypeId::NULL_TYPE = extends {
			checking_data.raise_unimplemented_error(
				"extends `null` edge case",
				extends_expression.get_position().with_source(environment.get_source()),
			);
		}

		extends
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
				let publicity = if method.name.get_ast_ref().is_private() {
					Publicity::Private
				} else {
					Publicity::Public
				};

				let key = parser_property_key_to_checker_property_key(
					method.name.get_ast_ref(),
					environment,
					checking_data,
					true,
				);

				// TODO abstract
				let (getter_setter, is_async, is_generator) = match &method.header {
					MethodHeader::Get => (Some(GetterSetter::Getter), false, false),
					MethodHeader::Set => (Some(GetterSetter::Setter), false, false),
					MethodHeader::Regular { is_async, generator } => {
						(None, *is_async, generator.is_some())
					}
				};

				crate::utilities::notify!("{:?}", (getter_setter, is_async, is_generator));

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

				let internal = internal_marker.is_some();
				let has_defined_this = method.parameters.leading.0.is_some();

				let behavior = FunctionRegisterBehavior::ClassMethod {
					is_async,
					is_generator,
					// TODO
					super_type: None,
					// TODO
					expecting: TypeId::ANY_TYPE,
					this_shape: if internal && !has_defined_this {
						TypeId::ANY_TYPE
					} else {
						class_prototype
					},
					internal_marker,
					name: key.into_name_type(&mut checking_data.types),
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
					key,
					property,
					position,
				);
			}
			ClassMember::Property(false, property) => {
				let publicity = if property.key.get_ast_ref().is_private() {
					Publicity::Private
				} else {
					Publicity::Public
				};
				let key = parser_property_key_to_checker_property_key(
					property.key.get_ast_ref(),
					environment,
					checking_data,
					true,
				);

				// TODO temp fix for array.length
				if let Some(ref property_type) = property.type_annotation {
					let value =
						synthesise_type_annotation(property_type, environment, checking_data);
					let value = PropertyValue::Value(value);
					environment.info.register_property_on_type(
						class_type,
						Publicity::Public,
						key.clone(),
						value,
					);
				}

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
			ClassMember::Indexer { name, indexer_type, return_type, is_readonly, position } => {
				// TODO this redoes work done at registration. Because the info gets overwritten
				let key = synthesise_type_annotation(indexer_type, environment, checking_data);
				let value = synthesise_type_annotation(return_type, environment, checking_data);

				if *is_readonly {
					checking_data.raise_unimplemented_error(
						"readonly class index",
						position.with_source(environment.get_source()),
					);
				}

				// TODO check declare

				// TODO WIP
				crate::utilities::notify!("Indexing (again) for  '{}'", name);
				let value = PropertyValue::Value(value);
				environment.info.register_property_on_type(
					class_type,
					Publicity::Public,
					PropertyKey::Type(key),
					value,
				);
			}
			_item => {
				// crate::utilities::notify!("Skipping {:?}", _item);
			}
		}
	}

	let name_as_type_id = if let Some(name) = class.name.as_option_str() {
		checking_data.types.new_constant_type(crate::Constant::String(name.to_owned()))
	} else {
		crate::features::functions::extract_name(expected, &checking_data.types, environment)
	};

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
			name: name_as_type_id,
		};
		synthesise_function(constructor, behavior, environment, checking_data)
	} else {
		let function_id = FunctionId(environment.get_source(), class.position.start);
		FunctionType::new_auto_constructor(
			function_id,
			class_prototype,
			extends,
			name_as_type_id,
			ClassPropertiesToRegister { properties },
			environment,
			checking_data,
			class.position.with_source(environment.get_source()),
		)
	};

	// TODO abstract
	let function_id = constructor.id;
	let class_variable_type = checking_data.types.new_class_constructor_type(constructor);
	// Adds event
	environment.register_constructable_function(class_variable_type, function_id);

	if let Some(variable) = class.name.get_variable_id(environment.get_source()) {
		environment.info.variable_current_value.insert(variable, class_variable_type);
	}

	crate::utilities::notify!("At end {:?}", environment.context_type.free_variables);

	{
		// Static items and blocks
		static_property_keys.reverse();

		for member in &class.members {
			match &member.on {
				ClassMember::Method(true, method) => {
					let publicity = if method.name.get_ast_ref().is_private() {
						Publicity::Private
					} else {
						Publicity::Public
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
						MethodHeader::Get => (Some(GetterSetter::Getter), false, false),
						MethodHeader::Set => (Some(GetterSetter::Setter), false, false),
						MethodHeader::Regular { is_async, generator } => {
							(None, *is_async, generator.is_some())
						}
					};

					let key = static_property_keys.pop().unwrap();

					let internal = internal_marker.is_some();
					let has_defined_this = method.parameters.leading.0.is_some();

					let behavior = FunctionRegisterBehavior::ClassMethod {
						is_async,
						is_generator,
						// TODO
						super_type: None,
						// TODO
						expecting: TypeId::ANY_TYPE,
						// Important that it points to the marker
						this_shape: if internal && !has_defined_this {
							TypeId::ANY_TYPE
						} else {
							class_variable_type
						},
						internal_marker,
						name: key.into_name_type(&mut checking_data.types),
					};

					let function =
						synthesise_function(method, behavior, environment, checking_data);

					let property = function_to_property(
						getter_setter,
						function,
						&mut checking_data.types,
						is_declare,
					);

					environment.info.register_property(
						class_variable_type,
						publicity,
						key,
						property,
						// TODO not needed right?
						method.position.with_source(environment.get_source()),
					);
				}
				ClassMember::Property(true, property) => {
					let publicity = if property.key.get_ast_ref().is_private() {
						Publicity::Private
					} else {
						Publicity::Public
					};

					let value = if let Some(ref value) = property.value {
						let expecting = TypeId::ANY_TYPE;
						synthesise_expression(value, environment, checking_data, expecting)
					} else if is_declare {
						if let Some(ref type_annotation) = property.type_annotation {
							synthesise_type_annotation(type_annotation, environment, checking_data)
						} else {
							crate::utilities::notify!("Declare without type annotation");
							TypeId::UNIMPLEMENTED_ERROR_TYPE
						}
					} else {
						TypeId::UNDEFINED_TYPE
					};

					environment.info.register_property(
						class_variable_type,
						publicity,
						static_property_keys.pop().unwrap(),
						PropertyValue::Value(value),
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

	class_variable_type
}

/// Also sets variable for hoisting
///
/// Builds the type of the class
#[must_use]
pub(super) fn register_statement_class_with_members<T: crate::ReadFromFS>(
	class_type: TypeId,
	class: &ClassDeclaration<StatementPosition>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> TypeId {
	let class_type2 = checking_data.types.get_type_by_id(class_type);

	let Type::Class { name: _, type_parameters } = class_type2 else {
		unreachable!("expecting class type {:?}", class_type2)
	};

	if let Some(type_parameters) = type_parameters {
		let mut sub_environment = environment.new_lexical_environment(Scope::TypeAlias);
		for parameter in type_parameters {
			let parameter_ty = checking_data.types.get_type_by_id(*parameter);
			let Type::RootPolyType(PolyNature::StructureGeneric { name, extends: _ }) =
				parameter_ty
			else {
				unreachable!("{parameter_ty:?}")
			};

			sub_environment.named_types.insert(name.clone(), *parameter);
		}

		let result =
			register_extends_and_member(class, class_type, &mut sub_environment, checking_data);
		{
			let crate::context::LocalInformation { current_properties, prototypes, .. } =
				sub_environment.info;
			environment.info.current_properties.extend(current_properties);
			environment.info.prototypes.extend(prototypes);
		}
		result
	} else {
		register_extends_and_member(class, class_type, environment, checking_data)
	}
}

fn register_extends_and_member<T: crate::ReadFromFS>(
	class: &ClassDeclaration<StatementPosition>,
	class_type: TypeId,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> TypeId {
	if let Some(ref extends) = class.extends {
		let extends = get_extends_as_simple_type(extends, environment, checking_data);

		crate::utilities::notify!("Hoisting class with extends {:?}", extends);
		if let Some(extends) = extends {
			environment.info.prototypes.insert(class_type, extends);
		}
	}

	// Set the class type, should be okay
	// checking_data.local_type_mappings.types_to_types.push(class.position, class_type);

	let mut members_iter = class.members.iter().peekable();

	let mut found_constructor = None::<TypeId>;

	while let Some(member) = members_iter.next() {
		match &member.on {
			ClassMember::Method(initial_is_static, method) => {
				let publicity = if method.name.get_ast_ref().is_private() {
					Publicity::Private
				} else {
					Publicity::Public
				};

				// TODO refactor. Maybe do in reverse?
				let (overloads, actual) = if method.body.0.is_none() {
					let mut overloads = Vec::new();
					let shape = synthesise_shape(method, environment, checking_data);
					overloads.push(shape);

					// Read declarations until
					while let Some(overload_declaration) = members_iter.next_if(|dec_mem| {
						next_key_matches(&dec_mem.on, method.name.get_ast_ref(), *initial_is_static)
					}) {
						let ClassMember::Method(_, method) = &overload_declaration.on else {
							unreachable!()
						};
						let shape = synthesise_shape(method, environment, checking_data);
						overloads.push(shape);
					}

					let upcoming = members_iter.peek().and_then(|next| {
						next_key_matches(&next.on, method.name.get_ast_ref(), *initial_is_static)
							.then_some(&next.on)
					});

					if let Some(ClassMember::Method(_, method)) = upcoming {
						let actual = synthesise_shape(method, environment, checking_data);
						(overloads, actual)
					} else if class.name.is_declare {
						let actual = overloads.pop().unwrap();
						(overloads, actual)
					} else {
						checking_data.diagnostics_container.add_error(
							TypeCheckError::FunctionWithoutBodyNotAllowedHere {
								position: ASTNode::get_position(method)
									.with_source(environment.get_source()),
							},
						);
						continue;
					}
				} else {
					let actual = synthesise_shape(method, environment, checking_data);
					(Vec::new(), actual)
				};

				let name =
					if let parser::PropertyKey::Identifier(name, ..) = method.name.get_ast_ref() {
						name
					} else {
						// TODO skip decorator
						"no_name"
					};
				let internal_effect = get_internal_function_effect_from_decorators(
					&member.decorators,
					name,
					environment,
				);

				let (getter_setter, is_async, is_generator) = match &method.header {
					MethodHeader::Get => (Some(GetterSetter::Getter), false, false),
					MethodHeader::Set => (Some(GetterSetter::Setter), false, false),
					MethodHeader::Regular { is_async, generator } => {
						(None, *is_async, generator.is_some())
					}
				};

				let value = build_overloaded_function(
					FunctionId(environment.get_source(), method.position.start),
					crate::types::functions::FunctionBehavior::Method {
						free_this_id: TypeId::ANY_TYPE,
						is_async,
						is_generator,
						// TODO
						name: TypeId::ANY_TYPE,
					},
					overloads,
					actual,
					environment,
					&mut checking_data.types,
					&mut checking_data.diagnostics_container,
					if let Some(ie) = internal_effect {
						ie.into()
					} else {
						crate::types::functions::FunctionEffect::Unknown
					},
				);

				let under = crate::synthesis::parser_property_key_to_checker_property_key(
					method.name.get_ast_ref(),
					environment,
					checking_data,
					false,
				);

				if *initial_is_static {
					crate::utilities::notify!("TODO static item?");
				} else {
					use crate::types::calling::Callable;
					let value = match getter_setter {
						Some(GetterSetter::Getter) => {
							PropertyValue::Getter(Callable::from_type(value, &checking_data.types))
						}
						Some(GetterSetter::Setter) => {
							PropertyValue::Setter(Callable::from_type(value, &checking_data.types))
						}
						None => PropertyValue::Value(value),
					};
					environment.info.register_property_on_type(class_type, publicity, under, value);
				}
			}
			ClassMember::Property(is_static, property) => {
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
				let publicity = if property.key.get_ast_ref().is_private() {
					Publicity::Private
				} else {
					Publicity::Public
				};
				if *is_static {
					crate::utilities::notify!("TODO static item?");
				} else {
					environment.info.register_property_on_type(
						class_type,
						publicity,
						under,
						PropertyValue::Value(value),
					);
				}
			}
			ClassMember::Indexer { name, indexer_type, return_type, is_readonly, position } => {
				// TODO think this is okay
				let key = synthesise_type_annotation(indexer_type, environment, checking_data);
				let value = synthesise_type_annotation(return_type, environment, checking_data);

				if *is_readonly {
					checking_data.raise_unimplemented_error(
						"readonly class index",
						position.with_source(environment.get_source()),
					);
				}

				// TODO check declare

				// TODO WIP
				crate::utilities::notify!("Indexing for  '{}'", name);
				let value = PropertyValue::Value(value);
				crate::utilities::notify!("{:?}", class_type);
				environment.info.register_property_on_type(
					class_type,
					Publicity::Public,
					PropertyKey::Type(key),
					value,
				);
			}
			ClassMember::Constructor(constructor) => {
				let internal_effect = get_internal_function_effect_from_decorators(
					&member.decorators,
					"",
					environment,
				);

				let mut actual = synthesise_shape(constructor, environment, checking_data);
				actual.0 = class_generics_to_function_generics(class_type, &checking_data.types);

				let constructor = build_overloaded_function(
					FunctionId(environment.get_source(), constructor.position.start),
					crate::types::functions::FunctionBehavior::Constructor {
						// The prototype of the base object
						prototype: class_type,
						// The id of the generic that needs to be pulled out
						this_object_type: TypeId::ERROR_TYPE,
						name: TypeId::ANY_TYPE,
					},
					Vec::new(),
					actual,
					environment,
					&mut checking_data.types,
					&mut checking_data.diagnostics_container,
					if let Some(ie) = internal_effect {
						ie.into()
					} else {
						crate::types::functions::FunctionEffect::Unknown
					},
				);

				found_constructor = Some(constructor);
			}
			ClassMember::StaticBlock(_) | ClassMember::Comment(_, _, _) => {}
		}
	}

	if let Some(constructor) = found_constructor {
		constructor
	} else {
		let return_type = ReturnType(class_type, class.position.with_source(environment.get_source()));
		build_overloaded_function(
			FunctionId(environment.get_source(), class.position.start),
			crate::types::functions::FunctionBehavior::Constructor {
				// The prototype of the base object
				prototype: class_type,
				// The id of the generic that needs to be pulled out
				this_object_type: TypeId::ERROR_TYPE,
				name: TypeId::ANY_TYPE,
			},
			Vec::new(),
			crate::features::functions::PartialFunction(
				class_generics_to_function_generics(class_type, &checking_data.types),
				SynthesisedParameters::default(),
				Some(return_type),
			),
			environment,
			&mut checking_data.types,
			&mut checking_data.diagnostics_container,
			crate::types::functions::FunctionEffect::Unknown
		)
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

fn next_key_matches(
	member: &ClassMember,
	initial_name: &parser::PropertyKey<parser::property_key::PublicOrPrivate>,
	initial_is_static: bool,
) -> bool {
	if let ClassMember::Method(is_static, method) = member {
		initial_is_static == *is_static
			&& method.name.get_ast_ref() == initial_name
			&& !method.has_body()
	} else {
		false
	}
}
