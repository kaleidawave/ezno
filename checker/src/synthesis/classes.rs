use parser::{
	declarations::{classes::ClassMember, ClassDeclaration},
	functions::MethodHeader,
	ASTNode, Decorated, Expression, PropertyKey as ParserPropertyKey, StatementPosition,
};
use source_map::{Nullable, SpanWithSource};

use crate::{
	context::{
		information::{InformationChain, Publicity},
		Environment, VariableRegisterArguments,
	},
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	features::functions::{
		function_to_property, synthesise_function, ClassPropertiesToRegister, FunctionBehavior,
		FunctionRegisterBehavior, GetterSetter, PartialFunction, ReturnType, SynthesisableFunction,
	},
	subtyping::{type_is_subtype, BasicEquality, SubTypeResult},
	synthesis::{
		definitions::get_internal_function_effect_from_decorators,
		functions::variable_field_to_string, parser_property_key_to_checker_property_key,
		type_annotations::synthesise_type_annotation, variables::register_variable_identifier,
	},
	types::{
		classes::ClassValue, properties::PropertyKey, FunctionType, PolyNature,
		SynthesisedParameter, SynthesisedParameters, TypeStore,
	},
	CheckingData, DiagnosticsContainer, FunctionId, PropertyValue, Scope, Type, TypeId,
};

use super::{block::synthesise_block, expressions::synthesise_expression};

/// Doesn't have any metadata yet
///
/// Returns the constructor
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

	let existing_id =
		checking_data.local_type_mappings.types_to_types.get_exact(class.position).cloned();

	// Will leak hoisted properties on existing class ...?
	let class_prototype = if let Some(existing_id) = existing_id {
		existing_id
	} else {
		// For classes in expression position
		crate::utils::notify!("TODO class expression parameters");
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
			Some(c)
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
					ParserPropertyKey::Ident(
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

				let internal_marker =
					if let ParserPropertyKey::Ident(name, _, _) = method.name.get_ast_ref() {
						get_internal_function_effect_from_decorators(&member.decorators, name)
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

				let position = Some(method.position.with_source(environment.get_source()));

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
					ParserPropertyKey::Ident(
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
	let constructor = if let Some(constructor) = class_constructor {
		let behavior = FunctionRegisterBehavior::Constructor {
			prototype: class_prototype,
			super_type: extends,
			properties: ClassPropertiesToRegister(properties),
		};
		synthesise_function(constructor, behavior, environment, checking_data)
	} else {
		FunctionType::new_auto_constructor(
			class_prototype,
			ClassPropertiesToRegister(properties),
			environment,
			checking_data,
		)
	};

	let class_type = checking_data.types.new_class_constructor_type(name, constructor);

	{
		// Static items and blocks
		static_property_keys.reverse();

		for member in &class.members {
			match &member.on {
				ClassMember::Method(true, method) => {
					let publicity_kind = match method.name.get_ast_ref() {
						ParserPropertyKey::Ident(
							_,
							_,
							parser::property_key::PublicOrPrivate::Private,
						) => Publicity::Private,
						_ => Publicity::Public,
					};

					let internal_marker =
						if let ParserPropertyKey::Ident(name, _, _) = method.name.get_ast_ref() {
							get_internal_function_effect_from_decorators(&member.decorators, name)
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
						this_shape: class_type,
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
						class_type,
						publicity_kind,
						key,
						property,
						// TODO
						true,
						// TODO not needed right?
						None,
					);
				}
				ClassMember::Property(true, property) => {
					let publicity_kind = match property.key.get_ast_ref() {
						ParserPropertyKey::Ident(
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
							crate::utils::notify!("Declare without type annotation");
							TypeId::ERROR_TYPE
						}
					} else {
						TypeId::UNDEFINED_TYPE
					};

					environment.info.register_property(
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
	}

	if let Some(variable) = class.name.get_variable_id(environment.get_source()) {
		environment.info.variable_current_value.insert(variable, class_type);
	}

	class_type
}

/// Also sets variable
pub(super) fn register_statement_class_with_members<T: crate::ReadFromFS>(
	class: &ClassDeclaration<StatementPosition>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	{
		const CLASS_VARIABLE_CONSTANT: bool = true;
		crate::utils::notify!("registering {:?}", class.name.identifier);

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
		.get(class.get_position().start)
		.expect("class type not lifted");

	if let Some(ref extends) = class.extends {
		let extends = get_extends_as_simple_type(extends, environment, checking_data);

		crate::utils::notify!("Hoisting class with extends {:?}", extends);
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
				unreachable!()
			};

			environment.named_types.insert(name.clone(), *parameter);
		}
	}

	// Set the class type, should be okay
	checking_data.local_type_mappings.types_to_types.push(class.position, class_type);

	let mut members_iter = class.members.iter().peekable();
	while let Some(member) = members_iter.next() {
		match &member.on {
			ClassMember::Method(is_static, method) => {
				if *is_static {
					continue;
				}

				// TODO refactor. Maybe do in reverse?
				let (overloads, actual) = if method.body.0.is_none() {
					let mut overloads = Vec::new();
					let shape = synthesise_shape(method, environment, checking_data);
					overloads.push(shape);

					while let Some(overload_declaration) = members_iter
						.next_if(|t| matches!(&t.on, ClassMember::Method(_, m) if m.has_body()))
					{
						let ClassMember::Method(overload_is_static, method) =
							&overload_declaration.on
						else {
							unreachable!()
						};
						if is_static != overload_is_static {
							todo!()
						}
						// todo check name equals =
						let shape = synthesise_shape(method, environment, checking_data);
						overloads.push(shape);
					}

					if let Some(Decorated {
						on: ClassMember::Method(overload_is_static, method),
						..
					}) = members_iter.next()
					{
						if is_static != overload_is_static {
							todo!()
						}
						// todo check name equals =
						let actual = synthesise_shape(method, environment, checking_data);
						(overloads, actual)
					} else if class.name.declare {
						let actual = overloads.pop().unwrap();
						(overloads, actual)
					} else {
						todo!("error")
					}
				} else {
					let actual = synthesise_shape(method, environment, checking_data);
					(Vec::new(), actual)
				};

				// TODO also check generics?
				fn build_overloaded_function(
					id: FunctionId,
					behavior: FunctionBehavior,
					overloads: Vec<PartialFunction>,
					actual: PartialFunction,
					environment: &Environment,
					types: &mut TypeStore,
					diagnostics: &mut DiagnosticsContainer,
				) -> TypeId {
					// TODO bad
					let expected_parameters = actual.1.clone();

					let as_function = FunctionType {
						id,
						behavior,
						type_parameters: actual.0,
						parameters: actual.1,
						return_type: actual.2.map_or(TypeId::ANY_TYPE, |rt| rt.0),
						effect: crate::types::FunctionEffect::Unknown,
					};

					let actual_func = types.new_hoisted_function_type(as_function);

					let mut result = actual_func;

					for overload in overloads {
						for (idx, op) in overload.1.parameters.iter().enumerate() {
							if let Some((base_type, position)) =
								expected_parameters.get_parameter_type_at_index(idx)
							{
								let res = type_is_subtype(
									base_type,
									op.ty,
									&mut BasicEquality {
										add_property_restrictions: false,
										allow_errors: true,
										position,
										object_constraints: Vec::new(),
									},
									environment,
									types,
								);
								if let SubTypeResult::IsNotSubType(..) = res {
									diagnostics.add_error(
										TypeCheckError::IncompatibleOverloadParameter {
											parameter_position: position,
											overloaded_parameter_position: op.position,
											parameter: TypeStringRepresentation::from_type_id(
												base_type,
												environment,
												types,
												false,
											),
											overloaded_parameter:
												TypeStringRepresentation::from_type_id(
													op.ty,
													environment,
													types,
													false,
												),
										},
									);
								}
							} else {
								// TODO warning
							}
						}

						// TODO other cases
						if let (
							Some(ReturnType(base, base_position)),
							Some(ReturnType(overload, overload_position)),
						) = (actual.2, overload.2)
						{
							let res = type_is_subtype(
								base,
								overload,
								&mut BasicEquality {
									add_property_restrictions: false,
									allow_errors: true,
									position: SpanWithSource::NULL,
									object_constraints: Vec::new(),
								},
								environment,
								types,
							);
							if let SubTypeResult::IsNotSubType(..) = res {
								diagnostics.add_error(
									TypeCheckError::IncompatibleOverloadReturnType {
										base_position,
										overload_position,
										base: TypeStringRepresentation::from_type_id(
											base,
											environment,
											types,
											false,
										),
										overload: TypeStringRepresentation::from_type_id(
											overload,
											environment,
											types,
											false,
										),
									},
								);
							}
						} else {
							// TODO warning
						}

						// Partial
						let as_function = FunctionType {
							id,
							behavior,
							type_parameters: overload.0,
							parameters: overload.1,
							return_type: overload.2.map_or(TypeId::ANY_TYPE, |rt| rt.0),
							effect: crate::types::FunctionEffect::Unknown,
						};

						// TODO
						let func = types.new_hoisted_function_type(as_function);

						// IMPORTANT THAT RESULT IS ON THE RIGHT OF AND TYPE
						result = types.new_and_type(func, result).unwrap();
					}

					result
				}

				let value = build_overloaded_function(
					FunctionId(environment.get_source(), method.position.start),
					FunctionBehavior::Method {
						free_this_id: TypeId::ANY_TYPE,
						is_async: method.header.is_async(),
						is_generator: method.header.is_generator(),
					},
					overloads,
					actual,
					&environment,
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
					None,
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
					None,
				);
			}
			ClassMember::Indexer {
				name: _,
				indexer_type,
				return_type,
				is_readonly: _,
				position: _,
			} => {
				crate::utils::notify!("Warn if not declare");
				// TODO think this is okay
				let key = synthesise_type_annotation(indexer_type, environment, checking_data);
				let value = synthesise_type_annotation(return_type, environment, checking_data);
				environment.info.register_property(
					class_type,
					Publicity::Public,
					PropertyKey::Type(key),
					PropertyValue::Value(value),
					false,
					None,
				);
			}
			ClassMember::Constructor(_)
			| ClassMember::StaticBlock(_)
			| ClassMember::Comment(_, _, _) => {}
		}
	}
}

fn synthesise_shape<T: crate::ReadFromFS>(
	method: &parser::FunctionBase<parser::ast::ClassFunctionBase>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> PartialFunction {
	let type_parameters = method.type_parameters.as_ref().map(|type_parameters| {
		super::functions::synthesise_type_parameters(type_parameters, environment, checking_data)
	});

	let parameters = method
		.parameters
		.parameters
		.iter()
		.map(|parameter| {
			let parameter_constraint =
				parameter.type_annotation.as_ref().map_or(TypeId::ANY_TYPE, |ta| {
					synthesise_type_annotation(&ta, environment, checking_data)
				});

			// TODO I think this is correct
			let is_optional = parameter.additionally.is_some();
			let ty = if is_optional {
				checking_data.types.new_or_type(parameter_constraint, TypeId::UNDEFINED_TYPE)
			} else {
				parameter_constraint
			};

			SynthesisedParameter {
				name: variable_field_to_string(parameter.name.get_ast_ref()),
				is_optional,
				ty,
				position: parameter.position.with_source(environment.get_source()),
			}
		})
		.collect();

	let rest_parameter = method.parameters.rest_parameter.as_ref().map(|_rest_parameter| todo!());

	let return_type = method.return_type.as_ref().map(|annotation| {
		ReturnType(
			synthesise_type_annotation(annotation, environment, checking_data),
			annotation.get_position().with_source(environment.get_source()),
		)
	});

	PartialFunction(
		type_parameters,
		SynthesisedParameters { parameters, rest_parameter },
		return_type,
	)
}

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
