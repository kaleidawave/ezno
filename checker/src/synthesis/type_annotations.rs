//! Logic for getting [`TypeId`] from [`parser::TypeAnnotation`]s

use super::{
	assignments::synthesise_access_to_reference, functions::synthesise_function_annotation,
};
use crate::{
	context::{Environment, LocalInformation, Scope},
	diagnostics::{TypeCheckError, TypeCheckWarning, TypeStringRepresentation},
	features::objects::ObjectBuilder,
	types::{
		generics::generic_type_arguments::GenericArguments,
		generics::ExplicitTypeArguments,
		helpers::{ArrayItem, Counter},
		intrinsics::{self, distribute_tsc_string_intrinsic},
		properties::{PropertyKey, PropertyValue, Publicity},
		Constant, Constructor, PartiallyAppliedGenerics, PolyNature, Type, TypeId,
	},
	CheckingData, Map,
};
use parser::{
	strings,
	type_annotations::{CommonTypes, TupleElementKind, TupleLiteralElement, TypeName},
	ASTNode, TypeAnnotation,
};
use source_map::SpanWithSource;

/// Turns a [`parser::TypeAnnotation`] into [`TypeId`]
///
/// [`CheckingData`] contains [Memory] and [`crate::ErrorAndWarningHandler`]
///
/// Returns a Type if it is found else a [`Result::Err`].
/// Errors other than non existent type are instead appended to the warning handler and a "default" is returned:
/// Example errors:
/// - Reference to generic without generic types
/// - Reference to non generic with generic types
pub fn synthesise_type_annotation<T: crate::ReadFromFS>(
	annotation: &TypeAnnotation,
	// TODO shouldn't be mutable. Currently required because of checking just generic specialisation
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> TypeId {
	let ty = match annotation {
		TypeAnnotation::CommonName(name, _) => match name {
			CommonTypes::String => TypeId::STRING_TYPE,
			CommonTypes::Number => TypeId::NUMBER_TYPE,
			CommonTypes::Boolean => TypeId::BOOLEAN_TYPE,
			// TODO #137
			CommonTypes::Unknown | CommonTypes::Any => TypeId::ANY_TYPE,
			CommonTypes::Null => TypeId::NULL_TYPE,
			CommonTypes::Undefined => TypeId::UNDEFINED_TYPE,
			CommonTypes::Never => TypeId::NEVER_TYPE,
		},
		TypeAnnotation::StringLiteral(value, ..) => {
			let value = strings::unescape_string_content(value).into_owned();
			checking_data.types.new_constant_type(Constant::String(value))
		}
		TypeAnnotation::NumberLiteral(value, _) => {
			let constant =
				Constant::Number(f64::try_from(value.clone()).expect("big int number type"));
			checking_data.types.new_constant_type(constant)
		}
		TypeAnnotation::BooleanLiteral(value, _) => {
			checking_data.types.new_constant_type(Constant::Boolean(*value))
		}
		TypeAnnotation::Name(name, position) => {
			let inner_type_id = synthesise_type_name(name, *position, environment, checking_data);
			if checking_data.types.get_type_by_id(inner_type_id).get_parameters().is_some() {
				// TODO check defaults...
				let name = match name {
					TypeName::Name(name) => name.to_owned(),
					TypeName::FromNamespace(..) => "TODO join .".to_owned(),
				};
				checking_data.diagnostics_container.add_error(
					TypeCheckError::TypeNeedsTypeArguments(
						&name,
						position.with_source(environment.get_source()),
					),
				);
				TypeId::ERROR_TYPE
			} else {
				inner_type_id
			}
		}
		// This will take the found type and generate a `PartiallyAppliedGeneric` based on the type arguments
		TypeAnnotation::NameWithGenericArguments(name, arguments, position) => {
			let inner_type_id = synthesise_type_name(name, *position, environment, checking_data);
			let inner_type = checking_data.types.get_type_by_id(inner_type_id);

			let inner_type_alias_id = if let Type::AliasTo { to, .. } = inner_type {
				// Fix for recursion
				if *to == TypeId::ANY_TO_INFER_TYPE {
					None
				} else {
					Some(*to)
				}
			} else {
				None
			};

			// crate::utilities::notify!("{:?}", inner_type);

			if let Some(parameters) = inner_type.get_parameters() {
				let mut type_arguments: crate::Map<TypeId, (TypeId, SpanWithSource)> =
					crate::Map::default();

				// TODO better diagnostic
				if parameters.len() != arguments.len() {
					checking_data.diagnostics_container.add_error(
						TypeCheckError::GenericArgumentCountMismatch {
							expected_count: parameters.len(),
							count: arguments.len(),
							position: position.with_source(environment.get_source()),
						},
					);
					// Continue is fine
				}

				let mut argument_type_annotations = arguments.iter();
				for parameter in parameters.iter().copied() {
					let parameter_restriction =
						if let Type::RootPolyType(PolyNature::StructureGeneric {
							extends, ..
						}) = checking_data.types.get_type_by_id(parameter)
						{
							*extends
						} else {
							crate::utilities::notify!("Shouldn't be here");
							parameter
						};

					let (argument, position) = if let Some(argument_type_annotation) =
						argument_type_annotations.next()
					{
						let argument =
							if let TypeAnnotation::Infer { name, extends: None, position: _ } =
								argument_type_annotation
							{
								environment.new_infer_type(
									parameter_restriction,
									name,
									&mut checking_data.types,
								)
							} else {
								synthesise_type_annotation(
									argument_type_annotation,
									environment,
									checking_data,
								)
							};

						{
							use crate::types::subtyping;

							let mut state = subtyping::State {
								already_checked: Default::default(),
								mode: Default::default(),
								contributions: Default::default(),
								others: subtyping::SubTypingOptions { allow_errors: true },
								object_constraints: None,
							};

							let result = subtyping::type_is_subtype(
								parameter_restriction,
								argument,
								&mut state,
								environment,
								&checking_data.types,
							);

							if let subtyping::SubTypeResult::IsNotSubType(_matches) = result {
								let error = TypeCheckError::GenericArgumentDoesNotMeetRestriction {
									parameter_restriction: TypeStringRepresentation::from_type_id(
										parameter_restriction,
										environment,
										&checking_data.types,
										checking_data.options.debug_types,
									),
									argument: TypeStringRepresentation::from_type_id(
										argument,
										environment,
										&checking_data.types,
										checking_data.options.debug_types,
									),
									position: argument_type_annotation
										.get_position()
										.with_source(environment.get_source()),
								};

								checking_data.diagnostics_container.add_error(error);
							}
						}
						let position = argument_type_annotation
							.get_position()
							.with_source(environment.get_source());

						(argument, position)
					} else {
						(TypeId::ERROR_TYPE, <SpanWithSource as source_map::Nullable>::NULL)
					};

					type_arguments.insert(parameter, (argument, position));
				}

				if intrinsics::is_tsc_string_intrinsic(inner_type_id) {
					distribute_tsc_string_intrinsic(
						inner_type_id,
						type_arguments.get(&TypeId::STRING_GENERIC).unwrap().0,
						&mut checking_data.types,
					)
				} else if let (Some(inner_type_alias_id), false) =
					(inner_type_alias_id, intrinsics::is_intrinsic(inner_type_id))
				{
					// Important that these wrappers are kept as there 'wrap' holds information
					// {
					// 	use crate::types::printing::print_type;

					// 	let ty = print_type(
					// 		inner_type,
					// 		&mut checking_data.types,
					// 		environment,
					// 		true,
					// 	);
					// 	crate::utilities::notify!("Here substituting alias eagerly {}", ty);
					// }

					let substitution_arguments =
						ExplicitTypeArguments(type_arguments).into_substitution_arguments();

					crate::types::substitute(
						inner_type_alias_id,
						&substitution_arguments,
						environment,
						&mut checking_data.types,
					)
				} else {
					let arguments = GenericArguments::ExplicitRestrictions(type_arguments);

					let ty = Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
						on: inner_type_id,
						arguments,
					});

					checking_data.types.register_type(ty)
				}
			} else {
				checking_data.diagnostics_container.add_error(
					TypeCheckError::GenericArgumentCountMismatch {
						expected_count: 0,
						count: arguments.len(),
						position: position.with_source(environment.get_source()),
					},
				);
				TypeId::ERROR_TYPE
			}
		}
		TypeAnnotation::NameWithProperties(name, _members, position) => {
			let inner_type_id = synthesise_type_name(name, *position, environment, checking_data);
			// TODO must be class
			// TODO object type with
			if let Type::Class { .. } = checking_data.types.get_type_by_id(inner_type_id) {
				todo!("unwrap members")
			} else {
				todo!("error")
			}
		}
		TypeAnnotation::Union(type_annotations, _) => {
			let iterator = type_annotations
				.iter()
				.map(|type_annotation| {
					synthesise_type_annotation(type_annotation, environment, checking_data)
				})
				.collect::<Vec<_>>()
				.into_iter();

			iterator
				.reduce(|acc, right| checking_data.types.new_or_type(acc, right))
				.expect("Empty union")
		}
		TypeAnnotation::Intersection(type_annotations, position) => {
			let mut iterator = type_annotations
				.iter()
				.map(|type_annotation| {
					synthesise_type_annotation(type_annotation, environment, checking_data)
				})
				.collect::<Vec<_>>()
				.into_iter();

			let mut acc = iterator.next().expect("Empty intersection");
			for right in iterator {
				let is_disjoint = crate::types::disjoint::types_are_disjoint(
					acc,
					right,
					&mut Vec::new(),
					environment,
					&checking_data.types,
				);
				if is_disjoint {
					checking_data.diagnostics_container.add_warning(
						TypeCheckWarning::TypesDoNotIntersect {
							left: TypeStringRepresentation::from_type_id(
								acc,
								environment,
								&checking_data.types,
								checking_data.options.debug_types,
							),
							right: TypeStringRepresentation::from_type_id(
								right,
								environment,
								&checking_data.types,
								checking_data.options.debug_types,
							),
							position: position.with_source(environment.get_source()),
						},
					);
					return TypeId::NEVER_TYPE;
				}

				acc = checking_data.types.new_and_type(acc, right);
			}
			acc
		}
		TypeAnnotation::FunctionLiteral {
			type_parameters,
			parameters,
			return_type,
			position,
			..
		} => {
			let position = position.with_source(environment.get_source());
			let function_type = synthesise_function_annotation(
				type_parameters.as_deref(),
				parameters,
				Some(return_type),
				environment,
				checking_data,
				&position,
				// TODO async
				crate::types::functions::FunctionBehavior::ArrowFunction { is_async: false },
			);
			// TODO bit messy
			checking_data.types.new_function_type_annotation(
				function_type.type_parameters,
				function_type.parameters,
				function_type.return_type,
				&position,
			)
		}
		TypeAnnotation::Abstract(_type_annotation, position) => {
			checking_data.raise_unimplemented_error(
				"abstact type annotation",
				position.with_source(environment.get_source()),
			);
			TypeId::UNIMPLEMENTED_ERROR_TYPE
		}
		TypeAnnotation::Readonly(type_annotation, position) => {
			let underlying_type =
				synthesise_type_annotation(type_annotation, environment, checking_data);

			let restrictions = Map::from_iter([(
				TypeId::T_TYPE,
				(underlying_type, position.with_source(environment.get_source())),
			)]);
			let ty = Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
				on: TypeId::READONLY_RESTRICTION,
				arguments: GenericArguments::ExplicitRestrictions(restrictions),
			});

			checking_data.types.register_type(ty)

			// environment.frozen.insert(ty_to_be_readonly, TypeId::TRUE);
		}
		TypeAnnotation::ArrayLiteral(item_annotation, _) => {
			let item_type = synthesise_type_annotation(item_annotation, environment, checking_data);
			let position = item_annotation.get_position().with_source(environment.get_source());
			checking_data.types.new_array_type(item_type, position)
		}
		TypeAnnotation::ConstructorLiteral {
			type_parameters: _,
			parameters: _,
			return_type: _,
			position,
		} => {
			checking_data.raise_unimplemented_error(
				"constructor literal",
				position.with_source(environment.get_source()),
			);
			TypeId::UNIMPLEMENTED_ERROR_TYPE
		}
		// Object literals are first turned into types as if they were interface declarations and then
		// returns reference to object literal
		TypeAnnotation::ObjectLiteral(members, _) => {
			let properties = super::interfaces::synthesise_signatures(
				None,
				None,
				members,
				super::interfaces::PropertiesList(Vec::new()),
				environment,
				checking_data,
			);

			checking_data.types.new_anonymous_interface_type(properties.0)
		}
		TypeAnnotation::TupleLiteral(members, position) => {
			let mut items = Vec::<(SpanWithSource, ArrayItem)>::with_capacity(members.len());

			for TupleLiteralElement(spread, member, pos) in members {
				let annotation_ty =
					synthesise_type_annotation(&member.type_annotation, environment, checking_data);

				let pos = pos.with_source(environment.get_source());

				match spread {
					TupleElementKind::Standard => {
						items.push((pos, ArrayItem::Member(annotation_ty)));
					}
					TupleElementKind::Optional => {
						items.push((pos, ArrayItem::Optional(annotation_ty)));
					}
					TupleElementKind::Spread => {
						let slice = crate::types::helpers::as_slice(
							annotation_ty,
							&checking_data.types,
							environment,
						);

						crate::utilities::notify!("slice = {:?}", slice);

						// Flattening
						match slice {
							Ok(new_items) => {
								// TODO position here?
								items.extend(new_items.into_iter().map(|value| (pos, value)));
							}
							Err(()) => items.push((pos, ArrayItem::Wildcard(annotation_ty))),
						}
					}
				}
			}

			let mut idx: Counter = 0.into();

			// TODO maybe should be special type
			let mut obj = ObjectBuilder::new(
				Some(TypeId::ARRAY_TYPE),
				&mut checking_data.types,
				position.with_source(environment.get_source()),
				&mut environment.info,
			);

			for (ty_position, item) in items {
				use crate::types::helpers::ArrayItem;

				let value = match item {
					ArrayItem::Member(item_ty) => PropertyValue::Value(item_ty),
					ArrayItem::Optional(item_ty) => PropertyValue::ConditionallyExists {
						condition: TypeId::OPEN_BOOLEAN_TYPE,
						truthy: Box::new(PropertyValue::Value(item_ty)),
					},
					ArrayItem::Wildcard(on) => {
						crate::utilities::notify!("found wildcard");
						let after = idx.into_type(&mut checking_data.types);

						let key = crate::types::intrinsics::new_intrinsic(
							&crate::types::intrinsics::Intrinsic::GreaterThan,
							after,
							&mut checking_data.types,
						);

						let item_type = checking_data.types.register_type(Type::Constructor(
							Constructor::Property {
								on,
								under: PropertyKey::Type(TypeId::OPEN_NUMBER_TYPE),
								result: TypeId::UNIMPLEMENTED_ERROR_TYPE,
								mode: crate::types::properties::AccessMode::Regular,
							},
						));

						obj.append(
							Publicity::Public,
							PropertyKey::Type(key),
							PropertyValue::Value(item_type),
							ty_position,
							&mut environment.info,
						);
						idx.increment(&mut checking_data.types);

						continue;
					}
				};
				{
					obj.append(
						Publicity::Public,
						idx.into_property_key(),
						value,
						ty_position,
						&mut environment.info,
					);
					idx.increment(&mut checking_data.types);
				}
			}

			let length_value = idx.into_type(&mut checking_data.types);
			let position = annotation.get_position().with_source(environment.get_source());

			obj.append(
				Publicity::Public,
				PropertyKey::String("length".into()),
				PropertyValue::Value(length_value),
				position,
				&mut environment.info,
			);

			// TODO this should be anonymous object type
			obj.build_object()
		}
		TypeAnnotation::ParenthesizedReference(ref reference, _) => {
			synthesise_type_annotation(reference, environment, checking_data)
		}
		TypeAnnotation::Index(being_indexed, indexer, _) => {
			let being_indexed =
				synthesise_type_annotation(being_indexed, environment, checking_data);
			let indexer = synthesise_type_annotation(indexer, environment, checking_data);

			checking_data.types.new_property_on_type_annotation(being_indexed, indexer, environment)
		}
		TypeAnnotation::KeyOf(of, _position) => {
			let of = synthesise_type_annotation(of, environment, checking_data);
			checking_data.types.new_key_of(of)
		}
		TypeAnnotation::TypeOf(item, position) => {
			let reference = synthesise_access_to_reference(item, environment, checking_data);
			if let Some(value) = environment.get_reference_constraint(reference) {
				value
			} else {
				checking_data.raise_unimplemented_error(
					"throw error for annotation",
					position.with_source(environment.get_source()),
				);
				TypeId::UNIMPLEMENTED_ERROR_TYPE
			}
		}
		TypeAnnotation::Conditional { condition, resolve_true, resolve_false, position: _ } => {
			let (condition, infer_types) = {
				let mut sub_environment =
					environment.new_lexical_environment(Scope::TypeAnnotationCondition {
						infer_parameters: Default::default(),
					});
				let condition =
					synthesise_type_annotation(condition, &mut sub_environment, checking_data);
				let Scope::TypeAnnotationCondition { infer_parameters } =
					sub_environment.context_type.scope
				else {
					unreachable!()
				};

				// TODO temp as object types use the same environment.properties representation
				{
					let LocalInformation { current_properties, prototypes, .. } =
						sub_environment.info;
					environment.info.current_properties.extend(current_properties);
					environment.info.prototypes.extend(prototypes);
				}

				(condition, infer_parameters)
			};

			let truthy_result = {
				if infer_types.is_empty() {
					synthesise_type_annotation(resolve_true, environment, checking_data)
				} else {
					let mut sub_environment =
						environment.new_lexical_environment(Scope::TypeAnnotationConditionResult);

					sub_environment.named_types.extend(infer_types);
					let ty = synthesise_type_annotation(
						resolve_true,
						&mut sub_environment,
						checking_data,
					);

					// TODO temp as object types use the same environment.properties representation
					{
						let LocalInformation { current_properties, prototypes, .. } =
							sub_environment.info;
						environment.info.current_properties.extend(current_properties);
						environment.info.prototypes.extend(prototypes);
					}

					ty
				}
			};

			let otherwise_result =
				synthesise_type_annotation(resolve_false, environment, checking_data);

			// TODO WIP
			if let Type::Constructor(Constructor::TypeExtends(crate::types::TypeExtends {
				item,
				extends,
			})) = checking_data.types.get_type_by_id(condition)
			{
				if let Type::Constant(_) = checking_data.types.get_type_by_id(*item) {
					use crate::types::generics::substitution::{
						compute_extends_rule, SubstitutionArguments,
					};
					let temp_args = SubstitutionArguments::new_arguments_for_use_in_loop();
					crate::utilities::notify!("Here");
					return compute_extends_rule(
						*extends,
						*item,
						environment,
						&mut checking_data.types,
						truthy_result,
						&temp_args,
						otherwise_result,
					);
				}
			}

			// TODO might want to record whether infer_types is_empty here
			let result_union = checking_data.types.new_or_type(truthy_result, otherwise_result);
			let ty = Type::Constructor(Constructor::ConditionalResult {
				condition,
				truthy_result,
				otherwise_result,
				result_union,
			});

			checking_data.types.register_type(ty)
		}
		TypeAnnotation::Marker(_, _) => {
			crate::utilities::notify!("Dump available object types in environment to somewhere..?");
			TypeId::ANY_TYPE
		}
		// TODO these are all work in progress
		TypeAnnotation::Decorated(decorator, inner, _) => {
			crate::utilities::notify!("Unknown decorator skipping {:#?}", decorator.name);
			synthesise_type_annotation(inner, environment, checking_data)
		}
		TypeAnnotation::TemplateLiteral { parts, final_part, .. } => {
			// Using the existing thing breaks because we try to do `"..." + string` and
			// the evaluate_mathematical_operator expects literal or poly values (not just types)
			let mut acc = TypeId::EMPTY_STRING;
			for (static_part, dynamic_part) in parts {
				let lhs = checking_data.types.new_constant_type(Constant::String(
					strings::unescape_string_content(static_part).into_owned(),
				));
				acc = if let TypeId::EMPTY_STRING = acc {
					lhs
				} else {
					checking_data.types.register_type(Type::Constructor(
						crate::types::Constructor::BinaryOperator {
							lhs: acc,
							operator:
								crate::features::operations::MathematicalOrBitwiseOperation::Add,
							rhs: lhs,
							result: TypeId::STRING_TYPE,
						},
					))
				};
				// WIP fix correcting `infer T` to `infer T extends string` so that string addition works
				let rhs = if let TypeAnnotation::Infer { name, extends: None, position: _ } =
					&dynamic_part.type_annotation
				{
					environment.new_infer_type(TypeId::STRING_TYPE, name, &mut checking_data.types)
				} else {
					synthesise_type_annotation(
						&dynamic_part.type_annotation,
						environment,
						checking_data,
					)
				};
				let constructor = crate::types::Constructor::BinaryOperator {
					lhs: acc,
					operator: crate::features::operations::MathematicalOrBitwiseOperation::Add,
					rhs,
					result: TypeId::STRING_TYPE,
				};
				acc = checking_data.types.register_type(Type::Constructor(constructor));
			}
			if final_part.is_empty() {
				acc
			} else {
				let lhs = checking_data.types.new_constant_type(Constant::String(
					strings::unescape_string_content(final_part).into_owned(),
				));
				if let TypeId::EMPTY_STRING = acc {
					lhs
				} else {
					checking_data.types.register_type(Type::Constructor(
						crate::types::Constructor::BinaryOperator {
							lhs: acc,
							operator:
								crate::features::operations::MathematicalOrBitwiseOperation::Add,
							rhs: lhs,
							result: TypeId::STRING_TYPE,
						},
					))
				}
			}
		}
		TypeAnnotation::Infer { name, extends, position: _ } => {
			let extends = if let Some(ref extends) = extends {
				synthesise_type_annotation(extends, environment, checking_data)
			} else {
				TypeId::ANY_TYPE
			};

			if let Scope::TypeAnnotationCondition { ref mut infer_parameters } =
				environment.context_type.scope
			{
				let infer_type = checking_data.types.register_type(Type::RootPolyType(
					PolyNature::InferGeneric { name: name.clone(), extends },
				));

				let existing = infer_parameters.insert(name.clone(), infer_type);
				if existing.is_some() {
					crate::utilities::notify!("Raise error diagnostic");
				}
				infer_type
			} else {
				crate::utilities::notify!("Raise error diagnostic");
				TypeId::UNIMPLEMENTED_ERROR_TYPE
			}
		}
		TypeAnnotation::Extends { item, extends, position: _ } => {
			let item = synthesise_type_annotation(item, environment, checking_data);
			let extends = synthesise_type_annotation(extends, environment, checking_data);
			let ty = Type::Constructor(Constructor::TypeExtends(crate::types::TypeExtends {
				item,
				extends,
			}));
			checking_data.types.register_type(ty)
		}
		TypeAnnotation::Is { reference, is, position: _ } => {
			let item_type = match reference {
				parser::type_annotations::IsItem::Reference(name) => {
					// Fine to do this environment?
					environment
						.variables
						.get(name)
						.and_then(|variable| {
							environment
								.info
								.variable_current_value
								.get(&variable.get_origin_variable_id())
						})
						.copied()
				}
				parser::type_annotations::IsItem::This => {
					// TODO
					let based_on = TypeId::UNIMPLEMENTED_ERROR_TYPE;
					let ty = Type::RootPolyType(PolyNature::FreeVariable {
						reference: crate::events::RootReference::This,
						based_on,
					});
					Some(checking_data.types.register_type(ty))
				}
			};
			if let Some(item) = item_type {
				super::extensions::is_expression::new_is_type(item, is, environment, checking_data)
			} else {
				crate::utilities::notify!("Here, it is fine for hoisting?");
				TypeId::ERROR_TYPE
			}
		}
		TypeAnnotation::Symbol { name, unique: _unique, position: _ } => {
			// TODO what does unique do?
			if let Some(name) = name {
				// TODO existing names
				if name == "iterator" {
					TypeId::SYMBOL_ITERATOR
				} else {
					crate::utilities::notify!("New symbol:{}", name);
					checking_data.types.new_constant_type(Constant::Symbol { key: name.clone() })
				}
			} else {
				TypeId::SYMBOL_TYPE
			}
		}
		TypeAnnotation::Asserts(condition, _position) => {
			let condition = synthesise_type_annotation(condition, environment, checking_data);
			let ty = Type::Constructor(Constructor::ConditionalResult {
				condition,
				truthy_result: TypeId::ANY_TYPE,
				otherwise_result: TypeId::NEVER_TYPE,
				// TODO not needed
				result_union: TypeId::ERROR_TYPE,
			});
			checking_data.types.register_type(ty)
		}
		TypeAnnotation::This(position) => {
			checking_data.raise_unimplemented_error(
				"`this` annotation",
				position.with_source(environment.get_source()),
			);
			TypeId::UNIMPLEMENTED_ERROR_TYPE
		}
	};

	if checking_data.options.store_type_mappings {
		checking_data
			.local_type_mappings
			.types_to_types
			.push(annotation.get_position().with_source(environment.get_source()), ty);
	}

	ty
}

pub fn synthesise_type_name<T: crate::ReadFromFS>(
	name: &TypeName,
	position: crate::Span,
	environment: &Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> TypeId {
	match name {
		TypeName::Name(name) => {
			if let Some(ty) = environment.get_type_from_name(name) {
				// Warn if it requires parameters. e.g. Array
				ty
			} else {
				let possibles = {
					let mut possibles = crate::get_closest(environment.get_all_named_types(), name)
						.unwrap_or(vec![]);
					possibles.sort_unstable();
					possibles
				};
				checking_data.diagnostics_container.add_error(TypeCheckError::CouldNotFindType(
					name,
					possibles,
					position.with_source(environment.get_source()),
				));
				TypeId::ERROR_TYPE
			}
		}
		TypeName::FromNamespace(..) => {
			checking_data.raise_unimplemented_error(
				"namespace item",
				position.with_source(environment.get_source()),
			);
			TypeId::ERROR_TYPE
		}
	}
}

/// Comment as type annotation
pub(crate) fn comment_as_type_annotation<T: crate::ReadFromFS>(
	possible_declaration: &str,
	position: &source_map::SpanWithSource,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> Option<(TypeId, source_map::SpanWithSource)> {
	let source = environment.get_source();
	let offset = Some(position.end - 1 - possible_declaration.len() as u32);

	let possible_declaration =
		possible_declaration.strip_prefix('*').unwrap_or(possible_declaration);

	let annotation = parser::TypeAnnotation::from_string_with_options(
		possible_declaration.to_owned(),
		Default::default(),
		offset,
	);
	if let Ok((annotation, _)) = annotation {
		Some((
			synthesise_type_annotation(&annotation, environment, checking_data),
			annotation.get_position().with_source(source),
		))
	} else {
		crate::utilities::notify!("Failed comment as type annotation");
		// TODO warning
		None
	}
}

pub(crate) fn get_annotation_from_declaration<
	T: crate::ReadFromFS,
	U: parser::declarations::variable::DeclarationExpression + 'static,
>(
	declaration: &parser::declarations::VariableDeclarationItem<U>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> Option<TypeId> {
	let result = if let Some(annotation) = declaration.type_annotation.as_ref() {
		Some((
			synthesise_type_annotation(annotation, environment, checking_data),
			annotation.get_position().with_source(environment.get_source()),
		))
	}
	// TODO only under config
	else if let parser::WithComment::PostfixComment(_item, possible_declaration, position) =
		&declaration.name
	{
		crate::utilities::notify!("Here {:?}", possible_declaration);
		comment_as_type_annotation(
			possible_declaration,
			&position.with_source(environment.get_source()),
			environment,
			checking_data,
		)
	} else {
		None
	};

	if let Some((ty, span)) = result {
		let get_position = declaration.get_position();
		checking_data
			.local_type_mappings
			.variable_restrictions
			.insert((environment.get_source(), get_position.start), (ty, span));
	}

	result.map(|(value, _span)| value)
}
