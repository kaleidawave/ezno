//! Logic for getting [`TypeId`] from [`parser::TypeAnnotation`]s

use std::convert::TryInto;

use super::{
	assignments::synthesise_access_to_reference, functions::synthesise_function_annotation,
};
use crate::{
	context::{Environment, LocalInformation, Scope},
	diagnostics::{TypeCheckError, TypeCheckWarning, TypeStringRepresentation},
	features::objects::ObjectBuilder,
	types::{
		generics::generic_type_arguments::GenericArguments,
		properties::{PropertyKey, PropertyValue, Publicity},
		Constant, Constructor, PartiallyAppliedGenerics, Type, TypeId,
	},
	types::{
		generics::ExplicitTypeArguments,
		intrinsics::{self, distribute_tsc_string_intrinsic},
		ArrayItem, Counter,
	},
	CheckingData, Map,
};
use parser::{
	type_annotations::{AnnotationWithBinder, CommonTypes, TupleElementKind, TupleLiteralElement},
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
			CommonTypes::Null => TypeId::NULL,
			CommonTypes::Undefined => TypeId::UNDEFINED,
			CommonTypes::Never => TypeId::NEVER,
		},
		TypeAnnotation::StringLiteral(value, ..) => {
			checking_data.types.new_constant_type(Constant::String(value.clone()))
		}
		TypeAnnotation::NumberLiteral(value, _) => {
			let constant = Constant::Number(
				f64::try_from(value.clone()).expect("big int number type").try_into().unwrap(),
			);
			checking_data.types.new_constant_type(constant)
		}
		TypeAnnotation::BooleanLiteral(value, _) => {
			checking_data.types.new_constant_type(Constant::Boolean(*value))
		}
		TypeAnnotation::Name(name, pos) => {
			if let Some(ty) = environment.get_type_from_name(name) {
				// Warn if it requires parameters. e.g. Array
				if checking_data.types.get_type_by_id(ty).get_parameters().is_some() {
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
				}
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
					pos.with_source(environment.get_source()),
				));
				TypeId::ERROR_TYPE
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
				if let Ok(new_ty) = checking_data.types.new_and_type(acc, right) {
					acc = new_ty;
				} else {
					checking_data.diagnostics_container.add_error(
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
					return TypeId::ERROR_TYPE;
				}
			}
			acc
		}
		// This will take the found type and generate a `StructureGeneric` based on the type arguments
		TypeAnnotation::NameWithGenericArguments(name, arguments, position) => {
			// match name.as_str() {
			// 	"ReturnType" => todo!(),
			// 	"Constructor" => todo!(),
			// 	_ => {}
			// }

			if let Some(inner_type_id) = environment.get_type_from_name(name) {
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

					for (parameter, argument_type_annotation) in
						parameters.clone().into_iter().zip(arguments.iter())
					{
						let argument = synthesise_type_annotation(
							argument_type_annotation,
							environment,
							checking_data,
						);

						{
							// TODO check restriction on parameter
							// let mut basic_equality = BasicEquality {
							// 	add_property_restrictions: true,
							// 	position: argument_type_annotation
							// 		.get_position()
							// 		.with_source(environment.get_source()),
							// 	// TODO not needed
							// 	object_constraints: Default::default(),
							// 	allow_errors: true,
							// };

							// let Type::RootPolyType(PolyNature::InterfaceGeneric { name: _ }) =
							// 	checking_data.types.get_type_by_id(parameter)
							// else {
							// 	unreachable!()
							// };

							// // TODO it is a bit weird with the arguments, maybe should get their restriction directly here?
							// // Definition files don't necessary need to check ...
							// let result = type_is_subtype(
							// 	*parameter_restriction,
							// 	argument,
							// 	&mut basic_equality,
							// 	environment,
							// 	&checking_data.types,
							// );

							// if let SubTypeResult::IsNotSubType(_matches) = result {
							// 	let error = TypeCheckError::GenericArgumentDoesNotMeetRestriction {
							// 		parameter_restriction: TypeStringRepresentation::from_type_id(
							// 			*parameter_restriction,
							// 			environment,
							// 			&checking_data.types,
							// 			checking_data.options.debug_types,
							// 		),
							// 		argument: TypeStringRepresentation::from_type_id(
							// 			argument,
							// 			environment,
							// 			&checking_data.types,
							// 			checking_data.options.debug_types,
							// 		),
							// 		position: argument_type_annotation
							// 			.get_position()
							// 			.with_source(environment.get_source()),
							// 	};

							// 	checking_data.diagnostics_container.add_error(error);
							// }
						}

						let with_source = argument_type_annotation
							.get_position()
							.with_source(environment.get_source());

						type_arguments.insert(parameter, (argument, with_source));
					}

					// Inline alias with arguments unless intrinsic
					// crate::utilities::notify!(
					// 	"{:?} and {:?}",
					// 	inner_type_alias_id,
					// 	inner_type_alias_id.is_some_and(intrinsics::tsc_string_intrinsic)
					// );

					if intrinsics::tsc_string_intrinsic(inner_type_id) {
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
						// 		inner_type_id,
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
						TypeCheckError::TypeHasNoGenericParameters(
							name.clone(),
							position.with_source(environment.get_source()),
						),
					);
					TypeId::ERROR_TYPE
				}
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
		TypeAnnotation::FunctionLiteral {
			type_parameters,
			parameters,
			return_type,
			position,
			..
		} => {
			let position = position.with_source(environment.get_source());
			let function_type = synthesise_function_annotation(
				type_parameters,
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
		TypeAnnotation::Readonly(type_annotation, pos) => {
			let underlying_type =
				synthesise_type_annotation(type_annotation, environment, checking_data);

			let restrictions = Map::from_iter([(
				TypeId::T_TYPE,
				(underlying_type, pos.with_source(environment.get_source())),
			)]);
			let ty = Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
				on: TypeId::READONLY_RESTRICTION,
				arguments: GenericArguments::ExplicitRestrictions(restrictions),
			});

			checking_data.types.register_type(ty)

			// environment.frozen.insert(ty_to_be_readonly, TypeId::TRUE);
		}
		TypeAnnotation::NamespacedName(_, _, _) => unimplemented!(),
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
			TypeId::ERROR_TYPE
		}
		// Object literals are first turned into types as if they were interface declarations and then
		// returns reference to object literal
		TypeAnnotation::ObjectLiteral(members, _) => {
			// TODO rather than onto, generate a new type...
			let onto = checking_data
				.types
				.register_type(Type::Object(crate::types::ObjectNature::AnonymousTypeAnnotation));

			super::interfaces::synthesise_signatures(
				None,
				None,
				members,
				super::interfaces::OnToType(onto),
				environment,
				checking_data,
			)
			.0
		}
		TypeAnnotation::TupleLiteral(members, position) => {
			let mut items = Vec::<(SpanWithSource, ArrayItem)>::with_capacity(members.len());

			for TupleLiteralElement(spread, member, pos) in members {
				// TODO store binder name...?
				let type_annotation = match member {
					AnnotationWithBinder::Annotated { ty, .. }
					| AnnotationWithBinder::NoAnnotation(ty) => ty,
				};

				let annotation_ty =
					synthesise_type_annotation(type_annotation, environment, checking_data);

				let pos = pos.with_source(environment.get_source());

				match spread {
					TupleElementKind::Standard => {
						items.push((pos, ArrayItem::Member(annotation_ty)));
					}
					TupleElementKind::Optional => {
						items.push((pos, ArrayItem::Optional(annotation_ty)));
					}
					TupleElementKind::Spread => {
						let slice = crate::types::as_slice(
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
				let value = match item {
					crate::types::ArrayItem::Member(item_ty) => PropertyValue::Value(item_ty),
					crate::types::ArrayItem::Optional(item_ty) => {
						PropertyValue::ConditionallyExists {
							condition: TypeId::OPEN_BOOLEAN_TYPE,
							truthy: Box::new(PropertyValue::Value(item_ty)),
						}
					}
					crate::types::ArrayItem::Wildcard(on) => {
						crate::utilities::notify!("found wildcard");
						let after = idx.into_type(&mut checking_data.types);

						let key = checking_data.types.new_intrinsic(
							&crate::types::intrinsics::Intrinsic::GreaterThan,
							after,
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
				TypeId::ERROR_TYPE
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
			if let Type::Constructor(Constructor::TypeRelationOperator(
				crate::types::TypeRelationOperator::Extends { item, extends },
			)) = checking_data.types.get_type_by_id(condition)
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
		TypeAnnotation::TemplateLiteral { parts, last, .. } => {
			// Using the existing thing breaks because we try to do `"..." + string` and
			// the evaluate_mathematical_operator expects literal or poly values (not just types)
			let mut acc = TypeId::EMPTY_STRING;
			for (static_part, dynamic_part) in parts {
				let lhs =
					checking_data.types.new_constant_type(Constant::String(static_part.to_owned()));
				let rhs = synthesise_type_annotation(
					dynamic_part.get_inner_ref(),
					environment,
					checking_data,
				);
				let constructor = crate::types::Constructor::BinaryOperator {
					lhs,
					operator: crate::features::operations::MathematicalAndBitwise::Add,
					rhs,
				};
				acc = checking_data.types.register_type(Type::Constructor(constructor));
			}
			if !last.is_empty() {
				let value =
					checking_data.types.new_constant_type(Constant::String(last.to_owned()));
				let result = crate::features::operations::evaluate_mathematical_operation(
					acc,
					crate::features::operations::MathematicalAndBitwise::Add,
					value,
					&mut checking_data.types,
					checking_data.options.strict_casts,
				);
				match result {
					Ok(result) => result,
					Err(()) => {
						crate::utilities::notify!("Invalid template literal concatenation");
						return TypeId::ERROR_TYPE;
					}
				}
			} else {
				acc
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
					crate::types::PolyNature::InferGeneric { name: name.clone(), extends },
				));

				let existing = infer_parameters.insert(name.clone(), infer_type);
				if existing.is_some() {
					crate::utilities::notify!("Raise error diagnostic");
				}
				infer_type
			} else {
				crate::utilities::notify!("Raise error diagnostic");
				TypeId::ERROR_TYPE
			}
		}
		TypeAnnotation::Extends { item, extends, position: _ } => {
			let item = synthesise_type_annotation(item, environment, checking_data);
			let extends = synthesise_type_annotation(extends, environment, checking_data);
			let ty = Type::Constructor(Constructor::TypeRelationOperator(
				crate::types::TypeRelationOperator::Extends { item, extends },
			));
			checking_data.types.register_type(ty)
		}
		TypeAnnotation::Is { item, is, position } => {
			let _item = synthesise_type_annotation(item, environment, checking_data);
			let _is = synthesise_type_annotation(is, environment, checking_data);
			checking_data.raise_unimplemented_error(
				"is type annotation",
				position.with_source(environment.get_source()),
			);
			TypeId::ERROR_TYPE
			// let ty = Type::Constructor(Constructor::TypeRelationOperator(
			// 	crate::types::TypeRelationOperator::E { ty: item, is },
			// ));
			// checking_data.types.register_type(ty)
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
		TypeAnnotation::Asserts(_, position) => {
			// TODO construct condition for never
			checking_data.raise_unimplemented_error(
				"asserts annotation",
				position.with_source(environment.get_source()),
			);
			TypeId::ERROR_TYPE
		}
		TypeAnnotation::This(position) => {
			checking_data.raise_unimplemented_error(
				"`this` annotation",
				position.with_source(environment.get_source()),
			);
			TypeId::ERROR_TYPE
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
