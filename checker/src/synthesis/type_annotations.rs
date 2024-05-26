//! Logic for getting [`TypeId`] from [`parser::TypeAnnotation`]s
//!
//! ### There are several behaviors for type references depending on their position:
//! #### Sources:
//! - Type reference of any source variable declarations is a [`crate::TypeConstraint`]
//! - Type references in parameters are [`crate::TypeConstraint`]s
//! - Type references in returns types are also [`crate::TypeConstraint`]s, because ezno uses the body to get the return
//! type
//!
//! #### Declarations
//! - Type reference in any declaration or return type is a internal type [`crate::Type::InternalObjectReference`]
//!     - Return types need to know whether they return a unique object (todo don't know any examples)
//! or a new object. e.g. `Array.from`
//! - Parameters shouldn't do generic resolving
//!
//! ### Treatment of `any`
//! - Any has no properties because it is a union of all the types. It also means that it could be be `{}`
//! - To allow for compat it treats it as inferred generic **so it can get properties off of it**. Would be better
//! to allow this as a condition in the future

use std::convert::TryInto;

use map_vec::Map;
use parser::{
	type_annotations::{AnnotationWithBinder, CommonTypes, TupleElementKind, TupleLiteralElement},
	ASTNode, TypeAnnotation,
};
use source_map::SpanWithSource;

use crate::{
	context::information::Publicity,
	diagnostics::{TypeCheckError, TypeCheckWarning, TypeStringRepresentation},
	features::{objects::ObjectBuilder, template_literal::synthesize_template_literal_type},
	synthesis::functions::synthesise_function_annotation,
	types::{
		generics::generic_type_arguments::StructureGenericArguments,
		properties::{PropertyKey, PropertyValue},
		Constant, Constructor, StructureGenerics, Type, TypeId,
	},
	CheckingData, Environment,
};

/// Turns a [`parser::TypeAnnotation`] into [`TypeId`]
///
/// [`CheckingData`] contains [Memory] and [`crate::ErrorAndWarningHandler`]
///
/// Returns a Type if it is found else a [`Result::Err`].
/// Errors other than non existent type are instead appended to the warning handler and a "default" is returned:
/// Example errors:
/// - Reference to generic without generic types
/// - Reference to non generic with generic types
pub(super) fn synthesise_type_annotation<T: crate::ReadFromFS>(
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
		TypeAnnotation::Name(name, pos) => match name.as_str() {
			"any" => TypeId::ANY_TYPE,
			"this" => todo!(), // environment.get_value_of_this(&mut checking_data.types),
			"self" => TypeId::ANY_INFERRED_FREE_THIS,
			name => {
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
					checking_data.diagnostics_container.add_error(TypeCheckError::CannotFindType(
						name,
						pos.with_source(environment.get_source()),
					));
					TypeId::ERROR_TYPE
				}
			}
		},
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

			let Some(inner_type_id) = environment.get_type_from_name(name) else {
				checking_data.diagnostics_container.add_error(TypeCheckError::CouldNotFindType(
					name,
					position.with_source(environment.get_source()),
				));
				return TypeId::ERROR_TYPE;
			};

			let inner_type = checking_data.types.get_type_by_id(inner_type_id);

			// crate::utilities::notify!("{:?}", inner_type);

			if let Some(parameters) = inner_type.get_parameters() {
				let is_flattenable_alias = if let Type::AliasTo { to, .. } = inner_type {
					// Important that these wrappers are kept as there 'wrap' holds information
					if matches!(
						inner_type_id,
						TypeId::LITERAL_RESTRICTION | TypeId::READONLY_RESTRICTION
					) {
						None
					} else {
						Some(*to)
					}
				} else {
					None
				};

				let mut type_arguments: map_vec::Map<TypeId, (TypeId, SpanWithSource)> =
					map_vec::Map::new();

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

				// Eagerly specialise for type alias. TODO don't do for object types...
				let mut arguments = StructureGenericArguments::ExplicitRestrictions(type_arguments);
				if let Some(on) = is_flattenable_alias {
					crate::types::substitute(
						on,
						&mut arguments,
						environment,
						&mut checking_data.types,
					)
				} else {
					let ty = Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
						on: inner_type_id,
						arguments,
					}));

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
				crate::features::functions::FunctionBehavior::ArrowFunction { is_async: false },
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
			let ty = Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
				on: TypeId::READONLY_RESTRICTION,
				arguments: StructureGenericArguments::ExplicitRestrictions(restrictions),
			}));

			checking_data.types.register_type(ty)

			// let ty_to_be_readonly = checking_data.types.register_type(Type::AliasTo {
			// 	to: underlying_type,
			// 	name: None,
			// 	parameters: None,
			// });

			// // TODO I think Readonly == freeze...?
			// environment.frozen.insert(ty_to_be_readonly, TypeId::TRUE);

			// ty_to_be_readonly)
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
			position: _,
		} => unimplemented!(),
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
			// TODO maybe should be special type
			let mut obj = ObjectBuilder::new(
				Some(TypeId::ARRAY_TYPE),
				&mut checking_data.types,
				position.with_source(environment.get_source()),
				&mut environment.info,
			);

			for (idx, TupleLiteralElement(spread, member, _)) in members.iter().enumerate() {
				// TODO binder name under data...?
				match spread {
					TupleElementKind::Standard => {
						let type_annotation = match member {
							AnnotationWithBinder::Annotated { ty, .. }
							| AnnotationWithBinder::NoAnnotation(ty) => ty,
						};

						let item_ty =
							synthesise_type_annotation(type_annotation, environment, checking_data);

						let ty_position =
							type_annotation.get_position().with_source(environment.get_source());

						obj.append(
							environment,
							Publicity::Public,
							PropertyKey::from_usize(idx),
							PropertyValue::Value(item_ty),
							ty_position,
						);
					}
					TupleElementKind::Optional => {
						todo!()
					}
					TupleElementKind::Spread => {
						todo!();
					}
				}
			}

			let constant = Constant::Number((members.len() as f64).try_into().unwrap());
			let length_value = checking_data.types.new_constant_type(constant);

			// TODO: Does `constant` have a position? Or should it have one?
			obj.append(
				environment,
				Publicity::Public,
				PropertyKey::String("length".into()),
				PropertyValue::Value(length_value),
				annotation.get_position().with_source(environment.get_source()),
			);

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
		TypeAnnotation::KeyOf(_, _) => unimplemented!(),
		TypeAnnotation::Conditional { condition, resolve_true, resolve_false, position: _ } => {
			let condition = synthesise_type_annotation(condition, environment, checking_data);

			let truthy_result =
				synthesise_type_annotation(resolve_true, environment, checking_data);
			let otherwise_result =
				synthesise_type_annotation(resolve_false, environment, checking_data);

			let ty = Type::Constructor(Constructor::ConditionalResult {
				condition,
				truthy_result,
				otherwise_result,
				result_union: checking_data.types.new_or_type(truthy_result, otherwise_result),
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
		TypeAnnotation::TemplateLiteral(parts, _) => {
			let parts = parts
				.iter()
				.map(|part| match part {
					parser::ast::TemplateLiteralPart::Static(s) => {
						checking_data.types.new_constant_type(Constant::String(s.clone()))
					}
					parser::ast::TemplateLiteralPart::Dynamic(p) => {
						let annotation = match &**p {
							AnnotationWithBinder::Annotated { ty, .. }
							| AnnotationWithBinder::NoAnnotation(ty) => ty,
						};
						synthesise_type_annotation(annotation, environment, checking_data)
					}
				})
				.collect();

			synthesize_template_literal_type(parts, &mut checking_data.types)
		}
		TypeAnnotation::TypeOf(_item, position) => {
			checking_data.raise_unimplemented_error(
				"typeof annotation",
				position.with_source(environment.get_source()),
			);
			TypeId::ERROR_TYPE
		}
		TypeAnnotation::Infer(_name, position) => {
			checking_data.raise_unimplemented_error(
				"infer annotation",
				position.with_source(environment.get_source()),
			);
			TypeId::ERROR_TYPE
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
				"is annotation",
				position.with_source(environment.get_source()),
			);
			TypeId::ERROR_TYPE
			// let ty = Type::Constructor(Constructor::TypeRelationOperator(
			// 	crate::types::TypeRelationOperator::E { ty: item, is },
			// ));
			// checking_data.types.register_type(ty)
		}
		TypeAnnotation::Symbol { position, .. } => {
			checking_data.raise_unimplemented_error(
				"symbol annotation",
				position.with_source(environment.get_source()),
			);
			TypeId::ERROR_TYPE
		}
		TypeAnnotation::Asserts(_, position) => {
			// TODO construct condition for never
			checking_data.raise_unimplemented_error(
				"asserts annotation",
				position.with_source(environment.get_source()),
			);
			TypeId::ERROR_TYPE
		}
	};

	if checking_data.options.store_expression_type_mappings {
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
