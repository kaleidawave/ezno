//! Logic for getting [TypeId] from [parser::TypeAnnotation]s
//!
//! ### There are several behaviors for type references depending on their position:
//! #### Sources:
//! - Type reference of any source variable declarations is a [crate::TypeConstraint]
//! - Type references in parameters are [crate::TypeConstraint]s
//! - Type references in returns types are also [crate::TypeConstraint]s, because ezno uses the body to get the return
//! type
//!
//! #### Declarations
//! - Type reference in any declaration or return type is a internal type [crate::Type::InternalObjectReference]
//!     - Return types need to know whether they return a unique object (todo don't know any examples)
//! or a new object. e.g. `Array.from`
//! - Parameters shouldn't do generic resolving
//!
//! ### Treatment of `any`
//! - Any has no properties because it is a union of all the types. It also means that it could be be `{}`
//! - To allow for compat it treats it as inferred generic **so it can get properties off of it**. Would be better
//! to allow this as a condition in the future

use std::{convert::TryInto, iter::FromIterator};

use indexmap::IndexSet;
use parser::{type_annotations::*, ASTNode};
use source_map::{SourceId, SpanWithSource};

use crate::{
	behavior::objects::ObjectBuilder,
	context::facts::PublicityKind,
	diagnostics::{TypeCheckError, TypeCheckWarning},
	subtyping::{self, type_is_subtype, BasicEquality, SubTypeResult},
	synthesis::functions::type_function_reference,
	types::{
		poly_types::generic_type_arguments::StructureGenericArguments, properties::Property,
		Constant, PolyNature, StructureGenerics, Type,
	},
	types::{Constructor, TypeId},
	CheckingData, Environment,
};

use crate::context::{Context, ContextType};

/// Turns a [parser::TypeAnnotation] into [TypeId]
///
/// [CheckingData] contains [Memory] and [crate::ErrorAndWarningHandler]
///
/// Returns a Type if it is found else a [Result::Err].
/// Errors other than non existent type are instead appended to the warning handler and a "default" is returned:
/// Example errors:
/// - Reference to generic without generic types
/// - Reference to non generic with generic types
pub(super) fn synthesise_type_annotation<T: crate::ReadFromFS>(
	annotation: &TypeAnnotation,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> TypeId {
	let ty = match annotation {
		TypeAnnotation::CommonName(name, _) => match name {
			CommonTypes::String => TypeId::STRING_TYPE,
			CommonTypes::Number => TypeId::NUMBER_TYPE,
			CommonTypes::Boolean => TypeId::BOOLEAN_TYPE,
		},
		TypeAnnotation::StringLiteral(value, _) => {
			checking_data.types.new_constant_type(Constant::String(value.clone()))
		}
		TypeAnnotation::NumberLiteral(value, _) => {
			let constant = Constant::Number(f64::from(value.clone()).try_into().unwrap());
			checking_data.types.new_constant_type(constant)
		}
		TypeAnnotation::BooleanLiteral(value, _) => {
			checking_data.types.new_constant_type(Constant::Boolean(value.clone()))
		}
		TypeAnnotation::Name(name, _) => match name.as_str() {
			"any" => checking_data.types.new_any_parameter(environment),
			"this" => todo!(), // environment.get_value_of_this(&mut checking_data.types),
			"self" => TypeId::THIS_ARG,
			name => {
				match environment.get_type_from_name(name) {
					Some(ty) => {
						// TODO warn if it requires parameters. e.g. Array
						// if let Type::AliasTo { parameters: Some(_), .. }
						// | Type::NamedRooted { parameters: Some(_), .. } = checking_data.types.get_type_by_id(ty)
						// {
						// 	todo!("Error");
						// }
						ty
					}
					None => TypeId::ERROR_TYPE,
				}
			}
		},
		TypeAnnotation::Union(type_annotations, _) => {
			// TODO remove duplicates here maybe
			let mut iterator = type_annotations
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
		TypeAnnotation::Intersection(type_annotations, _) => {
			let mut iterator = type_annotations
				.iter()
				.map(|type_annotation| {
					synthesise_type_annotation(type_annotation, environment, checking_data)
				})
				.collect::<Vec<_>>()
				.into_iter();

			iterator
				.reduce(|acc, right| checking_data.types.new_and_type(acc, right))
				.expect("Empty intersection")
		}
		// This will take the found type and generate a InstanceOfGeneric based on the type arguments
		TypeAnnotation::NameWithGenericArguments(name, arguments, position) => {
			match name.as_str() {
				"ReturnType" => todo!(),
				"Constructor" => todo!(),
				_ => {}
			}

			let inner_type = environment.get_type_from_name(name).unwrap();

			if let Some(parameters) =
				checking_data.types.get_type_by_id(inner_type).get_parameters()
			{
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

					let mut basic_equality = BasicEquality {
						add_property_restrictions: true,
						position: argument_type_annotation
							.get_position()
							.clone()
							.with_source(environment.get_source()),
					};

					let Type::RootPolyType(PolyNature::Generic {
						name: _,
						eager_fixed: parameter_restriction,
					}) = checking_data.types.get_type_by_id(parameter)
					else {
						unreachable!()
					};

					// TODO it is a bit weird with the arguments, maybe should get their restriction directly here?
					// Definition files don't necessary need to check ...
					let result = type_is_subtype(
						*parameter_restriction,
						argument,
						&mut basic_equality,
						environment,
						&checking_data.types,
					);

					if let SubTypeResult::IsNotSubType(matches) = result {
						let error = crate::diagnostics::TypeCheckError::GenericArgumentDoesNotMeetRestriction {
							parameter_restriction: crate::diagnostics::TypeStringRepresentation::from_type_id(
								*parameter_restriction,
								&environment.as_general_context(),
								&checking_data.types,
								checking_data.options.debug_types,
							),
							argument: crate::diagnostics::TypeStringRepresentation::from_type_id(
								argument,
								&environment.as_general_context(),
								&checking_data.types,
								checking_data.options.debug_types,
							),
							position: argument_type_annotation.get_position().clone().with_source(environment.get_source()),
						};

						checking_data.diagnostics_container.add_error(error);
					}

					let with_source = argument_type_annotation
						.get_position()
						.clone()
						.with_source(environment.get_source());

					type_arguments.insert(parameter, (argument, with_source));
				}

				let ty = Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
					on: inner_type,
					arguments: StructureGenericArguments {
						type_arguments,
						closures: Default::default(),
					},
				}));

				checking_data.types.register_type(ty)
			} else {
				checking_data.diagnostics_container.add_error(
					TypeCheckError::TypeHasNoGenericParameters(
						name.clone(),
						position.clone().with_source(environment.get_source()),
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
			let position = position.clone().with_source(environment.get_source());
			let function_type = type_function_reference(
				type_parameters,
				parameters,
				Some(&*return_type),
				environment,
				checking_data,
				super::Performs::None,
				position.clone(),
				crate::types::FunctionKind::Arrow,
				None,
			);
			// TODO bit messy
			checking_data.types.new_function_type_annotation(
				function_type.type_parameters,
				function_type.parameters,
				function_type.return_type,
				position.clone(),
				function_type.effects,
				None,
			)
		}
		TypeAnnotation::Readonly(type_annotation, _) => {
			let underlying_type =
				synthesise_type_annotation(&*type_annotation, environment, checking_data);

			todo!();

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
			let item_type =
				synthesise_type_annotation(&*item_annotation, environment, checking_data);
			let with_source =
				item_annotation.get_position().clone().with_source(environment.get_source());
			let ty = Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
				on: TypeId::ARRAY_TYPE,
				arguments: StructureGenericArguments {
					type_arguments: FromIterator::from_iter([(
						TypeId::T_TYPE,
						(item_type, with_source),
					)]),
					closures: Default::default(),
				},
			}));
			checking_data.types.register_type(ty)
		}
		TypeAnnotation::ConstructorLiteral {
			type_parameters: _,
			parameters: _,
			return_type: _,
			new_keyword: _,
			position,
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
				&members,
				super::interfaces::OnToType(onto),
				environment,
				checking_data,
			)
			.0
		}
		TypeAnnotation::TupleLiteral(members, _) => {
			// TODO maybe should be special type
			let mut obj = ObjectBuilder::new(
				Some(TypeId::ARRAY_TYPE),
				&mut checking_data.types,
				&mut environment.facts,
			);

			for (idx, (spread, member)) in members.iter().enumerate() {
				// TODO binder name under data...?
				match spread {
					SpreadKind::NonSpread => {
						let idx_ty = checking_data
							.types
							.new_constant_type(Constant::Number((idx as f64).try_into().unwrap()));

						let type_annotation = match member {
							AnnotationWithBinder::Annotated { ty, .. }
							| AnnotationWithBinder::NoAnnotation(ty) => ty,
						};

						let item_ty =
							synthesise_type_annotation(type_annotation, environment, checking_data);
						let ty_position = type_annotation
							.get_position()
							.clone()
							.with_source(environment.get_source());
						obj.append(
							environment,
							idx_ty,
							Property::Value(item_ty),
							PublicityKind::Public,
							Some(ty_position),
						);
					}
					SpreadKind::Spread => {
						todo!();
					}
				}
			}

			let constant = Constant::Number((members.len() as f64).try_into().unwrap());
			let length_value = checking_data.types.new_constant_type(constant);

			// TODO: Does `constant` have a position? Or should it have one?
			obj.append(
				environment,
				TypeId::LENGTH_AS_STRING,
				Property::Value(length_value),
				PublicityKind::Public,
				None,
			);

			obj.build_object()
		}
		TypeAnnotation::ParenthesizedReference(ref reference, _) => {
			synthesise_type_annotation(reference, environment, checking_data)
		}
		TypeAnnotation::Index(indexee, indexer, _) => {
			let indexee = synthesise_type_annotation(indexee, environment, checking_data);
			let indexer = synthesise_type_annotation(indexer, environment, checking_data);
			if let Some(prop) = environment.get_property_unbound(
				indexee,
				indexer,
				PublicityKind::Public,
				&checking_data.types,
			) {
				match prop {
					crate::context::Logical::Pure(ty) => ty.as_get_type(),
					crate::context::Logical::Or { .. } => todo!(),
					crate::context::Logical::Implies { .. } => todo!(),
				}
			} else {
				todo!("Error")
			}
			// indexee.get_property_using_type_annotation(
			//     &indexer,
			//     checking_data,
			//     crate::types::GetPropertySettings::HowAboutNo,
			// ))
		}
		TypeAnnotation::KeyOf(_, _) => unimplemented!(),
		TypeAnnotation::Conditional { condition, resolve_true, resolve_false, position } => {
			let condition = synthesise_type_condition(condition, environment, checking_data);

			fn synthesise_condition(result: &TypeConditionResult) -> &TypeAnnotation {
				match result {
					TypeConditionResult::Reference(reference) => reference,
					TypeConditionResult::Infer(_infer, _) => todo!(),
				}
			}

			let truthy_result = synthesise_type_annotation(
				synthesise_condition(resolve_true),
				environment,
				checking_data,
			);
			let else_result = synthesise_type_annotation(
				synthesise_condition(resolve_false),
				environment,
				checking_data,
			);

			let ty = Type::Constructor(Constructor::ConditionalResult {
				condition,
				truthy_result,
				else_result,
				result_union: checking_data.types.new_or_type(truthy_result, else_result),
			});

			checking_data.types.register_type(ty)
		}
		TypeAnnotation::Cursor(_, _) => {
			todo!("Dump available object types in environment to somewhere..?")
		}
		// TODO these are all work in progress
		TypeAnnotation::Decorated(decorator, inner, _) => {
			crate::utils::notify!("Unknown decorator skipping {:#?}", decorator.name);
			synthesise_type_annotation(&inner, environment, checking_data)
		}
		TypeAnnotation::TemplateLiteral(_, _) => todo!(),
	};

	checking_data
		.type_mappings
		.types_to_types
		.push(annotation.get_position().clone().with_source(environment.get_source()), ty);

	ty
}

fn synthesise_type_condition<T: crate::ReadFromFS>(
	condition: &TypeCondition,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> TypeId {
	match condition {
		TypeCondition::Extends { ty, extends, position } => {
			let item = synthesise_type_annotation(ty, environment, checking_data);
			let extends = synthesise_type_annotation(extends, environment, checking_data);
			todo!();
			// let ty = Type::Constructor(Constructor::BinaryOperator {
			// 	operator: crate::structures::operators::CanonicalBinaryOperator::InstanceOf,
			// 	lhs: item,
			// 	rhs: extends,
			// });
			// checking_data.types.register_type(ty)
		}
		// TODO requires a kind of strict instance of ???
		TypeCondition::Is { ty, is, position } => todo!(),
	}
}
