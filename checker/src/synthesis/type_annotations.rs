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

use crate::{
	synthesis::functions::type_function_reference,
	types::{properties::Property, Constant, Type},
	types::{Constructor, TypeId},
	CheckingData,
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
///
/// Use [Context::get_type] instead
pub(super) fn synthesize_type_annotation<S: ContextType, T: crate::FSResolver>(
	annotation: &TypeAnnotation,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T>,
) -> TypeId {
	let ty = match annotation {
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
		TypeAnnotation::Union(type_annotations) => {
			// TODO remove duplicates here maybe
			let mut iterator = type_annotations
				.iter()
				.map(|type_annotation| {
					synthesize_type_annotation(type_annotation, environment, checking_data)
				})
				.collect::<Vec<_>>()
				.into_iter();

			iterator
				.reduce(|acc, right| checking_data.types.new_or_type(acc, right))
				.expect("Empty union")
		}
		TypeAnnotation::Intersection(type_annotations) => {
			let mut iterator = type_annotations
				.iter()
				.map(|type_annotation| {
					synthesize_type_annotation(type_annotation, environment, checking_data)
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
				// TODO check restrictions + deferred
				let with = parameters.iter().copied().collect::<Vec<_>>().into_iter().zip(
					arguments.iter().map(|type_annotation| {
						synthesize_type_annotation(type_annotation, environment, checking_data)
					}),
				);
				let ty = Type::Constructor(Constructor::StructureGenerics {
					on: inner_type,
					with: with.collect(),
				});

				checking_data.types.register_type(ty)
			} else {
				todo!(
					"Parameters on non parameter type {:?} {} {:?}",
					inner_type,
					name,
					checking_data.types.get_type_by_id(inner_type)
				);
				TypeId::ERROR_TYPE
			}
		}
		TypeAnnotation::FunctionLiteral { type_parameters, parameters, return_type, .. } => {
			let function_type = type_function_reference(
				type_parameters,
				parameters,
				Some(&*return_type),
				environment,
				checking_data,
				super::Performs::None,
				parameters.position.union(&return_type.get_position()),
				crate::types::FunctionKind::Arrow,
				None,
			);
			checking_data.types.new_type_annotation_function_type(function_type)
		}
		TypeAnnotation::Readonly(type_annotation, _) => {
			let underlying_type =
				synthesize_type_annotation(&*type_annotation, environment, checking_data);

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
		TypeAnnotation::ArrayLiteral(item_type, _) => {
			let item_type = synthesize_type_annotation(&*item_type, environment, checking_data);
			let ty = Type::Constructor(Constructor::StructureGenerics {
				on: TypeId::ARRAY_TYPE,
				with: FromIterator::from_iter([(TypeId::T_TYPE, item_type)]),
			});
			checking_data.types.register_type(ty)
		}
		TypeAnnotation::ConstructorLiteral {
			type_parameters: _,
			parameters: _,
			return_type: _,
			new_keyword: _,
		} => unimplemented!(),
		// Object literals are first turned into types as if they were interface declarations and then
		// returns reference to object literal
		TypeAnnotation::ObjectLiteral(members, _) => {
			// TODO rather than onto, generate a new type...
			let onto = checking_data
				.types
				.register_type(Type::Object(crate::types::ObjectNature::AnonymousTypeAnnotation));

			super::interfaces::synthesize_signatures(
				&members,
				super::interfaces::OnToType(onto),
				environment,
				checking_data,
			)
			.0
		}
		// TODO little temp
		TypeAnnotation::TupleLiteral(members, _) => {
			let obj = todo!(); // environment.new_object(Some(TypeId::ARRAY_TYPE));

			let mut keys = IndexSet::new();
			for (idx, member) in members.iter().enumerate() {
				// TODO use name...?
				match member {
					TupleElement::NonSpread { name, ty } => {
						let idx_ty = checking_data
							.types
							.new_constant_type(Constant::Number((idx as f64).try_into().unwrap()));

						let item_ty = synthesize_type_annotation(ty, environment, checking_data);

						keys.insert(idx_ty);
						environment
							.facts
							.current_properties
							.entry(obj)
							.or_default()
							.push((idx_ty, Property::Value(item_ty)));
					}
					TupleElement::Spread { name, ty } => {
						todo!();
					}
				}
			}

			let constant = Constant::Number((members.len() as f64).try_into().unwrap());
			let length_value = checking_data.types.new_constant_type(constant);

			environment
				.facts
				.current_properties
				.entry(obj)
				.or_default()
				.push((TypeId::LENGTH_AS_STRING, Property::Value(length_value)));

			keys.insert(TypeId::LENGTH_AS_STRING);
			todo!();
			// environment.pro.insert(obj, keys);

			obj
		}
		TypeAnnotation::ParenthesizedReference(ref reference, _) => {
			synthesize_type_annotation(reference, environment, checking_data)
		}
		TypeAnnotation::Index(indexee, indexer, _) => {
			let indexee = synthesize_type_annotation(indexee, environment, checking_data);
			let indexer = synthesize_type_annotation(indexer, environment, checking_data);
			if let Some(prop) =
				environment.get_property_unbound(indexee, indexer, &checking_data.types)
			{
				match prop {
					crate::context::Logical::Pure(ty) => ty.as_get_type(),
					crate::context::Logical::Or(_) => todo!(),
					crate::context::Logical::Implies(_, _) => todo!(),
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
			let condition = synthesize_type_condition(condition, environment, checking_data);

			fn synthesize_condition(result: &TypeConditionResult) -> &TypeAnnotation {
				match result {
					TypeConditionResult::Reference(reference) => reference,
					TypeConditionResult::Infer(_infer, _) => todo!(),
				}
			}

			let truthy_result = synthesize_type_annotation(
				synthesize_condition(resolve_true),
				environment,
				checking_data,
			);
			let else_result = synthesize_type_annotation(
				synthesize_condition(resolve_false),
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
			match decorator.name.as_str() {
				// TODO temp
				"Proof" => {
					todo!("could do fun things here")
				}
				// Not sure if/how this will be used but have it anyway
				"InternalFunctionId" => {
					todo!()
				}
				"ExistingOpenDependentType" => {
					todo!()
				}
				"Events" => {
					todo!()
				}
				"OpenDependentTypeId" => {
					todo!()
					// let inner_type = environment.get_type(&inner, checking_data)?;
					// let constraint = checking_data.memory.new_fixed_constraint_id(inner_type);
					// let dependent_type =
					// 	Type::DependentType(DependentType::OpenDependentType { id: constraint });
					// Some(dependent_type))
				}
				// TODO not sure. this needs to feed from events somehow to actually do the specialization...
				"NewObjectWithProperties" => {
					if let Some(parser::Expression::ObjectLiteral(obj)) =
						decorator.arguments.as_ref().and_then(|args| args.first())
					{
						for member in obj.members.iter() {
							if let parser::expressions::object_literal::ObjectLiteralMember::Property(key, value, _) = member {
						} else {
							todo!("Invalid usage of 'NewObjectWithProperties' decorator")
						}
						}
						todo!()
					} else {
						todo!()
					}
				}
				decorator_name => {
					crate::utils::notify!("Unknown decorator skipping {:#?}", decorator_name);
					let inner_type = synthesize_type_annotation(&inner, environment, checking_data);
					inner_type
				}
			}
		}
		TypeAnnotation::TemplateLiteral(_, _) => todo!(),
	};

	checking_data.type_mappings.types_to_types.push(annotation.get_position().into_owned(), ty);

	ty
}

fn synthesize_type_condition<S: ContextType, T: crate::FSResolver>(
	condition: &TypeCondition,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T>,
) -> TypeId {
	match condition {
		TypeCondition::Extends { ty, extends, position } => {
			let item = synthesize_type_annotation(ty, environment, checking_data);
			let extends = synthesize_type_annotation(extends, environment, checking_data);
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
