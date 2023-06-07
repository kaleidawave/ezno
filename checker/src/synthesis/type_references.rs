//! Logic for getting [TypeId] from [parser::TypeReference]s
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
use parser::{type_references::*, ASTNode};

use crate::{
	errors::CannotFindTypeError,
	synthesis::{functions::type_function_reference, interfaces::type_interface_member},
	types::{Constant, Type},
	types::{Constructor, TypeId},
	CheckingData,
};

use crate::context::{Context, ContextType};

/// Turns a [parser::TypeReference] into [TypeId]
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
pub(crate) fn get_type_from_reference<'a, S: ContextType, T: crate::FSResolver>(
	environment: &mut Context<S>,
	reference: &'a TypeReference,
	checking_data: &mut CheckingData<T>,
) -> Result<TypeId, CannotFindTypeError<'a>> {
	match reference {
		TypeReference::StringLiteral(value, _) => {
			Ok(checking_data.types.new_constant_type(Constant::String(value.clone())))
		}
		TypeReference::NumberLiteral(value, _) => {
			let constant = Constant::Number(f64::from(value.clone()).try_into().unwrap());
			Ok(checking_data.types.new_constant_type(constant))
		}
		TypeReference::BooleanLiteral(value, _) => {
			Ok(checking_data.types.new_constant_type(Constant::Boolean(value.clone())))
		}
		TypeReference::Name(name, _) => match name.as_str() {
			"any" => Ok(checking_data.types.new_any_parameter(environment)),
			"this" => Ok(environment.get_value_of_this(&mut checking_data.types)),
			"self" => Ok(TypeId::THIS_ARG),
			name => {
				let ty = environment
					.get_type_from_name(name)
					.ok_or_else(|| CannotFindTypeError(reference))?;

				if let Type::AliasTo { parameters: Some(_), .. }
				| Type::NamedRooted { parameters: Some(_), .. } = checking_data.types.get_type_by_id(ty)
				{
					todo!("Error");
				}

				Ok(ty)
			}
		},
		TypeReference::Union(type_references) => {
			// TODO remove duplicates here maybe
			let mut iterator = type_references
				.iter()
				.map(|type_reference| {
					environment.get_type_handle_errors(type_reference, checking_data)
				})
				.collect::<Vec<_>>()
				.into_iter();

			let ty = iterator
				.reduce(|acc, right| checking_data.types.new_type(Type::Or(acc, right)))
				.expect("Empty union");

			Ok(ty)
		}
		TypeReference::Intersection(type_references) => {
			let mut iterator = type_references
				.iter()
				.map(|type_reference| {
					environment.get_type_handle_errors(type_reference, checking_data)
				})
				.collect::<Vec<_>>()
				.into_iter();

			let ty = iterator
				.reduce(|acc, right| checking_data.types.new_type(Type::And(acc, right)))
				.expect("Empty intersection");

			Ok(ty)
		}
		// This will take the found type and generate a InstanceOfGeneric based on the type arguments
		TypeReference::NameWithGenericArguments(name, arguments, position) => {
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
					arguments.iter().map(|type_reference| {
						environment.get_type_handle_errors(type_reference, checking_data)
					}),
				);
				let ty = Type::Constructor(Constructor::StructureGenerics {
					on: inner_type,
					with: with.collect(),
				});

				Ok(checking_data.types.new_type(ty))
			} else {
				todo!(
					"Parameters on non parameter type {:?} {} {:?}",
					inner_type,
					name,
					checking_data.types.get_type_by_id(inner_type)
				);
				Ok(TypeId::ERROR_TYPE)
			}
		}
		TypeReference::FunctionLiteral { type_parameters, parameters, return_type, .. } => {
			let function_type = type_function_reference(
				type_parameters,
				parameters,
				Some(&*return_type),
				environment,
				checking_data,
				parameters.position.union(&return_type.get_position()),
				crate::structures::functions::FunctionNature::Arrow,
			);

			// let id = checking_data.types.new_type(Type::AliasTo {
			// 	to: TypeId::FUNCTION_TYPE,
			// 	name: None,
			// 	parameters: None,
			// });
			todo!();
			// environment.functions_on_type.insert(id, function_type);
			// Ok(id)
		}
		TypeReference::Readonly(type_reference, _) => {
			let underlying_type = environment.get_type(&*type_reference, checking_data)?;

			todo!();

			// let ty_to_be_readonly = checking_data.types.new_type(Type::AliasTo {
			// 	to: underlying_type,
			// 	name: None,
			// 	parameters: None,
			// });

			// // TODO I think Readonly == freeze...?
			// environment.frozen.insert(ty_to_be_readonly, TypeId::TRUE);

			// Ok(ty_to_be_readonly)
		}
		TypeReference::NamespacedName(_, _, _) => unimplemented!(),
		TypeReference::ArrayLiteral(item_type, _) => {
			let item_type = environment.get_type(&*item_type, checking_data)?;
			let ty = Type::Constructor(Constructor::StructureGenerics {
				on: TypeId::ARRAY_TYPE,
				with: FromIterator::from_iter([(TypeId::T_TYPE, item_type)]),
			});
			Ok(checking_data.types.new_type(ty))
		}
		TypeReference::ConstructorLiteral {
			type_parameters: _,
			parameters: _,
			return_type: _,
			new_keyword: _,
		} => unimplemented!(),
		// Object literals are first turned into types as if they were interface declarations and then
		// returns reference to object literal
		TypeReference::ObjectLiteral(members, type_id, _) => {
			let ty_id = checking_data
				.types
				.new_type(Type::Object(crate::types::ObjectNature::AnonymousTypeAnnotation));

			for member in members {
				type_interface_member(member, environment, checking_data, ty_id);
			}

			Ok(ty_id)
		}
		// TODO little temp
		TypeReference::TupleLiteral(members, _, _) => {
			let obj = todo!(); // environment.new_object(Some(TypeId::ARRAY_TYPE));

			let mut keys = IndexSet::new();
			for (idx, member) in members.iter().enumerate() {
				// TODO use name...?
				match member {
					TupleElement::NonSpread { name, ty } => {
						let idx_ty = checking_data
							.types
							.new_constant_type(Constant::Number((idx as f64).try_into().unwrap()));

						let item_ty = environment.get_type_handle_errors(ty, checking_data);

						keys.insert(idx_ty);
						environment.properties.entry(obj).or_default().push((idx_ty, item_ty));
					}
					TupleElement::Spread { name, ty } => {
						todo!();
					}
				}
			}

			let constant = Constant::Number((members.len() as f64).try_into().unwrap());
			let length_value = checking_data.types.new_constant_type(constant);

			environment
				.properties
				.entry(obj)
				.or_default()
				.push((TypeId::LENGTH_AS_STRING, length_value));

			keys.insert(TypeId::LENGTH_AS_STRING);
			todo!();
			// environment.pro.insert(obj, keys);

			Ok(obj)
		}
		TypeReference::ParenthesizedReference(ref reference, _) => {
			environment.get_type(reference, checking_data)
		}
		TypeReference::Index(indexee, indexer, _) => {
			let indexee = environment.get_type_handle_errors(indexee, checking_data);
			let indexer = environment.get_type_handle_errors(indexer, checking_data);
			if let Some(prop) =
				environment.get_property_unbound(indexee, indexer, &checking_data.types)
			{
				match prop {
					crate::context::Logical::Pure(ty) => Ok(ty),
					crate::context::Logical::Or(_) => todo!(),
					crate::context::Logical::Implies(_, _) => todo!(),
				}
			} else {
				todo!("Error")
			}
			// Ok(indexee.get_property_using_type_reference(
			//     &indexer,
			//     checking_data,
			//     crate::types::GetPropertySettings::HowAboutNo,
			// ))
		}
		TypeReference::KeyOf(_, _) => unimplemented!(),
		TypeReference::Conditional { condition, resolve_true, resolve_false, position } => {
			let condition = type_condition_to_type(environment, condition, checking_data)?;

			fn a(result: &TypeConditionResult) -> &TypeReference {
				match result {
					TypeConditionResult::Reference(reference) => reference,
					TypeConditionResult::Infer(_infer, _) => todo!(),
				}
			}

			let resolve_true = environment.get_type_handle_errors(a(resolve_true), checking_data);
			let resolve_false = environment.get_type_handle_errors(a(resolve_false), checking_data);

			let ty = Type::Constructor(Constructor::ConditionalTernary {
				on: condition,
				t_res: resolve_true,
				f_res: resolve_false,
			});

			Ok(checking_data.types.new_type(ty))
		}
		TypeReference::Cursor(_, _) => {
			todo!("Dump available object types in environment to somewhere..?")
		}
		// TODO these are all work in progress
		TypeReference::Decorated(decorator, inner, _) => {
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
					// Ok(Some(dependent_type))
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
					let inner_type = environment.get_type_handle_errors(&inner, checking_data);
					Ok(inner_type)
				}
			}
		}
		TypeReference::TemplateLiteral(_, _) => todo!(),
	}
}

fn argument_to_number(expression: &parser::Expression) -> Result<f64, ()> {
	if let parser::Expression::NumberLiteral(nl, _, _) = expression {
		Ok(f64::from(*nl))
	} else if let parser::Expression::VariableReference(name, _, _) = expression {
		if name == "inf" {
			Ok(f64::INFINITY)
		} else if name == "neg_inf" {
			Ok(f64::NEG_INFINITY)
		} else {
			Err(())
		}
	} else {
		Err(())
	}
}

fn type_condition_to_type<'a, S: ContextType, T: crate::FSResolver>(
	environment: &mut Context<S>,
	condition: &'a TypeCondition,
	checking_data: &mut CheckingData<T>,
) -> Result<TypeId, CannotFindTypeError<'a>> {
	match condition {
		TypeCondition::Extends { r#type, extends, position } => {
			let item = environment.get_type(&*r#type, checking_data)?;
			let extends = environment.get_type(&*extends, checking_data)?;
			let ty = todo!();
			// let ty = Type::Constructor(Constructor::BinaryOperator {
			// 	operator: parser::operators::BinaryOperator::InstanceOf,
			// 	lhs: item,
			// 	rhs: extends,
			// });
			let ty = checking_data.types.new_type(ty);
			Ok(ty)
		}
		// TODO requires a kind of strict instance of ???
		TypeCondition::Is { r#type, is, position } => todo!(),
	}
}
