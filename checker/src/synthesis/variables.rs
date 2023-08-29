use std::borrow::Cow;

use parser::{
	declarations::VariableDeclarationItem, ASTNode, ArrayDestructuringField,
	ObjectDestructuringField, VariableField, VariableIdentifier,
};

use super::{expressions::synthesize_expression, type_annotations::synthesize_type_annotation};
use crate::{context::Logical, types::Constant, CheckingData, Environment, TypeId};

/// For eagerly registering variables, before the statement and its RHS is actually evaluate
///
/// TODO shouldn't return type, for performs...
pub(crate) fn register_variable<T: crate::FSResolver, U: parser::VariableFieldKind>(
	name: &parser::VariableField<U>,
	environment: &mut crate::context::Context<crate::context::Syntax<'_>>,
	checking_data: &mut CheckingData<'_, T>,
	behavior: crate::context::VariableRegisterBehavior,
	constraint: Option<TypeId>,
) -> TypeId {
	fn register_variable_identifier<T: crate::FSResolver>(
		name: &VariableIdentifier,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
		behavior: crate::context::VariableRegisterBehavior,
		constraint: Option<TypeId>,
	) -> TypeId {
		match name {
			parser::VariableIdentifier::Standard(name, pos) => {
				let ty = environment.register_variable_handle_error(
					&name,
					pos.clone(),
					behavior,
					checking_data,
				);
				if let Some(constraint) = constraint {
					checking_data
						.type_mappings
						.variables_to_constraints
						.0
						.insert(crate::VariableId(pos.source, pos.start), constraint);
				}
				ty
			}
			parser::VariableIdentifier::Cursor(_) => todo!(),
		}
	}

	match name {
		parser::VariableField::Name(variable) => {
			register_variable_identifier(variable, environment, checking_data, behavior, constraint)
		}
		parser::VariableField::Array(items, _) => {
			for field in items.iter() {
				match field {
					ArrayDestructuringField::Spread(pos, variable) => {
						let ty = register_variable_identifier(
							variable,
							environment,
							checking_data,
							behavior.clone(),
							constraint,
						);
						if let Some(constraint) = constraint {
							// TODO
							// checking_data
							// 	.type_mappings
							// 	.variables_to_constraints
							// 	.0
							// 	.insert(crate::VariableId(pos.source, pos.start), constraint);
						}
					}
					ArrayDestructuringField::Name(variable, _) => {
						// TODO get property on constraint
						register_variable(
							variable.get_ast_ref(),
							environment,
							checking_data,
							behavior.clone(),
							constraint,
						);
					}
					ArrayDestructuringField::None => {}
				}
			}
			TypeId::ERROR_TYPE
		}
		parser::VariableField::Object(items, _) => {
			for field in items.iter() {
				match field.get_ast_ref() {
					ObjectDestructuringField::Spread(_, variable) => {
						let ty = register_variable_identifier(
							variable,
							environment,
							checking_data,
							behavior.clone(),
							constraint,
						);
						if let Some(constraint) = constraint {
							// TODO
							// checking_data
							// 	.type_mappings
							// 	.variables_to_constraints
							// 	.0
							// 	.insert(crate::VariableId(pos.source, pos.start), constraint);
						}
					}
					ObjectDestructuringField::Name(variable, _) => {
						let ty = register_variable_identifier(
							variable,
							environment,
							checking_data,
							behavior.clone(),
							constraint,
						);
						if let Some(constraint) = constraint {
							// TODO
							// checking_data
							// 	.type_mappings
							// 	.variables_to_constraints
							// 	.0
							// 	.insert(crate::VariableId(pos.source, pos.start), constraint);
						}
					}
					ObjectDestructuringField::Map {
						from,
						name: variable_name,
						default_value,
						position,
					} => {
						register_variable(
							variable_name.get_ast_ref(),
							environment,
							checking_data,
							behavior.clone(),
							constraint,
						);
					}
				}
			}
			TypeId::ERROR_TYPE
		}
	}
}
/// Assigns values in the variable field into the environment. Will also synthesize default values
#[deprecated]
pub(super) fn synthesize_variable_field<T: crate::FSResolver>(
	variable_field: &VariableField<parser::VariableFieldInSourceCode>,
	variable_type: Option<TypeId>,
	expression_type: TypeId,
	is_constant: bool,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) {
	match variable_field {
		VariableField::Name(name) => {
			todo!()
			// declare_variable_in_environment(
			// 	environment,
			// 	name,
			// 	variable_type,
			// 	expression_type,
			// 	is_constant,
			// 	checking_data,
			// );
		}
		VariableField::Object(members, _) => {
			// TODO keep track of destructured properties for spread
			for member in members.iter() {
				match member.get_ast_ref() {
					ObjectDestructuringField::Name(name, default_value) => {
						if let Some(default_value) = default_value {
							synthesize_expression(default_value, environment, checking_data);
						}

						let VariableIdentifier::Standard(prop_name, _) = &name else { todo!() };

						let property = checking_data
							.types
							.new_constant_type(Constant::String(prop_name.clone()));

						let referencing_property: TypeId = environment
							.get_property(expression_type, property, &mut checking_data.types, None)
							.unwrap()
							.into();

						let variable_constraint = variable_type.map(|variable_type| {
							environment
								.get_property_unbound(variable_type, property, &checking_data.types)
								.map(Logical::prop_to_type)
								.expect("Pulling from object whose constraint doesn't match")
						});

						todo!()

						// declare_variable_in_environment(
						// 	environment,
						// 	name,
						// 	variable_constraint,
						// 	referencing_property,
						// 	is_constant,
						// 	checking_data,
						// );
					}
					ObjectDestructuringField::Map {
						from,
						name: variable_name,
						default_value,
						..
					} => {
						if let Some(default_value) = default_value {
							synthesize_expression(default_value, environment, checking_data);
						}
						todo!()
						// synthesize_variable_field(
						// 	variable_field.get_ast_mut(),
						// 	None,
						// 	referencing_property,
						// 	is_constant,
						// 	environment,
						// 	checking_data,
						//
						// )
					}
					ObjectDestructuringField::Spread(..) => unimplemented!(),
				};
			}
		}
		VariableField::Array(members, _) => {
			for (index, member) in members.iter().enumerate() {
				match member {
					ArrayDestructuringField::Spread(_, id) => todo!(),
					ArrayDestructuringField::Name(variable_field, _) => {
						let property = checking_data.types.new_constant_type(Constant::Number(
							(index as f64).try_into().unwrap(),
						));

						let referencing_property: TypeId = environment
							.get_property(expression_type, property, &mut checking_data.types, None)
							.unwrap()
							.into();

						synthesize_variable_field(
							variable_field.get_ast_ref(),
							None,
							referencing_property,
							is_constant,
							environment,
							checking_data,
						)
					}
					ArrayDestructuringField::None => todo!(),
				}
			}
		}
	}
}

/// Name already been hoisted,
///
/// TODO U::as_option_expr()
///
/// TODO no idea how arrays and objects are checked here
pub(super) fn synthesize_variable_declaration_item<
	T: crate::FSResolver,
	U: parser::ast::variable::DeclarationExpression + 'static,
>(
	variable_declaration: &VariableDeclarationItem<U>,
	environment: &mut Environment,
	is_constant: bool,
	checking_data: &mut CheckingData<T>,
) where
	for<'a> Option<&'a parser::Expression>: From<&'a U>,
{
	let var_ty_and_pos = variable_declaration.type_annotation.as_ref().map(|reference| {
		(
			synthesize_type_annotation(reference, environment, checking_data),
			reference.get_position().into_owned(),
		)
	});

	let value_ty = if let Some(value) =
		Option::<&parser::Expression>::from(&variable_declaration.expression)
	{
		let value_ty = super::expressions::synthesize_expression(value, environment, checking_data);

		if let Some((var_ty, ta_pos)) = var_ty_and_pos {
			crate::check_variable_initialization(
				(var_ty, Cow::Owned(ta_pos)),
				(value_ty, value.get_position()),
				environment,
				checking_data,
			);
		}

		value_ty
	} else {
		TypeId::UNDEFINED_TYPE
	};

	let item = variable_declaration.name.get_ast_ref();
	assign_to_fields(item, environment, checking_data, value_ty);
}

fn assign_to_fields<T: crate::FSResolver>(
	item: &VariableField<parser::VariableFieldInSourceCode>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	value: TypeId,
) {
	match item {
		VariableField::Name(name) => {
			let get_position = name.get_position();
			let id = crate::VariableId(get_position.source, get_position.start);
			environment.register_initial_variable_declaration_value(id, value)
		}
		VariableField::Array(items, _) => {
			for (idx, item) in items.iter().enumerate() {
				match item {
					ArrayDestructuringField::Spread(_, _) => todo!(),
					ArrayDestructuringField::Name(variable_field, _) => {
						let idx = checking_data
							.types
							.new_constant_type(Constant::Number((idx as f64).try_into().unwrap()));
						let value =
							environment.get_property(value, idx, &mut checking_data.types, None);

						if let Some(value) = value {
							assign_to_fields(
								variable_field.get_ast_ref(),
								environment,
								checking_data,
								value.into(),
							)
						}

						// TODO
					}
					ArrayDestructuringField::None => {}
				}
			}
		}
		VariableField::Object(items, _) => {
			for item in items {
				match item.get_ast_ref() {
					ObjectDestructuringField::Spread(_, _) => todo!(),
					ObjectDestructuringField::Name(name, default_value) => {
						let get_position = name.get_position();

						let id = crate::VariableId(get_position.source, get_position.start);

						let key_ty = match name {
							VariableIdentifier::Standard(name, _) => checking_data
								.types
								.new_constant_type(Constant::String(name.clone())),
							VariableIdentifier::Cursor(_) => todo!(),
						};

						// TODO if LHS = undefined ...? conditional
						// TODO record information
						let property =
							environment.get_property(value, key_ty, &mut checking_data.types, None);
						let value = match property {
							Some(result) => result.into(),
							None => {
								// TODO non decidable error
								if let Some(else_expression) = default_value {
									synthesize_expression(
										else_expression,
										environment,
										checking_data,
									)
								} else {
									// TODO emit error
									TypeId::ERROR_TYPE
								}
							}
						};

						environment.register_initial_variable_declaration_value(id, value)
					}
					ObjectDestructuringField::Map { from, name, default_value, position } => {
						let key_ty = super::property_key_as_type(
							from,
							environment,
							&mut checking_data.types,
						);

						// TODO if LHS = undefined ...? conditional
						// TODO record information
						let property =
							environment.get_property(value, key_ty, &mut checking_data.types, None);
						let value = match property {
							Some(result) => result.into(),
							None => {
								// TODO non decidable error
								if let Some(default_value) = default_value {
									synthesize_expression(default_value, environment, checking_data)
								} else {
									// TODO emit error
									TypeId::ERROR_TYPE
								}
							}
						};

						assign_to_fields(name.get_ast_ref(), environment, checking_data, value)
					}
				}
			}
		}
	}
}
