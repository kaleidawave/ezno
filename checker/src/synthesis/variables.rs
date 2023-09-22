use std::borrow::Cow;

use ordered_float::NotNan;
use parser::{
	declarations::VariableDeclarationItem, ASTNode, ArrayDestructuringField,
	ObjectDestructuringField, VariableField, VariableIdentifier,
};

use super::{expressions::synthesize_expression, type_annotations::synthesize_type_annotation};
use crate::{
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	synthesis::property_key_as_type,
	types::Constant,
	CheckingData, Environment, TypeId,
};

/// For eagerly registering variables, before the statement and its RHS is actually evaluate
///
/// TODO shouldn't return type, for performs...
pub(crate) fn register_variable<T: crate::FSResolver, U: parser::VariableFieldKind>(
	name: &parser::VariableField<U>,
	environment: &mut Environment,
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
					pos.clone().with_source(environment.get_source()),
					behavior,
					checking_data,
				);
				if let Some(constraint) = constraint {
					checking_data
						.type_mappings
						.variables_to_constraints
						.0
						.insert(crate::VariableId(environment.get_source(), pos.start), constraint);
				}
				ty
			}
			parser::VariableIdentifier::Cursor(..) => todo!(),
		}
	}

	match name {
		parser::VariableField::Name(variable) => {
			register_variable_identifier(variable, environment, checking_data, behavior, constraint)
		}
		parser::VariableField::Array(items, _) => {
			for (idx, field) in items.iter().enumerate() {
				match field {
					ArrayDestructuringField::Spread(pos, variable) => {
						let ty = register_variable_identifier(
							variable,
							environment,
							checking_data,
							behavior.clone(),
							// TODO is this valid
							constraint,
						);
						// constraint.map(|constraint| environment.get_property_unbound(constraint, under, types) )
						// TODO
						// checking_data
						// 	.type_mappings
						// 	.variables_to_constraints
						// 	.0
						// 	.insert(crate::VariableId(pos.source, pos.start), constraint);
						// }
					}
					ArrayDestructuringField::Name(name, _) => {
						let property_constraint = constraint.map(|constraint| {
							let under = checking_data
								.types
								.new_constant_type(Constant::Number(NotNan::from(idx as u32)));
							let property_constraint = environment.get_property_unbound(
								constraint,
								under,
								&checking_data.types,
							);
							match property_constraint {
								Some(value) => value.prop_to_type(),
								None => {
									checking_data.diagnostics_container.add_error(
										TypeCheckError::PropertyDoesNotExist {
											property: TypeStringRepresentation::from_type_id(
												constraint,
												&environment.into_general_context(),
												&checking_data.types,
												false,
											),
											on: TypeStringRepresentation::from_type_id(
												constraint,
												&environment.into_general_context(),
												&checking_data.types,
												false,
											),
											site: name
												.get_position()
												.clone()
												.with_source(environment.get_source()),
										},
									);
									TypeId::ERROR_TYPE
								}
							}
						});
						register_variable(
							name.get_ast_ref(),
							environment,
							checking_data,
							behavior.clone(),
							property_constraint,
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
					ObjectDestructuringField::Spread(variable, _) => {
						let ty = register_variable_identifier(
							variable,
							environment,
							checking_data,
							behavior.clone(),
							// TODO
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
					ObjectDestructuringField::Name(variable, ..) => {
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
					ObjectDestructuringField::Map { from, name, default_value, position } => {
						let property_constraint = constraint.map(|constraint| {
							let under =
								property_key_as_type(from, environment, &mut checking_data.types);
							let property_constraint = environment.get_property_unbound(
								constraint,
								under,
								&checking_data.types,
							);
							match property_constraint {
								Some(value) => value.prop_to_type(),
								None => {
									checking_data.diagnostics_container.add_error(
										TypeCheckError::PropertyDoesNotExist {
											property: TypeStringRepresentation::from_type_id(
												constraint,
												&environment.into_general_context(),
												&checking_data.types,
												false,
											),
											on: TypeStringRepresentation::from_type_id(
												constraint,
												&environment.into_general_context(),
												&checking_data.types,
												false,
											),
											site: name
												.get_position()
												.clone()
												.with_source(environment.get_source()),
										},
									);
									TypeId::ERROR_TYPE
								}
							}
						});
						register_variable(
							name.get_ast_ref(),
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
	// This is only added if there is an annotation, so can be None
	let get_position = variable_declaration.get_position();
	let var_ty_and_pos = checking_data
		.type_mappings
		.variable_restrictions
		.get(&(environment.get_source(), get_position.start))
		.map(|(ty, pos)| (*ty, pos.clone()));

	let value_ty = if let Some(value) =
		Option::<&parser::Expression>::from(&variable_declaration.expression)
	{
		let value_ty = super::expressions::synthesize_expression(value, environment, checking_data);

		if let Some((var_ty, ta_pos)) = var_ty_and_pos {
			crate::check_variable_initialization(
				(var_ty, ta_pos),
				(value_ty, value.get_position().clone().with_source(environment.get_source())),
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
			let id = crate::VariableId(environment.get_source(), get_position.start);
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

						if let Some((_, value)) = value {
							assign_to_fields(
								variable_field.get_ast_ref(),
								environment,
								checking_data,
								value,
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
					ObjectDestructuringField::Name(name, default_value, _) => {
						let get_position = name.get_position();

						let id = crate::VariableId(environment.get_source(), get_position.start);

						let key_ty = match name {
							VariableIdentifier::Standard(name, _) => checking_data
								.types
								.new_constant_type(Constant::String(name.clone())),
							VariableIdentifier::Cursor(..) => todo!(),
						};

						// TODO if LHS = undefined ...? conditional
						// TODO record information
						let property =
							environment.get_property(value, key_ty, &mut checking_data.types, None);
						let value = match property {
							Some((_, value)) => value,
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
						let property_value =
							environment.get_property(value, key_ty, &mut checking_data.types, None);

						let value = match property_value {
							Some((_, value)) => value,
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
