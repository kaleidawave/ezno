use std::borrow::Cow;

use ordered_float::NotNan;
use parser::{
	declarations::VariableDeclarationItem, ASTNode, ArrayDestructuringField,
	ObjectDestructuringField, VariableField, VariableIdentifier,
};

use super::{expressions::synthesise_expression, type_annotations::synthesise_type_annotation};
use crate::{
	behavior::variables::VariableMutability,
	context::{facts::Publicity, Context, ContextType},
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	synthesis::parser_property_key_to_checker_property_key,
	types::{printing::print_type, properties::PropertyKey, Constant},
	CheckingData, Environment, TypeId,
};

/// For eagerly registering variables, before the statement and its RHS is actually evaluate
///
/// TODO shouldn't return type, for performs...
pub(crate) fn register_variable<T: crate::ReadFromFS, U: parser::VariableFieldKind>(
	name: &parser::VariableField<U>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	behavior: crate::context::VariableRegisterBehavior,
	constraint: Option<TypeId>,
) -> TypeId {
	fn register_variable_identifier<T: crate::ReadFromFS, V: ContextType>(
		name: &VariableIdentifier,
		environment: &mut Context<V>,
		checking_data: &mut CheckingData<T, super::EznoParser>,
		behavior: crate::context::VariableRegisterBehavior,
		constraint: Option<TypeId>,
	) -> TypeId {
		match name {
			parser::VariableIdentifier::Standard(name, pos) => {
				let ty = environment.register_variable_handle_error(
					name,
					pos.with_source(environment.get_source()),
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
							let under = crate::types::properties::PropertyKey::from_usize(idx);
							let property_constraint = environment.get_property_unbound(
								constraint,
								Publicity::Public,
								under.clone(),
								&checking_data.types,
							);
							if let Some(value) = property_constraint {
								value.prop_to_type()
							} else {
								checking_data
									.diagnostics_container
									.add_error(TypeCheckError::PropertyDoesNotExist {
									property: match under {
										PropertyKey::String(s) => {
											crate::diagnostics::PropertyRepresentation::StringKey(
												s.to_string(),
											)
										}
										PropertyKey::Type(t) => {
											crate::diagnostics::PropertyRepresentation::Type(
												print_type(
													t,
													&checking_data.types,
													&environment.as_general_context(),
													false,
												),
											)
										}
									},
									on: TypeStringRepresentation::from_type_id(
										constraint,
										&environment.as_general_context(),
										&checking_data.types,
										false,
									),
									site: name.get_position().with_source(environment.get_source()),
								});
								TypeId::ERROR_TYPE
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
			for field in items {
				match field.get_ast_ref() {
					ObjectDestructuringField::Name(variable, ..)
					| ObjectDestructuringField::Spread(variable, _) => {
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
							let under = parser_property_key_to_checker_property_key(
								from,
								environment,
								checking_data,
							);
							let property_constraint = environment.get_property_unbound(
								constraint,
								Publicity::Public,
								// TODO different above
								under.clone(),
								&checking_data.types,
							);
							if let Some(value) = property_constraint {
								value.prop_to_type()
							} else {
								checking_data
									.diagnostics_container
									.add_error(TypeCheckError::PropertyDoesNotExist {
									property: match under {
										PropertyKey::String(s) => {
											crate::diagnostics::PropertyRepresentation::StringKey(
												s.to_string(),
											)
										}
										PropertyKey::Type(t) => {
											crate::diagnostics::PropertyRepresentation::Type(
												print_type(
													t,
													&checking_data.types,
													&environment.as_general_context(),
													false,
												),
											)
										}
									},
									on: TypeStringRepresentation::from_type_id(
										constraint,
										&environment.as_general_context(),
										&checking_data.types,
										false,
									),
									site: name.get_position().with_source(environment.get_source()),
								});
								TypeId::ERROR_TYPE
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
/// TODO `U::as_option_expr()`
///
/// TODO no idea how arrays and objects are checked here
pub(super) fn synthesise_variable_declaration_item<
	T: crate::ReadFromFS,
	U: parser::ast::variable::DeclarationExpression + 'static,
>(
	variable_declaration: &VariableDeclarationItem<U>,
	environment: &mut Environment,
	is_constant: bool,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	exported: Option<VariableMutability>,
) {
	// This is only added if there is an annotation, so can be None
	let get_position = variable_declaration.get_position();
	let var_ty_and_pos = checking_data
		.type_mappings
		.variable_restrictions
		.get(&(environment.get_source(), get_position.start))
		.map(|(ty, pos)| (*ty, *pos));

	let value_ty = if let Some(value) = U::as_option_expr_ref(&variable_declaration.expression) {
		let expecting = if let Some((var_ty, _)) = var_ty_and_pos.as_ref() {
			*var_ty
		} else {
			TypeId::ANY_TYPE
		};

		let value_ty =
			super::expressions::synthesise_expression(value, environment, checking_data, expecting);

		if let Some((var_ty, ta_pos)) = var_ty_and_pos {
			crate::behavior::variables::check_variable_initialization(
				(var_ty, ta_pos),
				(value_ty, value.get_position().with_source(environment.get_source())),
				environment,
				checking_data,
			);
		}

		value_ty
	} else {
		TypeId::UNDEFINED_TYPE
	};

	let item = variable_declaration.name.get_ast_ref();
	assign_to_fields(item, environment, checking_data, value_ty, exported);
}

fn assign_to_fields<T: crate::ReadFromFS>(
	item: &VariableField<parser::VariableFieldInSourceCode>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	value: TypeId,
	exported: Option<VariableMutability>,
) {
	match item {
		VariableField::Name(name) => {
			let get_position = name.get_position();
			let id = crate::VariableId(environment.get_source(), get_position.start);
			environment.register_initial_variable_declaration_value(id, value);
			if let Some(mutability) = exported {
				if let crate::Scope::Module { ref mut exported, .. } =
					environment.context_type.scope
				{
					exported.named.push((name.as_str().to_owned(), (id, mutability)));
				} else {
					checking_data.diagnostics_container.add_error(
						TypeCheckError::NonTopLevelExport(
							name.get_position().with_source(environment.get_source()),
						),
					);
				}
			}
		}
		VariableField::Array(items, _) => {
			for (idx, item) in items.iter().enumerate() {
				match item {
					ArrayDestructuringField::Spread(_, _) => todo!(),
					ArrayDestructuringField::Name(variable_field, _) => {
						let idx = PropertyKey::from_usize(idx);

						let field_position =
							variable_field.get_position().with_source(environment.get_source());

						let value = environment.get_property(
							value,
							Publicity::Public,
							idx,
							&mut checking_data.types,
							None,
							field_position,
						);

						if let Some((_, value)) = value {
							assign_to_fields(
								variable_field.get_ast_ref(),
								environment,
								checking_data,
								value,
								exported,
							);
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
							VariableIdentifier::Standard(name, _) => {
								crate::types::properties::PropertyKey::String(Cow::Borrowed(name))
							}
							VariableIdentifier::Cursor(..) => todo!(),
						};

						let get_position_with_source =
							get_position.with_source(environment.get_source());

						// TODO if LHS = undefined ...? conditional
						// TODO record information
						let property = environment.get_property(
							value,
							Publicity::Public,
							key_ty,
							&mut checking_data.types,
							None,
							get_position_with_source,
						);
						let value = match property {
							Some((_, value)) => value,
							None => {
								// TODO non decidable error
								if let Some(else_expression) = default_value {
									synthesise_expression(
										else_expression,
										environment,
										checking_data,
										TypeId::ANY_TYPE,
									)
								} else {
									// TODO emit error
									TypeId::ERROR_TYPE
								}
							}
						};

						environment.register_initial_variable_declaration_value(id, value);
					}
					ObjectDestructuringField::Map { from, name, default_value, position } => {
						let key_ty = super::parser_property_key_to_checker_property_key(
							from,
							environment,
							checking_data,
						);

						// TODO if LHS = undefined ...? conditional
						// TODO record information
						let property_value = environment.get_property(
							value,
							Publicity::Public,
							// TODO different above
							key_ty,
							&mut checking_data.types,
							None,
							position.with_source(environment.get_source()),
						);

						let value = match property_value {
							Some((_, value)) => value,
							None => {
								// TODO non decidable error
								if let Some(default_value) = default_value {
									synthesise_expression(
										default_value,
										environment,
										checking_data,
										TypeId::ANY_TYPE,
									)
								} else {
									// TODO emit error
									TypeId::ERROR_TYPE
								}
							}
						};

						assign_to_fields(
							name.get_ast_ref(),
							environment,
							checking_data,
							value,
							exported,
						);
					}
				}
			}
		}
	}
}
