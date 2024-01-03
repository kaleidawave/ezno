use std::borrow::Cow;

use parser::{
	declarations::VariableDeclarationItem, ASTNode, ArrayDestructuringField,
	ObjectDestructuringField, VariableField, VariableIdentifier,
};
use source_map::Span;

use super::expressions::synthesise_expression;
use crate::{
	context::{facts::Publicity, Context, ContextType},
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	features::variables::VariableMutability,
	synthesis::parser_property_key_to_checker_property_key,
	types::{printing::print_type, properties::PropertyKey},
	CheckingData, Environment, Instance, PropertyValue, TypeId,
};

/// For eagerly registering variables, before the statement and its RHS is actually evaluate
pub(crate) fn register_variable<T: crate::ReadFromFS, U: parser::VariableFieldKind>(
	name: &parser::VariableField<U>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	mutability: VariableMutability,
	initial_value: Option<TypeId>,
) {
	fn register_variable_identifier<T: crate::ReadFromFS, V: ContextType>(
		name: &VariableIdentifier,
		environment: &mut Context<V>,
		checking_data: &mut CheckingData<T, super::EznoParser>,
		mutability: VariableMutability,
		initial_value: Option<TypeId>,
	) {
		match name {
			parser::VariableIdentifier::Standard(name, pos) => {
				environment.register_variable_handle_error(
					name,
					mutability,
					pos.with_source(environment.get_source()),
					initial_value,
					checking_data,
				);

				if let VariableMutability::Mutable {
					reassignment_constraint: Some(reassignment_constraint),
				} = mutability
				{
					checking_data.type_mappings.variables_to_constraints.0.insert(
						crate::VariableId(environment.get_source(), pos.start),
						reassignment_constraint,
					);
				}
			}
			parser::VariableIdentifier::Cursor(..) => todo!(),
		}
	}

	match name {
		parser::VariableField::Name(variable) => register_variable_identifier(
			variable,
			environment,
			checking_data,
			mutability,
			initial_value,
		),
		parser::VariableField::Array(items, _) => {
			for (idx, field) in items.iter().enumerate() {
				match field {
					ArrayDestructuringField::Spread(_pos, variable) => {
						register_variable_identifier(
							variable,
							environment,
							checking_data,
							mutability,
							// TODO is this valid?
							initial_value,
						);
					}
					ArrayDestructuringField::Name(name, _initial_value) => {
						// TODO account for spread in `idx`
						let key = PropertyKey::from_usize(idx);
						let mutability = get_new_mutable_constraint(
							mutability,
							key.clone(),
							environment,
							checking_data,
							*name.get_position(),
						);
						let initial_value = initial_value.clone().map(|initial_value| {
							environment
								.get_property_handle_errors(
									initial_value,
									Publicity::Public,
									key,
									checking_data,
									*name.get_position(),
								)
								.map_or(TypeId::ERROR_TYPE, Instance::get_value)
						});
						register_variable(
							name.get_ast_ref(),
							environment,
							checking_data,
							mutability,
							initial_value,
						);
					}
					ArrayDestructuringField::None => {}
				}
			}
		}
		parser::VariableField::Object(items, _) => {
			for field in items {
				match field.get_ast_ref() {
					ObjectDestructuringField::Name(variable, ..) => {
						let key = PropertyKey::String(Cow::Borrowed(variable.as_str()));
						let mutability = get_new_mutable_constraint(
							mutability,
							key.clone(),
							environment,
							checking_data,
							*variable.get_position(),
						);
						let initial_value = initial_value.clone().map(|initial_value| {
							environment
								.get_property_handle_errors(
									initial_value,
									Publicity::Public,
									key,
									checking_data,
									*name.get_position(),
								)
								.map_or(TypeId::ERROR_TYPE, Instance::get_value)
						});
						register_variable_identifier(
							variable,
							environment,
							checking_data,
							mutability,
							initial_value,
						);
					}
					ObjectDestructuringField::Spread(variable, _) => {
						let _ty = register_variable_identifier(
							variable,
							environment,
							checking_data,
							mutability,
							initial_value,
						);
					}
					ObjectDestructuringField::Map {
						from,
						name,
						default_value: _default_value,
						position: _,
					} => {
						let key = parser_property_key_to_checker_property_key(
							from,
							environment,
							checking_data,
						);
						let mutability = get_new_mutable_constraint(
							mutability,
							key.clone(),
							environment,
							checking_data,
							*name.get_position(),
						);
						let initial_value = initial_value.clone().map(|initial_value| {
							environment
								.get_property_handle_errors(
									initial_value,
									Publicity::Public,
									key,
									checking_data,
									*name.get_position(),
								)
								.map_or(TypeId::ERROR_TYPE, Instance::get_value)
						});
						register_variable(
							name.get_ast_ref(),
							environment,
							checking_data,
							mutability,
							initial_value,
						);
					}
				}
			}
		}
	}
}

fn get_new_mutable_constraint<T: crate::ReadFromFS>(
	on: VariableMutability,
	under: PropertyKey,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	at: Span,
) -> VariableMutability {
	if let VariableMutability::Mutable { reassignment_constraint: Some(reassignment_constraint) } =
		on
	{
		let property_constraint = environment.get_property_unbound(
			reassignment_constraint,
			Publicity::Public,
			under.clone(),
			&checking_data.types,
		);
		let ty = if let Some(value) = property_constraint {
			match value {
				crate::context::Logical::Pure(PropertyValue::Value(value)) => value,
				crate::context::Logical::Pure(_) => todo!(),
				crate::context::Logical::Or { left: _, right: _ } => todo!(),
				crate::context::Logical::Implies { on: _, antecedent: _ } => {
					todo!()
				}
			}
		} else {
			checking_data.diagnostics_container.add_error(TypeCheckError::PropertyDoesNotExist {
				property: match under {
					PropertyKey::String(s) => {
						crate::diagnostics::PropertyRepresentation::StringKey(s.to_string())
					}
					PropertyKey::Type(t) => {
						crate::diagnostics::PropertyRepresentation::Type(print_type(
							t,
							&checking_data.types,
							&environment.as_general_context(),
							false,
						))
					}
				},
				on: TypeStringRepresentation::from_type_id(
					reassignment_constraint,
					&environment.as_general_context(),
					&checking_data.types,
					false,
				),
				site: at.with_source(environment.get_source()),
			});
			TypeId::ERROR_TYPE
		};
		VariableMutability::Mutable { reassignment_constraint: Some(ty) }
	} else {
		on
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
	_is_constant: bool,
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
			crate::features::variables::check_variable_initialization(
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

						let value = environment.get_property(
							value,
							Publicity::Public,
							idx,
							&mut checking_data.types,
							None,
							*variable_field.get_position(),
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
						let id =
							crate::VariableId(environment.get_source(), name.get_position().start);

						let key_ty = match name {
							VariableIdentifier::Standard(name, _) => {
								crate::types::properties::PropertyKey::String(Cow::Borrowed(name))
							}
							VariableIdentifier::Cursor(..) => todo!(),
						};

						// TODO if LHS = undefined ...? conditional
						// TODO record information
						let property = environment.get_property(
							value,
							Publicity::Public,
							key_ty,
							&mut checking_data.types,
							None,
							*name.get_position(),
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
							*position,
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
