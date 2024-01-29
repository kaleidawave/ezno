use std::borrow::Cow;

use parser::{
	declarations::VariableDeclarationItem, ASTNode, ArrayDestructuringField,
	ObjectDestructuringField, VariableField, VariableIdentifier,
};

use super::expressions::synthesise_expression;
use crate::{
	context::{facts::Publicity, Context, ContextType, VariableRegisterArguments},
	diagnostics::TypeCheckError,
	features::variables::{get_new_register_argument_under, VariableMutability},
	synthesis::parser_property_key_to_checker_property_key,
	types::properties::PropertyKey,
	CheckingData, Environment, TypeId,
};

/// For eagerly registering variables, before the statement and its RHS is actually evaluate
pub(crate) fn register_variable<T: crate::ReadFromFS, U: parser::VariableFieldKind>(
	name: &parser::VariableField<U>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	argument: VariableRegisterArguments,
) {
	fn register_variable_identifier<T: crate::ReadFromFS, V: ContextType>(
		name: &VariableIdentifier,
		environment: &mut Context<V>,
		checking_data: &mut CheckingData<T, super::EznoParser>,
		argument: VariableRegisterArguments,
	) {
		match name {
			parser::VariableIdentifier::Standard(name, pos) => {
				if let Some(reassignment_constraint) = argument.space {
					let id = crate::VariableId(environment.get_source(), pos.start);
					checking_data
						.type_mappings
						.variables_to_constraints
						.0
						.insert(id, reassignment_constraint);
				}

				environment.register_variable_handle_error(
					name,
					argument,
					pos.with_source(environment.get_source()),
					&mut checking_data.diagnostics_container,
				);
			}
			parser::VariableIdentifier::Marker(..) => todo!(),
		}
	}

	match name {
		parser::VariableField::Name(variable) => {
			register_variable_identifier(variable, environment, checking_data, argument);
		}
		parser::VariableField::Array(items, _) => {
			for (idx, field) in items.iter().enumerate() {
				match field.get_ast_ref() {
					ArrayDestructuringField::Spread(variable, _pos) => {
						register_variable_identifier(
							variable,
							environment,
							checking_data,
							// TODO
							VariableRegisterArguments {
								constant: argument.constant,
								space: argument.space,
								initial_value: argument.initial_value,
							},
						);
					}
					ArrayDestructuringField::Name(name, _initial_value) => {
						// TODO account for spread in `idx`
						let key = PropertyKey::from_usize(idx);
						let argument = get_new_register_argument_under(
							&argument,
							key,
							environment,
							checking_data,
							*name.get_position(),
						);
						register_variable(name, environment, checking_data, argument);
					}
					ArrayDestructuringField::None => {}
				}
			}
		}
		parser::VariableField::Object(items, _) => {
			for field in items {
				match field.get_ast_ref() {
					ObjectDestructuringField::Name(variable, ..) => {
						let name = match variable {
							VariableIdentifier::Standard(ref name, _) => name,
							VariableIdentifier::Marker(_, _) => "?",
						};
						let key = PropertyKey::String(Cow::Borrowed(name));
						let argument = get_new_register_argument_under(
							&argument,
							key,
							environment,
							checking_data,
							*variable.get_position(),
						);
						register_variable_identifier(
							variable,
							environment,
							checking_data,
							argument,
						);
					}
					ObjectDestructuringField::Spread(variable, _) => {
						register_variable_identifier(
							variable,
							environment,
							checking_data,
							// TODO
							VariableRegisterArguments {
								constant: argument.constant,
								space: argument.space,
								initial_value: argument.initial_value,
							},
						);
					}
					ObjectDestructuringField::Map {
						from,
						name,
						default_value: _default_value,
						position,
					} => {
						let key = parser_property_key_to_checker_property_key(
							from,
							environment,
							checking_data,
						);
						let argument = get_new_register_argument_under(
							&argument,
							key,
							environment,
							checking_data,
							*position,
						);
						register_variable(name.get_ast_ref(), environment, checking_data, argument);
					}
				}
			}
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
					let name = match name {
						VariableIdentifier::Standard(ref name, _) => name.to_owned(),
						VariableIdentifier::Marker(_, _) => "?".to_owned(),
					};
					exported.named.push((name, (id, mutability)));
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
				match item.get_ast_ref() {
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
								variable_field,
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
							VariableIdentifier::Marker(..) => todo!(),
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
