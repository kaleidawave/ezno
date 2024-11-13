use std::borrow::Cow;

use parser::{
	declarations::VariableDeclarationItem, ASTNode, ArrayDestructuringField, Expression,
	ObjectDestructuringField, SpreadDestructuringField, VariableField, VariableIdentifier,
};

use super::expressions::synthesise_expression;
use crate::{
	context::{Context, ContextType, VariableRegisterArguments},
	diagnostics::{PropertyKeyRepresentation, TypeCheckError, TypeStringRepresentation},
	features::{
		self,
		variables::{get_new_register_argument_under, VariableMutability, VariableOrImport},
	},
	synthesis::parser_property_key_to_checker_property_key,
	types::{
		helpers::get_larger_type,
		printing,
		properties::{
			get_properties_on_single_type, get_property_key_names_on_a_single_type, PropertyKey,
			Publicity,
		},
	},
	CheckingData, Environment, TypeId,
};

pub(crate) fn register_variable_identifier<T: crate::ReadFromFS, V: ContextType>(
	name: &VariableIdentifier,
	environment: &mut Context<V>,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	argument: VariableRegisterArguments,
) {
	match name {
		parser::VariableIdentifier::Standard(name, pos) => {
			environment.register_variable_handle_error(
				name,
				argument,
				pos.with_source(environment.get_source()),
				&mut checking_data.diagnostics_container,
				&mut checking_data.local_type_mappings,
				checking_data.options.record_all_assignments_and_reads,
			);
		}
		parser::VariableIdentifier::Marker(..) => {
			crate::utilities::notify!("Skipping registering variable identifier that is Marker");
		}
	}
}

/// For eagerly registering variables, before the statement and its RHS is actually evaluate
///
/// TODO type annotations extras
pub(crate) fn register_variable<T: crate::ReadFromFS>(
	name: &parser::VariableField,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	argument: VariableRegisterArguments,
) {
	match name {
		parser::VariableField::Name(variable) => {
			register_variable_identifier(variable, environment, checking_data, argument);
		}
		parser::VariableField::Array { members, spread, position } => {
			if let Some(_spread) = spread {
				checking_data.raise_unimplemented_error(
					"spread variable field",
					position.with_source(environment.get_source()),
				);
			}
			for (idx, field) in members.iter().enumerate() {
				match field.get_ast_ref() {
					// ArrayDestructuringField::Spread(variable, _pos) => {
					// 	// TODO
					// 	let argument = VariableRegisterArguments {
					// 		constant: argument.constant,
					// 		space: argument.space,
					// 		initial_value: argument.initial_value,
					// 	};
					// 	register_variable(
					// 		variable,
					// 		environment,
					// 		checking_data,
					// 		// TODO
					// 		argument,
					// 	);
					// }
					ArrayDestructuringField::Name(name, _type, _initial_value) => {
						// TODO account for spread in `idx`
						let key = PropertyKey::from_usize(idx);
						let argument = get_new_register_argument_under(
							&argument,
							&key,
							environment,
							checking_data,
							name.get_position(),
						);
						register_variable(name, environment, checking_data, argument);
					}
					ArrayDestructuringField::Comment { .. } | ArrayDestructuringField::None => {}
				}
			}
		}
		parser::VariableField::Object { members, spread, .. } => {
			let mut taken_members = spread.is_some().then(Vec::<Cow<str>>::new);

			for field in members {
				match field.get_ast_ref() {
					ObjectDestructuringField::Name(variable, _type, ..) => {
						let name = match variable {
							VariableIdentifier::Standard(ref name, _) => name,
							VariableIdentifier::Marker(_, _) => "?",
						};
						if let Some(ref mut taken_members) = taken_members {
							taken_members.push(Cow::Borrowed(name));
						}
						let key = PropertyKey::String(Cow::Borrowed(name));
						let argument = get_new_register_argument_under(
							&argument,
							&key,
							environment,
							checking_data,
							variable.get_position(),
						);
						register_variable_identifier(
							variable,
							environment,
							checking_data,
							argument,
						);
					}
					ObjectDestructuringField::Map {
						from,
						// TODO
						annotation: _,
						name,
						default_value: _default_value,
						position,
					} => {
						let key = parser_property_key_to_checker_property_key(
							from,
							environment,
							checking_data,
							false,
						);
						if let Some(ref mut taken_members) = taken_members {
							match key {
								PropertyKey::String(ref s) => taken_members.push(s.clone()),
								PropertyKey::Type(_) => {
									crate::utilities::notify!("Cannot remove type");
								}
							}
						}
						let argument = get_new_register_argument_under(
							&argument,
							&key,
							environment,
							checking_data,
							*position,
						);
						register_variable(name.get_ast_ref(), environment, checking_data, argument);
					}
				}
			}
			if let (Some(taken_members), Some(SpreadDestructuringField(variable, _position))) =
				(taken_members, spread)
			{
				// TODO
				let initial_value = argument.initial_value;

				let space = if let Some(space) = argument.space {
					let mut rest = Vec::new();
					for (publicity, key, property) in get_properties_on_single_type(
						space,
						&checking_data.types,
						environment,
						true,
						TypeId::ANY_TYPE,
					) {
						if let PropertyKey::String(ref s) = key {
							if taken_members.contains(s) {
								continue;
							}
						}
						rest.push((publicity, key, property));
					}
					if rest.is_empty() {
						None
					} else {
						let rest = checking_data.types.new_anonymous_interface_type(rest);
						Some(rest)
					}
				} else {
					None
				};

				let argument = VariableRegisterArguments {
					constant: argument.constant,
					space,
					initial_value,
					allow_reregistration: argument.allow_reregistration,
				};

				register_variable(
					variable,
					environment,
					checking_data,
					// TODO
					argument,
				);
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
	checking_data: &mut CheckingData<T, super::EznoParser>,
	exported: Option<VariableMutability>,
	infer_constraint: bool,
) {
	// This is only added if there is an annotation, so can be None
	let get_position = variable_declaration.get_position();
	let var_ty_and_pos = checking_data
		.local_type_mappings
		.variable_restrictions
		.get(&(environment.get_source(), get_position.start))
		.map(|(ty, pos)| (*ty, *pos));

	// let name =
	// 	types.new_constant_type(crate::Constant::String(name_object.to_owned()));

	let value_ty = if let Some(expression) =
		U::as_option_expression_ref(&variable_declaration.expression)
	{
		let expected: TypeId =
			var_ty_and_pos.as_ref().map_or(TypeId::ANY_TYPE, |(var_ty, _)| *var_ty);

		// Crazy JavaScript behavior!!!
		let expected: TypeId = if let (
			VariableField::Name(name),
			Expression::ExpressionFunction(_) | Expression::ClassExpression(_),
		) = (variable_declaration.name.get_ast_ref(), expression)
		{
			let name = checking_data.types.new_constant_type(crate::Constant::String(
				name.as_option_str().unwrap_or_default().to_owned(),
			));
			features::functions::new_name_expected_object(
				name,
				expected,
				&mut checking_data.types,
				environment,
			)
		} else {
			expected
		};

		let value_ty = super::expressions::synthesise_expression(
			expression,
			environment,
			checking_data,
			expected,
		);

		if let Some((var_ty, ta_pos)) = var_ty_and_pos {
			let is_valid = crate::features::variables::check_variable_initialization(
				(var_ty, ta_pos),
				(value_ty, expression.get_position().with_source(environment.get_source())),
				environment,
				checking_data,
			);

			if !is_valid || value_ty == TypeId::ERROR_TYPE {
				// If error, then create a new type like the annotation
				checking_data.types.new_error_type(var_ty)
			} else {
				value_ty
			}
		} else if infer_constraint {
			let constraint = get_larger_type(value_ty, &checking_data.types);

			if let VariableField::Name(n) = variable_declaration.name.get_ast_ref() {
				if let VariableOrImport::Variable {
					mutability: VariableMutability::Mutable { reassignment_constraint },
					..
				} = environment.variables.get_mut(n.as_option_str().unwrap_or_default()).unwrap()
				{
					let _ = reassignment_constraint.insert(constraint);
				}
			} else {
				crate::utilities::notify!(
					"Infer constraint on {:?}",
					variable_declaration.name.get_ast_ref()
				);
			}

			value_ty
		} else {
			value_ty
		}
	} else {
		TypeId::UNDEFINED_TYPE
	};

	let item = variable_declaration.name.get_ast_ref();
	assign_initial_to_fields(item, environment, checking_data, value_ty, exported);
}

fn assign_initial_to_fields<T: crate::ReadFromFS>(
	item: &VariableField,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	value: TypeId,
	exported: Option<VariableMutability>,
) {
	match item {
		VariableField::Name(name) => {
			let position = name.get_position();
			let id = if let Some(aliases) =
				checking_data.local_type_mappings.var_aliases.get(&position.start)
			{
				*aliases
			} else {
				crate::VariableId(environment.get_source(), position.start)
			};

			environment.register_initial_variable_declaration_value(id, value);

			if let Some(mutability) = exported {
				if let crate::Scope::Module { ref mut exported, .. } =
					environment.context_type.scope
				{
					let name = match name {
						VariableIdentifier::Standard(ref name, _) => name.to_owned(),
						VariableIdentifier::Marker(_, _) => "?".to_owned(),
					};
					exported.named.insert(name, (id, mutability));
				} else {
					checking_data.diagnostics_container.add_error(
						TypeCheckError::NonTopLevelExport(
							name.get_position().with_source(environment.get_source()),
						),
					);
				}
			}
		}
		VariableField::Array { members: _, spread: _, position } => {
			checking_data.raise_unimplemented_error(
				"array spread",
				position.with_source(environment.get_source()),
			);
			// let position = position.with_source(environment.get_source());

			// if let Some(spread) = spread {
			// }
		}
		VariableField::Object { members, spread, position } => {
			for member in members {
				match member.get_ast_ref() {
					ObjectDestructuringField::Name(name, _, default_value, _) => {
						let position = name.get_position().with_source(environment.get_source());
						let id = crate::VariableId(environment.get_source(), position.start);

						let key_ty = match name {
							VariableIdentifier::Standard(name, _) => {
								PropertyKey::String(Cow::Borrowed(name))
							}
							VariableIdentifier::Marker(..) => PropertyKey::new_empty_property_key(),
						};

						// TODO if LHS = undefined ...? conditional
						// TODO record information
						let property = environment.get_property_handle_errors(
							value,
							Publicity::Public,
							&key_ty,
							checking_data,
							position,
							crate::types::properties::AccessMode::DoNotBindThis,
						);
						let value = match property {
							Ok(instance) => instance.get_value(),
							Err(()) => {
								if let Some(else_expression) = default_value {
									synthesise_expression(
										else_expression,
										environment,
										checking_data,
										TypeId::ANY_TYPE,
									)
								} else {
									checking_data.diagnostics_container.add_error(
										TypeCheckError::PropertyDoesNotExist {
											property: match key_ty {
												PropertyKey::String(s) => {
													PropertyKeyRepresentation::StringKey(
														s.to_string(),
													)
												}
												PropertyKey::Type(t) => {
													PropertyKeyRepresentation::Type(
														printing::print_type(
															t,
															&checking_data.types,
															environment,
															false,
														),
													)
												}
											},
											on: TypeStringRepresentation::from_type_id(
												value,
												environment,
												&checking_data.types,
												false,
											),
											position,
											possibles: get_property_key_names_on_a_single_type(
												value,
												&checking_data.types,
												environment,
											)
											.iter()
											.map(AsRef::as_ref)
											.collect::<Vec<&str>>(),
										},
									);
									TypeId::ANY_TYPE
								}
							}
						};

						environment.register_initial_variable_declaration_value(id, value);
					}
					ObjectDestructuringField::Map {
						from,
						name,
						annotation: _,
						default_value,
						position,
					} => {
						let key_ty = super::parser_property_key_to_checker_property_key(
							from,
							environment,
							checking_data,
							true,
						);

						// TODO if LHS = undefined ...? conditional
						let property_value = environment.get_property_handle_errors(
							value,
							Publicity::Public,
							&key_ty,
							checking_data,
							position.with_source(environment.get_source()),
							crate::types::properties::AccessMode::DoNotBindThis,
						);

						let value = match property_value {
							Ok(instance) => instance.get_value(),
							Err(()) => {
								if let Some(default_value) = default_value {
									synthesise_expression(
										default_value,
										environment,
										checking_data,
										TypeId::ANY_TYPE,
									)
								} else {
									checking_data.diagnostics_container.add_error(
										TypeCheckError::PropertyDoesNotExist {
											property: match key_ty {
												PropertyKey::String(s) => {
													PropertyKeyRepresentation::StringKey(
														s.to_string(),
													)
												}
												PropertyKey::Type(t) => {
													PropertyKeyRepresentation::Type(
														printing::print_type(
															t,
															&checking_data.types,
															environment,
															false,
														),
													)
												}
											},
											on: TypeStringRepresentation::from_type_id(
												value,
												environment,
												&checking_data.types,
												false,
											),
											position: position
												.with_source(environment.get_source()),
											possibles: get_property_key_names_on_a_single_type(
												value,
												&checking_data.types,
												environment,
											)
											.iter()
											.map(AsRef::as_ref)
											.collect::<Vec<&str>>(),
										},
									);

									TypeId::ERROR_TYPE
								}
							}
						};

						assign_initial_to_fields(
							name.get_ast_ref(),
							environment,
							checking_data,
							value,
							exported,
						);
					}
				}
			}
			if let Some(_spread) = spread {
				checking_data.raise_unimplemented_error(
					"spread variable field",
					position.with_source(environment.get_source()),
				);
			}
		}
	}
}
