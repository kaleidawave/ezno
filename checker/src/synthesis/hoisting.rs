use std::{collections::HashMap, iter};

use parser::{
	declarations::{export::Exportable, DeclareVariableDeclaration, ExportDeclaration},
	ASTNode, Declaration, Decorated, ExpressionOrStatementPosition, Statement,
	StatementOrDeclaration, VariableIdentifier, WithComment,
};

use crate::{
	context::{Environment, VariableRegisterArguments},
	features::{
		functions::synthesise_hoisted_statement_function,
		modules::{import_items, ImportKind, NamePair},
		variables::VariableMutability,
	},
	CheckingData, ReadFromFS, TypeId,
};

use super::{
	functions::synthesise_function_annotation,
	type_annotations::{comment_as_type_annotation, synthesise_type_annotation},
	variables::register_variable,
	EznoParser,
};

pub(crate) fn hoist_statements<T: crate::ReadFromFS>(
	items: &[StatementOrDeclaration],
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	let mut idx_to_types = HashMap::new();

	// First stage
	for item in items {
		if let StatementOrDeclaration::Declaration(declaration) = item {
			match declaration {
				parser::Declaration::DeclareVariable(_)
				| parser::Declaration::DeclareFunction(_)
				| parser::Declaration::Class(_)
				| parser::Declaration::Variable(_)
				| parser::Declaration::Function(_) => {}
				parser::Declaration::Enum(r#enum) => checking_data.raise_unimplemented_error(
					"enum",
					r#enum.on.position.with_source(environment.get_source()),
				),
				parser::Declaration::DeclareInterface(interface) => {
					// TODO any difference bc declare?
					let ty = environment.new_interface(
						&interface.name,
						interface.is_nominal,
						interface.type_parameters.as_deref(),
						interface.extends.as_deref(),
						interface.position.with_source(environment.get_source()),
						checking_data,
					);
					idx_to_types.insert(interface.position.start, ty);
				}
				parser::Declaration::Interface(interface) => {
					let ty = environment.new_interface(
						&interface.on.name,
						interface.on.is_nominal,
						interface.on.type_parameters.as_deref(),
						interface.on.extends.as_deref(),
						interface.on.position.with_source(environment.get_source()),
						checking_data,
					);
					idx_to_types.insert(interface.on.position.start, ty);
				}
				parser::Declaration::TypeAlias(alias) => {
					environment.new_alias(
						&alias.type_name.name,
						alias.type_name.type_parameters.as_deref(),
						&alias.type_expression,
						*alias.get_position(),
						checking_data,
					);
				}
				parser::Declaration::Import(import) => {
					let items = match &import.items {
						parser::declarations::import::ImportedItems::Parts(parts) => {
							crate::features::modules::ImportKind::Parts(
								parts.iter().flatten().filter_map(import_part_to_name_pair),
							)
						}
						parser::declarations::import::ImportedItems::All { under } => match under {
							VariableIdentifier::Standard(under, position) => {
								crate::features::modules::ImportKind::All {
									under,
									position: *position,
								}
							}
							VariableIdentifier::Marker(_, _) => todo!(),
						},
					};
					let default_import = import.default.as_ref().and_then(|default_identifier| {
						match default_identifier {
							VariableIdentifier::Standard(name, position) => {
								Some((name.as_str(), *position))
							}
							VariableIdentifier::Marker(..) => None,
						}
					});
					import_items(
						environment,
						import.from.get_path().unwrap(),
						import.position,
						default_import,
						items,
						checking_data,
						false,
						import.is_type_annotation_import_only,
					);
				}
				parser::Declaration::Export(export) => {
					if let ExportDeclaration::Variable { exported, position } = &export.on {
						// Imports & types
						match exported {
							Exportable::ImportAll { r#as, from } => {
								let kind = match r#as {
									Some(VariableIdentifier::Standard(name, position)) => {
										ImportKind::All { under: name, position: *position }
									}
									Some(VariableIdentifier::Marker(_, _)) => todo!(),
									None => ImportKind::Everything,
								};

								import_items::<iter::Empty<_>, _, _>(
									environment,
									from.get_path().unwrap(),
									*position,
									None,
									kind,
									checking_data,
									true,
									// TODO
									false,
								);
							}
							Exportable::ImportParts {
								parts, from, type_definitions_only, ..
							} => {
								let parts = parts.iter().filter_map(export_part_to_name_pair);

								import_items(
									environment,
									from.get_path().unwrap(),
									*position,
									None,
									crate::features::modules::ImportKind::Parts(parts),
									checking_data,
									true,
									*type_definitions_only,
								);
							}
							Exportable::TypeAlias(alias) => {
								let export = environment.new_alias::<_, EznoParser>(
									&alias.type_name.name,
									alias.type_name.type_parameters.as_deref(),
									&alias.type_expression,
									*alias.get_position(),
									checking_data,
								);

								if let crate::Scope::Module { ref mut exported, .. } =
									environment.context_type.scope
								{
									exported
										.named_types
										.push((alias.type_name.name.clone(), export));
								}
							}
							// Other exported things are skipped
							_ => {}
						}
					}
				}
			}
		}
	}

	// Second stage
	for item in items {
		match item {
			StatementOrDeclaration::Statement(stmt) => {
				if let Statement::VarVariable(_) = stmt {
					checking_data.raise_unimplemented_error(
						"var statement hoisting",
						stmt.get_position().with_source(environment.get_source()),
					);
				}
			}
			StatementOrDeclaration::Declaration(dec) => match dec {
				parser::Declaration::Variable(declaration) => {
					hoist_variable_declaration(declaration, environment, checking_data);
				}
				parser::Declaration::Function(func) => {
					if let Some(VariableIdentifier::Standard(name, ..)) =
						func.on.name.as_option_variable_identifier()
					{
						environment.register_variable_handle_error(
							name,
							VariableRegisterArguments {
								// TODO functions are constant references
								constant: true,
								space: None,
								initial_value: None,
							},
							func.get_position().with_source(environment.get_source()),
							&mut checking_data.diagnostics_container,
						);
					}
				}
				parser::Declaration::DeclareFunction(func) => {
					// TODO abstract
					let declared_at = func.position.with_source(environment.get_source());
					let base = synthesise_function_annotation(
						&func.type_parameters,
						&func.parameters,
						func.return_type.as_ref(),
						environment,
						checking_data,
						func.performs.as_ref().into(),
						&declared_at,
						crate::features::functions::FunctionBehavior::ArrowFunction {
							is_async: false,
						},
						None,
					);

					let base = checking_data.types.new_function_type_annotation(
						base.type_parameters,
						base.parameters,
						base.return_type,
						&declared_at,
						base.effects,
						base.constant_function,
					);

					environment.register_variable_handle_error(
						func.name.as_str(),
						VariableRegisterArguments {
							// TODO functions are constant references
							constant: true,
							space: None,
							initial_value: Some(base),
						},
						func.get_position().with_source(environment.get_source()),
						&mut checking_data.diagnostics_container,
					);
				}
				parser::Declaration::Enum(r#enum) => {
					checking_data.raise_unimplemented_error(
						"enum",
						r#enum.position.with_source(environment.get_source()),
					);
				}
				parser::Declaration::Interface(interface) => {
					let ty = idx_to_types.remove(&interface.on.position.start).unwrap();
					super::interfaces::synthesise_signatures(
						interface.on.type_parameters.as_deref(),
						&interface.on.members,
						super::interfaces::OnToType(ty),
						environment,
						checking_data,
					);
				}
				parser::Declaration::DeclareVariable(DeclareVariableDeclaration {
					keyword: _,
					declarations,
					position: _,
					decorators: _,
				}) => {
					for declaration in declarations {
						let constraint = get_annotation_from_declaration(
							declaration,
							environment,
							checking_data,
						);

						if constraint.is_none() {
							crate::utils::notify!("constraint with no type?");
						}

						let value = constraint.unwrap_or(TypeId::ANY_TYPE);

						let ty = checking_data.types.register_type(crate::Type::RootPolyType(
							crate::types::PolyNature::Open(value),
						));

						register_variable(
							declaration.name.get_ast_ref(),
							environment,
							checking_data,
							VariableRegisterArguments {
								// TODO based on keyword
								constant: true,
								space: None,
								initial_value: Some(ty),
							},
						);
					}
				}

				parser::Declaration::Export(exported) => match &exported.on {
					parser::declarations::ExportDeclaration::Variable { exported, position: _ } => {
						match exported {
							Exportable::Function(func) => {
								let declared_at =
									func.get_position().with_source(environment.get_source());

								if let Some(VariableIdentifier::Standard(name, ..)) =
									func.name.as_option_variable_identifier()
								{
									environment.register_variable_handle_error(
										name,
										VariableRegisterArguments {
											// TODO based on keyword
											constant: true,
											space: None,
											initial_value: None,
										},
										declared_at,
										&mut checking_data.diagnostics_container,
									);
								}
							}
							Exportable::Variable(declaration) => {
								// TODO mark exported
								hoist_variable_declaration(declaration, environment, checking_data);
							}
							Exportable::Interface(interface) => {
								let ty = idx_to_types.remove(&interface.position.start).unwrap();
								super::interfaces::synthesise_signatures(
									interface.type_parameters.as_deref(),
									&interface.members,
									super::interfaces::OnToType(ty),
									environment,
									checking_data,
								);
							}
							Exportable::TypeAlias(_)
							| Exportable::Parts(..)
							| Exportable::ImportAll { .. }
							| Exportable::ImportParts { .. }
							| Exportable::Class(_) => {}
						}
					}
					parser::declarations::ExportDeclaration::Default { .. } => {}
				},
				parser::Declaration::Class(_)
				| parser::Declaration::TypeAlias(_)
				| parser::Declaration::DeclareInterface(_)
				| parser::Declaration::Import(_) => {}
			},
			StatementOrDeclaration::Marker(_, _) => {}
		}
	}

	// Third stage: functions
	for item in items {
		match item {
			StatementOrDeclaration::Declaration(Declaration::Function(function)) => {
				let variable_id =
					crate::VariableId(environment.get_source(), item.get_position().start);
				let is_async = function.on.header.is_async();
				let is_generator = function.on.header.is_generator();
				let location = function.on.header.get_location().map(|location| match location {
					parser::functions::FunctionLocationModifier::Server => "server".to_owned(),
					parser::functions::FunctionLocationModifier::Worker => "worker".to_owned(),
				});

				synthesise_hoisted_statement_function(
					variable_id,
					is_async,
					is_generator,
					location,
					&function.on,
					environment,
					checking_data,
				);
			}
			StatementOrDeclaration::Declaration(Declaration::Export(Decorated {
				on:
					ExportDeclaration::Variable {
						exported: Exportable::Function(function),
						position: _,
					},
				..
			})) => {
				let variable_id =
					crate::VariableId(environment.get_source(), item.get_position().start);

				let is_async = function.header.is_async();
				let is_generator = function.header.is_generator();
				let location = function.header.get_location().map(|location| match location {
					parser::functions::FunctionLocationModifier::Server => "server".to_owned(),
					parser::functions::FunctionLocationModifier::Worker => "worker".to_owned(),
				});

				synthesise_hoisted_statement_function(
					variable_id,
					is_async,
					is_generator,
					location,
					function,
					environment,
					checking_data,
				);

				if let crate::Scope::Module { ref mut exported, .. } =
					environment.context_type.scope
				{
					// TODO check existing?
					if let Some(VariableIdentifier::Standard(name, ..)) =
						function.name.as_option_variable_identifier()
					{
						exported
							.named
							.push((name.clone(), (variable_id, VariableMutability::Constant)));
					}
				}
			}
			_ => (),
		}
	}
}

fn import_part_to_name_pair(item: &parser::declarations::ImportPart) -> Option<NamePair<'_>> {
	match item {
		parser::declarations::ImportPart::Name(name) => {
			if let VariableIdentifier::Standard(name, position) = name {
				Some(NamePair { value: name, r#as: name, position: *position })
			} else {
				None
			}
		}
		parser::declarations::ImportPart::NameWithAlias { name, alias, position } => {
			Some(NamePair {
				value: match alias {
					parser::declarations::ImportExportName::Reference(item)
					| parser::declarations::ImportExportName::Quoted(item, _) => item,
					parser::declarations::ImportExportName::Marker(_) => todo!(),
				},
				r#as: name,
				position: *position,
			})
		}
		parser::declarations::ImportPart::PrefixComment(_, item, _) => {
			item.as_deref().and_then(import_part_to_name_pair)
		}
		parser::declarations::ImportPart::PostfixComment(item, _, _) => {
			import_part_to_name_pair(item)
		}
	}
}

pub(super) fn export_part_to_name_pair(
	item: &parser::declarations::export::ExportPart,
) -> Option<NamePair<'_>> {
	match item {
		parser::declarations::export::ExportPart::Name(name) => {
			if let VariableIdentifier::Standard(name, position) = name {
				Some(NamePair { value: name, r#as: name, position: *position })
			} else {
				None
			}
		}
		parser::declarations::export::ExportPart::NameWithAlias { name, alias, position } => {
			Some(NamePair {
				value: name,
				r#as: match alias {
					parser::declarations::ImportExportName::Reference(item)
					| parser::declarations::ImportExportName::Quoted(item, _) => item,
					parser::declarations::ImportExportName::Marker(_) => todo!(),
				},
				position: *position,
			})
		}
		parser::declarations::export::ExportPart::PrefixComment(_, item, _) => {
			item.as_deref().and_then(export_part_to_name_pair)
		}
		parser::declarations::export::ExportPart::PostfixComment(item, _, _) => {
			export_part_to_name_pair(item)
		}
	}
}

pub(super) fn hoist_variable_declaration<T: ReadFromFS>(
	declaration: &parser::declarations::VariableDeclaration,
	environment: &mut crate::context::Context<crate::context::environment::Syntax<'_>>,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	match declaration {
		parser::declarations::VariableDeclaration::ConstDeclaration {
			declarations,
			position: _,
		} => {
			for declaration in declarations {
				crate::utils::notify!("TODO constraint needed to be set for free variable!!!");
				let constraint =
					get_annotation_from_declaration(declaration, environment, checking_data);

				register_variable(
					declaration.name.get_ast_ref(),
					environment,
					checking_data,
					VariableRegisterArguments {
						constant: true,
						space: constraint,
						// Value set later
						initial_value: None,
					},
				);
			}
		}
		parser::declarations::VariableDeclaration::LetDeclaration { declarations, position: _ } => {
			for declaration in declarations {
				let constraint =
					get_annotation_from_declaration(declaration, environment, checking_data);

				register_variable(
					declaration.name.get_ast_ref(),
					environment,
					checking_data,
					VariableRegisterArguments {
						constant: false,
						space: constraint,
						// Value set later
						initial_value: None,
					},
				);
			}
		}
	}
}

fn get_annotation_from_declaration<
	T: crate::ReadFromFS,
	U: parser::declarations::variable::DeclarationExpression + 'static,
>(
	declaration: &parser::declarations::VariableDeclarationItem<U>,
	environment: &mut crate::context::Context<crate::context::Syntax<'_>>,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> Option<TypeId> {
	let result = if let Some(annotation) = declaration.type_annotation.as_ref() {
		Some((
			synthesise_type_annotation(annotation, environment, checking_data),
			annotation.get_position().with_source(environment.get_source()),
		))
	}
	// TODO only under config
	else if let WithComment::PostfixComment(_item, possible_declaration, position) =
		&declaration.name
	{
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
			.type_mappings
			.variable_restrictions
			.insert((environment.get_source(), get_position.start), (ty, span));
	}

	result.map(|(value, _span)| value)
}
