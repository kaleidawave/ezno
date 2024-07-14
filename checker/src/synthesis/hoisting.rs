use std::iter;

use parser::{
	declarations::{export::Exportable, DeclareVariableDeclaration, ExportDeclaration},
	ASTNode, Declaration, Decorated, ExpressionOrStatementPosition, Statement,
	StatementOrDeclaration, VariableIdentifier,
};

use crate::{
	context::{Environment, VariableRegisterArguments},
	diagnostics::TypeCheckError,
	features::{
		functions::{
			synthesise_declare_statement_function, synthesise_hoisted_statement_function,
			SynthesisableFunction,
		},
		modules::{import_items, ImportKind, NamePair},
		variables::VariableMutability,
	},
	synthesis::{
		classes::register_statement_class_with_members,
		type_annotations::get_annotation_from_declaration,
	},
	CheckingData, ReadFromFS, TypeId,
};

use super::{variables::register_variable, EznoParser};

pub(crate) fn hoist_statements<T: crate::ReadFromFS>(
	items: &[StatementOrDeclaration],
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	// First stage: imports and types
	for item in items {
		if let StatementOrDeclaration::Declaration(declaration) = item {
			match declaration {
				parser::Declaration::DeclareVariable(_)
				| parser::Declaration::Variable(_)
				| parser::Declaration::Function(_) => {}
				parser::Declaration::Enum(r#enum) => checking_data.raise_unimplemented_error(
					"enum",
					r#enum.on.position.with_source(environment.get_source()),
				),
				parser::Declaration::Namespace(ns) => checking_data.raise_unimplemented_error(
					"namespace",
					ns.position.with_source(environment.get_source()),
				),
				parser::Declaration::Interface(interface) => {
					let ty = environment.register_interface(
						interface.on.name.as_option_str().unwrap_or_default(),
						interface.on.is_nominal,
						interface.on.type_parameters.as_deref(),
						interface.on.extends.as_deref(),
						interface.on.position.with_source(environment.get_source()),
						checking_data,
					);
					checking_data
						.local_type_mappings
						.types_to_types
						.push(interface.on.get_position(), ty);
				}
				parser::Declaration::Class(class) => {
					let ty = environment.register_class::<EznoParser>(
						class.on.name.as_option_str().unwrap_or_default(),
						class.on.type_parameters.as_deref(),
						class.on.extends.as_deref(),
						&mut checking_data.types,
					);
					checking_data
						.local_type_mappings
						.types_to_types
						.push(class.on.get_position(), ty);
				}
				parser::Declaration::TypeAlias(alias) => {
					let ty = environment.new_alias(
						alias.name.as_option_str().unwrap_or_default(),
						alias.parameters.as_deref(),
						&alias.references,
						alias.get_position(),
						checking_data,
					);
					checking_data.local_type_mappings.types_to_types.push(alias.get_position(), ty);
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
							VariableIdentifier::Marker(_, _) => {
								// TODO I think this is best
								continue;
							}
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
									Some(VariableIdentifier::Marker(_, _)) => {
										// TODO
										continue;
									}
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
							Exportable::Interface(interface) => {
								let ty = environment.register_interface(
									interface.name.as_option_str().unwrap_or_default(),
									interface.is_nominal,
									interface.type_parameters.as_deref(),
									interface.extends.as_deref(),
									interface.position.with_source(environment.get_source()),
									checking_data,
								);
								checking_data
									.local_type_mappings
									.types_to_types
									.push(interface.get_position(), ty);

								if let crate::Scope::Module { ref mut exported, .. } =
									environment.context_type.scope
								{
									let name = interface
										.name
										.as_option_str()
										.unwrap_or_default()
										.to_owned();
									exported.named_types.push((name, ty));
								}
							}
							Exportable::Class(class) => {
								let ty = environment.register_class::<EznoParser>(
									class.name.as_option_str().unwrap_or_default(),
									class.type_parameters.as_deref(),
									class.extends.as_deref(),
									&mut checking_data.types,
								);
								checking_data
									.local_type_mappings
									.types_to_types
									.push(class.get_position(), ty);

								if let crate::Scope::Module { ref mut exported, .. } =
									environment.context_type.scope
								{
									exported.named_types.push((
										class.name.as_option_str().unwrap_or_default().to_owned(),
										ty,
									));
								}
							}
							Exportable::TypeAlias(alias) => {
								let ty = environment.new_alias::<_, EznoParser>(
									alias.name.as_option_str().unwrap_or_default(),
									alias.parameters.as_deref(),
									&alias.references,
									alias.get_position(),
									checking_data,
								);

								if let crate::Scope::Module { ref mut exported, .. } =
									environment.context_type.scope
								{
									exported.named_types.push((
										alias.name.as_option_str().unwrap_or_default().to_owned(),
										ty,
									));
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

	// Second stage: variables and function type hoisting
	let mut second_items = items.iter().peekable();
	while let Some(item) = second_items.next() {
		match item {
			StatementOrDeclaration::Statement(stmt) => {
				if let Statement::VarVariable(stmt) = stmt {
					for declaration in &stmt.declarations {
						let constraint = get_annotation_from_declaration(
							declaration,
							environment,
							checking_data,
						);
						register_variable(
							declaration.name.get_ast_ref(),
							environment,
							checking_data,
							VariableRegisterArguments {
								constant: false,
								space: constraint,
								// Important!
								initial_value: Some(TypeId::UNDEFINED_TYPE),
								// `var` declarations can be redeclared!
								allow_reregistration: true,
							},
						);
					}
				}
			}
			StatementOrDeclaration::Declaration(dec) => match dec {
				parser::Declaration::Namespace(ns) => checking_data.raise_unimplemented_error(
					"namespace",
					ns.position.with_source(environment.get_source()),
				),
				parser::Declaration::Variable(declaration) => {
					hoist_variable_declaration(declaration, environment, checking_data);
				}
				parser::Declaration::Function(func) => {
					if let Some(VariableIdentifier::Standard(name, ..)) =
						func.on.name.as_option_variable_identifier()
					{
						// Copied from classes hoisting
						let (overloads, actual) = if func.on.body.0.is_none() {
							let mut overloads = Vec::new();
							let shape = super::functions::synthesise_shape(
								&func.on,
								environment,
								checking_data,
							);
							overloads.push(shape);

							// Read declarations until
							while let Some(overload_declaration) = second_items.next_if(|t| {
								matches!( t, StatementOrDeclaration::Declaration( parser::Declaration::Function(func)) if func.on.name.as_option_str().is_some_and(|n| n == name) && !func.on.has_body())
							}) {
								let parser::StatementOrDeclaration::Declaration(
									parser::Declaration::Function(func),
								) = &overload_declaration
								else {
									unreachable!()
								};
								let shape = super::functions::synthesise_shape(
									&func.on,
									environment,
									checking_data,
								);
								overloads.push(shape);
							}

							let upcoming = second_items.peek().and_then(|next| {
								matches!(
									next,
									StatementOrDeclaration::Declaration( parser::Declaration::Function(func))
									if
										func.on.name.as_option_str().is_some_and(|n| n == name)
										&& func.on.has_body()
								)
								.then_some(next)
							});

							if let Some(StatementOrDeclaration::Declaration(
								Declaration::Function(func),
							)) = upcoming
							{
								let actual = super::functions::synthesise_shape(
									&func.on,
									environment,
									checking_data,
								);
								(overloads, actual)
							} else if func.on.name.declare {
								let actual = overloads.pop().unwrap();
								(overloads, actual)
							} else {
								// TODO what about `checking_data.options.lsp_mode`?
								checking_data.diagnostics_container.add_error(
									TypeCheckError::FunctionWithoutBodyNotAllowedHere {
										position: func
											.get_position()
											.with_source(environment.get_source()),
									},
								);
								continue;
							}
						} else {
							let actual = super::functions::synthesise_shape(
								&func.on,
								environment,
								checking_data,
							);
							(Vec::new(), actual)
						};

						let value = super::functions::build_overloaded_function(
							crate::FunctionId(environment.get_source(), func.on.position.start),
							crate::features::functions::FunctionBehavior::Function {
								this_id: TypeId::ERROR_TYPE,
								prototype: TypeId::ERROR_TYPE,
								is_async: func.on.header.is_async(),
								is_generator: func.on.header.is_generator(),
							},
							overloads,
							actual,
							environment,
							&mut checking_data.types,
							&mut checking_data.diagnostics_container,
						);

						let k = crate::VariableId(environment.get_source(), func.on.position.start);
						checking_data
							.local_type_mappings
							.variables_to_constraints
							.0
							.insert(k, value);

						let argument = VariableRegisterArguments {
							// TODO functions are constant references
							constant: true,
							space: Some(value),
							initial_value: Some(value),
							allow_reregistration: false,
						};

						environment.register_variable_handle_error(
							name,
							argument,
							func.get_position().with_source(environment.get_source()),
							&mut checking_data.diagnostics_container,
							&mut checking_data.local_type_mappings,
							checking_data.options.record_all_assignments_and_reads,
						);
					}
				}
				parser::Declaration::Enum(r#enum) => {
					checking_data.raise_unimplemented_error(
						"enum",
						r#enum.position.with_source(environment.get_source()),
					);
				}
				parser::Declaration::Interface(interface) => {
					let ty = *checking_data
						.local_type_mappings
						.types_to_types
						.get(interface.on.get_position().start)
						.unwrap();

					super::interfaces::synthesise_signatures(
						interface.on.type_parameters.as_deref(),
						interface.on.extends.as_deref(),
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
							crate::utilities::notify!("constraint with no type?");
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
								allow_reregistration: false,
							},
						);
					}
				}

				parser::Declaration::Export(exported) => match &exported.on {
					parser::declarations::ExportDeclaration::Variable { exported, position: _ } => {
						match exported {
							Exportable::Function(func) => {
								let declared_at = ASTNode::get_position(func)
									.with_source(environment.get_source());

								if let Some(VariableIdentifier::Standard(name, ..)) =
									func.name.as_option_variable_identifier()
								{
									let argument = VariableRegisterArguments {
										constant: true,
										allow_reregistration: false,
										space: None,
										initial_value: None,
									};
									environment.register_variable_handle_error(
										name,
										argument,
										declared_at,
										&mut checking_data.diagnostics_container,
										&mut checking_data.local_type_mappings,
										checking_data.options.record_all_assignments_and_reads,
									);
								}
							}
							Exportable::Variable(declaration) => {
								// TODO mark exported
								hoist_variable_declaration(declaration, environment, checking_data);
							}
							Exportable::Interface(interface) => {
								let ty = *checking_data
									.local_type_mappings
									.types_to_types
									.get(interface.get_position().start)
									.unwrap();

								super::interfaces::synthesise_signatures(
									interface.type_parameters.as_deref(),
									interface.extends.as_deref(),
									&interface.members,
									super::interfaces::OnToType(ty),
									environment,
									checking_data,
								);
							}
							Exportable::Class(class) => {
								register_statement_class_with_members(
									class,
									environment,
									checking_data,
								);
							}
							Exportable::TypeAlias(_) | Exportable::ImportAll { .. } => {}
							// TODO
							Exportable::Parts(..) | Exportable::ImportParts { .. } => {
								crate::utilities::notify!("TODO");
							}
						}
					}
					parser::declarations::ExportDeclaration::Default { .. } => {}
					parser::declarations::ExportDeclaration::DefaultFunction {
						position, ..
					} => {
						checking_data.diagnostics_container.add_error(
							TypeCheckError::FunctionWithoutBodyNotAllowedHere {
								position: position.with_source(environment.get_source()),
							},
						);
						continue;
					}
				},
				parser::Declaration::Class(class) => {
					register_statement_class_with_members(&class.on, environment, checking_data);
				}
				parser::Declaration::TypeAlias(_) | parser::Declaration::Import(_) => {}
			},
			StatementOrDeclaration::Imported { .. } | StatementOrDeclaration::Marker(_, _) => {}
		}
	}

	// Third stage: functions
	let mut third_stage_items = items.iter().peekable();
	while let Some(item) = third_stage_items.next() {
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

				if function.on.name.declare {
					let (overloaded, _last) = if function.on.has_body() {
						(false, function)
					} else {
						let last = third_stage_items.find(|f| {
							if let StatementOrDeclaration::Declaration(Declaration::Function(
								function,
							)) = f
							{
								function.on.has_body()
							} else {
								false
							}
						});

						if let Some(StatementOrDeclaration::Declaration(Declaration::Function(
							function,
						))) = last
						{
							(true, function)
						} else {
							// Some error with non-overloads
							continue;
						}
					};

					synthesise_declare_statement_function(
						variable_id,
						overloaded,
						is_async,
						is_generator,
						location,
						None,
						&function.on,
						environment,
						checking_data,
					);
				} else {
					let (overloaded, _last) = if function.on.has_body() {
						(false, function)
					} else {
						let last = third_stage_items.find(|f| {
							if let StatementOrDeclaration::Declaration(Declaration::Function(
								function,
							)) = f
							{
								function.on.has_body()
							} else {
								false
							}
						});

						if let Some(StatementOrDeclaration::Declaration(Declaration::Function(
							function,
						))) = last
						{
							(true, function)
						} else {
							// Some error with non-overloads
							continue;
						}
					};

					synthesise_hoisted_statement_function(
						variable_id,
						overloaded,
						is_async,
						is_generator,
						location,
						&function.on,
						environment,
						checking_data,
					);
				}
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
					// TODO
					false,
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
					parser::declarations::ImportExportName::Marker(_) => {
						// TODO I think okay
						return None;
					}
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
					parser::declarations::ImportExportName::Marker(_) => {
						return None;
					}
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
				crate::utilities::notify!("TODO constraint needed to be set for free variable!!!");
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
						allow_reregistration: false,
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
						allow_reregistration: false,
					},
				);
			}
		}
	}
}
