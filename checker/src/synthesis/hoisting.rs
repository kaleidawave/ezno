use std::{collections::HashMap, iter};

use parser::{
	declarations::{export::Exportable, DeclareVariableDeclaration, ExportDeclaration},
	ASTNode, Declaration, Decorated, Statement, StatementOrDeclaration, VariableIdentifier,
	WithComment,
};
use source_map::{Span, SpanWithSource};

use crate::{
	behavior::{
		functions::synthesise_hoisted_statement_function,
		modules::{Exported, ImportKind, NamePair},
		variables::VariableMutability,
	},
	context::Environment,
	diagnostics::TypeCheckError,
	CheckingData, Constant, ReadFromFS, TypeId,
};

use super::{
	functions::synthesise_function_annotation, type_annotations::synthesise_type_annotation,
	variables::register_variable, EznoParser,
};

pub(crate) fn hoist_statements<T: crate::ReadFromFS>(
	items: &[StatementOrDeclaration],
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	let mut idx_to_types = HashMap::new();

	// First stage
	for item in items.iter() {
		if let StatementOrDeclaration::Declaration(declaration) = item {
			match declaration {
				parser::Declaration::DeclareVariable(_)
				| parser::Declaration::DeclareFunction(_)
				| parser::Declaration::Class(_)
				| parser::Declaration::Variable(_)
				| parser::Declaration::Function(_) => {}
				parser::Declaration::Enum(r#enum) => checking_data.raise_unimplemented_error(
					"enum",
					r#enum.on.position.clone().with_source(environment.get_source()),
				),
				parser::Declaration::DeclareInterface(interface) => {
					// TODO any difference bc declare?
					let ty = environment.new_interface::<super::EznoParser>(
						&interface.name,
						interface.nominal_keyword.is_some(),
						interface.type_parameters.as_deref(),
						interface.extends.as_deref(),
						interface.position.clone().with_source(environment.get_source()),
						&mut checking_data.types,
					);
					idx_to_types.insert(interface.position.start, ty);
				}
				parser::Declaration::Interface(interface) => {
					let ty = environment.new_interface::<super::EznoParser>(
						&interface.on.name,
						interface.on.nominal_keyword.is_some(),
						interface.on.type_parameters.as_deref(),
						interface.on.extends.as_deref(),
						interface.on.position.clone().with_source(environment.get_source()),
						&mut checking_data.types,
					);
					idx_to_types.insert(interface.on.position.start, ty);
				}
				parser::Declaration::TypeAlias(alias) => {
					environment.new_alias(
						&alias.type_name.name,
						alias.type_name.type_parameters.as_deref(),
						&alias.type_expression,
						checking_data,
					);
				}
				parser::Declaration::Import(import) => {
					let items = match &import.items {
						parser::declarations::import::ImportedItems::Parts(parts) => {
							crate::behavior::modules::ImportKind::Parts(
								parts
									.iter()
									.flatten()
									.filter_map(|item| import_part_to_name_pair(item)),
							)
						}
						parser::declarations::import::ImportedItems::All { under } => match under {
							VariableIdentifier::Standard(under, position) => {
								crate::behavior::modules::ImportKind::All {
									under,
									position: position.clone(),
								}
							}
							VariableIdentifier::Cursor(_, _) => todo!(),
						},
					};
					let default_import = import.default.as_ref().and_then(|default_identifier| {
						match default_identifier {
							VariableIdentifier::Standard(name, pos) => {
								Some((name.as_str(), pos.clone()))
							}
							VariableIdentifier::Cursor(..) => None,
						}
					});
					environment.import_items(
						&import.from.get_path().unwrap(),
						import.position.clone(),
						default_import,
						items,
						checking_data,
						false,
					);
				}
				parser::Declaration::Export(export) => {
					if let ExportDeclaration::Variable { exported, position } = &export.on {
						// Imports & types
						match exported {
							Exportable::ImportAll { r#as, from } => {
								let kind = match r#as {
									Some(VariableIdentifier::Standard(name, pos)) => {
										ImportKind::All { under: name, position: pos.clone() }
									}
									Some(VariableIdentifier::Cursor(_, _)) => todo!(),
									None => ImportKind::Everything,
								};

								environment.import_items::<iter::Empty<_>, _, _>(
									from.get_path().unwrap(),
									position.clone(),
									None,
									kind,
									checking_data,
									true,
								)
							}
							Exportable::ImportParts { parts, from } => {
								let parts =
									parts.iter().filter_map(|item| export_part_to_name_pair(item));

								environment.import_items(
									from.get_path().unwrap(),
									position.clone(),
									None,
									crate::behavior::modules::ImportKind::Parts(parts),
									checking_data,
									true,
								);
							}
							Exportable::TypeAlias(alias) => {
								let export = environment.new_alias::<_, EznoParser>(
									&alias.type_name.name,
									alias.type_name.type_parameters.as_deref(),
									&alias.type_expression,
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
							_ => {}
						}
					}
				}
			}
		}
	}

	// Second stage
	for (idx, item) in items.iter().enumerate() {
		match item {
			StatementOrDeclaration::Statement(stmt) => {
				if let Statement::VarVariable(_) = stmt {
					checking_data.raise_unimplemented_error(
						"var statement hoisting",
						stmt.get_position().clone().with_source(environment.get_source()),
					);
				}
			}
			StatementOrDeclaration::Declaration(dec) => match dec {
				parser::Declaration::Variable(declaration) => {
					hoist_variable_declaration(declaration, environment, checking_data)
				}
				parser::Declaration::Function(func) => {
					// TODO unsynthesised function? ...
					let behavior = crate::context::VariableRegisterBehavior::Register {
						// TODO
						mutability: crate::behavior::variables::VariableMutability::Constant,
					};
					environment.register_variable_handle_error(
						func.on.name.as_str(),
						func.get_position().clone().with_source(environment.get_source()),
						behavior,
						checking_data,
					);
				}
				parser::Declaration::DeclareFunction(func) => {
					// TODO abstract
					let declared_at = func.position.clone().with_source(environment.get_source());
					let base = synthesise_function_annotation(
						&func.type_parameters,
						&func.parameters,
						func.return_type.as_ref(),
						environment,
						checking_data,
						func.performs.as_ref().into(),
						declared_at.clone(),
						crate::behavior::functions::FunctionBehavior::ArrowFunction {
							is_async: false,
						},
						None,
					);

					let base = checking_data.types.new_function_type_annotation(
						base.type_parameters,
						base.parameters,
						base.return_type,
						declared_at,
						base.effects,
						base.constant_function,
					);

					let behavior =
						crate::context::VariableRegisterBehavior::Declare { base, context: None };
					environment.register_variable_handle_error(
						func.name.as_str(),
						func.get_position().clone().with_source(environment.get_source()),
						behavior,
						checking_data,
					);
				}
				parser::Declaration::Class(_) => {
					// TODO hoist type...
				}
				parser::Declaration::Enum(r#enum) => {
					checking_data.raise_unimplemented_error(
						"enum",
						r#enum.position.clone().with_source(environment.get_source()),
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
				parser::Declaration::TypeAlias(_) => {}
				parser::Declaration::DeclareVariable(DeclareVariableDeclaration {
					keyword: _,
					declarations,
					position,
					decorators,
				}) => {
					for declaration in declarations.iter() {
						let constraint = get_annotation_from_declaration(
							declaration,
							environment,
							checking_data,
						);

						// TODO warning here
						let behavior = crate::context::VariableRegisterBehavior::Declare {
							base: constraint.unwrap_or(TypeId::ANY_TYPE),
							context: None,
						};

						register_variable(
							declaration.name.get_ast_ref(),
							environment,
							checking_data,
							behavior,
							constraint,
						);
					}
				}
				parser::Declaration::DeclareInterface(_) => {}
				parser::Declaration::Import(_) => {}
				parser::Declaration::Export(exported) => match &exported.on {
					parser::declarations::ExportDeclaration::Variable { exported, position } => {
						match exported {
							Exportable::Class(_) => {}
							Exportable::Function(func) => {
								// TODO unsynthesised function? ...
								let mutability =
									crate::behavior::variables::VariableMutability::Constant;
								let behavior = crate::context::VariableRegisterBehavior::Register {
									mutability,
								};
								let declared_at = func
									.get_position()
									.clone()
									.with_source(environment.get_source());

								environment.register_variable_handle_error(
									func.name.as_str(),
									declared_at,
									behavior,
									checking_data,
								);
							}
							Exportable::Variable(declaration) => {
								// TODO mark exported
								hoist_variable_declaration(&declaration, environment, checking_data)
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
							| Exportable::ImportParts { .. } => {}
						}
					}
					parser::declarations::ExportDeclaration::Default { .. } => {}
				},
			},
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
					parser::functions::FunctionLocationModifier::Server(_) => "server".to_owned(),
					parser::functions::FunctionLocationModifier::Module(_) => "module".to_owned(),
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
					parser::functions::FunctionLocationModifier::Server(_) => "server".to_owned(),
					parser::functions::FunctionLocationModifier::Module(_) => "module".to_owned(),
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
					exported.named.push((
						function.name.as_str().to_owned(),
						(variable_id, VariableMutability::Constant),
					));
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
				Some(NamePair { value: &name, r#as: &name, position: position.clone() })
			} else {
				None
			}
		}
		parser::declarations::ImportPart::NameWithAlias { name, alias, position } => {
			Some(NamePair {
				value: match alias {
					parser::declarations::ImportExportName::Reference(item)
					| parser::declarations::ImportExportName::Quoted(item, _) => item,
					_ => todo!(),
				},
				r#as: &name,
				position: position.clone(),
			})
		}
		parser::declarations::ImportPart::PrefixComment(_, item, _) => {
			item.as_deref().map(import_part_to_name_pair).flatten()
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
				Some(NamePair { value: &name, r#as: &name, position: position.clone() })
			} else {
				None
			}
		}
		parser::declarations::export::ExportPart::NameWithAlias { name, alias, position } => {
			Some(NamePair {
				value: &name,
				r#as: match alias {
					parser::declarations::ImportExportName::Reference(item)
					| parser::declarations::ImportExportName::Quoted(item, _) => item,
					_ => todo!(),
				},
				position: position.clone(),
			})
		}
		parser::declarations::export::ExportPart::PrefixComment(_, item, _) => {
			item.as_deref().map(export_part_to_name_pair).flatten()
		}
		parser::declarations::export::ExportPart::PostfixComment(item, _, _) => {
			export_part_to_name_pair(item)
		}
	}
}

fn hoist_variable_declaration<T: ReadFromFS>(
	declaration: &parser::declarations::VariableDeclaration,
	environment: &mut crate::context::Context<crate::context::environment::Syntax<'_>>,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	match declaration {
		parser::declarations::VariableDeclaration::ConstDeclaration {
			keyword,
			declarations,
			position,
		} => {
			for declaration in declarations.iter() {
				let constraint =
					get_annotation_from_declaration(declaration, environment, checking_data);

				let behavior = crate::context::VariableRegisterBehavior::Register {
					mutability: VariableMutability::Constant,
				};

				register_variable(
					declaration.name.get_ast_ref(),
					environment,
					checking_data,
					behavior,
					constraint,
				);
			}
		}
		parser::declarations::VariableDeclaration::LetDeclaration {
			keyword,
			declarations,
			position,
		} => {
			for declaration in declarations.iter() {
				let constraint =
					get_annotation_from_declaration(declaration, environment, checking_data);

				let behavior = crate::context::VariableRegisterBehavior::Register {
					mutability: VariableMutability::Mutable { reassignment_constraint: constraint },
				};

				register_variable(
					declaration.name.get_ast_ref(),
					environment,
					checking_data,
					behavior,
					constraint,
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
			annotation.get_position().clone().with_source(environment.get_source()),
		))
	}
	// TODO only under config
	else if let WithComment::PostfixComment(item, possible_declaration, position) =
		&declaration.name
	{
		string_comment_to_type(
			possible_declaration,
			position.clone().with_source(environment.get_source()),
			environment,
			checking_data,
		)
	} else {
		None
	};

	if let Some((ty, span)) = result.clone() {
		let get_position = declaration.get_position();
		checking_data
			.type_mappings
			.variable_restrictions
			.insert((environment.get_source(), get_position.start), (ty, span));
	}

	result.map(|(value, _span)| value)
}

pub(crate) fn string_comment_to_type<T: crate::ReadFromFS>(
	possible_declaration: &String,
	position: source_map::SpanWithSource,
	environment: &mut crate::context::Context<crate::context::Syntax<'_>>,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> Option<(TypeId, source_map::SpanWithSource)> {
	let source = environment.get_source();
	use parser::ASTNode;
	let offset = Some(position.end - 2 - possible_declaration.len() as u32);
	let annotation = parser::TypeAnnotation::from_string(
		possible_declaration.clone(),
		Default::default(),
		source,
		offset,
	);
	if let Ok(annotation) = annotation {
		Some((
			synthesise_type_annotation(&annotation, environment, checking_data),
			annotation.get_position().clone().with_source(source),
		))
	} else {
		// TODO warning
		None
	}
}
