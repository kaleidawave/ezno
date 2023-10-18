use std::collections::HashMap;

use parser::{
	declarations::{export::Exportable, DeclareVariableDeclaration, ExportDeclaration},
	ASTNode, Declaration, Decorated, Statement, StatementOrDeclaration, VariableIdentifier,
	WithComment,
};

use crate::{
	behavior::functions::RegisterOnExisting, context::Environment,
	structures::variables::VariableMutability, CheckingData, ReadFromFS, TypeId,
};

use super::{
	functions::type_function_reference, type_annotations::synthesise_type_annotation,
	variables::register_variable,
};

/// TODO imports and exports
pub(crate) fn hoist_statements<T: crate::ReadFromFS>(
	items: &[StatementOrDeclaration],
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, parser::Module>,
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
					let ty = environment.new_interface(
						&interface.name,
						interface.position.clone().with_source(environment.get_source()),
						&mut checking_data.types,
					);
					idx_to_types.insert(interface.position.start, ty);
				}
				parser::Declaration::Interface(interface) => {
					let ty = environment.new_interface(
						&interface.on.name,
						interface.on.position.clone().with_source(environment.get_source()),
						&mut checking_data.types,
					);
					idx_to_types.insert(interface.on.position.start, ty);
				}
				parser::Declaration::TypeAlias(alias) => {
					if alias.type_name.type_parameters.is_some() {
						checking_data.raise_unimplemented_error(
							"type alias with generic type parameters",
							alias.get_position().clone().with_source(environment.get_source()),
						)
					}
					let to = synthesise_type_annotation(
						&alias.type_expression,
						environment,
						checking_data,
					);

					environment.new_alias(&alias.type_name.name, to, &mut checking_data.types);
				}
				parser::Declaration::Import(import) => {
					// TODO get types from checking_data.modules.exported
					if let Some(ref default) = import.default {
						checking_data.raise_unimplemented_error(
							"default imports",
							import.position.clone().with_source(environment.get_source()),
						);
					}

					match &import.kind {
						parser::declarations::import::ImportKind::Parts(parts) => {
							for part in parts {
								match part {
									parser::declarations::ImportExportPart::Name(identifier) => {
										if let VariableIdentifier::Standard(name, pos) = identifier
										{
											environment.register_variable_handle_error(
												name,
												pos.clone().with_source(environment.get_source()),
												crate::context::VariableRegisterBehavior::Declare {
													base: TypeId::UNIMPLEMENTED_ERROR_TYPE,
												},
												checking_data,
											);
										}
									}
									parser::declarations::ImportExportPart::NameWithAlias {
										name,
										alias,
										position,
									} => {
										environment.register_variable_handle_error(
											name,
											position.clone().with_source(environment.get_source()),
											crate::context::VariableRegisterBehavior::Declare {
												base: TypeId::UNIMPLEMENTED_ERROR_TYPE,
											},
											checking_data,
										);
									}
								}
							}
							checking_data.raise_unimplemented_error(
								"imports",
								import.position.clone().with_source(environment.get_source()),
							);
						}
						parser::declarations::import::ImportKind::All { under } => {
							checking_data.raise_unimplemented_error(
								"import * as",
								import.position.clone().with_source(environment.get_source()),
							);
						}
						parser::declarations::import::ImportKind::SideEffect => {
							checking_data.raise_unimplemented_error(
								"import side effect",
								import.position.clone().with_source(environment.get_source()),
							);
						}
					}
				}
				// TODO I don't think anything needs to happen here
				parser::Declaration::Export(_export) => {}
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
					// TODO unsynthesized function? ...
					let behavior = crate::context::VariableRegisterBehavior::Register {
						// TODO
						mutability: crate::structures::variables::VariableMutability::Constant,
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
					let base = type_function_reference(
						&func.type_parameters,
						&func.parameters,
						func.return_type.as_ref(),
						environment,
						checking_data,
						func.performs.as_ref().into(),
						declared_at.clone(),
						crate::types::FunctionKind::Arrow,
						None,
					);

					let base = checking_data.types.new_function_type_annotation(
						base.type_parameters,
						base.parameters,
						base.return_type,
						declared_at,
						base.effects,
						base.constant_id,
					);

					let behavior = crate::context::VariableRegisterBehavior::Declare { base };
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
				}) => {
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
				parser::Declaration::DeclareInterface(_) => {}
				parser::Declaration::Import(_) => {}
				parser::Declaration::Export(exported) => match &exported.on {
					parser::declarations::ExportDeclaration::Variable { exported, position } => {
						match exported {
							parser::declarations::export::Exportable::Class(_) => {}
							parser::declarations::export::Exportable::Function(func) => {
								// TODO unsynthesized function? ...
								let behavior = crate::context::VariableRegisterBehavior::Register {
									// TODO
									mutability:
										crate::structures::variables::VariableMutability::Constant,
								};
								environment.register_variable_handle_error(
									func.name.as_str(),
									func.get_position()
										.clone()
										.with_source(environment.get_source()),
									behavior,
									checking_data,
								);
							}
							parser::declarations::export::Exportable::Variable(declaration) => {
								// TODO mark exported
								hoist_variable_declaration(&declaration, environment, checking_data)
							}
							parser::declarations::export::Exportable::Interface(interface) => {
								let ty = idx_to_types.remove(&interface.position.start).unwrap();
								super::interfaces::synthesise_signatures(
									&interface.members,
									super::interfaces::OnToType(ty),
									environment,
									checking_data,
								);
							}
							parser::declarations::export::Exportable::TypeAlias(_) => {}
							parser::declarations::export::Exportable::Parts(parts) => {
								checking_data.raise_unimplemented_error(
									"export parts",
									position.clone().with_source(environment.get_source()),
								);
							}
						}
					}
					parser::declarations::ExportDeclaration::Default { expression, position } => {
						checking_data.raise_unimplemented_error(
							"default export",
							position.clone().with_source(environment.get_source()),
						);
					}
				},
			},
		}
	}

	// Third stage: functions
	for item in items {
		match item {
			StatementOrDeclaration::Declaration(Declaration::Function(func)) => {
				environment.new_function(
					checking_data,
					&func.on,
					RegisterOnExisting(func.on.name.as_str().to_owned()),
				);
			}
			StatementOrDeclaration::Declaration(Declaration::Export(Decorated {
				on:
					ExportDeclaration::Variable { exported: Exportable::Function(func), position: _ },
				..
			})) => {
				// TODO does it need to be exportable marked
				environment.new_function(
					checking_data,
					func,
					RegisterOnExisting(func.name.as_str().to_owned()),
				);
			}
			_ => (),
		}
	}
}

fn hoist_variable_declaration<T: ReadFromFS>(
	declaration: &parser::declarations::VariableDeclaration,
	environment: &mut crate::context::Context<crate::context::environment::Syntax<'_>>,
	checking_data: &mut CheckingData<'_, T, parser::Module>,
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
	checking_data: &mut CheckingData<'_, T, parser::Module>,
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
	checking_data: &mut CheckingData<'_, T, parser::Module>,
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
