use std::collections::HashMap;

use parser::{
	declarations::DeclareVariableDeclaration, ASTNode, Declaration, Statement,
	StatementOrDeclaration, VariableIdentifier, WithComment,
};

use crate::{
	behavior::functions::RegisterOnExisting, context::Environment,
	structures::variables::VariableMutability, CheckingData, TypeId,
};

use super::{
	functions::type_function_reference, type_annotations::synthesize_type_annotation,
	variables::register_variable,
};

/// TODO imports and exports
pub(crate) fn hoist_statements<T: crate::FSResolver>(
	items: &[StatementOrDeclaration],
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
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
				parser::Declaration::DeclareInterface(_interface) => todo!(),
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
						todo!()
					}
					let to = synthesize_type_annotation(
						&alias.type_expression,
						environment,
						checking_data,
					);

					environment.new_alias(&alias.type_name.name, to, &mut checking_data.types);
				}
				parser::Declaration::Import(import) => {
					checking_data.raise_unimplemented_error(
						"imports",
						import.position.clone().with_source(environment.get_source()),
					);
					// TODO get types from checking_data.modules.exported
					if let Some(ref imports) = import.imports {
						for part in imports {
							match part {
								parser::declarations::ImportPart::Name(identifier) => {
									if let VariableIdentifier::Standard(name, pos) = identifier {
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
								parser::declarations::ImportPart::NameWithAlias {
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
					}
				}
				parser::Declaration::Export(export) => {
					checking_data.raise_unimplemented_error(
						"export",
						export.get_position().clone().with_source(environment.get_source()),
					);
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
				parser::Declaration::Variable(declaration) => match declaration {
					parser::declarations::VariableDeclaration::ConstDeclaration {
						keyword,
						declarations,
						position,
					} => {
						for declaration in declarations.iter() {
							let constraint = get_annotation_from_declaration(
								declaration,
								environment,
								checking_data,
							);

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
							let constraint = get_annotation_from_declaration(
								declaration,
								environment,
								checking_data,
							);

							let behavior = crate::context::VariableRegisterBehavior::Register {
								mutability: VariableMutability::Mutable {
									reassignment_constraint: constraint,
								},
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
				},
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
				parser::Declaration::Enum(_) => todo!(),
				parser::Declaration::Interface(interface) => {
					let ty = idx_to_types.remove(&interface.on.position.start).unwrap();
					super::interfaces::synthesize_signatures(
						&interface.on.members,
						super::interfaces::OnToType(ty),
						environment,
						checking_data,
					);
				}
				parser::Declaration::TypeAlias(_) => {}
				parser::Declaration::DeclareVariable(DeclareVariableDeclaration {
					name,
					type_restriction,
					decorators,
					position,
				}) => {
					// TODO tidy up
					let variable_ty =
						synthesize_type_annotation(&type_restriction, environment, checking_data);

					// // TODO not sure...
					// if let Some(frozen) = environment.is_frozen(variable_ty) {
					// 	environment.frozen.insert(var_type, frozen);
					// }

					let declare_variable = environment.declare_variable(
						&name,
						position.clone().with_source(environment.get_source()),
						variable_ty,
						&mut checking_data.types,
					);

					checking_data.type_mappings.variables_to_constraints.0.insert(
						crate::VariableId(environment.get_source(), position.start),
						variable_ty,
					);

					if let Err(error) = declare_variable {
						checking_data.diagnostics_container.add_error(
							crate::diagnostics::TypeCheckError::CannotRedeclareVariable {
								name: error.name.to_owned(),
								position: position.clone().with_source(environment.get_source()),
							},
						)
					}
				}
				parser::Declaration::DeclareInterface(_) => {}
				parser::Declaration::Import(_) => {}
				parser::Declaration::Export(_) => {}
			},
		}
	}

	// Third stage
	for item in items {
		if let StatementOrDeclaration::Declaration(Declaration::Function(func)) = item {
			environment.new_function(
				checking_data,
				&func.on,
				RegisterOnExisting(func.on.name.as_str().to_owned()),
			);
		}
	}
}

fn get_annotation_from_declaration<
	T: crate::FSResolver,
	U: parser::declarations::variable::DeclarationExpression + 'static,
>(
	declaration: &parser::declarations::VariableDeclarationItem<U>,
	environment: &mut crate::context::Context<crate::context::Syntax<'_>>,
	checking_data: &mut CheckingData<'_, T>,
) -> Option<TypeId> {
	let result = if let Some(annotation) = declaration.type_annotation.as_ref() {
		Some((
			synthesize_type_annotation(annotation, environment, checking_data),
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

pub(crate) fn string_comment_to_type<T: crate::FSResolver>(
	possible_declaration: &String,
	position: source_map::SpanWithSource,
	environment: &mut crate::context::Context<crate::context::Syntax<'_>>,
	checking_data: &mut CheckingData<'_, T>,
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
			synthesize_type_annotation(&annotation, environment, checking_data),
			annotation.get_position().clone().with_source(source),
		))
	} else {
		// TODO warning
		None
	}
}
