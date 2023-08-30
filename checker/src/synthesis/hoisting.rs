use std::collections::HashMap;

use parser::{
	declarations::DeclareVariableDeclaration, ASTNode, Declaration, Statement,
	StatementOrDeclaration, VariableIdentifier,
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
				parser::Declaration::Enum(r#enum) => {
					checking_data.raise_unimplemented_error("enum", r#enum.on.position.clone())
				}
				parser::Declaration::DeclareInterface(_interface) => todo!(),
				parser::Declaration::Interface(interface) => {
					let ty = environment.new_interface(
						&interface.on.name,
						interface.on.position.clone(),
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
					checking_data.raise_unimplemented_error("imports", import.position.clone());
					// TODO get types from checking_data.modules.exported
					if let Some(ref imports) = import.imports {
						for part in imports {
							match part {
								parser::declarations::ImportPart::Name(identifier) => {
									if let VariableIdentifier::Standard(name, pos) = identifier {
										environment.register_variable_handle_error(
											name,
											pos.clone(),
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
										position.clone(),
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
					checking_data
						.raise_unimplemented_error("export", export.get_position().into_owned());
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
						stmt.get_position().into_owned(),
					);
				}
			}
			StatementOrDeclaration::Declaration(dec) => match dec {
				parser::Declaration::Variable(declaration) => match declaration {
					parser::declarations::VariableDeclaration::ConstDeclaration {
						keyword,
						declarations,
					} => {
						for declaration in declarations.iter() {
							let constraint =
								declaration.type_annotation.as_ref().map(|reference| {
									synthesize_type_annotation(
										&reference,
										environment,
										checking_data,
									)
								});

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
					} => {
						for declaration in declarations.iter() {
							let constraint =
								declaration.type_annotation.as_ref().map(|reference| {
									synthesize_type_annotation(
										&reference,
										environment,
										checking_data,
									)
								});

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
						func.get_position().into_owned(),
						behavior,
						checking_data,
					);
				}
				parser::Declaration::DeclareFunction(func) => {
					// TODO abstract
					let base = type_function_reference(
						&func.type_parameters,
						&func.parameters,
						func.return_type.as_ref(),
						environment,
						checking_data,
						func.performs.as_ref().into(),
						func.position.clone(),
						crate::types::FunctionKind::Arrow,
						None,
					);

					let base = checking_data.types.register_type(crate::Type::Function(
						base,
						crate::types::FunctionNature::Reference,
					));

					let behavior = crate::context::VariableRegisterBehavior::Declare { base };
					environment.register_variable_handle_error(
						func.name.as_str(),
						func.get_position().into_owned(),
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
						position.clone(),
						variable_ty,
						&mut checking_data.types,
					);

					checking_data
						.type_mappings
						.variables_to_constraints
						.0
						.insert(crate::VariableId(position.source, position.start), variable_ty);

					if let Err(error) = declare_variable {
						checking_data.diagnostics_container.add_error(
							crate::diagnostics::TypeCheckError::CannotRedeclareVariable {
								name: error.name.to_owned(),
								position: position.clone(),
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
