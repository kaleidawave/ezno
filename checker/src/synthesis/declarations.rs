use parser::{declarations::VariableDeclaration, ASTNode, Declaration};

use crate::{
	context::Environment, diagnostics::TypeCheckError, features::variables::VariableMutability,
	CheckingData, Scope, TypeId,
};

use super::{
	classes::synthesise_class_declaration, expressions::synthesise_expression,
	variables::synthesise_variable_declaration_item,
};

pub(super) fn synthesise_variable_declaration<T: crate::ReadFromFS>(
	declaration: &VariableDeclaration,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	exported: bool,
	infer_constraint: bool,
) {
	match declaration {
		VariableDeclaration::ConstDeclaration { declarations, .. } => {
			for variable_declaration in declarations {
				synthesise_variable_declaration_item(
					variable_declaration,
					environment,
					checking_data,
					exported.then_some(VariableMutability::Constant),
					infer_constraint,
				);
			}
		}
		VariableDeclaration::LetDeclaration { declarations, .. } => {
			for variable_declaration in declarations {
				let exported = exported.then(|| {
					let restriction = checking_data
						.local_type_mappings
						.variable_restrictions
						.get(&(environment.get_source(), variable_declaration.position.start))
						.map(|(first, _)| *first);
					VariableMutability::Mutable { reassignment_constraint: restriction }
				});

				synthesise_variable_declaration_item(
					variable_declaration,
					environment,
					checking_data,
					exported,
					infer_constraint,
				);
			}
		}
	}
}

pub(crate) fn synthesise_declaration<T: crate::ReadFromFS>(
	declaration: &Declaration,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	match declaration {
		Declaration::Variable(declaration) => {
			synthesise_variable_declaration(declaration, environment, checking_data, false, false);
		}
		Declaration::Class(class) => {
			use super::StatementOrExpressionVariable;

			let existing_id = checking_data
				.local_type_mappings
				.types_to_types
				.get_exact(class.on.name.identifier.get_position())
				.copied();

			// Adding variable is done inside
			let constructor = synthesise_class_declaration(
				&class.on,
				existing_id,
				TypeId::ANY_TYPE,
				environment,
				checking_data,
			);

			if let Some(variable) = class.on.name.get_variable_id(environment.get_source()) {
				environment.info.variable_current_value.insert(variable, constructor);
			}
		}
		Declaration::Export(exported) => match &exported.on {
			parser::declarations::ExportDeclaration::Variable { exported, position: _ } => {
				match exported {
					// Skipped as this is done earlier
					parser::declarations::export::Exportable::Class(class) => {
						let existing_id = checking_data
							.local_type_mappings
							.types_to_types
							.get_exact(class.name.identifier.get_position())
							.copied();
						// TODO mark as exported
						let _ = synthesise_class_declaration(
							class,
							existing_id,
							TypeId::ANY_TYPE,
							environment,
							checking_data,
						);
					}
					parser::declarations::export::Exportable::Variable(variable) => {
						synthesise_variable_declaration(
							variable,
							environment,
							checking_data,
							true,
							false,
						);
					}
					parser::declarations::export::Exportable::Parts(parts) => {
						for part in parts {
							let pair = super::hoisting::part_to_name_pair(part);
							if let Some(pair) = pair {
								let position = pair.position.with_source(environment.get_source());
								let value = environment.get_variable_handle_error(
									pair.value,
									position,
									checking_data,
								);
								if let crate::Scope::Module { ref mut exported, .. } =
									environment.context_type.scope
								{
									if let Ok(value) = value {
										exported.named.insert(
											pair.r#as.to_owned(),
											(value.0.get_id(), value.0.get_mutability()),
										);
									}
								}
							}
						}
					}
					parser::declarations::export::Exportable::ImportAll { .. }
					| parser::declarations::export::Exportable::ImportParts { .. }
					| parser::declarations::export::Exportable::Function(_)
					| parser::declarations::export::Exportable::Interface(_)
					| parser::declarations::export::Exportable::TypeAlias(_) => {}
				}
			}
			parser::declarations::ExportDeclaration::Default { expression, position } => {
				// TODO can be inferred sometimes
				let result =
					synthesise_expression(expression, environment, checking_data, TypeId::ANY_TYPE);

				if let Scope::Module { ref mut exported, .. } = environment.context_type.scope {
					if exported.default.is_some() {
						checking_data.diagnostics_container.add_error(
							TypeCheckError::DoubleDefaultExport(
								position.with_source(environment.get_source()),
							),
						);
					} else {
						exported.default = Some(result);
					}
				} else {
					checking_data.diagnostics_container.add_error(
						TypeCheckError::NonTopLevelExport(
							position.with_source(environment.get_source()),
						),
					);
				}
			}
			parser::declarations::ExportDeclaration::DefaultFunction { position, .. } => {
				checking_data.diagnostics_container.add_error(
					TypeCheckError::FunctionWithoutBodyNotAllowedHere {
						position: position.with_source(environment.get_source()),
					},
				);
			}
		},
		Declaration::DeclareVariable(_)
		| Declaration::Function(_)
		| Declaration::Enum(_)
		| Declaration::Interface(_)
		| Declaration::TypeAlias(_)
		| Declaration::Namespace(_)
		| Declaration::Import(_) => {}
	}
}
