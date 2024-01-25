use parser::{
	Declaration, ExpressionOrStatementPosition, Statement, StatementOrDeclaration,
	VariableIdentifier,
};

use crate::{context::Environment, diagnostics::TypeCheckError, CheckingData, Scope, TypeId};

use super::{
	classes::synthesise_class_declaration, declarations::synthesise_variable_declaration,
	expressions::synthesise_expression, hoisting::hoist_statements,
	statements::synthesise_statement,
};

/// Note that this expects the environment to be new lexically
pub(super) fn synthesise_block<T: crate::ReadFromFS>(
	statements: &[StatementOrDeclaration],
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	hoist_statements(statements, environment, checking_data);

	// Run through accessible statements
	let mut unreachable_at_idx = None::<usize>;

	for (idx, statement) in statements.iter().enumerate() {
		match statement {
			StatementOrDeclaration::Statement(statement) => {
				synthesise_statement(statement, None, environment, checking_data);

				// TODO Statement::is_control_flow ...?
				if let Statement::Return(..) | Statement::Break(..) | Statement::Continue(..) =
					statement
				{
					unreachable_at_idx = Some(idx + 1);
					break;
				}
			}
			StatementOrDeclaration::Declaration(declaration) => {
				synthesise_declaration(declaration, environment, checking_data);
			}
			StatementOrDeclaration::Marker(_, _) => {
				crate::utils::notify!("should be unreachable");
			}
		}
	}

	if let Some(idx) = unreachable_at_idx {
		let statements_not_run = &statements[idx..];
		if !statements_not_run.is_empty() {
			crate::utils::notify!(
				"Statements not run, only run for expressions and such {:?}",
				statements_not_run
			);
			// if statements.iter().all(|stmt|)
			// let span = statements.first().unwrap().get_position().union(&statements.last().unwrap().get_position());
			// checking_data.diagnostics_container.add_error(TypeCheckError::StatementsNotRun { between: span });
		}
	}
}

pub(crate) fn synthesise_declaration<T: crate::ReadFromFS>(
	declaration: &Declaration,
	environment: &mut crate::context::Context<crate::context::Syntax<'_>>,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	match declaration {
		Declaration::Variable(declaration) => {
			synthesise_variable_declaration(declaration, environment, checking_data, false);
		}
		Declaration::Class(class) => {
			let constructor = synthesise_class_declaration(&class.on, environment, checking_data);
			let position = class.on.position.with_source(environment.get_source());
			if let Some(VariableIdentifier::Standard(name, ..)) =
				class.on.name.as_option_variable_identifier()
			{
				let result = environment.declare_variable(
					name,
					position,
					constructor,
					&mut checking_data.types,
					None,
				);
				if let Err(_err) = result {
					// TODO is this an issue?
					checking_data
						.diagnostics_container
						.add_error(TypeCheckError::ReDeclaredVariable { name, position });
				}
			}
		}
		Declaration::DeclareVariable(_)
		| Declaration::DeclareFunction(_)
		| Declaration::DeclareInterface(_)
		| Declaration::Function(_)
		| Declaration::Enum(_)
		| Declaration::Interface(_)
		| Declaration::TypeAlias(_)
		| Declaration::Import(_) => {}
		Declaration::Export(exported) => match &exported.on {
			parser::declarations::ExportDeclaration::Variable { exported, position: _ } => {
				match exported {
					// Skipped as this is done earlier
					parser::declarations::export::Exportable::Class(class) => {
						// TODO mark as exported
						synthesise_class_declaration(class, environment, checking_data);
					}
					parser::declarations::export::Exportable::Variable(variable) => {
						synthesise_variable_declaration(variable, environment, checking_data, true);
					}
					parser::declarations::export::Exportable::Parts(parts) => {
						for part in parts {
							let pair = super::hoisting::export_part_to_name_pair(part);
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
										exported.named.push((
											pair.r#as.to_owned(),
											(value.0.get_id(), value.0.get_mutability()),
										));
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
		},
	}
}
