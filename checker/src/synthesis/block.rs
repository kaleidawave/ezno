use parser::{declarations::VariableDeclaration, Declaration, Statement, StatementOrDeclaration};

use crate::{
	context::Environment, diagnostics::TypeCheckError, structures::modules::Exported, CheckingData,
	Scope,
};

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
				let statement_result = synthesise_statement(statement, environment, checking_data);

				// TODO Statement::is_control_flow ...?
				if let Statement::Return(..) | Statement::Break(..) | Statement::Continue(..) =
					statement
				{
					unreachable_at_idx = Some(idx + 1);
					break;
				}
			}
			StatementOrDeclaration::Declaration(declaration) => {
				synthesize_declaration(declaration, environment, checking_data)
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

pub(crate) fn synthesize_declaration<T: crate::ReadFromFS>(
	declaration: &Declaration,
	environment: &mut crate::context::Context<crate::context::Syntax<'_>>,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	match declaration {
		Declaration::Variable(declaration) => {
			synthesise_variable_declaration(declaration, environment, checking_data, false)
		}
		Declaration::Class(class) => {
			let constructor = synthesise_class_declaration(&class.on, environment, checking_data);
			let position = class.on.position.clone().with_source(environment.get_source());
			let result = environment.declare_variable(
				class.on.name.as_str(),
				position.clone(),
				constructor,
				&mut checking_data.types,
			);
			if let Err(err) = result {
				checking_data.diagnostics_container.add_error(TypeCheckError::ReDeclaredVariable {
					name: class.on.name.as_str(),
					position,
				})
			}
		}
		Declaration::DeclareVariable(_)
		| Declaration::DeclareFunction(_)
		| Declaration::DeclareInterface(_)
		| Declaration::Function(_)
		| Declaration::Enum(_)
		| Declaration::Interface(_)
		| Declaration::TypeAlias(_) => {}
		// Imports are hoisted
		Declaration::Import(_) => {}
		Declaration::Export(exported) => match &exported.on {
			parser::declarations::ExportDeclaration::Variable { exported, position } => {
				match exported {
					// Skipped as this is done earlier
					parser::declarations::export::Exportable::Function(_)
					| parser::declarations::export::Exportable::Interface(_)
					| parser::declarations::export::Exportable::TypeAlias(_) => {}
					parser::declarations::export::Exportable::Class(class) => {
						// TODO mark as exported
						synthesise_class_declaration(class, environment, checking_data);
					}
					parser::declarations::export::Exportable::Variable(variable) => {
						synthesise_variable_declaration(variable, environment, checking_data, true);
					}
					parser::declarations::export::Exportable::Parts(_) => todo!(),
				}
			}
			parser::declarations::ExportDeclaration::Default { expression, position } => {
				let result = synthesise_expression(expression, environment, checking_data);
				if let Scope::Module { ref mut exported, .. } = environment.context_type.kind {
					if exported.default.is_some() {
						checking_data.diagnostics_container.add_error(
							TypeCheckError::DoubleDefaultExport(
								position.clone().with_source(environment.get_source()),
							),
						);
					} else {
						exported.default = Some(result);
					}
				} else {
					checking_data.diagnostics_container.add_error(
						TypeCheckError::NonTopLevelExport(
							position.clone().with_source(environment.get_source()),
						),
					);
				}
			}
		},
	}
}
