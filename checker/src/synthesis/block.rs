use parser::{Declaration, Statement, StatementOrDeclaration};

use crate::{context::Environment, diagnostics::TypeCheckError, CheckingData};

use super::{
	classes::synthesize_class_declaration, declarations::synthesize_variable_declaration,
	hoisting::hoist_statements, statements::synthesize_statement,
};

/// Note that this expects the environment to be new lexically
pub(super) fn synthesize_block<T: crate::FSResolver>(
	statements: &[StatementOrDeclaration],
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) {
	hoist_statements(statements, environment, checking_data);

	// Run through accessible statements
	let mut unreachable_at_idx = None::<usize>;

	for (idx, statement) in statements.iter().enumerate() {
		match statement {
			StatementOrDeclaration::Statement(statement) => {
				let statement_result = synthesize_statement(statement, environment, checking_data);

				// TODO Statement::is_control_flow ...?
				if let Statement::Return(..) | Statement::Break(..) | Statement::Continue(..) =
					statement
				{
					unreachable_at_idx = Some(idx + 1);
					break;
				}
			}
			StatementOrDeclaration::Declaration(declaration) => match declaration {
				Declaration::Variable(declaration) => {
					synthesize_variable_declaration(declaration, environment, checking_data)
				}
				Declaration::Class(class) => {
					let constructor =
						synthesize_class_declaration(class, environment, checking_data);
					let result = environment.declare_variable(
						class.on.name.as_str(),
						class.on.position.clone(),
						constructor,
						&mut checking_data.types,
					);
					if let Err(err) = result {
						checking_data.diagnostics_container.add_error(
							TypeCheckError::ReDeclaredVariable {
								name: class.on.name.as_str(),
								position: class.on.position.clone(),
							},
						)
					}
				}
				Declaration::DeclareVariable(_)
				| Declaration::DeclareFunction(_)
				| Declaration::DeclareInterface(_)
				| Declaration::Function(_)
				| Declaration::Enum(_)
				| Declaration::Interface(_)
				| Declaration::TypeAlias(_) => {}
				Declaration::Import(_) => todo!(),
				Declaration::Export(_) => todo!(),
			},
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
