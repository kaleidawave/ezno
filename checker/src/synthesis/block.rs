use parser::{Statement, StatementOrDeclaration};

use crate::{context::Environment, CheckingData};

use super::{
	declarations::synthesise_declaration, hoisting::hoist_statements,
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
