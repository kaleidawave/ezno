use parser::{ASTNode, Declaration, Statement, StatementOrDeclaration};

use crate::{context::Environment, diagnostics::TypeCheckWarning, CheckingData};

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

	let mut elements = statements.iter();
	for element in elements.by_ref() {
		match element {
			StatementOrDeclaration::Statement(statement) => {
				synthesise_statement(statement, None, environment, checking_data);
			}
			StatementOrDeclaration::Declaration(declaration) => {
				synthesise_declaration(declaration, environment, checking_data);
			}
			StatementOrDeclaration::Marker(_, _) => {
				crate::utilities::notify!("should be unreachable");
			}
			StatementOrDeclaration::Imported { moved: _moved, .. } => {
				crate::utilities::notify!("TODO Checking imported?");
			}
		}

		// TODO conditionals and more etc
		if environment.info.is_finished() {
			break;
		}
	}

	for element in elements.filter(|e| {
		!matches!(
			e,
			StatementOrDeclaration::Statement(
				Statement::Comment(..) | Statement::MultiLineComment(..) | Statement::Empty(..)
			) | StatementOrDeclaration::Declaration(Declaration::Function(..))
		)
	}) {
		checking_data.diagnostics_container.add_warning(TypeCheckWarning::Unreachable(
			element.get_position().with_source(environment.get_source()),
		));
	}
}
