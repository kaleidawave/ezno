use parser::{ASTNode, StatementOrDeclaration};

use crate::{context::Environment, diagnostics::TypeCheckWarning, CheckingData};

use super::{
	hoisting::hoist_statements, statements_and_declarations::synthesise_statement_or_declaration,
};

/// Note that this expects the environment to be new lexically
pub(super) fn synthesise_block<T: crate::ReadFromFS>(
	statements: &[StatementOrDeclaration],
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	hoist_statements(statements, environment, checking_data);

	let mut items = statements.iter();
	for item in items.by_ref() {
		synthesise_statement_or_declaration(item, None, environment, checking_data);
		// TODO conditionals and more etc
		if environment.info.is_finished() {
			break;
		}
	}

	for item in items.filter(|e| {
		!matches!(
			e,
			StatementOrDeclaration::Comment(..)
				| StatementOrDeclaration::MultiLineComment(..)
				| StatementOrDeclaration::Empty(..)
				| StatementOrDeclaration::AestheticSemiColon(..)
				| StatementOrDeclaration::Function(..)
		)
	}) {
		checking_data.diagnostics_container.add_warning(TypeCheckWarning::Unreachable(
			item.get_position().with_source(environment.get_source()),
		));
	}
}
