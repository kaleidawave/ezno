use parser::{Declaration, Statement, StatementOrDeclaration};

use crate::{context::Environment, diagnostics::TypeCheckError, CheckingData};

use super::{
	classes::synthesise_class_declaration, declarations::synthesise_variable_declaration,
	hoisting::hoist_statements, statements::synthesise_statement,
};

/// Note that this expects the environment to be new lexically
pub(super) fn synthesise_block<T: crate::FSResolver>(
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

pub(crate) fn synthesize_declaration<T: crate::FSResolver>(
	declaration: &Declaration,
	environment: &mut crate::context::Context<crate::context::Syntax<'_>>,
	checking_data: &mut CheckingData<'_, T>,
) {
	match declaration {
		Declaration::Variable(declaration) => {
			synthesise_variable_declaration(declaration, environment, checking_data)
		}
		Declaration::Class(class) => {
			let constructor = synthesise_class_declaration(class, environment, checking_data);
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
		Declaration::Import(_) => todo!(),
		Declaration::Export(_) => todo!(),
	}
}
