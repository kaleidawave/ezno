use parser::{Block, Declaration, Statement, StatementOrDeclaration};

use crate::{context::Environment, CheckingData};

use super::synthesize_statement;

/// Note that this expects the environment to be new lexically
pub(crate) fn synthesize_block<T: crate::FSResolver>(
	Block(statements, block_id, _): &mut Block,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut temporary_annex::Annex<parser::Chain>,
) {
	// TODO out_exports: &mut Option<&mut HashMap<String, Variable>>,

	// let collect: Vec<_> = statements
	// 	.iter()
	// 	.filter_map(|item| {
	// 		if let StatementOrDeclaration::Declaration(Declaration::Interface(i)) = item {
	// 			Some(i)
	// 		} else {
	// 			None
	// 		}
	// 	})
	// 	.collect();

	// declare_hoisted_items(checking_data, block_id, environment);

	// Run through accessible statements
	let mut unreachable_at_idx = None::<usize>;

	for (idx, statement) in statements.iter_mut().enumerate() {
		match statement {
			StatementOrDeclaration::Statement(statement) => {
				let statement_result =
					synthesize_statement(statement, environment, checking_data, chain);

				// TODO Statement::is_control_flow ...?
				if let Statement::Return(..) | Statement::Break(..) | Statement::Continue(..) =
					statement
				{
					unreachable_at_idx = Some(idx + 1);
					break;
				}
			}
			StatementOrDeclaration::Declaration(declaration) => match declaration {
				Declaration::Variable(_) => todo!(),
				Declaration::Function(_) => todo!(),
				Declaration::Class(_) => todo!(),
				Declaration::Enum(_) => todo!(),
				Declaration::Interface(_) => todo!(),
				Declaration::TypeAlias(_) => todo!(),
				Declaration::DeclareVariable(_) => todo!(),
				Declaration::DeclareFunction(_) => todo!(),
				Declaration::DeclareInterface(_) => todo!(),
				Declaration::Import(_) => todo!(),
				Declaration::Export(_) => todo!(),
			},
		}
	}

	if let Some(statements) = unreachable_at_idx.map(|idx| &statements[idx..]) {
		crate::utils::notify!("Statements not run, only run for expressions and such");
		// if statements.iter().all(|stmt|)
		// let span = statements.first().unwrap().get_position().union(&statements.last().unwrap().get_position());
		// checking_data.error_warning_info_handler.add_error(TypeCheckError::StatementsNotRun { between: span });
	}
}
