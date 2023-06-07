use parser::{declarations::VariableDeclaration, Chain};
use temporary_annex::Annex;

use crate::{context::Environment, CheckingData};

use super::variables::synthesize_variable_declaration_item;

pub(super) fn synthesize_variable_declaration<T: crate::FSResolver>(
	declaration: &mut VariableDeclaration,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) {
	match declaration {
		VariableDeclaration::ConstDeclaration { declarations, .. } => {
			for variable_declaration in declarations.iter_mut() {
				synthesize_variable_declaration_item(
					variable_declaration,
					environment,
					true,
					checking_data,
					chain,
				);
			}
		}
		VariableDeclaration::LetDeclaration {
			declarations,
			keyword: parser::Keyword(_, position),
			..
		} => {
			for variable_declaration in declarations.iter_mut() {
				synthesize_variable_declaration_item(
					variable_declaration,
					environment,
					false,
					checking_data,
					chain,
				);
			}
		}
	}
}
