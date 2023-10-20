use parser::declarations::VariableDeclaration;

use crate::{context::Environment, CheckingData};

use super::variables::synthesise_variable_declaration_item;

pub(super) fn synthesise_variable_declaration<T: crate::ReadFromFS>(
	declaration: &VariableDeclaration,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	match declaration {
		VariableDeclaration::ConstDeclaration { declarations, .. } => {
			for variable_declaration in declarations.iter() {
				synthesise_variable_declaration_item(
					&variable_declaration,
					environment,
					true,
					checking_data,
				);
			}
		}
		VariableDeclaration::LetDeclaration {
			declarations,
			keyword: parser::Keyword(_, position),
			..
		} => {
			for variable_declaration in declarations.iter() {
				synthesise_variable_declaration_item(
					&variable_declaration,
					environment,
					false,
					checking_data,
				);
			}
		}
	}
}
