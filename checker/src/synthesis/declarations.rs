use parser::declarations::VariableDeclaration;

use crate::{behavior::variables::VariableMutability, context::Environment, CheckingData};

use super::variables::synthesise_variable_declaration_item;

pub(super) fn synthesise_variable_declaration<T: crate::ReadFromFS>(
	declaration: &VariableDeclaration,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	exported: bool,
) {
	match declaration {
		VariableDeclaration::ConstDeclaration { declarations, .. } => {
			for variable_declaration in declarations {
				synthesise_variable_declaration_item(
					variable_declaration,
					environment,
					true,
					checking_data,
					exported.then_some(VariableMutability::Constant),
				);
			}
		}
		VariableDeclaration::LetDeclaration {
			declarations,
			keyword: parser::Keyword(_, _position),
			..
		} => {
			for variable_declaration in declarations {
				let exported = exported.then(|| {
					let restriction = checking_data
						.type_mappings
						.variable_restrictions
						.get(&(environment.get_source(), variable_declaration.position.start))
						.map(|(first, _)| *first);
					VariableMutability::Mutable { reassignment_constraint: restriction }
				});
				synthesise_variable_declaration_item(
					variable_declaration,
					environment,
					false,
					checking_data,
					exported,
				);
			}
		}
	}
}
