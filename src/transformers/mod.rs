pub mod optimisations;

use parser::{
	statements_and_declarations::VariableDeclarationKeyword, Module, StatementOrDeclaration,
};

pub struct ConstToLet;

impl parser::visiting::VisitorMut<StatementOrDeclaration, ()> for ConstToLet {
	fn visit_mut(
		&mut self,
		item: &mut StatementOrDeclaration,
		_data: &mut (),
		_chain: &parser::visiting::Chain,
	) {
		if let StatementOrDeclaration::Variable(variable_declaration) = item {
			variable_declaration.kind = VariableDeclarationKeyword::Let;
		}
	}
}

// Removes non import statements
pub fn filter_imports(m: &mut Module) {
	m.items =
		m.items.drain(..).filter(|p| matches!(p, StatementOrDeclaration::Import(..))).collect();
}
