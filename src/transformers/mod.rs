pub mod optimisations;

use parser::{
	declarations::VariableDeclarationKeyword, visiting::BlockItemMut, Declaration, Module,
	StatementOrDeclaration,
};

pub struct ConstToLet;

impl parser::visiting::VisitorMut<BlockItemMut<'_>, ()> for ConstToLet {
	fn visit_mut(
		&mut self,
		item: &mut BlockItemMut,
		_data: &mut (),
		_chain: &parser::visiting::Chain,
	) {
		if let BlockItemMut::StatementOrDeclaration(StatementOrDeclaration::Declaration(
			Declaration::Variable(variable_declaration),
		)) = item
		{
			variable_declaration.kind = VariableDeclarationKeyword::Let;
		}
	}
}

// Removes non import statements
pub fn filter_imports(m: &mut Module) {
	m.items = m
		.items
		.drain(..)
		.filter(|p| matches!(p, StatementOrDeclaration::Declaration(Declaration::Import(..))))
		.collect();
}
