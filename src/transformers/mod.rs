pub mod optimisations;

use parser::{visiting::BlockItemMut, Declaration, Module, StatementOrDeclaration};

pub struct ConstToLet;

impl parser::visiting::VisitorMut<BlockItemMut<'_>, ()> for ConstToLet {
	fn visit_mut(
		&mut self,
		item: &mut BlockItemMut,
		_data: &mut (),
		_chain: &parser::visiting::Chain,
	) {
		if let BlockItemMut::StatementOrDeclaration(StatementOrDeclaration::Declaration(
			Declaration::Variable(decl),
		)) = item
		{
			if let parser::declarations::VariableDeclaration::ConstDeclaration {
				keyword,
				declarations,
				position,
			} = decl
			{
				*decl = parser::declarations::VariableDeclaration::LetDeclaration {
					keyword: parser::Keyword::new(keyword.get_position().clone()),
					declarations: declarations
						.drain(..)
						.map(|dec| parser::declarations::VariableDeclarationItem {
							name: dec.name,
							type_annotation: dec.type_annotation,
							expression: Some(dec.expression),
							position: dec.position,
						})
						.collect(),
					position: position.clone(),
				};
			}
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
