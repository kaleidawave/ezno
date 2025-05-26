use crate::{
	derive_ASTNode,
	statements_and_declarations::variable::{VariableDeclarationItem, VariableKeyword},
	ASTNode, Decorator, ParseResult, Span,
};

/// A `declare var/let/const` thingy.
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct DeclareVariableDeclaration {
	pub keyword: VariableKeyword,
	/// TODO expressions advised against, but still parse
	pub declarations: Vec<VariableDeclarationItem>,
	pub position: Span,
	pub decorators: Vec<Decorator>,
}

impl ASTNode for DeclareVariableDeclaration {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		reader.expect_keyword("declare")?;
		Self::from_reader_without_declare(reader)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.include_type_annotations {
			buf.push_str("declare ");
			buf.push_str(self.keyword.as_str());
			crate::statements_and_declarations::variable::declarations_to_string(
				&self.declarations,
				buf,
				options,
				local,
				false,
			);
		}
	}
}

impl DeclareVariableDeclaration {
	pub fn from_reader_without_declare(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let keyword = VariableKeyword::from_reader(reader)?;
		let mut declarations = Vec::new();
		loop {
			let value = VariableDeclarationItem::from_reader(reader)?;
			declarations.push(value);
			if !reader.is_operator_advance(",") {
				break;
			}
		}

		let position = start.union(declarations.last().unwrap().get_position());

		Ok(DeclareVariableDeclaration {
			keyword,
			declarations,
			position,
			decorators: Default::default(),
		})
	}
}
