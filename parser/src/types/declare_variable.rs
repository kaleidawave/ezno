use crate::{
	declarations::VariableDeclarationItem, derive_ASTNode, ASTNode, Decorator, ParseOptions,
	ParseResult, Span, VariableKeyword,
};

/// A `declare var/let/const` thingy.
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct DeclareVariableDeclaration {
	pub keyword: VariableKeyword,
	/// TODO expressions advised against, but still parse
	pub declarations: Vec<VariableDeclarationItem<Option<crate::Expression>>>,
	pub position: Span,
	pub decorators: Vec<Decorator>,
}

impl ASTNode for DeclareVariableDeclaration {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		todo!()
		// let token = reader.next().ok_or_else(parse_lexing_error)?;
		// let start = start.unwrap_or(token.1);
		// let keyword = VariableKeyword::from_reader(token)?;
		// let mut declarations = Vec::new();
		// loop {
		// 	let value = VariableDeclarationItem::from_reader(reader, state, options)?;
		// 	declarations.push(value);
		// 	if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
		// 		reader.next();
		// 	} else {
		// 		break;
		// 	}
		// }

		// let position = start.union(declarations.last().unwrap().get_position());

		// Ok(DeclareVariableDeclaration { keyword, declarations, position, decorators })
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
			crate::declarations::variable::declarations_to_string(
				&self.declarations,
				buf,
				options,
				local,
				false,
			);
		}
	}
}
