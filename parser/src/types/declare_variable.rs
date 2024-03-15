use tokenizer_lib::{sized_tokens::TokenStart, Token};

use crate::{
	declarations::VariableDeclarationItem, derive_ASTNode, errors::parse_lexing_error, ASTNode,
	Decorator, ParseOptions, ParseResult, Span, TSXKeyword, TSXToken, TokenReader, VariableKeyword,
};

/// A `declare var/let/const` thingy.
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Eq, get_field_by_type::GetFieldByType)]
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

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = state.expect_keyword(reader, TSXKeyword::Declare)?;
		Self::from_reader_sub_declare(reader, state, options, Some(start), Vec::new())
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
			);
		}
	}
}

impl DeclareVariableDeclaration {
	pub fn from_reader_sub_declare(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		start: Option<TokenStart>,
		decorators: Vec<Decorator>,
	) -> ParseResult<Self> {
		let token = reader.next().ok_or_else(parse_lexing_error)?;
		let start = start.unwrap_or(token.1);
		let keyword = VariableKeyword::from_reader(token)?;
		let mut declarations = Vec::new();
		loop {
			let value = VariableDeclarationItem::from_reader(reader, state, options)?;
			declarations.push(value);
			if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
				reader.next();
			} else {
				break;
			}
		}

		let position = start.union(declarations.last().unwrap().get_position());

		Ok(DeclareVariableDeclaration { keyword, declarations, position, decorators })
	}
}
