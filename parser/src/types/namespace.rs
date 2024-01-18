#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Namespace(String, Vec<crate::TypeDefinitionModuleDeclaration>);

impl crate::ASTNode for Namespace {
	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> crate::ParseResult<Self> {
		reader.expect_next(crate::TSXToken::Keyword(crate::TSXKeyword::Namespace))?;
		let (namespace_name, _) = crate::tokens::token_as_identifier(
			reader.next().ok_or_else(crate::errors::parse_lexing_error)?,
			"namespace name",
		)?;
		reader.expect_next(crate::TSXToken::OpenBrace)?;
		let mut declarations = Vec::new();
		while let Some(token) = reader.peek() {
			if let tokenizer_lib::Token(crate::TSXToken::CloseBrace, _) = token {
				break;
			}
			declarations
				.push(crate::TypeDefinitionModuleDeclaration::from_reader(reader, state, options)?);
			if let Some(tokenizer_lib::Token(crate::TSXToken::SemiColon, _)) = reader.peek() {
				reader.next();
			}
		}
		reader.next();
		Ok(Self(namespace_name, declarations))
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		_buf: &mut T,
		_options: &crate::ToStringOptions,
		_local: crate::LocalToStringInformation,
	) {
		todo!()
	}

	fn get_position(&self) -> &source_map::Span {
		todo!()
	}
}
