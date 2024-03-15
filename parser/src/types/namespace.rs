use get_field_by_type::GetFieldByType;
use source_map::Span;

use crate::{derive_ASTNode, Block};

/// While `Block`, only some items are allowed
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct Namespace {
	pub name: String,
	pub inner: Block,
	pub position: Span,
}

impl crate::ASTNode for Namespace {
	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> crate::ParseResult<Self> {
		let start = reader.expect_next(crate::TSXToken::Keyword(crate::TSXKeyword::Namespace))?;
		let (name, _) = crate::tokens::token_as_identifier(
			reader.next().ok_or_else(crate::errors::parse_lexing_error)?,
			"namespace name",
		)?;
		let inner = Block::from_reader(reader, state, options)?;
		let position = start.union(inner.get_position());
		Ok(Self { name, inner, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.include_type_annotations {
			buf.push_str("namespace ");
			buf.push_str(&self.name);
			buf.push(' ');
			self.inner.to_string_from_buffer(buf, options, local);
		}
	}

	fn get_position(&self) -> source_map::Span {
		*self.get()
	}
}
