use source_map::Span;

use crate::{ASTNode, TSXToken, TypeAnnotation, TypeDeclaration};

/// e.g. `type NumberArray = Array<number>`
#[derive(Debug, Clone, PartialEq, Eq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct TypeAlias {
	pub type_name: TypeDeclaration,
	pub type_expression: TypeAnnotation,
	pub position: Span,
}

impl ASTNode for TypeAlias {
	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseOptions,
	) -> crate::ParseResult<Self> {
		let start = reader.expect_next(TSXToken::Keyword(crate::TSXKeyword::Type))?;
		let type_name = TypeDeclaration::from_reader(reader, state, settings)?;
		reader.expect_next(TSXToken::Assign)?;
		let type_expression = TypeAnnotation::from_reader(reader, state, settings)?;
		let position = start.union(type_expression.get_position());
		Ok(Self { type_name, type_expression, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		if settings.include_types {
			buf.push_str("type ");
			self.type_name.to_string_from_buffer(buf, settings, depth);
			buf.push_str(" = ");
			self.type_expression.to_string_from_buffer(buf, settings, depth);
		}
	}

	fn get_position(&self) -> &Span {
		&self.position
	}
}
