use get_field_by_type::GetFieldByType;
use source_map::Span;
use visitable_derive::Visitable;

use crate::{derive_ASTNode, Block};

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, get_field_by_type::GetFieldByType, Visitable)]
#[get_field_by_type_target(Span)]
pub struct Namespace {
	pub is_declare: bool,
	pub name: String,
	pub inner: Block,
	pub position: Span,
}

impl crate::ASTNode for Namespace {
	fn get_position(&self) -> source_map::Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> crate::ParseResult<Self> {
		let start = reader.expect_keyword("namespace")?;
		let name = reader.parse_identifier("namespace name", true)?.to_owned();
		let inner = Block::from_reader(reader)?;
		let position = start.union(inner.get_position());
		Ok(Self { is_declare: false, name, inner, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.include_type_annotations {
			if self.is_declare {
				buf.push_str("declare ");
			}
			buf.push_str("namespace ");
			buf.push_str(&self.name);
			buf.push(' ');
			self.inner.to_string_from_buffer(buf, options, local);
		}
	}
}
