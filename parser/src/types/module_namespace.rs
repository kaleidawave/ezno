use get_field_by_type::GetFieldByType;
use source_map::Span;
use visitable_derive::Visitable;

use crate::{Block, derive_ASTNode};

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, get_field_by_type::GetFieldByType, Visitable)]
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

	fn from_reader(reader: &mut crate::Lexer) -> crate::ParseResult<Self> {
		let start = reader.expect_keyword("namespace")?;
		let name = reader.parse_identifier("namespace name", true)?.into_owned();
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

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, get_field_by_type::GetFieldByType, Visitable)]
#[get_field_by_type_target(Span)]
pub struct Module {
	pub is_declare: bool,
	pub name: (String, Option<crate::Quoting>),
	pub inner: Option<Block>,
	pub position: Span,
}

impl crate::ASTNode for Module {
	fn get_position(&self) -> source_map::Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::Lexer) -> crate::ParseResult<Self> {
		let start = reader.expect_keyword("module")?;
		let name = if reader.starts_with_string_delimeter() {
			let (content, quoting, _width) = reader.parse_string_literal()?;
			(content.into_owned(), Some(quoting))
		} else {
			let content = reader.parse_identifier("module name", true)?.into_owned();
			(content, None)
		};
		if reader.starts_with('{') {
			let inner = Block::from_reader(reader)?;
			let position = start.union(inner.get_position());
			Ok(Self { is_declare: false, name, inner: Some(inner), position })
		} else {
			let position = start.union(reader.get_end());
			Ok(Self { is_declare: false, name, inner: None, position })
		}
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
			buf.push_str("module ");
			let (name, quoting) = &self.name;
			match quoting {
				Some(quoting) => {
					buf.push(quoting.as_char());
					buf.push_str(name);
					buf.push(quoting.as_char());
				}
				None => {
					buf.push_str(name);
				}
			}
			if let Some(ref inner) = self.inner {
				buf.push(' ');
				inner.to_string_from_buffer(buf, options, local);
			}
		}
	}
}
