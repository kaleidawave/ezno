use crate::{declarations::classes::ClassMember, derive_ASTNode, ASTNode, Expression};
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use visitable_derive::Visitable;

#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub struct EnumDeclaration {
	pub is_constant: bool,
	pub name: String,
	pub members: Vec<EnumMember>,
	pub position: Span,
}

impl ASTNode for EnumDeclaration {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> Result<Self, crate::ParseError> {
		let start = reader.get_start();
		let is_constant = reader.is_keyword_advance("const");
		reader.expect_keyword("enum")?;
		let name = reader.parse_identifier("enum name", true)?.to_owned();
		reader.expect('{')?;
		let mut members = Vec::new();
		loop {
			if reader.is_operator("}") {
				break;
			}
			// TODO temp
			if reader.is_one_of(&["//", "/*"]).is_some() {
				let is_multiline = reader.starts_with_slice("/*");
				reader.advance(2);
				let _content = reader.parse_comment_literal(is_multiline)?;
				continue;
			}
			members.push(EnumMember::from_reader(reader)?);
			let comma_delimited = reader.is_operator_advance(",");
			if !comma_delimited {
				reader.expect_semi_colon()?;
			}
		}
		reader.expect('}')?;
		let position = start.union(reader.get_end());
		Ok(EnumDeclaration { is_constant, name, members, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.include_type_annotations {
			if self.is_constant {
				buf.push_str("const ");
			}
			buf.push_str("enum ");
			buf.push_str(&self.name);
			options.push_gap_optionally(buf);
			buf.push_str("{");
			for (at_end, member) in self.members.iter().endiate() {
				if options.pretty {
					buf.push_new_line();
					options.add_indent(local.depth + 1, buf);
				}
				member.to_string_from_buffer(buf, options, local);
				if !options.pretty && !at_end {
					buf.push(',');
				}
			}
			if options.pretty && !self.members.is_empty() {
				buf.push_new_line();
			}
			buf.push('}');
		}
	}
}

#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub enum EnumMemberValue {
	ClassMembers(Vec<crate::Decorated<ClassMember>>),
	Value(Expression),
	None,
}

#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub struct EnumMember {
	pub name: String,
	pub value: EnumMemberValue,
	pub position: Span,
}

impl ASTNode for EnumMember {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> Result<Self, crate::ParseError> {
		let start = reader.get_start();
		let name = reader.parse_identifier("enum member name", true)?.to_owned();
		let value = if reader.is_operator_advance("=") {
			let expression = Expression::from_reader(reader)?;
			EnumMemberValue::Value(expression)
		} else if reader.is_operator_advance("{") {
			let mut members: Vec<crate::Decorated<ClassMember>> = Vec::new();
			loop {
				reader.skip();
				if reader.starts_with('}') {
					break;
				}
				let value = crate::Decorated::<ClassMember>::from_reader(reader)?;
				if let ClassMember::Property { .. } | ClassMember::Indexer { .. } = &value.on {
					reader.expect_semi_colon()?;
				}
				members.push(value);
			}
			let _end = reader.expect('}')?;
			EnumMemberValue::ClassMembers(members)
		} else {
			EnumMemberValue::None
		};
		let position = start.union(reader.get_end());
		Ok(EnumMember { name, value, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str(&self.name);
		match &self.value {
			EnumMemberValue::Value(value) => {
				buf.push_str(if options.pretty { " = " } else { "=" });
				value.to_string_from_buffer(buf, options, local);
			}
			EnumMemberValue::ClassMembers(_members) => todo!(),
			EnumMemberValue::None => {}
		}
	}
}
