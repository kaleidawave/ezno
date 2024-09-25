use crate::{derive_ASTNode, ASTNode, Expression};
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

	fn from_reader(reader: &mut crate::new::Lexer) -> Result<Self, crate::ParseError> {
		let start = reader.get_start();
		let is_constant = reader.is_keyword_advance("const");
		reader.expect_keyword("enum")?;
		// token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "Enum name")?;
		let name = reader.parse_identifier()?.to_owned();
		reader.expect('{')?;
		let mut members = Vec::new();
		loop {
			if reader.is_operator("}") {
				break;
			}
			// TODO temp
			if reader.is_operator_advance("//") {
				let _content = reader.parse_until("\n").expect("TODO");
				continue;
			} else if reader.is_operator_advance("/*") {
				let _content = reader.parse_until("*/").expect("TODO");
				continue;
			}
			members.push(EnumMember::from_reader(reader)?);
			if !reader.is_operator_advance(",") {
				reader.expect_semi_colon();
			}
		}
		reader.expect('}')?;
		let position = start.union(reader.get_end());
		Ok(EnumDeclaration { is_constant, name, position, members })
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
pub enum EnumMember {
	Variant { name: String, value: Option<Expression>, position: Span },
}

impl ASTNode for EnumMember {
	fn get_position(&self) -> Span {
		match self {
			EnumMember::Variant { position, .. } => *position,
		}
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> Result<Self, crate::ParseError> {
		let start = reader.get_start();
		let name = reader.parse_identifier()?.to_owned();
		let (position, value) = if reader.is_operator_advance("=") {
			let expression = Expression::from_reader(reader)?;
			(start.union(expression.get_position()), Some(expression))
		} else {
			(start.with_length(name.len()), None)
		};
		Ok(EnumMember::Variant { name, value, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			EnumMember::Variant { name, value, .. } => {
				buf.push_str(name);
				if let Some(value) = value {
					buf.push_str(if options.pretty { " = " } else { "=" });
					value.to_string_from_buffer(buf, options, local);
				}
			}
		}
	}
}
