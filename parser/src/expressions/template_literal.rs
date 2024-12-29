use super::{Expression, MultipleExpression};
use crate::{derive_ASTNode, ASTNode, ParseOptions, ParseResult, Span};
use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct TemplateLiteral {
	pub tag: Option<Box<Expression>>,
	pub parts: Vec<(String, MultipleExpression)>,
	pub last: String,
	pub position: Span,
}

impl ASTNode for TemplateLiteral {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let tag = if reader.is_operator_advance("`") {
			None
		} else {
			Some(Box::new(Expression::from_reader_with_precedence(
				reader,
				super::COMMA_PRECEDENCE,
			)?))
		};

		let mut parts = Vec::new();
		loop {
			let current = reader.get_current();
			let mut escaped = false;
			for (idx, chr) in current.char_indices() {
				if escaped {
					escaped = false;
					continue;
				} else if let '\\' = chr {
					escaped = true;
					continue;
				}

				if let '`' = chr {
					let start = reader.get_start();
					reader.advance((idx + '`'.len_utf8()) as u32);
					return Ok(Self {
						parts,
						last: current[..idx].to_owned(),
						tag,
						position: start.union(reader.get_end()),
					});
				} else if current[idx..].starts_with("${") {
					let content = current[..idx].to_owned();
					reader.advance((idx + "${".len()) as u32);
					let expression = MultipleExpression::from_reader(reader)?;
					reader.expect('}')?;
					parts.push((content, expression));
					break;
				}
			}
		}
		let position = reader.get_start().with_length(reader.get_current().len());
		Err(crate::ParseError::new(crate::ParseErrors::UnexpectedEnd, position))
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if let Some(tag) = &self.tag {
			tag.to_string_from_buffer(buf, options, local);
		}
		buf.push('`');
		for (static_part, dynamic_part) in &self.parts {
			buf.push_str_contains_new_line(static_part.as_str());

			buf.push_str("${");
			dynamic_part.to_string_from_buffer(buf, options, local);
			buf.push('}');
		}
		buf.push_str_contains_new_line(self.last.as_str());
		buf.push('`');
	}
}
