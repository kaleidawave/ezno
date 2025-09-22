use super::{Expression, MultipleExpression};
use crate::{derive_ASTNode, ASTNode, ParseError, ParseErrors, ParseResult, Span};
use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct TemplateLiteral {
	pub tag: Option<Box<Expression>>,
	pub parts: Vec<(String, MultipleExpression)>,
	pub final_part: String,
	pub position: Span,
}

impl ASTNode for TemplateLiteral {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let tag = if reader.is_operator_advance("`") {
			None
		} else {
			let tag = Expression::from_reader_with_precedence(reader, super::COMMA_PRECEDENCE)?;
			if tag.is_optional_like_expression() {
				return Err(crate::ParseError::new(
					crate::ParseErrors::TaggedTemplateCannotBeUsedWithOptionalChain,
					tag.get_position(),
				));
			}
			Some(Box::new(tag))
		};

		let (parts, final_part) = parse_template_literal::<MultipleExpression>(reader, start)?;
		let position = start.union(reader.get_end());
		Ok(Self { tag, parts, final_part, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if let Some(tag) = &self.tag {
			let requires_parenthesis = !matches!(
				&**tag,
				Expression::VariableReference(..)
					| Expression::PropertyAccess { .. }
					| Expression::Parenthesised(..)
			);
			if requires_parenthesis {
				buf.push('(');
			}
			tag.to_string_from_buffer(buf, options, local);
			if requires_parenthesis {
				buf.push(')');
			}
		}
		buf.push('`');
		for (static_part, dynamic_part) in &self.parts {
			buf.push_str_contains_new_line(static_part.as_str());

			buf.push_str("${");
			dynamic_part.to_string_from_buffer(buf, options, local);
			buf.push('}');
		}
		buf.push_str_contains_new_line(self.final_part.as_str());
		buf.push('`');
	}
}

pub fn parse_template_literal<T: ASTNode>(
	reader: &mut crate::Lexer,
	start: source_map::Start,
) -> Result<(Vec<(String, T)>, String), ParseError> {
	let mut parts = Vec::new();
	let mut last_part = std::borrow::Cow::Borrowed("");
	while !reader.is_finished() {
		let current = reader.get_current();
		if current.starts_with("${") {
			reader.advance(2);
			let expression = T::from_reader(reader)?;
			reader.expect('}')?;
			parts.push((std::mem::take(&mut last_part).into_owned(), expression));
		} else {
			let mut buf = std::borrow::Cow::Borrowed("");
			let chars: [char; _] = ['$', '`', '\\'];
			let mut delimeters = current.match_indices(chars);

			let mut last = 0;
			while let Some((idx, matched)) = delimeters.next() {
				buf += &current[last..idx];

				if matched == "`" {
					reader.advance(idx as u32 + 1);
					return Ok((parts, buf.into_owned()));
				} else if current[idx..].starts_with("${") {
					reader.advance(idx as u32);
					last_part = buf;
					break;
				} else if matched == "\\" {
					let chr = current[idx + 1..].chars().next();
					let result = crate::lexer::utilities::escape_character(
						chr,
						&current[idx + 2..],
						idx,
						&mut last,
						&mut buf,
					);
					match result {
						Ok(skip_next) => {
							if skip_next {
								let _ = delimeters.next();
							}
						}
						Err(()) => {
							return Err(ParseError::new(
								ParseErrors::InvalidStringLiteral,
								reader.get_start().with_length(reader.get_current().len()),
							));
						}
					}
				} else {
					return Err(ParseError::new(
						ParseErrors::InvalidStringLiteral,
						start.with_length(idx),
					));
				}
			}
		}
	}

	Err(ParseError::new(ParseErrors::InvalidStringLiteral, start.union(reader.get_end())))
}
