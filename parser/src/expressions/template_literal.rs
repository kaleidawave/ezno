use super::{Expression, MultipleExpression};
use crate::{
	derive_ASTNode, errors::parse_lexing_error, ASTNode, ParseOptions, ParseResult, Span, TSXToken,
	Token, TokenReader,
};
use tokenizer_lib::sized_tokens::TokenStart;
use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
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

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = reader.expect_next(TSXToken::TemplateLiteralStart)?;
		Self::from_reader_sub_start_with_tag(reader, state, options, None, start)
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
		buf.push_str_contains_new_line(self.final_part.as_str());
		buf.push('`');
	}
}

impl TemplateLiteral {
	pub(crate) fn from_reader_sub_start_with_tag(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		tag: Option<Box<Expression>>,
		start: TokenStart,
	) -> ParseResult<Self> {
		let mut parts = Vec::new();
		let mut last = String::new();
		loop {
			match reader.next().ok_or_else(parse_lexing_error)? {
				Token(TSXToken::TemplateLiteralChunk(chunk), _) => {
					last = chunk;
				}
				Token(TSXToken::TemplateLiteralExpressionStart, _) => {
					let expression = MultipleExpression::from_reader(reader, state, options)?;
					parts.push((std::mem::take(&mut last), expression));
					reader.expect_next(TSXToken::TemplateLiteralExpressionEnd)?;
				}
				t @ Token(TSXToken::TemplateLiteralEnd, _) => {
					return Ok(Self {
						parts,
						final_part: last,
						tag,
						position: start.union(t.get_end()),
					});
				}
				Token(TSXToken::EOS, _) => return Err(parse_lexing_error()),
				t => unreachable!("Token {:?}", t),
			}
		}
	}
}
