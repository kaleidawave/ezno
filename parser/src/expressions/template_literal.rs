use crate::{
	errors::parse_lexing_error, ASTNode, Expression, ParseOptions, ParseResult, Span, TSXToken,
	Token, TokenReader,
};
use tokenizer_lib::sized_tokens::TokenStart;
use visitable_derive::Visitable;

#[derive(Debug, Clone, PartialEq, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct TemplateLiteral {
	pub tag: Option<Box<Expression>>,
	pub parts: Vec<TemplateLiteralPart<Expression>>,
	pub position: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum TemplateLiteralPart<T: ASTNode> {
	Static(String),
	Dynamic(Box<T>),
}

impl<T: ASTNode + crate::Visitable> crate::Visitable for TemplateLiteralPart<T> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::VisitSettings,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		if let Self::Dynamic(dynamic) = self {
			dynamic.visit(visitors, data, options, chain)
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::VisitSettings,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		if let Self::Dynamic(dynamic) = self {
			dynamic.visit_mut(visitors, data, options, chain)
		}
	}
}

impl ASTNode for TemplateLiteral {
	fn get_position(&self) -> &Span {
		&self.position
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
		depth: u8,
	) {
		if let Some(tag) = &self.tag {
			tag.to_string_from_buffer(buf, options, depth);
		}
		buf.push('`');
		for part in self.parts.iter() {
			match part {
				TemplateLiteralPart::Static(content) => {
					buf.push_str_contains_new_line(content.as_str())
				}
				TemplateLiteralPart::Dynamic(expression) => {
					buf.push_str("${");
					expression.to_string_from_buffer(buf, options, depth);
					buf.push('}');
				}
			}
		}
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
		let mut parts = Vec::<TemplateLiteralPart<_>>::new();
		loop {
			match reader.next().ok_or_else(parse_lexing_error)? {
				Token(TSXToken::TemplateLiteralChunk(chunk), _) => {
					parts.push(TemplateLiteralPart::Static(chunk));
				}
				Token(TSXToken::TemplateLiteralExpressionStart, _) => {
					let expression = Expression::from_reader(reader, state, options)?;
					reader.expect_next(TSXToken::TemplateLiteralExpressionEnd)?;
					parts.push(TemplateLiteralPart::Dynamic(Box::new(expression)));
				}
				Token(TSXToken::EOS, _) => return Err(parse_lexing_error()),
				t @ Token(TSXToken::TemplateLiteralEnd, _) => {
					return Ok(Self { parts, tag, position: start.union(t.get_end()) });
				}
				t => unreachable!("Token {:?}", t),
			}
		}
	}
}
