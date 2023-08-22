use std::borrow::Cow;

use crate::{
	errors::parse_lexing_error, ASTNode, Expression, ParseOptions, ParseResult, Span, TSXToken,
	Token, TokenReader,
};
use visitable_derive::Visitable;

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct TemplateLiteral {
	pub tag: Option<Box<Expression>>,
	pub parts: Vec<TemplateLiteralPart<Expression>>,
	pub position: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TemplateLiteralPart<T: ASTNode> {
	Static(String),
	Dynamic(Box<T>),
}

#[cfg(feature = "self-rust-tokenize")]
impl<T: ASTNode + self_rust_tokenize::SelfRustTokenize> self_rust_tokenize::SelfRustTokenize
	for TemplateLiteralPart<T>
{
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		match self {
			TemplateLiteralPart::Static(inner) => {
				let inner = self_rust_tokenize::SelfRustTokenize::to_tokens(inner);
				token_stream
					.extend(self_rust_tokenize::quote!(TemplateLiteralPart::Static(#inner)));
			}
			TemplateLiteralPart::Dynamic(inner) => {
				let inner = self_rust_tokenize::SelfRustTokenize::to_tokens(inner);
				token_stream
					.extend(self_rust_tokenize::quote!(TemplateLiteralPart::Dynamic(#inner)));
			}
		}
	}
}

impl<T: ASTNode + crate::Visitable> crate::Visitable for TemplateLiteralPart<T> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &crate::VisitSettings,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		if let Self::Dynamic(dynamic) = self {
			dynamic.visit(visitors, data, settings, chain)
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &crate::VisitSettings,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		if let Self::Dynamic(dynamic) = self {
			dynamic.visit_mut(visitors, data, settings, chain)
		}
	}
}

impl ASTNode for TemplateLiteral {
	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let start_pos = reader.expect_next(TSXToken::TemplateLiteralStart)?;
		Self::from_reader_sub_start_with_tag(reader, state, settings, None, start_pos)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		if let Some(tag) = &self.tag {
			tag.to_string_from_buffer(buf, settings, depth);
		}
		buf.push('`');
		for part in self.parts.iter() {
			match part {
				TemplateLiteralPart::Static(content) => {
					buf.push_str_contains_new_line(content.as_str())
				}
				TemplateLiteralPart::Dynamic(expression) => {
					buf.push_str("${");
					expression.to_string_from_buffer(buf, settings, depth);
					buf.push('}');
				}
			}
		}
		buf.push('`');
	}
}

impl TemplateLiteral {
	pub(crate) fn from_reader_sub_start_with_tag(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
		tag: Option<Box<Expression>>,
		start_position: Span,
	) -> ParseResult<Self> {
		let mut parts = Vec::<TemplateLiteralPart<_>>::new();
		loop {
			match reader.next().ok_or_else(parse_lexing_error)? {
				Token(TSXToken::TemplateLiteralChunk(chunk), _) => {
					parts.push(TemplateLiteralPart::Static(chunk));
				}
				Token(TSXToken::TemplateLiteralExpressionStart, _) => {
					let expression = Expression::from_reader(reader, state, settings)?;
					reader.expect_next(TSXToken::TemplateLiteralExpressionEnd)?;
					parts.push(TemplateLiteralPart::Dynamic(Box::new(expression)));
				}
				Token(TSXToken::TemplateLiteralEnd, end_position) => {
					return Ok(Self { parts, tag, position: start_position.union(&end_position) });
				}
				_ => unreachable!(),
			}
		}
	}
}
