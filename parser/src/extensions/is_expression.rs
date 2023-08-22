use std::borrow::Cow;

use crate::{tsx_keywords::Is, TSXKeyword, TSXToken};
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::TokenReader;
use visitable_derive::Visitable;

use crate::{
	errors::parse_lexing_error,
	expressions::{ExpressionOrBlock, MultipleExpression},
	ASTNode, Keyword, TypeAnnotation,
};

#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
pub struct IsExpression {
	pub is: Keyword<Is>,
	pub matcher: Box<MultipleExpression>,
	pub branches: Vec<(TypeAnnotation, ExpressionOrBlock)>,
	pub position: Span,
}

#[cfg(feature = "self-rust-tokenize")]
impl self_rust_tokenize::SelfRustTokenize for IsExpression {
	fn append_to_token_stream(
		&self,
		_token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		todo!()
	}
}

impl ASTNode for IsExpression {
	fn get_position(&self) -> Cow<source_map::Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, source_map::Span>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseOptions,
	) -> crate::ParseResult<Self> {
		let span = reader.expect_next(TSXToken::Keyword(TSXKeyword::Is))?;
		let is = Keyword::new(span);
		is_expression_from_reader_sub_is_keyword(reader, state, settings, is)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str("is (");
		self.matcher.to_string_from_buffer(buf, settings, depth);
		buf.push_str(") {");
		for (at_end, (l, r)) in self.branches.iter().endiate() {
			l.to_string_from_buffer(buf, settings, depth);
			buf.push_str(" => ");
			r.to_string_from_buffer(buf, settings, depth);
			if !at_end {
				buf.push_str(", ");
			}
		}
		buf.push('}');
	}
}

pub(crate) fn is_expression_from_reader_sub_is_keyword(
	reader: &mut impl TokenReader<TSXToken, source_map::Span>,
	state: &mut crate::ParsingState,
	settings: &crate::ParseOptions,
	is: Keyword<Is>,
) -> Result<IsExpression, crate::ParseError> {
	reader.expect_next(TSXToken::OpenParentheses)?;
	let matcher = MultipleExpression::from_reader(reader, state, settings)?;
	reader.expect_next(TSXToken::CloseParentheses)?;
	reader.expect_next(TSXToken::OpenBrace)?;
	let mut branches = Vec::new();
	loop {
		let type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
		reader.expect_next(TSXToken::Arrow)?;
		let body = ExpressionOrBlock::from_reader(reader, state, settings)?;
		let tokenizer_lib::Token(next, pos) = reader.next().ok_or_else(parse_lexing_error)?;
		branches.push((type_annotation, body));
		if next == TSXToken::CloseBrace {
			return Ok(IsExpression {
				position: is.1.union(&pos),
				is,
				matcher: Box::new(matcher),
				branches,
			});
		} else if next != TSXToken::Comma {
			todo!("Error")
		}
	}
}
