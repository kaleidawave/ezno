use crate::{tsx_keywords::Is, TSXToken};
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::TokenReader;
use visitable_derive::Visitable;

use crate::{
	expressions::{ExpressionOrBlock, MultipleExpression},
	ASTNode, Keyword, TypeAnnotation,
};

#[derive(Debug, PartialEq, Eq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
#[get_field_by_type_target(Span)]
pub struct IsExpression {
	pub is: Keyword<Is>,
	pub matcher: Box<MultipleExpression>,
	pub branches: Vec<(TypeAnnotation, ExpressionOrBlock)>,
	pub position: Span,
}

impl ASTNode for IsExpression {
	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseOptions,
	) -> crate::ParseResult<Self> {
		let is = Keyword::from_reader(reader)?;
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

	fn get_position(&self) -> &Span {
		&self.position
	}
}

pub(crate) fn is_expression_from_reader_sub_is_keyword(
	reader: &mut impl TokenReader<crate::TSXToken, crate::TokenStart>,
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
		let type_annotation =
			TypeAnnotation::from_reader_with_config(reader, state, settings, false, true)?;
		reader.expect_next(TSXToken::Arrow)?;
		let body = ExpressionOrBlock::from_reader(reader, state, settings)?;
		if let Some(token) = reader.conditional_next(|t| matches!(t, TSXToken::CloseBrace)) {
			branches.push((type_annotation, body));
			return Ok(IsExpression {
				position: is.get_position().union(token.get_end()),
				is,
				matcher: Box::new(matcher),
				branches,
			});
		}
		if matches!(body, ExpressionOrBlock::Expression(..)) {
			reader.expect_next(TSXToken::Comma)?;
		}
		branches.push((type_annotation, body));
	}
}
