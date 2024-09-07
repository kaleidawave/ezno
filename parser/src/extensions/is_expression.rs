use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::{sized_tokens::TokenStart, TokenReader};
use visitable_derive::Visitable;

use crate::{
	derive_ASTNode,
	expressions::{ExpressionOrBlock, MultipleExpression},
	ASTNode, TSXToken, TypeAnnotation,
};

#[apply(derive_ASTNode)]
#[derive(Debug, PartialEq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct IsExpression {
	pub matcher: Box<MultipleExpression>,
	pub branches: Vec<(TypeAnnotation, ExpressionOrBlock)>,
	pub position: Span,
}

impl ASTNode for IsExpression {
	fn from_reader(reader: &mut crate::new::Lexer) -> crate::ParseResult<Self> {
		let _existing = r#"let start = state.expect_keyword(reader, crate::TSXKeyword::Is)?;
		is_expression_from_reader_sub_is_keyword(reader, state, options, start)"#;
		todo!();
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("is (");
		self.matcher.to_string_from_buffer(buf, options, local);
		buf.push_str(") {");
		for (at_end, (l, r)) in self.branches.iter().endiate() {
			l.to_string_from_buffer(buf, options, local);
			buf.push_str(" => ");
			r.to_string_from_buffer(buf, options, local);
			if !at_end {
				buf.push_str(", ");
			}
		}
		buf.push('}');
	}

	fn get_position(&self) -> Span {
		self.position
	}
}

// pub(crate) fn is_expression_from_reader_sub_is_keyword(
// 	reader: &mut impl TokenReader<crate::TSXToken, crate::TokenStart>,
// 	state: &mut crate::ParsingState,
// 	options: &crate::ParseOptions,
// 	start: TokenStart,
// ) -> Result<IsExpression, crate::ParseError> {
// 	reader.expect_next(TSXToken::OpenParentheses)?;
// 	let matcher = MultipleExpression::from_reader(reader, state, options)?;
// 	reader.expect_next(TSXToken::CloseParentheses)?;
// 	reader.expect_next(TSXToken::OpenBrace)?;
// 	let mut branches = Vec::new();
// 	loop {
// 		// Function important here for
// 		let type_annotation = TypeAnnotation::from_reader_with_config(
// 			reader,
// 			state,
// 			options,
// 			Some(crate::type_annotations::TypeOperatorKind::Function),
// 			None,
// 		)?;
// 		reader.expect_next(TSXToken::Arrow)?;
// 		let body = ExpressionOrBlock::from_reader(reader, state, options)?;
// 		if let Some(token) = reader.conditional_next(|t| matches!(t, TSXToken::CloseBrace)) {
// 			branches.push((type_annotation, body));
// 			return Ok(IsExpression {
// 				position: start.union(token.get_end()),
// 				matcher: Box::new(matcher),
// 				branches,
// 			});
// 		}
// 		if matches!(body, ExpressionOrBlock::Expression(..)) {
// 			reader.expect_next(TSXToken::Comma)?;
// 		}
// 		branches.push((type_annotation, body));
// 	}
// }
