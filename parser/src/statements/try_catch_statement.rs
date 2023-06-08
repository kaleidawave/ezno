use std::borrow::Cow;

use crate::{ASTNode, Block, TSXKeyword, TSXToken, VariableIdentifier};
use source_map::Span;
use visitable_derive::Visitable;

#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct TryCatchStatement {
	pub inner: Block, // both try and catch blocks must always be braced
	pub error_var: VariableIdentifier,
	pub catch_inner: Block,
	pub position: Span,
}

impl ASTNode for TryCatchStatement {
	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseSettings,
	) -> Result<Self, crate::ParseError> {
		let start_span = reader.expect_next(TSXToken::Keyword(TSXKeyword::Try))?;
		let inner = Block::from_reader(reader, state, settings)?;
		reader.expect_next(TSXToken::Keyword(TSXKeyword::Catch))?;
		reader.expect_next(TSXToken::OpenParentheses)?;
		let error_var = VariableIdentifier::from_reader(reader, state, settings)?;
		reader.expect_next(TSXToken::CloseParentheses)?;
		let catch_inner = Block::from_reader(reader, state, settings)?;
		Ok(Self {
			position: start_span.union(&catch_inner.get_position()),
			inner,
			error_var,
			catch_inner,
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		buf.push_str("try");
		settings.add_gap(buf);
		self.inner.to_string_from_buffer(buf, settings, depth + 1);
		settings.add_gap(buf);
		buf.push_str("catch");
		settings.add_gap(buf);
		buf.push('(');
		self.error_var.to_string_from_buffer(buf, settings, depth);
		buf.push(')');
		settings.add_gap(buf);
		self.catch_inner.to_string_from_buffer(buf, settings, depth + 1);
	}
}
