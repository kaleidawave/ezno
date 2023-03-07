use std::borrow::Cow;

use source_map::Span;
use visitable_derive::Visitable;

use crate::{block::BlockOrSingleStatement, ASTNode, Block, Expression, TSXKeyword, TSXToken};

#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct WhileStatement {
	pub condition: Expression,
	pub inner: BlockOrSingleStatement,
	pub position: Span,
}

impl ASTNode for WhileStatement {
	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseSettings,
	) -> Result<Self, crate::ParseError> {
		let start_span = reader.expect_next(TSXToken::Keyword(TSXKeyword::While))?;
		reader.expect_next(TSXToken::OpenParentheses)?;
		let condition = Expression::from_reader(reader, state, settings)?;
		reader.expect_next(TSXToken::CloseParentheses)?;
		let inner = BlockOrSingleStatement::from_reader(reader, state, settings)?;
		Ok(Self { position: start_span.union(&inner.get_position()), condition, inner })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		buf.push_str("while");
		settings.0.add_gap(buf);
		buf.push('(');
		self.condition.to_string_from_buffer(buf, settings, depth);
		buf.push(')');
		settings.0.add_gap(buf);
		self.inner.to_string_from_buffer(buf, settings, depth + 1);
	}
}

/// TODO what about a do statement
#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct DoWhileStatement {
	pub condition: Expression,
	// TODO not sure about true here
	pub statements: Block,
	pub position: Span,
}

impl ASTNode for DoWhileStatement {
	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseSettings,
	) -> Result<Self, crate::ParseError> {
		let start_span = reader.expect_next(TSXToken::Keyword(TSXKeyword::Do))?;
		let statements = Block::from_reader(reader, state, settings)?;
		reader.expect_next(TSXToken::Keyword(TSXKeyword::While))?;
		reader.expect_next(TSXToken::OpenParentheses)?;
		let condition = Expression::from_reader(reader, state, settings)?;
		reader.expect_next(TSXToken::CloseParentheses)?;
		Ok(Self { position: start_span.union(&statements.get_position()), condition, statements })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		buf.push_str("do");
		settings.0.add_gap(buf);
		self.statements.to_string_from_buffer(buf, settings, depth + 1);
		settings.0.add_gap(buf);
		buf.push_str("while");
		settings.0.add_gap(buf);
		buf.push('(');
		self.condition.to_string_from_buffer(buf, settings, depth);
		buf.push(')');
	}
}
