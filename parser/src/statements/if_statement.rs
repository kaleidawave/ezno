use crate::{
	block::BlockOrSingleStatement, expressions::MultipleExpression, ParseOptions, TSXKeyword,
};
use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use tokenizer_lib::sized_tokens::TokenStart;
use visitable_derive::Visitable;

use super::{ASTNode, ParseResult, Span, TSXToken, Token, TokenReader};

/// A [if...else statement](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/if...else)
#[derive(Debug, Clone, PartialEq, Eq, Visitable, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct IfStatement {
	pub condition: MultipleExpression,
	pub inner: BlockOrSingleStatement,
	pub else_conditions: Vec<ConditionalElseStatement>,
	pub trailing_else: Option<UnconditionalElseStatement>,
	pub position: Span,
}

/// `... else if (...) { ... }`
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct ConditionalElseStatement {
	pub condition: MultipleExpression,
	pub inner: BlockOrSingleStatement,
	pub position: Span,
}

/// `... else { ... }`
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct UnconditionalElseStatement {
	pub inner: BlockOrSingleStatement,
	pub position: Span,
}

impl ASTNode for IfStatement {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = state.expect_keyword(reader, TSXKeyword::If)?;

		reader.expect_next(TSXToken::OpenParentheses)?;
		let condition = MultipleExpression::from_reader(reader, state, options)?;
		reader.expect_next(TSXToken::CloseParentheses)?;
		let inner = BlockOrSingleStatement::from_reader(reader, state, options)?;
		let (mut else_conditions, mut trailing_else) = (Vec::new(), None);
		while let Some(Token(TSXToken::Keyword(TSXKeyword::Else), _)) = reader.peek() {
			let Token(_, else_position) = reader.next().unwrap();
			if matches!(reader.peek(), Some(Token(TSXToken::Keyword(TSXKeyword::If), _))) {
				let value = ConditionalElseStatement::from_reader_sub_without_else(
					reader,
					state,
					options,
					else_position,
				)?;
				else_conditions.push(value);
			} else {
				let unconditional_else_statement =
					UnconditionalElseStatement::from_reader_sub_without_else(
						reader,
						state,
						options,
						else_position,
					)?;
				trailing_else = Some(unconditional_else_statement);
				break;
			}
		}
		let position = start.union(inner.get_position());
		Ok(IfStatement { condition, inner, else_conditions, trailing_else, position })
	}

	fn get_position(&self) -> &Span {
		&self.position
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str("if");
		options.add_gap(buf);
		buf.push('(');
		self.condition.to_string_from_buffer(buf, options, depth);
		buf.push(')');
		options.add_gap(buf);
		self.inner.to_string_from_buffer(buf, options, depth + 1);
		if !options.pretty
			&& matches!(self.inner, BlockOrSingleStatement::SingleStatement(_))
			&& (!self.else_conditions.is_empty() || self.trailing_else.is_some())
		{
			buf.push(';');
		}

		for (at_end, else_statement) in self.else_conditions.iter().endiate() {
			options.add_gap(buf);
			else_statement.to_string_from_buffer(buf, options, depth);
			if !options.pretty
				&& matches!(else_statement.inner, BlockOrSingleStatement::SingleStatement(_))
				&& at_end
			{
				buf.push(';');
			}
		}
		if let Some(else_statement) = &self.trailing_else {
			options.add_gap(buf);
			else_statement.to_string_from_buffer(buf, options, depth);
		}
	}
}

impl ASTNode for ConditionalElseStatement {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let else_start = state.expect_keyword(reader, TSXKeyword::Else)?;
		Self::from_reader_sub_without_else(reader, state, options, else_start)
	}

	fn get_position(&self) -> &Span {
		&self.position
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str("else if");
		options.add_gap(buf);
		buf.push('(');
		self.condition.to_string_from_buffer(buf, options, depth);
		buf.push(')');
		options.add_gap(buf);
		self.inner.to_string_from_buffer(buf, options, depth + 1);
	}
}

impl ConditionalElseStatement {
	fn from_reader_sub_without_else(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		else_position: TokenStart,
	) -> ParseResult<Self> {
		let _ = state.expect_keyword(reader, TSXKeyword::If)?;
		reader.expect_next(TSXToken::OpenParentheses)?;
		let condition = MultipleExpression::from_reader(reader, state, options)?;
		reader.expect_next(TSXToken::CloseParentheses)?;
		let statements = BlockOrSingleStatement::from_reader(reader, state, options)?;
		Ok(Self {
			condition,
			position: else_position.union(statements.get_position()),
			inner: statements,
		})
	}
}

impl ASTNode for UnconditionalElseStatement {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let else_position = state.expect_keyword(reader, TSXKeyword::Else)?;
		Self::from_reader_sub_without_else(reader, state, options, else_position)
	}

	fn get_position(&self) -> &Span {
		&self.position
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str("else");
		if !options.pretty && matches!(self.inner, BlockOrSingleStatement::SingleStatement(_)) {
			buf.push(' ');
		}
		options.add_gap(buf);
		self.inner.to_string_from_buffer(buf, options, depth + 1);
	}
}

impl UnconditionalElseStatement {
	fn from_reader_sub_without_else(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		else_position: TokenStart,
	) -> ParseResult<Self> {
		let statements = BlockOrSingleStatement::from_reader(reader, state, options)?;
		Ok(Self { position: else_position.union(statements.get_position()), inner: statements })
	}
}
