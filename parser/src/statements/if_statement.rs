use crate::{
	block::BlockOrSingleStatement, derive_ASTNode, expressions::MultipleExpression, ParseOptions,
};
use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use visitable_derive::Visitable;

use super::{ASTNode, ParseResult, Span};

/// A [if...else statement](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/if...else)
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct IfStatement {
	pub condition: MultipleExpression,
	pub inner: BlockOrSingleStatement,
	pub else_conditions: Vec<ConditionalElseStatement>,
	pub trailing_else: Option<UnconditionalElseStatement>,
	pub position: Span,
}

/// `... else if (...) { ... }`
#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub struct ConditionalElseStatement {
	pub condition: MultipleExpression,
	pub inner: BlockOrSingleStatement,
	pub position: Span,
}

/// `... else { ... }`
#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub struct UnconditionalElseStatement {
	pub inner: BlockOrSingleStatement,
	pub position: Span,
}

impl ASTNode for IfStatement {
	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("if")?;

		reader.expect('(')?;
		let condition = MultipleExpression::from_reader(reader)?;
		reader.expect(')')?;

		let inner = BlockOrSingleStatement::from_reader(reader)?;
		let (mut else_conditions, mut trailing_else) =
			(Vec::<ConditionalElseStatement>::new(), None::<UnconditionalElseStatement>);

		// Doesn't use their own `ASTNode::from_reader` implementation but should be fine wrt `_advance` & peeking
		while reader.is_keyword_advance("else") {
			if reader.is_keyword_advance("if") {
				let value = reader.expect('(')?;
				let condition = MultipleExpression::from_reader(reader)?;
				reader.expect(')')?;
				let inner = BlockOrSingleStatement::from_reader(reader)?;
				let value = ConditionalElseStatement {
					condition,
					position: start.union(inner.get_position()),
					inner: inner,
				};
				else_conditions.push(value);
			} else {
				let inner = BlockOrSingleStatement::from_reader(reader)?;
				let position = start.union(inner.get_position());
				trailing_else = Some(UnconditionalElseStatement { inner, position });
				break;
			}
		}

		let position = start.union(if let Some(ref t) = trailing_else {
			t.get_position()
		} else if let Some(t) = else_conditions.last() {
			t.get_position()
		} else {
			inner.get_position()
		});

		Ok(IfStatement { condition, inner, else_conditions, trailing_else, position })
	}

	fn get_position(&self) -> Span {
		self.position
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("if");
		options.push_gap_optionally(buf);
		buf.push('(');
		self.condition.to_string_from_buffer(buf, options, local);
		buf.push(')');
		options.push_gap_optionally(buf);
		self.inner.to_string_from_buffer(buf, options, local.next_level());
		if !options.pretty
			&& matches!(self.inner, BlockOrSingleStatement::SingleStatement(_))
			&& (self.else_conditions.is_empty() || self.trailing_else.is_none())
		{
			buf.push(';');
		}

		for (at_end, else_statement) in self.else_conditions.iter().endiate() {
			options.push_gap_optionally(buf);
			else_statement.to_string_from_buffer(buf, options, local);
			if !options.pretty
				&& matches!(else_statement.inner, BlockOrSingleStatement::SingleStatement(_))
				&& at_end
			{
				buf.push(';');
			}
		}
		if let Some(else_statement) = &self.trailing_else {
			options.push_gap_optionally(buf);
			else_statement.to_string_from_buffer(buf, options, local);
		}
	}
}

impl ASTNode for ConditionalElseStatement {
	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("else")?;
		reader.expect_keyword("if")?;
		reader.expect('(')?;
		let condition = MultipleExpression::from_reader(reader)?;
		reader.expect(')')?;
		let statements = BlockOrSingleStatement::from_reader(reader)?;
		Ok(Self { condition, position: start.union(statements.get_position()), inner: statements })
	}

	fn get_position(&self) -> Span {
		self.position
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("else if");
		options.push_gap_optionally(buf);
		buf.push('(');
		self.condition.to_string_from_buffer(buf, options, local);
		buf.push(')');
		options.push_gap_optionally(buf);
		self.inner.to_string_from_buffer(buf, options, local.next_level());
	}
}

impl ASTNode for UnconditionalElseStatement {
	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("else")?;
		let statements = BlockOrSingleStatement::from_reader(reader)?;
		Ok(Self { position: start.union(statements.get_position()), inner: statements })
	}

	fn get_position(&self) -> Span {
		self.position
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("else");
		if !options.pretty && matches!(self.inner, BlockOrSingleStatement::SingleStatement(_)) {
			buf.push(' ');
		}
		options.push_gap_optionally(buf);
		self.inner.to_string_from_buffer(buf, options, local.next_level());
	}
}
