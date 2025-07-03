use crate::{
	block::BlockOrSingleStatement, derive_ASTNode, expressions::MultipleExpression, ASTNode,
	ParseResult, Span,
};
use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use visitable_derive::Visitable;

/// A [if...else statement](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/if...else)
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct IfStatement {
	pub condition: Box<MultipleExpression>,
	pub inner: BlockOrSingleStatement,
	pub else_conditions: Vec<ConditionalElseStatement>,
	pub trailing_else: Option<UnconditionalElseStatement>,
	pub position: Span,
}

/// `... else if (...) { ... }`
#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub struct ConditionalElseStatement {
	pub condition: Box<MultipleExpression>,
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
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("if")?;

		reader.expect('(')?;
		let condition = MultipleExpression::from_reader(reader).map(Box::new)?;
		reader.expect(')')?;

		let inner = BlockOrSingleStatement::from_reader(reader)?;

		let (mut else_conditions, mut trailing_else) =
			(Vec::<ConditionalElseStatement>::new(), None::<UnconditionalElseStatement>);

		while reader.is_keyword("else") || reader.is_one_of(&["//", "/*"]).is_some() {
			if reader.is_one_of(&["//", "/*"]).is_some() {
				let is_multiline = reader.starts_with_slice("/*");
				reader.advance(2);
				let _content = reader.parse_comment_literal(is_multiline)?;
				continue;
			}

			// TODO doesn't use `ConditionalElseStatement` or `UnconditionalElseStatement`, `ASTNode::from_reader` implementations
			reader.advance("else".len() as u32);
			if reader.is_keyword_advance("if") {
				let _value = reader.expect('(')?;
				let condition = MultipleExpression::from_reader(reader).map(Box::new)?;
				reader.expect(')')?;
				let inner = BlockOrSingleStatement::from_reader(reader)?;
				let value = ConditionalElseStatement {
					condition,
					position: start.union(inner.get_position()),
					inner,
				};
				else_conditions.push(value);
			} else {
				let inner = BlockOrSingleStatement::from_reader(reader)?;
				let position = start.union(inner.get_position());
				trailing_else = Some(UnconditionalElseStatement { inner, position });
				break;
			}
		}

		let position = start.union(reader.get_end());

		Ok(IfStatement { condition, inner, else_conditions, trailing_else, position })
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
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("else")?;
		reader.expect_keyword("if")?;
		reader.expect('(')?;
		let condition = MultipleExpression::from_reader(reader).map(Box::new)?;
		reader.expect(')')?;
		let statements = BlockOrSingleStatement::from_reader(reader)?;
		Ok(Self { condition, position: start.union(statements.get_position()), inner: statements })
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
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("else")?;
		let statements = BlockOrSingleStatement::from_reader(reader)?;
		Ok(Self { position: start.union(statements.get_position()), inner: statements })
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
