use source_map::Span;
use visitable_derive::Visitable;

use crate::{ast::MultipleExpression, block::BlockOrSingleStatement, derive_ASTNode, ASTNode};

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct WhileStatement {
	pub condition: Box<MultipleExpression>,
	pub inner: BlockOrSingleStatement,
	pub position: Span,
}

impl ASTNode for WhileStatement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> Result<Self, crate::ParseError> {
		let start = reader.expect_keyword("while")?;
		reader.expect('(')?;
		let condition = MultipleExpression::from_reader(reader).map(Box::new)?;
		reader.expect(')')?;
		let inner = BlockOrSingleStatement::from_reader(reader)?;
		Ok(Self { position: start.union(inner.get_position()), condition, inner })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("while");
		options.push_gap_optionally(buf);
		buf.push('(');
		self.condition.to_string_from_buffer(buf, options, local);
		buf.push(')');
		options.push_gap_optionally(buf);
		self.inner.to_string_from_buffer(buf, options, local.next_level());
	}
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct DoWhileStatement {
	pub inner: BlockOrSingleStatement,
	pub condition: Box<MultipleExpression>,
	pub position: Span,
}

impl ASTNode for DoWhileStatement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> Result<Self, crate::ParseError> {
		let start = reader.expect_keyword("do")?;
		let inner = BlockOrSingleStatement::from_reader(reader)?;
		let _ = reader.expect_keyword("while")?;
		let _ = reader.expect('(')?;
		let condition = MultipleExpression::from_reader(reader).map(Box::new)?;
		let position = start.union(reader.expect(')')?);
		Ok(Self { inner, condition, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("do ");
		self.inner.to_string_from_buffer(buf, options, local.next_level());
		buf.push_str(" while");
		options.push_gap_optionally(buf);
		buf.push('(');
		self.condition.to_string_from_buffer(buf, options, local);
		buf.push(')');
	}
}
