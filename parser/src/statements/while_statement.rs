use source_map::Span;
use visitable_derive::Visitable;

use crate::{
	ast::MultipleExpression, block::BlockOrSingleStatement, derive_ASTNode, ASTNode, TSXKeyword,
	TSXToken,
};

#[apply(derive_ASTNode)]
#[derive(Debug, PartialEq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct WhileStatement {
	pub condition: MultipleExpression,
	pub inner: BlockOrSingleStatement,
	pub position: Span,
}

impl ASTNode for WhileStatement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> Result<Self, crate::ParseError> {
		let _existing = r#"let start = state.expect_keyword(reader, TSXKeyword::While)?;
		reader.expect_next(TSXToken::OpenParentheses)?;
		let condition = MultipleExpression::from_reader(reader, state, options)?;
		reader.expect_next(TSXToken::CloseParentheses)?;
		let inner = BlockOrSingleStatement::from_reader(reader, state, options)?;
		Ok(Self { position: start.union(inner.get_position()), condition, inner })"#;
		todo!();
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
#[derive(Debug, PartialEq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct DoWhileStatement {
	pub condition: MultipleExpression,
	// TODO unsure about true here
	pub inner: BlockOrSingleStatement,
	pub position: Span,
}

impl ASTNode for DoWhileStatement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> Result<Self, crate::ParseError> {
		let _existing = r#"let start = state.expect_keyword(reader, TSXKeyword::Do)?;
		let inner = BlockOrSingleStatement::from_reader(reader, state, options)?;
		let _ = state.expect_keyword(reader, TSXKeyword::While)?;
		reader.expect_next(TSXToken::OpenParentheses)?;
		let condition = MultipleExpression::from_reader(reader, state, options)?;
		let position =
			start.union(reader.expect_next(TSXToken::CloseParentheses)?.get_end_after(1));
		Ok(Self { condition, inner, position })"#;
		todo!();
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("do ");
		self.inner.to_string_from_buffer(buf, options, local);
		buf.push_str(" while");
		options.push_gap_optionally(buf);
		buf.push('(');
		self.condition.to_string_from_buffer(buf, options, local);
		buf.push(')');
	}
}
