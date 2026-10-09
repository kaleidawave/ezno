use crate::{Span, derive_ASTNode};
use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct WithStatement {
	pub expression: Box<crate::MultipleExpression>,
	pub inner: crate::block::BlockOrSingleStatement,
	pub position: Span,
}

impl crate::ASTNode for WithStatement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> crate::ParseResult<Self> {
		let start = reader.expect_keyword("with")?;
		reader.expect_operator("(")?;
		let expression = crate::MultipleExpression::from_reader(reader).map(Box::new)?;
		reader.expect_operator(")")?;
		reader.start_variable_region();
		let inner = crate::block::BlockOrSingleStatement::from_reader(reader)?;

		// WIP
		if let crate::block::BlockOrSingleStatement::SingleStatement(ref statement) = inner
			&& let crate::StatementOrDeclaration::Function(ref func) = statement.0
		{
			return Err(crate::ParseError::new(
				crate::ParseErrors::TODO("function with"),
				func.get_position(),
			));
		}

		reader.end_block();
		let position = start.union(reader.get_end());
		Ok(Self { expression, inner, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("with (");
		self.expression.to_string_from_buffer(buf, options, local);
		buf.push_str(")");
		self.inner.to_string_from_buffer(buf, options, local);
	}
}
