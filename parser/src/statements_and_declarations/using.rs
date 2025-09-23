use crate::{derive_ASTNode, Expression, Span};
use iterator_endiate::EndiateIteratorExt;
use visitable_derive::Visitable;

/// [See](https://github.com/tc39/proposal-explicit-resource-management?tab=readme-ov-file#syntax)
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct UsingDeclaration {
	pub is_await: bool,
	pub bindings: Vec<(String, Expression)>,
	pub position: Span,
}

impl crate::ASTNode for UsingDeclaration {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> crate::ParseResult<Self> {
		let is_await = reader.is_operator_advance("await");
		let start = reader.expect_keyword("using")?;
		let mut bindings = Vec::new();
		loop {
			let identifier = reader.parse_identifier("using binding", false)?.to_owned();
			reader.expect_operator("=")?;
			let expression = Expression::from_reader(reader)?;
			bindings.push((identifier, expression));
			if !reader.is_operator_advance(",") {
				break;
			}
		}
		let position = start.union(reader.get_end());
		Ok(Self { is_await, bindings, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if self.is_await {
			buf.push_str("await ");
		}
		buf.push_str("using ");
		for (not_at_end, (name, expression)) in self.bindings.iter().nendiate() {
			if not_at_end {
				buf.push_str(",");
				options.push_gap_optionally(buf);
			}
			buf.push_str(name);
			options.push_gap_optionally(buf);
			buf.push_str("=");
			options.push_gap_optionally(buf);
			expression.to_string_from_buffer(buf, options, local);
		}
	}
}
