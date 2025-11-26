use crate::{Expression, Span, TypeAnnotation, derive_ASTNode};
use iterator_endiate::EndiateIteratorExt;
use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable)]
pub struct UsingBinding {
	pub name: String,
	pub annotation: Option<TypeAnnotation>,
	pub value: Expression,
}

/// [See](https://github.com/tc39/proposal-explicit-resource-management?tab=readme-ov-file#syntax)
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct UsingDeclaration {
	pub is_await: bool,
	pub bindings: Vec<UsingBinding>,
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
			let name = reader.parse_identifier("using name", false)?.into_owned();
			let annotation = if reader.is_operator_advance(":") {
				Some(TypeAnnotation::from_reader(reader)?)
			} else {
				None
			};
			reader.expect_operator("=")?;
			let value = Expression::from_reader(reader)?;
			let binding = UsingBinding { name, annotation, value };
			bindings.push(binding);
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
		for (not_at_end, binding) in self.bindings.iter().nendiate() {
			if not_at_end {
				buf.push_str(",");
				options.push_gap_optionally(buf);
			}
			buf.push_str(&binding.name);
			if let Some(ref annotation) = binding.annotation
				&& options.include_type_annotations
			{
				buf.push_str(":");
				annotation.to_string_from_buffer(buf, options, local);
			}
			options.push_gap_optionally(buf);
			buf.push_str("=");
			options.push_gap_optionally(buf);
			binding.value.to_string_from_buffer(buf, options, local);
		}
	}
}
