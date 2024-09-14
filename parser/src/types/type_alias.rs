use source_map::Span;

use crate::{
	bracketed_items_to_string, derive_ASTNode, ASTNode, ExpressionOrStatementPosition,
	StatementPosition, TypeAnnotation, TypeParameter,
};

/// e.g. `type NumberArray = Array<number>`
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct TypeAlias {
	pub name: StatementPosition,
	pub parameters: Option<Vec<TypeParameter>>,
	pub references: TypeAnnotation,
	pub position: Span,
}

impl ASTNode for TypeAlias {
	fn from_reader(reader: &mut crate::new::Lexer) -> crate::ParseResult<Self> {
		let start = reader.get_start();
		let _ = reader.expect_keyword("type")?;
		let name = StatementPosition::from_reader(reader)?;
		let parameters = if reader.is_operator_advance("<") {
			let (params, _) = crate::bracketed_items_from_reader(reader, ">")?;
			Some(params)
		} else {
			None
		};

		reader.expect('=')?;
		let references = TypeAnnotation::from_reader(reader)?;
		let position = start.union(references.get_position());

		Ok(Self { name, parameters, references, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.include_type_annotations {
			if self.name.is_declare() {
				buf.push_str("declare ");
			}
			buf.push_str("type ");
			self.name.identifier.to_string_from_buffer(buf, options, local);
			if let Some(type_parameters) = &self.parameters {
				bracketed_items_to_string(type_parameters, ('<', '>'), buf, options, local);
			}
			buf.push_str(" = ");
			self.references.to_string_from_buffer(buf, options, local);
		}
	}

	fn get_position(&self) -> Span {
		self.position
	}
}
