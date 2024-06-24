use source_map::Span;

use crate::{
	derive_ASTNode, to_string_bracketed, ASTNode, ExpressionOrStatementPosition, StatementPosition,
	TSXToken, TypeAnnotation, TypeParameter,
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
	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> crate::ParseResult<Self> {
		let start = state.expect_keyword(reader, crate::TSXKeyword::Type)?;
		let name = StatementPosition::from_reader(reader, state, options)?;
		let parameters = reader
			.conditional_next(|token| *token == TSXToken::OpenChevron)
			.is_some()
			.then(|| {
				crate::parse_bracketed(reader, state, options, None, TSXToken::CloseChevron)
					.map(|(params, _, _)| params)
			})
			.transpose()?;

		reader.expect_next(TSXToken::Assign)?;
		let references = TypeAnnotation::from_reader(reader, state, options)?;
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
				to_string_bracketed(type_parameters, ('<', '>'), buf, options, local);
			}
			buf.push_str(" = ");
			self.references.to_string_from_buffer(buf, options, local);
		}
	}

	fn get_position(&self) -> Span {
		self.position
	}
}
