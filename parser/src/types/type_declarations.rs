use crate::{derive_ASTNode, ASTNode, ListItem, ParseResult, Span, TypeAnnotation};

/// Represents a generic parameter. Can have default or constraint to extend a type or a key of a type
///
/// TODO is default and extends are mut ex
#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub struct TypeParameter {
	pub name: String,
	pub default: Option<TypeAnnotation>,
	pub extends: Option<TypeAnnotation>,
	pub position: Span,
	#[cfg(feature = "full-typescript")]
	pub is_constant: bool,
}

impl ListItem for TypeParameter {
	type LAST = ();
}

impl ASTNode for TypeParameter {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		#[cfg(feature = "full-typescript")]
		let is_constant = reader.is_keyword_advance("const");

		let start = reader.get_start();
		let name = reader.parse_identifier("type parameter name", false)?.to_owned();

		let extends = reader
			.is_keyword_advance("extends")
			.then(|| TypeAnnotation::from_reader(reader))
			.transpose()?;

		let default = reader
			.is_operator_advance("=")
			.then(|| TypeAnnotation::from_reader(reader))
			.transpose()?;

		let position = start.union(reader.get_end());

		Ok(Self {
			name,
			default,
			extends,
			position,
			#[cfg(feature = "full-typescript")]
			is_constant,
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str(&self.name);
		if let Some(ref extends) = self.extends {
			buf.push_str(" extends ");
			extends.to_string_from_buffer(buf, options, local);
		}
		if let Some(ref default) = self.default {
			buf.push_str(" = ");
			default.to_string_from_buffer(buf, options, local);
		}
	}
}
