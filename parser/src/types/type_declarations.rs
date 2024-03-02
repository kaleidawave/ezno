use crate::{
	derive_ASTNode, errors::parse_lexing_error, parse_bracketed, to_string_bracketed,
	tokens::token_as_identifier, ASTNode, ListItem, ParseOptions, ParseResult, Span, TSXKeyword,
	TSXToken, TypeAnnotation,
};
use tokenizer_lib::TokenReader;

/// Similar to type reference but no unions or intersections AND includes generic constraints.
/// Used for declaring classes, interfaces and functions
#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub struct TypeDeclaration {
	pub name: String,
	pub type_parameters: Option<Vec<TypeParameter>>,
	pub position: Span,
}

impl ASTNode for TypeDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		// Get initial name
		let (name, position) = token_as_identifier(
			reader.next().ok_or_else(parse_lexing_error)?,
			"type declaration name",
		)?;

		let type_parameters = reader
			.conditional_next(|token| *token == TSXToken::OpenChevron)
			.is_some()
			.then(|| {
				parse_bracketed(reader, state, options, None, TSXToken::CloseChevron)
					.map(|(params, _)| params)
			})
			.transpose()?;
		Ok(Self { name, type_parameters, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str(&self.name);
		if let Some(ref type_parameters) = self.type_parameters {
			to_string_bracketed(type_parameters, ('<', '>'), buf, options, local);
		}
	}

	fn get_position(&self) -> &Span {
		&self.position
	}
}

/// Represents a generic parameter. Can have default or constraint to extend a type or a key of a type
///
/// TODO is default and extends mut ex
#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub struct TypeParameter {
	pub name: String,
	pub default: Option<TypeAnnotation>,
	pub extends: Option<TypeAnnotation>,
	pub position: Span,
	#[cfg(feature = "full-typescript")]
	pub is_constant: bool,
}

impl ListItem for TypeParameter {}

impl ASTNode for TypeParameter {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		#[cfg(feature = "full-typescript")]
		let is_constant = reader
			.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Const)))
			.is_some();

		let token = reader.next().ok_or_else(parse_lexing_error)?;
		let (name, pos) = token_as_identifier(token, "type parameter name")?;

		let extends = reader
			.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Extends)))
			.is_some()
			.then(|| TypeAnnotation::from_reader(reader, state, options))
			.transpose()?;

		let default = reader
			.conditional_next(|t| matches!(t, TSXToken::Assign))
			.is_some()
			.then(|| TypeAnnotation::from_reader(reader, state, options))
			.transpose()?;

		let position = pos.get_start().union(
			default
				.as_ref()
				.or(extends.as_ref())
				.map_or(pos.get_end(), |ta| ta.get_position().get_end()),
		);

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
			extends.to_string_from_buffer(buf, options, local);
		}
		if let Some(ref default) = self.default {
			default.to_string_from_buffer(buf, options, local);
		}
	}

	fn get_position(&self) -> &Span {
		&self.position
	}
}
