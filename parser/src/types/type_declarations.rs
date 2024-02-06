use crate::{
	default_derive_bundle, errors::parse_lexing_error, parse_bracketed, to_string_bracketed,
	tokens::token_as_identifier, ASTNode, ListItem, ParseOptions, ParseResult, Span, TSXKeyword,
	TSXToken, TypeAnnotation,
};
use tokenizer_lib::{Token, TokenReader};

/// Similar to type reference but no unions or intersections AND includes generic constraints.
/// Used for declaring classes, interfaces and functions
#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(default_derive_bundle)]
pub struct TypeDeclaration {
	pub name: String,
	pub type_parameters: Option<Vec<GenericTypeConstraint>>,
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
#[apply(default_derive_bundle)]
pub enum GenericTypeConstraint {
	Parameter { name: String, default: Option<TypeAnnotation> },
	Extends(String, TypeAnnotation),
	ExtendsKeyOf(String, TypeAnnotation),
	// TODO this should go
	Spread { name: String, default: Option<TypeAnnotation> },
}

impl GenericTypeConstraint {
	#[must_use]
	pub fn name(&self) -> &str {
		match self {
			GenericTypeConstraint::Parameter { name, .. }
			| GenericTypeConstraint::Extends(name, _)
			| GenericTypeConstraint::ExtendsKeyOf(name, _)
			| GenericTypeConstraint::Spread { name, .. } => name,
		}
	}
}

impl ListItem for GenericTypeConstraint {}

impl ASTNode for GenericTypeConstraint {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		// Get name:
		let token = reader.next().ok_or_else(parse_lexing_error)?;
		let (name, _pos) = token_as_identifier(token, "generic constraint name")?;
		match reader.peek() {
			Some(Token(TSXToken::Keyword(TSXKeyword::Extends), _)) => {
				reader.next();
				let key_of = reader
					.conditional_next(|token| *token == TSXToken::Keyword(TSXKeyword::KeyOf))
					.is_some();
				let extends_type = TypeAnnotation::from_reader_with_config(
					reader, state, options, false, false, None,
				)?;
				if key_of {
					Ok(Self::ExtendsKeyOf(name, extends_type))
				} else {
					Ok(Self::Extends(name, extends_type))
				}
			}
			Some(Token(TSXToken::Assign, _)) => {
				reader.next();
				let default_type = TypeAnnotation::from_reader_with_config(
					reader, state, options, false, false, None,
				)?;
				Ok(Self::Parameter { name, default: Some(default_type) })
			}
			_ => Ok(Self::Parameter { name, default: None }),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			GenericTypeConstraint::Parameter { name, default } => {
				buf.push_str(name);
				if let Some(default) = default {
					buf.push('=');
					default.to_string_from_buffer(buf, options, local);
				}
			}
			GenericTypeConstraint::Extends(name, extends) => {
				buf.push_str(name);
				buf.push_str(" extends ");
				extends.to_string_from_buffer(buf, options, local);
			}
			GenericTypeConstraint::ExtendsKeyOf(name, extends_key_of) => {
				buf.push_str(name);
				buf.push_str(" extends keyof ");
				extends_key_of.to_string_from_buffer(buf, options, local);
			}
			GenericTypeConstraint::Spread { name, default } => {
				buf.push_str("...");
				buf.push_str(name);
				if let Some(default) = default {
					buf.push('=');
					default.to_string_from_buffer(buf, options, local);
				}
			}
		}
	}

	fn get_position(&self) -> &Span {
		todo!()
	}
}
