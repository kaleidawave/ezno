use std::borrow::Cow;

use crate::{
	errors::parse_lexing_error, parse_bracketed, to_string_bracketed, tokens::token_as_identifier,
	ASTNode, ParseResult, ParseSettings, Span, TSXKeyword, TSXToken, TypeReference,
};
use tokenizer_lib::{Token, TokenReader};

/// Similar to type reference but no unions or intersections AND includes generic constraints.
/// Used for declaring classes, interfaces and functions
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct TypeDeclaration {
	pub name: String,
	pub type_parameters: Option<Vec<GenericTypeConstraint>>,
	pub position: Span,
}

impl ASTNode for TypeDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
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
				parse_bracketed(reader, state, settings, None, TSXToken::CloseChevron)
					.map(|(params, _)| params)
			})
			.transpose()?;

		// TODO modify position?
		Ok(Self { name, position, type_parameters })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		buf.push_str(&self.name);
		if let Some(ref type_parameters) = self.type_parameters {
			to_string_bracketed(type_parameters, ('<', '>'), buf, settings, depth)
		}
	}

	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}
}

/// Represents a generic parameter. Can have default or constraint to extend a type or a key of a type
///
/// TODO is default and extends mut ex
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum GenericTypeConstraint {
	Parameter { name: String, default: Option<TypeReference> },
	Extends(String, TypeReference),
	ExtendsKeyOf(String, TypeReference),
	// TODO this should go
	Spread { name: String, default: Option<TypeReference> },
}

impl GenericTypeConstraint {
	pub fn name(&self) -> &str {
		match self {
			GenericTypeConstraint::Parameter { name, .. }
			| GenericTypeConstraint::Extends(name, _)
			| GenericTypeConstraint::ExtendsKeyOf(name, _)
			| GenericTypeConstraint::Spread { name, .. } => name,
		}
	}
}

impl ASTNode for GenericTypeConstraint {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
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
				let extends_type =
					TypeReference::from_reader_with_config(reader, state, settings, false)?;
				if key_of {
					Ok(Self::ExtendsKeyOf(name, extends_type))
				} else {
					Ok(Self::Extends(name, extends_type))
				}
			}
			Some(Token(TSXToken::Assign, _)) => {
				reader.next();
				let default_type =
					TypeReference::from_reader_with_config(reader, state, settings, false)?;
				Ok(Self::Parameter { name, default: Some(default_type) })
			}
			_ => Ok(Self::Parameter { name, default: None }),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		match self {
			GenericTypeConstraint::Parameter { name, default } => {
				buf.push_str(name);
				if let Some(default) = default {
					buf.push('=');
					default.to_string_from_buffer(buf, settings, depth);
				}
			}
			GenericTypeConstraint::Extends(name, extends) => {
				buf.push_str(name);
				buf.push_str(" extends ");
				extends.to_string_from_buffer(buf, settings, depth);
			}
			GenericTypeConstraint::ExtendsKeyOf(name, extends_key_of) => {
				buf.push_str(name);
				buf.push_str(" extends keyof ");
				extends_key_of.to_string_from_buffer(buf, settings, depth);
			}
			GenericTypeConstraint::Spread { name, default } => {
				buf.push_str("...");
				buf.push_str(name);
				if let Some(default) = default {
					buf.push('=');
					default.to_string_from_buffer(buf, settings, depth);
				}
			}
		}
	}

	fn get_position(&self) -> Cow<Span> {
		todo!()
	}
}
