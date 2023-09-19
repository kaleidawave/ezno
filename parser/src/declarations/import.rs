use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::{Token, TokenReader};

use crate::{
	errors::parse_lexing_error, parse_bracketed, tokens::token_as_identifier, ASTNode, ParseError,
	ParseErrors, ParseOptions, ParseResult, TSXKeyword, TSXToken, VariableIdentifier,
};
use visitable_derive::Visitable;

/// TODO a few more thing needed here
#[derive(Debug, Clone, PartialEq, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct ImportDeclaration {
	pub default_import: Option<String>,
	pub imports: Option<Vec<ImportPart>>,
	pub from: String,
	pub only_type: bool,
	pub position: Span,
}

impl ASTNode for ImportDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let start_position = reader.expect_next(TSXToken::Keyword(TSXKeyword::Import))?;
		let only_type: bool =
			reader.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Type))).is_some();
		let (default_import, imports) = if matches!(
			reader.peek(),
			Some(Token(
				TSXToken::SingleQuotedStringLiteral(..) | TSXToken::DoubleQuotedStringLiteral(..),
				_
			))
		) {
			(None, None)
		} else if !matches!(reader.peek(), Some(Token(TSXToken::OpenBrace, _))) {
			let default_import =
				Some(token_as_identifier(reader.next().unwrap(), "default import")?.0);
			if !matches!(reader.peek(), Some(Token(TSXToken::Comma, _))) {
				(default_import, None)
			} else {
				(
					default_import,
					Some(
						parse_bracketed::<ImportPart>(
							reader,
							state,
							settings,
							Some(TSXToken::OpenBrace),
							TSXToken::CloseBrace,
						)?
						.0,
					),
				)
			}
		} else {
			(
				None,
				Some(
					parse_bracketed::<ImportPart>(
						reader,
						state,
						settings,
						Some(TSXToken::OpenBrace),
						TSXToken::CloseBrace,
					)?
					.0,
				),
			)
		};

		if default_import.is_some() || imports.is_some() {
			reader.expect_next(TSXToken::Keyword(TSXKeyword::From))?;
		}

		let (span, from) = match reader.next().ok_or_else(parse_lexing_error)? {
			Token(
				TSXToken::DoubleQuotedStringLiteral(from)
				| TSXToken::SingleQuotedStringLiteral(from),
				start,
			) => {
				let span = start.with_length(from.len() + 2);
				(span, from)
			}
			token => {
				let position = token.get_span();
				return Err(ParseError::new(
					ParseErrors::ExpectedStringLiteral { found: token.0 },
					position,
				));
			}
		};
		Ok(ImportDeclaration {
			default_import,
			imports,
			only_type,
			from,
			position: start_position.union(span),
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		_settings: &crate::ToStringOptions,
		_depth: u8,
	) {
		buf.push_str("import ");
		// TODO type script only
		if self.only_type {
			buf.push_str("type ");
		}
		if let Some(default_import) = &self.default_import {
			buf.push_str(default_import);
			if self.imports.is_some() {
				buf.push_str(", ");
			}
		}
		if let Some(imports) = &self.imports {
			buf.push('{');
			for (at_end, import) in imports.iter().endiate() {
				import.to_string_from_buffer(buf, _settings, _depth);
				if !at_end {
					buf.push(',');
				}
			}
			buf.push('}');
		}
		if self.default_import.is_some() || self.imports.is_some() {
			buf.push_str(" from ");
		}
		buf.push('"');
		buf.push_str(&self.from);
		buf.push('"');
	}

	fn get_position(&self) -> &Span {
		&self.position
	}
}

/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import#syntax>
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ImportPart {
	Name(VariableIdentifier),
	NameWithAlias { name: String, alias: String, position: Span },
}

impl ASTNode for ImportPart {
	fn get_position(&self) -> &Span {
		match self {
			ImportPart::Name(identifier) => identifier.get_position(),
			ImportPart::NameWithAlias { position, .. } => position,
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		_state: &mut crate::ParsingState,
		_settings: &ParseOptions,
	) -> ParseResult<Self> {
		let (name, pos) =
			token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "imported name")?;
		if let Some(Token(TSXToken::Keyword(TSXKeyword::As), _)) = reader.peek() {
			reader.next();
			let (alias, alias_pos) =
				token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "import alias")?;
			let position = pos.union(&alias_pos);
			Ok(Self::NameWithAlias { name, alias, position })
		} else {
			Ok(Self::Name(VariableIdentifier::Standard(name, pos)))
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		_settings: &crate::ToStringOptions,
		_depth: u8,
	) {
		match self {
			ImportPart::Name(identifier) => buf.push_str(identifier.as_str()),
			ImportPart::NameWithAlias { name, alias, .. } => {
				buf.push_str(name);
				buf.push_str(" as ");
				buf.push_str(alias);
			}
		}
	}
}
