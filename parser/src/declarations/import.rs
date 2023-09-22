use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::{Token, TokenReader};

use crate::{
	errors::parse_lexing_error, parse_bracketed, tokens::token_as_identifier, ASTNode, ParseError,
	ParseErrors, ParseOptions, ParseResult, TSXKeyword, TSXToken, VariableIdentifier,
};
use visitable_derive::Visitable;

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ImportKind {
	Parts(Vec<ImportExportPart>),
	All { under: String },
	SideEffect,
}

/// TODO a few more thing needed here
#[derive(Debug, Clone, PartialEq, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct ImportDeclaration {
	pub default: Option<String>,
	pub kind: ImportKind,
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

		let peek = reader.peek();
		let default = if !matches!(
			peek,
			Some(Token(
				TSXToken::OpenBrace
					| TSXToken::Multiply | TSXToken::SingleQuotedStringLiteral(_)
					| TSXToken::DoubleQuotedStringLiteral(_),
				_
			))
		) {
			let default = token_as_identifier(reader.next().unwrap(), "default import")?.0;
			Some((default, !matches!(reader.peek(), Some(Token(TSXToken::Comma, _)))))
		} else if matches!(peek, Some(Token(t, _ )) if t.is_string_literal()) {
			let Token(
				TSXToken::SingleQuotedStringLiteral(from)
				| TSXToken::DoubleQuotedStringLiteral(from),
				pos,
			) = reader.next().unwrap()
			else {
				unreachable!()
			};
			return Ok(ImportDeclaration {
				position: start_position.union(pos.get_end_after(from.len() + 2)),
				default: None,
				kind: ImportKind::SideEffect,
				only_type: false,
				from,
			});
		} else {
			None
		};

		let kind = if default.as_ref().map(|(_, no_comma)| *no_comma).unwrap_or_default() {
			// From default keyword
			ImportKind::Parts(Vec::new())
		} else if matches!(reader.peek(), Some(Token(TSXToken::Multiply, _))) {
			reader.next();
			let _as = reader.expect_next(TSXToken::Keyword(TSXKeyword::As))?;
			let under = token_as_identifier(reader.next().unwrap(), "import alias")?.0;
			ImportKind::All { under }
		} else {
			let parts = parse_bracketed::<ImportExportPart>(
				reader,
				state,
				settings,
				Some(TSXToken::OpenBrace),
				TSXToken::CloseBrace,
			)?
			.0;
			ImportKind::Parts(parts)
		};

		reader.expect_next(TSXToken::Keyword(TSXKeyword::From))?;

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
			default: default.map(|(left, _)| left),
			kind,
			only_type,
			from,
			position: start_position.union(span),
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str("import ");
		// TODO type script only
		if self.only_type {
			buf.push_str("type ");
		}

		if let Some(ref default) = self.default {
			buf.push_str(default);
			buf.push(' ')
		}

		match self.kind {
			ImportKind::All { ref under } => {
				if self.default.is_some() {
					buf.push_str(", ");
				}
				buf.push_str("* as ");
				buf.push_str(&under);
			}
			ImportKind::SideEffect => {
				buf.push('"');
				buf.push_str(&self.from);
				buf.push('"');
				return;
			}
			ImportKind::Parts(ref parts) => {
				if !parts.is_empty() {
					if self.default.is_some() {
						buf.push_str(", ");
					}
					buf.push('{');
					for (at_end, part) in parts.iter().endiate() {
						part.to_string_from_buffer(buf, settings, depth);
						if !at_end {
							buf.push(',');
						}
					}
					buf.push('}');
				}
			}
		}
		buf.push_str("from \"");
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
pub enum ImportExportPart {
	Name(VariableIdentifier),
	NameWithAlias { name: String, alias: String, position: Span },
}

impl ASTNode for ImportExportPart {
	fn get_position(&self) -> &Span {
		match self {
			ImportExportPart::Name(identifier) => identifier.get_position(),
			ImportExportPart::NameWithAlias { position, .. } => position,
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
			ImportExportPart::Name(identifier) => buf.push_str(identifier.as_str()),
			ImportExportPart::NameWithAlias { name, alias, .. } => {
				buf.push_str(name);
				buf.push_str(" as ");
				buf.push_str(alias);
			}
		}
	}
}
