use std::{
	borrow::Cow,
	sync::atomic::{AtomicU16, Ordering},
};

use derive_debug_extras::DebugExtras;
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::{Token, TokenReader};

use crate::{
	errors::parse_lexing_error, parse_bracketed, tokens::token_as_identifier, ASTNode, ParseError,
	ParseErrors, ParseResult, ParseSettings, TSXKeyword, TSXToken, VariableId, VariableIdentifier,
};
use visitable_derive::Visitable;

static IMPORT_STATEMENT_ID_COUNTER: AtomicU16 = AtomicU16::new(1);

#[derive(PartialEq, Eq, Clone, Copy, DebugExtras, Hash)]
pub struct ImportStatementId(u16);

impl ImportStatementId {
	pub fn new() -> Self {
		Self(IMPORT_STATEMENT_ID_COUNTER.fetch_add(1, Ordering::SeqCst))
	}
}

// TODO not sure
#[cfg(feature = "self-rust-tokenize")]
impl self_rust_tokenize::SelfRustTokenize for ImportStatementId {
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		token_stream.extend(self_rust_tokenize::quote!(ImportStatementId::new()))
	}
}

/// TODO a few more thing needed here
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct ImportDeclaration {
	pub default_import: Option<String>,
	pub imports: Option<Vec<ImportPart>>,
	pub import_statement_id: ImportStatementId,
	pub from: String,
	pub only_type: bool,
	pub position: Span,
}

impl ASTNode for ImportDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
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

		let (from, end_position) = match reader.next().ok_or_else(parse_lexing_error)? {
			Token(
				TSXToken::DoubleQuotedStringLiteral(expression)
				| TSXToken::SingleQuotedStringLiteral(expression),
				end_position,
			) => (expression, end_position),
			Token(token, position) => {
				return Err(ParseError::new(
					ParseErrors::ExpectedStringLiteral { found: token },
					position,
				));
			}
		};
		Ok(ImportDeclaration {
			default_import,
			imports,
			only_type,
			from,
			import_statement_id: ImportStatementId::new(),
			position: start_position.union(&end_position),
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		_settings: &crate::ToStringSettingsAndData,
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

	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}
}

/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import#syntax>
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum ImportPart {
	Name(VariableIdentifier),
	NameWithAlias { name: String, alias: String, variable_id: VariableId, position: Span },
}

impl ASTNode for ImportPart {
	fn get_position(&self) -> Cow<Span> {
		match self {
			ImportPart::Name(identifier) => identifier.get_position(),
			ImportPart::NameWithAlias { position, .. } => Cow::Borrowed(position),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		_state: &mut crate::ParsingState,
		_settings: &ParseSettings,
	) -> ParseResult<Self> {
		let (name, pos) =
			token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "imported name")?;
		if let Some(Token(TSXToken::Keyword(TSXKeyword::As), _)) = reader.peek() {
			reader.next();
			let (alias, alias_pos) =
				token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "import alias")?;
			let position = pos.union(&alias_pos);
			Ok(Self::NameWithAlias { name, alias, position, variable_id: VariableId::new() })
		} else {
			Ok(Self::Name(VariableIdentifier::Standard(name, VariableId::new(), pos)))
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		_settings: &crate::ToStringSettingsAndData,
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
