use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use source_map::{End, Span};
use tokenizer_lib::{sized_tokens::TokenStart, Token, TokenReader};

use crate::{
	errors::parse_lexing_error, parse_bracketed, tokens::token_as_identifier, tsx_keywords,
	ASTNode, Keyword, ParseError, ParseErrors, ParseOptions, ParseResult, Quoted, TSXKeyword,
	TSXToken, VariableIdentifier,
};
use visitable_derive::Visitable;

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ImportKind {
	Parts(Vec<ImportPart>),
	All { under: VariableIdentifier },
	SideEffect,
}

/// TODO a few more thing needed here
#[derive(Debug, Clone, PartialEq, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct ImportDeclaration {
	pub type_keyword: Option<Keyword<tsx_keywords::Type>>,
	pub default: Option<VariableIdentifier>,
	pub kind: ImportKind,
	pub from: String,
	pub position: Span,
}

/// TODO default
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ImportExportName {
	Reference(String),
	Quoted(String, Quoted),
}

impl ImportExportName {
	pub(crate) fn from_token(token: Token<TSXToken, TokenStart>) -> ParseResult<(Self, End)> {
		if let TSXToken::DoubleQuotedStringLiteral(_) | TSXToken::SingleQuotedStringLiteral(_) =
			token.0
		{
			let (start, alias, quoted) = match token {
				Token(TSXToken::SingleQuotedStringLiteral(content), start) => {
					(start, content, Quoted::Single)
				}
				Token(TSXToken::DoubleQuotedStringLiteral(content), start) => {
					(start, content, Quoted::Double)
				}
				_ => unreachable!(),
			};
			let with_length = start.get_end_after(alias.len() + 1);
			Ok((ImportExportName::Quoted(alias, quoted), with_length))
		} else {
			let (ident, pos) = token_as_identifier(token, "import alias")?;
			Ok((ImportExportName::Reference(ident), pos.get_end()))
		}
	}
}

impl ASTNode for ImportDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start_position = reader.expect_next(TSXToken::Keyword(TSXKeyword::Import))?;
		let type_keyword = reader
			.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Type)))
			.map(|tok| Keyword::new(tok.get_span()));

		let peek = reader.peek();
		let default = if let Some(Token(TSXToken::OpenBrace | TSXToken::Multiply, _)) = peek {
			None
		} else if let Some(Token(
			TSXToken::DoubleQuotedStringLiteral(_) | TSXToken::SingleQuotedStringLiteral(_),
			_,
		)) = peek
		{
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
				type_keyword: None,
				from,
			});
		} else {
			let default_identifier = VariableIdentifier::from_reader(reader, state, options)?;
			if !matches!(reader.peek(), Some(Token(TSXToken::Keyword(TSXKeyword::From), _))) {
				reader.expect_next(TSXToken::Comma)?;
			}
			Some(default_identifier)
		};

		let kind = if default.is_some()
			&& matches!(reader.peek(), Some(Token(TSXToken::Keyword(TSXKeyword::From), _)))
		{
			// From default keyword
			ImportKind::Parts(Vec::new())
		} else if let Some(Token(TSXToken::Multiply, _)) = reader.peek() {
			reader.next();
			let _as = reader.expect_next(TSXToken::Keyword(TSXKeyword::As))?;
			let under = VariableIdentifier::from_reader(reader, state, options)?;
			ImportKind::All { under }
		} else {
			let parts = parse_bracketed::<ImportPart>(
				reader,
				state,
				options,
				Some(TSXToken::OpenBrace),
				TSXToken::CloseBrace,
			)?
			.0;
			ImportKind::Parts(parts)
		};

		reader.expect_next(TSXToken::Keyword(TSXKeyword::From))?;

		let token = reader.next().ok_or_else(parse_lexing_error)?;
		let (end, from) = match token {
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
			default,
			kind,
			type_keyword,
			from,
			position: start_position.union(end),
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str("import");
		if self.type_keyword.is_some() && options.include_types {
			buf.push_str(" type");
		}

		if let Some(ref default) = self.default {
			buf.push(' ');
			default.to_string_from_buffer(buf, options, depth)
		} else {
			options.add_gap(buf);
		}

		match self.kind {
			ImportKind::All { ref under } => {
				if self.default.is_some() {
					buf.push_str(", ");
				}
				buf.push_str("* as ");
				under.to_string_from_buffer(buf, options, depth);
				buf.push(' ');
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
					options.add_gap(buf);
					for (at_end, part) in parts.iter().endiate() {
						part.to_string_from_buffer(buf, options, depth);
						if !at_end {
							buf.push(',');
							options.add_gap(buf);
						}
					}
					options.add_gap(buf);
					buf.push('}');
					options.add_gap(buf);
				} else if self.default.is_some() {
					buf.push(' ');
				}
			}
		}
		buf.push_str("from");
		options.add_gap(buf);
		buf.push('"');
		buf.push_str(&self.from);
		buf.push('"');
	}

	fn get_position(&self) -> &Span {
		&self.position
	}
}

/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import#syntax>
#[derive(Debug, Clone, PartialEq, Eq, Visitable, GetFieldByType)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
#[get_field_by_type_target(Span)]
pub enum ImportPart {
	Name(VariableIdentifier),
	NameWithAlias { name: String, alias: ImportExportName, position: Span },
	PrefixComment(String, Option<Box<Self>>, Span),
	PostfixComment(Box<Self>, String, Span),
}

impl ASTNode for ImportPart {
	fn get_position(&self) -> &Span {
		GetFieldByType::get(self)
	}

	// TODO also single line comments here
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let token = reader.next().ok_or_else(parse_lexing_error)?;
		if let Token(TSXToken::MultiLineComment(comment), start) = token {
			let (position, under) =
				if let Some(Token(TSXToken::CloseBrace | TSXToken::Comma, _)) = reader.peek() {
					(start.with_length(comment.len() + 2), None)
				} else {
					let part = Self::from_reader(reader, state, options)?;
					(start.union(part.get_position()), Some(Box::new(part)))
				};
			Ok(Self::PrefixComment(comment, under, position))
		} else {
			let (alias, alias_pos) = if let TSXToken::DoubleQuotedStringLiteral(_)
			| TSXToken::SingleQuotedStringLiteral(_) = token.0
			{
				let (start, alias, quoted) = match token {
					Token(TSXToken::SingleQuotedStringLiteral(content), start) => {
						(start, content, Quoted::Single)
					}
					Token(TSXToken::DoubleQuotedStringLiteral(content), start) => {
						(start, content, Quoted::Double)
					}
					_ => unreachable!(),
				};
				let with_length = start.with_length(alias.len() + 1);
				(ImportExportName::Quoted(alias, quoted), with_length)
			} else {
				let (ident, pos) = token_as_identifier(token, "import alias")?;
				(ImportExportName::Reference(ident), pos)
			};
			let mut value = match alias {
				ImportExportName::Quoted(..) => {
					reader.expect_next(TSXToken::Keyword(TSXKeyword::As))?;
					let (name, pos) = token_as_identifier(
						reader.next().ok_or_else(parse_lexing_error)?,
						"import name",
					)?;
					let position = alias_pos.union(pos);
					Self::NameWithAlias { name, alias, position }
				}
				ImportExportName::Reference(reference) => {
					if let Some(Token(TSXToken::Keyword(TSXKeyword::As), _)) = reader.peek() {
						reader.next();
						let (name, pos) = token_as_identifier(
							reader.next().ok_or_else(parse_lexing_error)?,
							"import name",
						)?;
						let position = alias_pos.union(pos);
						Self::NameWithAlias {
							name,
							alias: ImportExportName::Reference(reference),
							position,
						}
					} else {
						Self::Name(VariableIdentifier::Standard(reference, alias_pos))
					}
				}
			};
			while let Some(Token(TSXToken::MultiLineComment(_), _)) = reader.peek() {
				let Some(Token(TSXToken::MultiLineComment(c), start)) = reader.next() else {
					unreachable!()
				};
				let pos = value.get_position().union(start.get_end_after(c.len() + 2));
				value = Self::PostfixComment(Box::new(value), c, pos)
			}
			Ok(value)
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			ImportPart::Name(identifier) => buf.push_str(identifier.as_str()),
			ImportPart::NameWithAlias { name, alias, .. } => {
				match alias {
					ImportExportName::Reference(alias) => buf.push_str(alias),
					ImportExportName::Quoted(alias, q) => {
						buf.push(q.as_char());
						buf.push_str(alias);
						buf.push(q.as_char());
					}
				}
				buf.push_str(" as ");
				buf.push_str(name);
			}
			ImportPart::PrefixComment(comment, inner, _) => {
				if options.include_comments {
					buf.push_str("/*");
					buf.push_str(&comment);
					buf.push_str("*/");
					if inner.is_some() {
						buf.push(' ');
					}
				}
				if let Some(inner) = inner {
					inner.to_string_from_buffer(buf, options, depth);
				}
			}
			ImportPart::PostfixComment(inner, comment, _) => {
				inner.to_string_from_buffer(buf, options, depth);
				if options.include_comments {
					buf.push_str("/*");
					buf.push_str(&comment);
					buf.push_str("*/ ");
				}
			}
		}
	}
}
