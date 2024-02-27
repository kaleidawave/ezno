use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::{sized_tokens::TokenStart, Token, TokenReader};

use crate::{
	ast::object_literal::ObjectLiteral, derive_ASTNode, errors::parse_lexing_error,
	parse_bracketed, throw_unexpected_token, tokens::token_as_identifier, ASTNode, ListItem,
	Marker, ParseOptions, ParseResult, ParsingState, Quoted, TSXKeyword, TSXToken,
	VariableIdentifier,
};
use visitable_derive::Visitable;

use super::ImportLocation;

/// Side effects is represented under the Parts variant where the vector is empty
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[apply(derive_ASTNode)]
pub enum ImportedItems {
	Parts(Option<Vec<ImportPart>>),
	All { under: VariableIdentifier },
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ImportDeclaration {
	#[cfg(feature = "extras")]
	pub is_deferred: bool,
	#[cfg(feature = "full-typescript")]
	pub is_type_annotation_import_only: bool,
	pub default: Option<VariableIdentifier>,
	pub items: ImportedItems,
	pub from: ImportLocation,
	pub with: Option<ObjectLiteral>,
	pub position: Span,
	#[cfg(feature = "extras")]
	pub reversed: bool,
}

/// TODO `default` should have its own variant?
#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub enum ImportExportName {
	Reference(String),
	Quoted(String, Quoted),
	/// For typing here
	#[cfg_attr(feature = "self-rust-tokenize", self_tokenize_field(0))]
	Marker(
		#[cfg_attr(target_family = "wasm", tsify(type = "Marker<ImportExportName>"))] Marker<Self>,
	),
}

impl ImportExportName {
	pub(crate) fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<(Self, source_map::End)> {
		if let Some(Token(TSXToken::Comma, pos)) = reader.peek() {
			let marker = state.new_partial_point_marker(*pos);
			return Ok((ImportExportName::Marker(marker), pos.get_end_after(0)));
		}
		let token = reader.next().unwrap();
		if let Token(TSXToken::StringLiteral(alias, quoted), start) = token {
			let with_length = start.get_end_after(alias.len() + 1);
			state.constant_imports.push(alias.clone());
			Ok((ImportExportName::Quoted(alias, quoted), with_length))
		} else {
			let (ident, pos) = token_as_identifier(token, "import alias")?;
			if options.interpolation_points && ident == crate::marker::MARKER {
				Ok((
					ImportExportName::Marker(state.new_partial_point_marker(pos.get_start())),
					pos.get_end(),
				))
			} else {
				Ok((ImportExportName::Reference(ident), pos.get_end()))
			}
		}
	}
}

impl ASTNode for ImportDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let out = parse_import_specifier_and_parts(reader, state, options)?;

		let start = if matches!(out.items, ImportedItems::Parts(None)) && out.default.is_none() {
			out.start
		} else {
			state.expect_keyword(reader, TSXKeyword::From)?
		};

		let (from, end) = ImportLocation::from_reader(reader, state, options, Some(start))?;

		let with = reader
			.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::With)))
			.is_some()
			.then(|| ObjectLiteral::from_reader(reader, state, options))
			.transpose()?;

		Ok(ImportDeclaration {
			default: out.default,
			items: out.items,
			#[cfg(feature = "full-typescript")]
			is_type_annotation_import_only: out.is_type_annotation_import_only,
			#[cfg(feature = "extras")]
			is_deferred: out.is_deferred,
			from,
			with,
			position: out.start.union(end),
			#[cfg(feature = "extras")]
			reversed: false,
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("import");

		#[cfg(feature = "full-typescript")]
		if self.is_type_annotation_import_only && options.include_type_annotations {
			buf.push_str(" type");
		}

		if let Some(ref default) = self.default {
			buf.push(' ');
			default.to_string_from_buffer(buf, options, local);
			if matches!(self.items, ImportedItems::Parts(None)) {
				buf.push(' ');
			}
		} else {
			options.push_gap_optionally(buf);
		}

		match self.items {
			ImportedItems::All { ref under } => {
				if self.default.is_some() {
					buf.push_str(", ");
				}
				buf.push_str("* as ");
				under.to_string_from_buffer(buf, options, local);
				buf.push(' ');
			}
			ImportedItems::Parts(ref parts) => {
				if let Some(parts) = parts {
					if !parts.is_empty() {
						if self.default.is_some() {
							buf.push_str(", ");
						}
						buf.push('{');
						options.push_gap_optionally(buf);
						for (at_end, part) in parts.iter().endiate() {
							part.to_string_from_buffer(buf, options, local);
							if !at_end {
								buf.push(',');
								options.push_gap_optionally(buf);
							}
						}
						options.push_gap_optionally(buf);
						buf.push('}');
						options.push_gap_optionally(buf);
					}
				}
			}
		}
		if !(matches!(self.items, ImportedItems::Parts(None)) && self.default.is_none()) {
			buf.push_str("from");
			options.push_gap_optionally(buf);
		}
		self.from.to_string_from_buffer(buf);
	}

	fn get_position(&self) -> &Span {
		&self.position
	}
}

impl ImportDeclaration {
	#[cfg(feature = "extras")]
	pub fn reversed_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = state.expect_keyword(reader, TSXKeyword::From)?;

		let (from, _end) = ImportLocation::from_reader(reader, state, options, Some(start))?;

		let out = parse_import_specifier_and_parts(reader, state, options)?;

		let with = reader
			.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Assert)))
			.is_some()
			.then(|| ObjectLiteral::from_reader(reader, state, options))
			.transpose()?;

		Ok(ImportDeclaration {
			default: out.default,
			items: out.items,
			is_type_annotation_import_only: out.is_type_annotation_import_only,
			with,
			#[cfg(feature = "extras")]
			is_deferred: out.is_deferred,
			from,
			position: start.union(out.end),
			reversed: true,
		})
	}
}

pub(crate) struct PartsResult {
	pub start: TokenStart,
	#[cfg(feature = "extras")]
	pub is_deferred: bool,
	pub is_type_annotation_import_only: bool,
	pub default: Option<VariableIdentifier>,
	pub items: ImportedItems,
	pub end: source_map::End,
}

/// Covers import and exports
pub(crate) fn parse_import_specifier_and_parts(
	reader: &mut impl TokenReader<TSXToken, TokenStart>,
	state: &mut ParsingState,
	options: &ParseOptions,
) -> Result<PartsResult, crate::ParseError> {
	let start = state.expect_keyword(reader, TSXKeyword::Import)?;

	#[cfg(feature = "extras")]
	let is_deferred = state.optionally_expect_keyword(reader, TSXKeyword::Deferred).is_some();

	let is_type_annotation_import_only =
		state.optionally_expect_keyword(reader, TSXKeyword::Type).is_some();

	let peek = reader.peek();

	let default = if let Some(Token(
		TSXToken::OpenBrace | TSXToken::Multiply | TSXToken::StringLiteral(..),
		_,
	)) = peek
	{
		None
	} else {
		let default_identifier = VariableIdentifier::from_reader(reader, state, options)?;
		if reader.conditional_next(|t| matches!(t, TSXToken::Comma)).is_some() {
			Some(default_identifier)
		} else {
			let end = default_identifier.get_position().get_end();
			return Ok(PartsResult {
				start,
				#[cfg(feature = "extras")]
				is_deferred,
				is_type_annotation_import_only,
				default: Some(default_identifier),
				items: ImportedItems::Parts(None),
				end,
			});
		}
	};

	let peek = reader.peek();
	let (items, end) = if let Some(Token(TSXToken::Multiply, _)) = peek {
		reader.next();
		state.expect_keyword(reader, TSXKeyword::As)?;
		let under = VariableIdentifier::from_reader(reader, state, options)?;
		let end = under.get_position().get_end();
		(ImportedItems::All { under }, end)
	} else if let Some(Token(TSXToken::OpenBrace, _)) = peek {
		let (parts, end) = parse_bracketed::<ImportPart>(
			reader,
			state,
			options,
			Some(TSXToken::OpenBrace),
			TSXToken::CloseBrace,
		)?;
		(ImportedItems::Parts(Some(parts)), end)
	} else if let Some(Token(TSXToken::StringLiteral(..), _)) = peek {
		(ImportedItems::Parts(None), start.get_end_after(6))
	} else {
		return throw_unexpected_token(reader, &[TSXToken::Multiply, TSXToken::OpenBrace]);
	};

	Ok(PartsResult {
		start,
		#[cfg(feature = "extras")]
		is_deferred,
		is_type_annotation_import_only,
		default,
		items,
		end,
	})
}

/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import#syntax>
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Eq, Visitable, GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum ImportPart {
	Name(VariableIdentifier),
	NameWithAlias {
		name: String,
		alias: ImportExportName,
		position: Span,
	},
	PrefixComment(
		String,
		#[cfg_attr(target_family = "wasm", tsify(type = "ImportPart | null"))] Option<Box<Self>>,
		Span,
	),
	PostfixComment(
		#[cfg_attr(target_family = "wasm", tsify(type = "ImportPart"))] Box<Self>,
		String,
		Span,
	),
}

impl ListItem for ImportPart {}

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
			let (alias, alias_pos) =
				if let Token(TSXToken::StringLiteral(alias, quoted), start) = token {
					let with_length = start.with_length(alias.len() + 2);
					(ImportExportName::Quoted(alias, quoted), with_length)
				} else {
					let (ident, pos) = token_as_identifier(token, "import alias")?;
					(ImportExportName::Reference(ident), pos)
				};

			let mut value = match alias {
				ImportExportName::Quoted(..) => {
					let _ = state.expect_keyword(reader, TSXKeyword::As)?;
					let (name, pos) = token_as_identifier(
						reader.next().ok_or_else(parse_lexing_error)?,
						"import name",
					)?;
					let position = alias_pos.union(pos);
					Self::NameWithAlias { name, alias, position }
				}
				ImportExportName::Reference(reference) => {
					if state.optionally_expect_keyword(reader, TSXKeyword::As).is_some() {
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
				ImportExportName::Marker(id) => Self::Name(VariableIdentifier::Marker(
					Marker(id.0, Default::default()),
					alias_pos,
				)),
			};
			while let Some(Token(TSXToken::MultiLineComment(_), _)) = reader.peek() {
				let Some(Token(TSXToken::MultiLineComment(c), start)) = reader.next() else {
					unreachable!()
				};
				let pos = value.get_position().union(start.get_end_after(c.len() + 2));
				value = Self::PostfixComment(Box::new(value), c, pos);
			}
			Ok(value)
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			ImportPart::Name(name) => {
				name.to_string_from_buffer(buf, options, local);
			}
			ImportPart::NameWithAlias { name, alias, .. } => {
				match alias {
					ImportExportName::Reference(alias) => buf.push_str(alias),
					ImportExportName::Quoted(alias, q) => {
						buf.push(q.as_char());
						buf.push_str(alias);
						buf.push(q.as_char());
					}
					ImportExportName::Marker(_) => {}
				}
				buf.push_str(" as ");
				buf.push_str(name);
			}
			ImportPart::PrefixComment(comment, inner, _) => {
				if options.should_add_comment(comment.starts_with('.')) {
					buf.push_str("/*");
					buf.push_str(comment);
					buf.push_str("*/");
					if inner.is_some() {
						buf.push(' ');
					}
				}
				if let Some(inner) = inner {
					inner.to_string_from_buffer(buf, options, local);
				}
			}
			ImportPart::PostfixComment(inner, comment, _) => {
				inner.to_string_from_buffer(buf, options, local);
				if options.should_add_comment(comment.starts_with('.')) {
					buf.push_str("/*");
					buf.push_str(comment);
					buf.push_str("*/ ");
				}
			}
		}
	}
}
