use source_map::Span;
use tokenizer_lib::{sized_tokens::TokenStart, Token, TokenReader};

use crate::{
	ast::object_literal::ObjectLiteral, derive_ASTNode, parse_bracketed, throw_unexpected_token,
	ASTNode, ParseOptions, ParseResult, ParsingState, TSXKeyword, TSXToken, VariableIdentifier,
};
use visitable_derive::Visitable;

use super::{ImportExportPart, ImportLocation};

/// Side effects is represented under the Parts variant where the vector is empty
#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub enum ImportedItems {
	Parts(Option<Vec<ImportExportPart<ImportDeclaration>>>),
	All { under: VariableIdentifier },
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
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
						super::import_export_parts_to_string_from_buffer(
							parts, buf, options, local,
						);
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

	fn get_position(&self) -> Span {
		self.position
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
		let (parts, _, end) = parse_bracketed::<ImportExportPart<_>>(
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
