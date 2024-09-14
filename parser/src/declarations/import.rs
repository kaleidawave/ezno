use super::{ImportExportPart, ImportLocation};
use crate::{
	ast::object_literal::ObjectLiteral, bracketed_items_from_reader, derive_ASTNode, ASTNode,
	ParseOptions, ParseResult, ParsingState, VariableIdentifier,
};
use source_map::Span;
use visitable_derive::Visitable;

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
	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let out = import_specifier_and_parts_from_reader(reader)?;

		if !matches!(out.items, ImportedItems::Parts(None)) && out.default.is_none() {
			reader.expect_keyword("from")?;
		}

		let from = ImportLocation::from_reader(reader)?;

		let with = reader
			.is_operator_advance("with")
			.then(|| ObjectLiteral::from_reader(reader))
			.transpose()?;

		let end = reader.get_end();

		Ok(ImportDeclaration {
			default: out.default,
			items: out.items,
			#[cfg(feature = "full-typescript")]
			is_type_annotation_import_only: out.is_type_annotation_import_only,
			#[cfg(feature = "extras")]
			is_deferred: out.is_deferred,
			from,
			with,
			position: start.union(end),
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
	pub fn reversed_from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("from")?;

		let from = ImportLocation::from_reader(reader)?;

		let out = import_specifier_and_parts_from_reader(reader)?;

		let with = if reader.is_keyword_advance("assert") {
			Some(ObjectLiteral::from_reader(reader)?)
		} else {
			None
		};

		let position = start.union(reader.get_end());

		Ok(ImportDeclaration {
			default: out.default,
			items: out.items,
			is_type_annotation_import_only: out.is_type_annotation_import_only,
			#[cfg(feature = "extras")]
			is_deferred: out.is_deferred,
			with,
			from,
			position,
			reversed: true,
		})
	}
}

pub(crate) struct PartsResult {
	#[cfg(feature = "extras")]
	pub is_deferred: bool,
	pub is_type_annotation_import_only: bool,
	pub default: Option<VariableIdentifier>,
	pub items: ImportedItems,
}

/// Covers import and exports
pub(crate) fn import_specifier_and_parts_from_reader(
	reader: &mut crate::new::Lexer,
) -> ParseResult<PartsResult> {
	reader.expect_keyword("import")?;

	#[cfg(feature = "extras")]
	let is_deferred = reader.is_operator_advance("deferred");

	let is_type_annotation_import_only = reader.is_operator_advance("type");

	// TODO temp

	reader.skip();
	let is_identifier = reader.get_current().chars().next().is_some_and(|c| c.is_alphabetic());

	let default = if is_identifier {
		let default_identifier = VariableIdentifier::from_reader(reader)?;
		if reader.is_operator_advance(",") {
			Some(default_identifier)
		} else {
			let end = default_identifier.get_position().get_end();
			return Ok(PartsResult {
				#[cfg(feature = "extras")]
				is_deferred,
				is_type_annotation_import_only,
				default: Some(default_identifier),
				items: ImportedItems::Parts(None),
			});
		}
	} else {
		None
	};

	// let peek = reader.peek();
	let items = if reader.is_operator_advance("*") {
		reader.expect_keyword("as")?;
		let under = VariableIdentifier::from_reader(reader)?;
		ImportedItems::All { under }
	} else if reader.is_operator_advance("{") {
		let (parts, _) = bracketed_items_from_reader::<ImportExportPart<_>>(reader, "}")?;
		ImportedItems::Parts(Some(parts))
	} else if reader.starts_with_string_delimeter() {
		todo!("what")
	// ImportedItems::Parts(None)
	} else {
		todo!("{:?}", reader.get_current())
		// return throw_unexpected_token(reader, &[TSXToken::Multiply, TSXToken::OpenBrace]);
	};

	Ok(PartsResult {
		#[cfg(feature = "extras")]
		is_deferred,
		is_type_annotation_import_only,
		default,
		items,
	})
}
