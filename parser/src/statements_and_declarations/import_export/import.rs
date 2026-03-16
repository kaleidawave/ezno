use super::{ImportAttribute, ImportExportPart, ImportKind, ImportLocation};
use crate::{
	ASTNode, ParseResult, VariableIdentifier, bracketed_items_from_reader, derive_ASTNode,
};
use source_map::Span;
use visitable_derive::Visitable;

/// Side effects is represented under the Parts variant where the vector is empty
#[derive(Debug, Clone, Visitable)]
#[apply(derive_ASTNode)]
pub enum ImportedItems {
	Parts(Option<Vec<ImportExportPart<ImportDeclaration>>>),
	All { under: VariableIdentifier },
}

impl ImportedItems {
	#[must_use]
	pub fn is_some(&self) -> bool {
		!matches!(self, Self::Parts(None))
	}
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ImportDeclaration {
	pub kind: ImportKind,
	pub default: Option<VariableIdentifier>,
	pub items: ImportedItems,
	pub from: ImportLocation,
	pub with: Option<ImportAttribute>,
	pub position: Span,
	#[cfg(feature = "extras")]
	pub reversed: bool,
}

impl ASTNode for ImportDeclaration {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let out = import_specifier_and_parts_from_reader(reader)?;
		Self::from_reader_with_parts(reader, start, out)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("import");

		#[cfg(feature = "extras")]
		if let ImportKind::Deferred = self.kind {
			buf.push_str(" defer");
		}

		#[cfg(feature = "full-typescript")]
		if let ImportKind::TypeOnly = self.kind {
			assert!(options.include_type_annotations);
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
				if let Some(parts) = parts
					&& !parts.is_empty()
				{
					if self.default.is_some() {
						buf.push_str(", ");
					}
					super::import_export_parts_to_string_from_buffer(parts, buf, options, local);
					options.push_gap_optionally(buf);
				}
			}
		}
		if !(matches!(self.items, ImportedItems::Parts(None)) && self.default.is_none()) {
			buf.push_str("from");
			options.push_gap_optionally(buf);
		}
		if let Some(ref with) = self.with {
			buf.push_str("with ");
			with.to_string_from_buffer(buf, options, local);
		}
		self.from.to_string_from_buffer(buf);
	}
}

impl ImportDeclaration {
	#[cfg(feature = "extras")]
	pub fn from_reader_reversed(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("from")?;

		let from = ImportLocation::from_reader(reader)?;

		let out = import_specifier_and_parts_from_reader(reader)?;

		let with = if reader.is_keyword_advance("with") {
			Some(ImportAttribute::from_reader(reader)?)
		} else {
			None
		};

		let position = start.union(reader.get_end());

		Ok(ImportDeclaration {
			default: out.default,
			items: out.items,
			kind: out.kind,
			with,
			from,
			position,
			reversed: true,
		})
	}

	pub(crate) fn from_reader_with_parts(
		reader: &mut crate::Lexer,
		start: source_map::Start,
		parts: PartsResult,
	) -> ParseResult<Self> {
		// If not `import "./side_effect.js"`
		if parts.items.is_some() || parts.default.is_some() {
			reader.expect_keyword("from")?;
		}

		let from = ImportLocation::from_reader(reader)?;

		// TODO validate
		let with = reader
			.is_operator_advance("with")
			.then(|| ImportAttribute::from_reader(reader))
			.transpose()?;

		let end = reader.get_end();

		Ok(ImportDeclaration {
			default: parts.default,
			items: parts.items,
			kind: parts.kind,
			from,
			with,
			position: start.union(end),
			#[cfg(feature = "extras")]
			reversed: false,
		})
	}
}

pub(crate) struct PartsResult {
	pub kind: super::ImportKind,
	pub default: Option<VariableIdentifier>,
	pub items: ImportedItems,
}

/// Covers `import` keyword, more 2
pub(crate) fn import_specifier_and_parts_from_reader(
	reader: &mut crate::Lexer,
) -> ParseResult<PartsResult> {
	reader.expect_keyword("import")?;
	import_specifier_and_parts_from_reader_without_import(reader)
}

pub(crate) fn import_specifier_and_parts_from_reader_without_import(
	reader: &mut crate::Lexer,
) -> ParseResult<PartsResult> {
	let start = reader.get_start();

	let kind = super::ImportKind::from_reader(reader);

	if let Some(name) = kind.as_identifier()
		&& reader.is_keyword("from")
	{
		return Ok(PartsResult {
			kind: super::ImportKind::Standard,
			default: Some(VariableIdentifier::Standard(
				name.to_owned(),
				start.with_length(name.len()),
			)),
			items: ImportedItems::Parts(None),
		});
	}

	let is_identifier =
		reader.get_current().starts_with(crate::lexer::utilities::is_identifier_continutation);

	let default = if is_identifier {
		let default_identifier = VariableIdentifier::from_reader(reader)?;
		if reader.is_operator_advance(",") {
			Some(default_identifier)
		} else {
			return Ok(PartsResult {
				kind,
				default: Some(default_identifier),
				items: ImportedItems::Parts(None),
			});
		}
	} else {
		None
	};

	let items = if reader.is_operator_advance("*") {
		reader.expect_keyword("as")?;
		let under = VariableIdentifier::from_reader(reader)?;
		ImportedItems::All { under }
	} else if reader.is_operator_advance("{") {
		let (parts, _) = bracketed_items_from_reader::<ImportExportPart<_>>(reader, "}")?;
		ImportedItems::Parts(Some(parts))
	} else if reader.starts_with_string_delimeter() || reader.is_keyword("from") {
		ImportedItems::Parts(None)
	} else {
		return Err(crate::lexer::utilities::expected_one_of_items(reader, &["*", "["]));
	};

	// #[cfg(feature = "extras")]
	// if let ImportKind::Deferred = kind
	// 	&& let ImportedItems::All { .. } = items
	// {
	// 	let position = start.union(reader.get_end());
	// 	return Err(crate::ParseError::new(
	// 		crate::ParseErrors::ImportDeferCannotBeUsedWithNamedImports,
	// 		position,
	// 	));
	// }

	Ok(PartsResult { kind, default, items })
}
