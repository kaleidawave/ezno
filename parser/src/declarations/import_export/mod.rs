pub mod export;
pub mod import;

use derive_enum_from_into::{EnumFrom, EnumTryInto};
use get_field_by_type::GetFieldByType;
use source_map::Span;
use visitable_derive::Visitable;

use crate::{
	derive_ASTNode, extensions::decorators, Decorated, Marker, ParseError, ParseErrors,
	ParseOptions, Quoted, StatementPosition,
};

pub trait ImportOrExport: std::fmt::Debug + Clone + PartialEq + Sync + Send + 'static {
	const PREFIX: bool;
}

impl ImportOrExport for import::ImportDeclaration {
	const PREFIX: bool = true;
}

impl ImportOrExport for export::ExportDeclaration {
	const PREFIX: bool = false;
}

/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import#syntax>
#[derive(Debug, Clone, PartialEq, Visitable, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct ImportExportPart<T: ImportOrExport> {
	pub just_type: bool,
	pub name: crate::VariableIdentifier,
	pub alias: Option<ImportExportName>,
	pub position: Span,
	#[visit_skip_field]
	pub _marker: std::marker::PhantomData<T>,
}

#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
#[allow(dead_code)]
const IMPORT_EXPORT_PART_TYPE: &str = r"
	type ImportExportPart<_T> = { just_type: boolean, name: VariableIdentifier, alias: ImportExportName | null, position: Span };
";

impl<T: ImportOrExport> crate::ListItem for ImportExportPart<T> {
	type LAST = ();
}

impl<U: ImportOrExport> crate::ASTNode for ImportExportPart<U> {
	fn get_position(&self) -> Span {
		*GetFieldByType::get(self)
	}

	// TODO also single line comments here
	fn from_reader(reader: &mut crate::new::Lexer) -> crate::ParseResult<Self> {
		let just_type = reader.is_keyword_advance("type");

		if U::PREFIX {
			let (alias, position) = ImportExportName::from_reader(reader)?;
			if reader.is_keyword_advance("as") {
				let name = crate::VariableIdentifier::from_reader(reader)?;
				let position = position.union(name.get_position());
				Ok(Self {
					just_type,
					name,
					alias: Some(alias),
					position,
					_marker: Default::default(),
				})
			} else if let ImportExportName::Reference(name) = alias {
				let name = crate::VariableIdentifier::Standard(name, position);
				Ok(Self { just_type, name, alias: None, position, _marker: Default::default() })
			} else {
				Err(ParseError::new(
					ParseErrors::ExpectedKeyword {
						expected: "as",
						// TODO
						found: reader.get_current(),
					},
					reader.next_item_span(),
				))
			}
		} else {
			let name = crate::VariableIdentifier::from_reader(reader)?;
			let mut position = name.get_position();
			let alias = if reader.is_keyword_advance("as") {
				let (alias, end) = ImportExportName::from_reader(reader)?;
				position = position.union(end);
				Some(alias)
			} else {
				None
			};
			Ok(Self { just_type, name, alias, position, _marker: Default::default() })
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if self.just_type && options.include_type_annotations {
			buf.push_str("type ");
		}
		if let Some(ref alias) = self.alias {
			if U::PREFIX {
				alias.to_string_from_buffer(buf, options, local);
				buf.push_str(" as ");
				self.name.to_string_from_buffer(buf, options, local);
			} else {
				self.name.to_string_from_buffer(buf, options, local);
				buf.push_str(" as ");
				alias.to_string_from_buffer(buf, options, local);
			}
		} else {
			self.name.to_string_from_buffer(buf, options, local);
		}
	}
}

// If `options.pretty` sort by name
fn import_export_parts_to_string_from_buffer<T: source_map::ToString, U: ImportOrExport>(
	parts: &[ImportExportPart<U>],
	buf: &mut T,
	options: &crate::ToStringOptions,
	local: crate::LocalToStringInformation,
) {
	use crate::ASTNode;
	use iterator_endiate::EndiateIteratorExt;

	buf.push('{');
	options.push_gap_optionally(buf);
	if options.pretty {
		let mut parts: Vec<&ImportExportPart<U>> = parts.iter().collect();
		parts.sort_unstable_by_key(|part| part.name.as_option_str().unwrap_or_default());
		for (at_end, part) in parts.iter().endiate() {
			part.to_string_from_buffer(buf, options, local);
			if !at_end {
				buf.push(',');
				options.push_gap_optionally(buf);
			}
		}
	} else {
		for (at_end, part) in parts.iter().endiate() {
			part.to_string_from_buffer(buf, options, local);
			if !at_end {
				buf.push(',');
				options.push_gap_optionally(buf);
			}
		}
	}
	options.push_gap_optionally(buf);
	buf.push('}');
}

#[cfg(feature = "self-rust-tokenize")]
impl<U: ImportOrExport> self_rust_tokenize::SelfRustTokenize for ImportExportPart<U> {
	fn append_to_token_stream(
		&self,
		_token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		todo!("import export part to token stream")
	}
}

/// TODO `default` should have its own variant?
#[derive(Debug, Clone, PartialEq)]
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
	// TODO remove Span return
	pub(crate) fn from_reader(reader: &mut crate::new::Lexer) -> crate::ParseResult<(Self, Span)> {
		reader.skip();
		let start = reader.get_start();
		if reader.starts_with_string_delimeter() {
			let (content, quoted) = reader.parse_string_literal()?;
			let position = start.with_length(content.len() + 2);
			Ok((ImportExportName::Quoted(content.to_owned(), quoted), position))
		} else if reader.is_keyword_advance("default") {
			// TODO separate identifier
			Ok((ImportExportName::Reference("default".into()), start.with_length("default".len())))
		} else if reader.is_operator(",") {
			let position = start.with_length(0);
			let marker = reader.new_partial_point_marker(position);
			Ok((ImportExportName::Marker(marker), position))
		} else {
			let identifier = reader.parse_identifier("import alias", true)?.to_owned();
			if reader.get_options().interpolation_points && identifier == crate::marker::MARKER {
				let position = start.with_length(0);
				Ok((ImportExportName::Marker(reader.new_partial_point_marker(position)), position))
			} else {
				let position = start.with_length(identifier.len());
				Ok((ImportExportName::Reference(identifier), position))
			}
		}
	}

	pub(crate) fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		_options: &crate::ToStringOptions,
		_local: crate::LocalToStringInformation,
	) {
		match self {
			ImportExportName::Reference(alias) => buf.push_str(alias),
			ImportExportName::Quoted(alias, q) => {
				buf.push(q.as_char());
				buf.push_str(alias);
				buf.push(q.as_char());
			}
			ImportExportName::Marker(_) => {}
		}
	}
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportLocation {
	Quoted(String, Quoted),
	#[cfg_attr(feature = "self-rust-tokenize", self_tokenize_field(0))]
	Marker(
		#[cfg_attr(target_family = "wasm", tsify(type = "Marker<ImportLocation>"))] Marker<Self>,
	),
}

impl ImportLocation {
	pub(crate) fn from_reader(reader: &mut crate::new::Lexer) -> crate::ParseResult<Self> {
		// let _existing = r#"if let (true, Some(start), Some(Token(peek, at))) =
		// 	(options.partial_syntax, start, reader.peek())
		// {
		// 	let next_is_not_location_like = peek.is_statement_or_declaration_start()
		// 		&& state
		// 			.line_starts
		// 			.byte_indexes_on_different_lines(start.0 as usize, at.0 as usize);

		// 	if next_is_not_location_like {
		// 		return Ok((
		// 			ImportLocation::Marker(state.new_partial_point_marker(*at)),
		// 			source_map::End(start.0),
		// 		));
		// 	}
		// }
		// else if options.interpolation_points
		// 	&& matches!(&token.0, TSXToken::Identifier(i) if i == crate::marker::MARKER)
		// {
		// Ok((Self::Marker(state.new_partial_point_marker(token.1)), source_map::End(token.1 .0)))
		// 	todo!()
		// Err(ParseError::new(
		// 	ParseErrors::ExpectedStringLiteral { found: token.0 },
		// 	token.1.with_length(0),
		// ))

		reader.skip();

		let start = reader.get_start();
		let (content, quoted) = reader.parse_string_literal()?;
		Ok(ImportLocation::Quoted(content.to_owned(), quoted))
	}

	pub(crate) fn to_string_from_buffer<T: source_map::ToString>(&self, buf: &mut T) {
		match self {
			ImportLocation::Quoted(inner, quoted) => {
				buf.push(quoted.as_char());
				buf.push_str(inner);
				buf.push(quoted.as_char());
			}
			ImportLocation::Marker(_) => {}
		}
	}

	/// Can be `None` if self is a marker point
	#[must_use]
	pub fn get_path(&self) -> Option<&str> {
		if let Self::Quoted(name, _) = self {
			Some(name)
		} else {
			None
		}
	}
}
