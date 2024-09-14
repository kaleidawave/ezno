pub mod classes;
pub mod export;
pub mod import;
pub mod variable;

pub use super::types::{
	declare_variable::*,
	enum_declaration::{EnumDeclaration, EnumMember},
	interface::InterfaceDeclaration,
	type_alias::TypeAlias,
};

use derive_enum_from_into::{EnumFrom, EnumTryInto};
use get_field_by_type::GetFieldByType;
use source_map::Span;
use visitable_derive::Visitable;

use crate::{
	derive_ASTNode, extensions::decorators, Decorated, Marker, ParseError, ParseErrors,
	ParseOptions, Quoted, StatementPosition,
};

pub use self::{
	classes::ClassDeclaration,
	export::ExportDeclaration,
	import::ImportDeclaration,
	variable::{VariableDeclaration, VariableDeclarationItem},
};

pub type StatementFunctionBase = crate::functions::GeneralFunctionBase<StatementPosition>;
pub type StatementFunction = crate::FunctionBase<StatementFunctionBase>;

#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
#[allow(dead_code)]
const TYPES_STATEMENT_FUNCTION: &str = r"
	export interface StatementFunction extends FunctionBase {
		header: FunctionHeader,
		parameters: FunctionParameters<ThisParameter | null, null>,
		body: Block,
		name: StatementPosition
	}
";

#[apply(derive_ASTNode)]
#[derive(
	Debug, Clone, Visitable, EnumFrom, EnumTryInto, PartialEq, get_field_by_type::GetFieldByType,
)]
#[get_field_by_type_target(Span)]
#[try_into_references(&, &mut)]
pub enum Declaration {
	Variable(VariableDeclaration),
	Function(Decorated<StatementFunction>),
	Class(Decorated<ClassDeclaration<StatementPosition>>),
	Enum(Decorated<EnumDeclaration>),
	Interface(Decorated<InterfaceDeclaration>),
	TypeAlias(TypeAlias),
	// Special TS only
	DeclareVariable(DeclareVariableDeclaration),
	#[cfg(feature = "full-typescript")]
	Namespace(crate::types::namespace::Namespace),
	// Top level only
	Import(ImportDeclaration),
	Export(Decorated<ExportDeclaration>),
}

impl Declaration {
	// 	// TODO strict mode can affect result
	// 	/// Takes `reader` as sometimes needs to `peek_n`
	pub(crate) fn is_declaration_start(reader: &crate::new::Lexer) -> bool {
		let declaration_keyword = reader.is_one_of_keyword(&[
			"let",
			"const",
			"function",
			"class",
			"enum",
			"interface",
			"type",
			"namespace",
			"declare",
			"import",
			"export",
		]);

		if let Some("import") = declaration_keyword {
			let is_import_meta =
				reader.get_current()[.."import".len()].trim_start().starts_with('.');
			!is_import_meta
		} else {
			declaration_keyword.is_some()
		}

		// #[cfg(feature = "extras")]
		// 		return result
		// 			|| matches!(token, TSXToken::Keyword(kw) if options.custom_function_headers && kw.is_special_function_header())
		// 			|| (matches!(token, TSXToken::Keyword(TSXKeyword::Namespace) if cfg!(feature = "full-typescript")))
		// 			|| {
		// 				let TSXToken::Keyword(token) = *token else { return false };
		// 				let Some(Token(after, _)) = reader.peek_n(1) else { return false };

		// 				#[allow(clippy::match_same_arms)]
		// 				match (token, after) {
		// 					// For dynamic import
		// 					(
		// 						TSXKeyword::Import,
		// 						TSXToken::OpenBrace
		// 						| TSXToken::Keyword(..)
		// 						| TSXToken::Identifier(..)
		// 						| TSXToken::StringLiteral(..)
		// 						| TSXToken::Multiply,
		// 					) => true,
		// 					(TSXKeyword::Declare | TSXKeyword::Interface, _) => options.type_annotations,
		// 					(TSXKeyword::Async, TSXToken::Keyword(TSXKeyword::Function)) => true,
		// 					(TSXKeyword::Async, TSXToken::Keyword(kw)) => {
		// 						options.custom_function_headers && kw.is_special_function_header()
		// 					}
		// 					// Extra
		// 					(TSXKeyword::From, TSXToken::StringLiteral(..)) => true,
		// 					(..) => false,
		// 				}
		// 			};

		// 		#[cfg(not(feature = "extras"))]
		// 		return result || {
		// 			let TSXToken::Keyword(token) = *token else { return false };

		// 			// For dynamic import
		// 			matches!(token, TSXKeyword::Import)
		// 				&& matches!(
		// 					reader.peek_n(1),
		// 					Some(Token(
		// 						TSXToken::OpenBrace
		// 							| TSXToken::Keyword(..) | TSXToken::Identifier(..)
		// 							| TSXToken::StringLiteral(..)
		// 							| TSXToken::Multiply,
		// 						_
		// 					))
		// 				)
		// 		};
		// 	}
		// }	// TODO '@'
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

		if reader.starts_with_string_delimeter() {
			let start = reader.get_start();
			let (content, quoted) = reader.parse_string_literal().expect("TODO");
			Ok((ImportLocation::Quoted(content.to_owned(), quoted)))
		} else {
			todo!("{:?}", reader.get_current())
			// else if options.interpolation_points
			// 	&& matches!(&token.0, TSXToken::Identifier(i) if i == crate::marker::MARKER)
			// {
			// Ok((Self::Marker(state.new_partial_point_marker(token.1)), source_map::End(token.1 .0)))
			// 	todo!()
			// Err(ParseError::new(
			// 	ParseErrors::ExpectedStringLiteral { found: token.0 },
			// 	token.1.with_length(0),
			// ))
		}
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

	/// Can be None if self is a marker point
	#[must_use]
	pub fn get_path(&self) -> Option<&str> {
		if let Self::Quoted(name, _) = self {
			Some(name)
		} else {
			None
		}
	}
}

impl crate::ASTNode for Declaration {
	fn from_reader(reader: &mut crate::new::Lexer) -> crate::ParseResult<Self> {
		// TODO assert decorators are used. If they exist but item is not `Decorated`
		// then need to throw a parse error
		let decorators = decorators::decorators_from_reader(reader)?;

		if reader.is_keyword("const") {
			// TODO `const enum`
			// Const can be either variable declaration or const enum
			// let after_const = reader.peek_n(2);
			// if after_const {
			// 	EnumDeclaration::from_reader(reader)
			// 		.map(|on| Declaration::Enum(Decorated::new(decorators, on)))
			// } else {
			let declaration = VariableDeclaration::from_reader(reader)?;
			Ok(Declaration::Variable(declaration))
		// }
		} else if reader.is_keyword("let") {
			let declaration = VariableDeclaration::from_reader(reader)?;
			Ok(Declaration::Variable(declaration))
		} else if reader.is_keyword("enum") {
			EnumDeclaration::from_reader(reader)
				.map(|on| Declaration::Enum(Decorated::new(decorators, on)))
		} else if reader.is_keyword("class") {
			// state.append_keyword_at_pos(start.0, TSXKeyword::Class);
			ClassDeclaration::from_reader(reader)
				.map(|on| Declaration::Class(Decorated::new(decorators, on)))
		} else if reader.is_keyword("export") {
			ExportDeclaration::from_reader(reader)
				.map(|on| Declaration::Export(Decorated::new(decorators, on)))
		} else if reader.is_keyword("import") {
			ImportDeclaration::from_reader(reader).map(Into::into)
		} else if reader.is_keyword("interface") {
			InterfaceDeclaration::from_reader(reader)
				.map(|on| Declaration::Interface(Decorated::new(decorators, on)))
		} else if reader.is_keyword("type") {
			// options.type_annotations => {
			TypeAlias::from_reader(reader).map(Into::into)
		} else if reader.is_keyword("declare") {
			let start = reader.get_start();
			reader.advance("declare".len() as u32);
			reader.skip();
			if reader.is_one_of_keyword(&["let", "const", "var"]).is_some() {
				let mut declare = DeclareVariableDeclaration::from_reader_without_declare(reader)?;
				// TODO pass these down
				declare.decorators = decorators;
				return Ok(Declaration::DeclareVariable(declare));
			} else if reader.is_keyword("class") {
				let mut class = ClassDeclaration::<StatementPosition>::from_reader(reader)?;
				class.name.is_declare = true;
				class.position.start = start.0;
				Ok(Declaration::Class(Decorated::new(decorators, class)))
			} else if reader.is_keyword("function") || reader.is_keyword("async") {
				let mut function = StatementFunction::from_reader(reader)?;
				function.name.is_declare = true;
				function.position.start = start.0;
				Ok(Declaration::Function(Decorated::new(decorators, function)))
			} else if reader.is_keyword("type") {
				let mut alias = TypeAlias::from_reader(reader)?;
				alias.name.is_declare = true;
				alias.position.start = start.0;
				Ok(Declaration::TypeAlias(alias))
			} else {
				todo!("{:?}", reader.get_current());
			}
		// 		#[cfg(feature = "full-typescript")]
		// 		TSXToken::Keyword(TSXKeyword::Namespace) => {
		// 			let mut namespace = crate::types::namespace::Namespace::from_reader(
		// 				reader,
		// 			)?;
		// 			namespace.is_declare = true;
		// 			namespace.position.start = start.0;
		// 			Ok(Declaration::Namespace(namespace))
		// 		}
		// 		_ => throw_unexpected_token_with_token(
		// 			reader.next().ok_or_else(parse_lexing_error)?,
		// 			&[
		// 				TSXToken::Keyword(TSXKeyword::Let),
		// 				TSXToken::Keyword(TSXKeyword::Const),
		// 				TSXToken::Keyword(TSXKeyword::Var),
		// 				TSXToken::Keyword(TSXKeyword::Function),
		// 				TSXToken::Keyword(TSXKeyword::Class),
		// 				TSXToken::Keyword(TSXKeyword::Type),
		// 				TSXToken::Keyword(TSXKeyword::Namespace),
		// 			],
		// 		),
		// 	}
		// }
		// #[cfg(feature = "extras")]
		// TSXToken::Keyword(TSXKeyword::From) => {
		// 	ImportDeclaration::reversed_from_reader(reader).map(Into::into)
		// }
		// #[cfg(feature = "full-typescript")]
		// TSXToken::Keyword(TSXKeyword::Namespace) => {
		// 	crate::types::namespace::Namespace::from_reader(reader)
		// 		.map(Into::into)
		// }
		} else if reader.is_keyword("function") || reader.is_keyword("async") {
			// TODO more above ^^^
			let function = StatementFunction::from_reader(reader)?;
			Ok(Declaration::Function(Decorated::new(decorators, function)))
		// #[cfg(feature = "extras")]
		// TSXToken::Keyword(ref kw) if kw.is_special_function_header() => {
		// }
		// TSXToken::Keyword(TSXKeyword::Function | TSXKeyword::Async) => {
		// 	let function = StatementFunction::from_reader(reader)?;
		// 	Ok(Declaration::Function(Decorated::new(decorators, function)))
		// }
		} else {
			// throw_unexpected_token_with_token(
			// 	reader.next().ok_or_else(parse_lexing_error)?,
			// 	&[
			// 		TSXToken::Keyword(TSXKeyword::Let),
			// 		TSXToken::Keyword(TSXKeyword::Const),
			// 		TSXToken::Keyword(TSXKeyword::Function),
			// 		TSXToken::Keyword(TSXKeyword::Class),
			// 		TSXToken::Keyword(TSXKeyword::Enum),
			// 		TSXToken::Keyword(TSXKeyword::Type),
			// 		TSXToken::Keyword(TSXKeyword::Declare),
			// 		TSXToken::Keyword(TSXKeyword::Import),
			// 		TSXToken::Keyword(TSXKeyword::Export),
			// 		TSXToken::Keyword(TSXKeyword::Async),
			// 		#[cfg(feature = "extras")]
			// 		TSXToken::Keyword(TSXKeyword::Generator),
			// 	],
			// );
			todo!("{:?}", &reader.get_current()[..100])
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			Declaration::Variable(var) => var.to_string_from_buffer(buf, options, local),
			Declaration::Class(cls) => cls.to_string_from_buffer(buf, options, local),
			Declaration::Import(is) => is.to_string_from_buffer(buf, options, local),
			Declaration::Export(es) => es.to_string_from_buffer(buf, options, local),
			Declaration::Function(f) => f.to_string_from_buffer(buf, options, local),
			Declaration::Interface(id) => id.to_string_from_buffer(buf, options, local),
			Declaration::TypeAlias(ta) => ta.to_string_from_buffer(buf, options, local),
			Declaration::Enum(r#enum) => r#enum.to_string_from_buffer(buf, options, local),
			Declaration::DeclareVariable(dvd) => dvd.to_string_from_buffer(buf, options, local),
			#[cfg(feature = "full-typescript")]
			Declaration::Namespace(ns) => ns.to_string_from_buffer(buf, options, local),
		}
	}

	fn get_position(&self) -> Span {
		*self.get()
	}
}

pub trait ImportOrExport: std::fmt::Debug + Clone + PartialEq + Sync + Send + 'static {
	const PREFIX: bool;
}

impl ImportOrExport for ImportDeclaration {
	const PREFIX: bool = true;
}

impl ImportOrExport for ExportDeclaration {
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
				todo!()
				// crate::throw_unexpected_token(reader, &[TSXToken::Keyword(TSXKeyword::As)])
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
	use super::ASTNode;
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
		todo!("")
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
	pub(crate) fn from_reader(reader: &mut crate::new::Lexer) -> crate::ParseResult<(Self, Span)> {
		// if let Some(Token(TSXToken::Comma, pos)) = reader.peek() {
		// 	let marker = state.new_partial_point_marker(*pos);
		// 	return Ok((ImportExportName::Marker(marker), pos.union(source_map::End(pos.0))));
		// }
		let start = reader.get_start();
		if reader.starts_with_string_delimeter() {
			let (content, quoted) = reader.parse_string_literal().expect("TODO");
			let position = start.with_length(content.len() + 2);
			Ok((ImportExportName::Quoted(content.to_owned(), quoted), position))
		} else {
			// let ident = crate::tokens::token_as_identifier(token, "import alias")?;
			let ident = reader.parse_identifier().expect("TODO").to_owned();
			// if options.interpolation_points && ident == crate::marker::MARKER {
			// 	Ok((ImportExportName::Marker(state.new_partial_point_marker(pos.get_start())), pos))
			// } else {
			// }
			let position = start.with_length(ident.len());
			Ok((ImportExportName::Reference(ident), position))
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
