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
use tokenizer_lib::{sized_tokens::TokenStart, Token};
use visitable_derive::Visitable;

use crate::{
	derive_ASTNode, errors::parse_lexing_error, extensions::decorators,
	throw_unexpected_token_with_token, Decorated, Marker, ParseError, ParseErrors, ParseOptions,
	Quoted, StatementPosition, TSXKeyword, TSXToken,
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
	// TODO strict mode can affect result
	/// Takes `reader` as sometimes needs to `peek_n`
	pub(crate) fn is_declaration_start(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
		options: &ParseOptions,
	) -> bool {
		let Some(Token(token, _)) = reader.peek() else { return false };

		let result = matches!(
			token,
			TSXToken::Keyword(
				TSXKeyword::Let
					| TSXKeyword::Const | TSXKeyword::Function
					| TSXKeyword::Class | TSXKeyword::Export
			) | TSXToken::At,
		);

		#[cfg(feature = "extras")]
		return result
			|| matches!(token, TSXToken::Keyword(kw) if options.custom_function_headers && kw.is_special_function_header())
			|| (matches!(token, TSXToken::Keyword(TSXKeyword::Namespace) if cfg!(feature = "full-typescript")))
			|| {
				let TSXToken::Keyword(token) = *token else { return false };
				let Some(Token(after, _)) = reader.peek_n(1) else { return false };

				#[allow(clippy::match_same_arms)]
				match (token, after) {
					// For dynamic import
					(
						TSXKeyword::Import,
						TSXToken::OpenBrace
						| TSXToken::Keyword(..)
						| TSXToken::Identifier(..)
						| TSXToken::StringLiteral(..)
						| TSXToken::Multiply,
					) => true,
					(TSXKeyword::Declare | TSXKeyword::Interface, _) => options.type_annotations,
					(TSXKeyword::Async, TSXToken::Keyword(TSXKeyword::Function)) => true,
					(TSXKeyword::Async, TSXToken::Keyword(kw)) => {
						options.custom_function_headers && kw.is_special_function_header()
					}
					// Extra
					(TSXKeyword::From, TSXToken::StringLiteral(..)) => true,
					(..) => false,
				}
			};

		#[cfg(not(feature = "extras"))]
		return result || {
			let TSXToken::Keyword(token) = *token else { return false };

			// For dynamic import
			matches!(token, TSXKeyword::Import)
				&& matches!(
					reader.peek_n(1),
					Some(Token(
						TSXToken::OpenBrace
							| TSXToken::Keyword(..) | TSXToken::Identifier(..)
							| TSXToken::StringLiteral(..)
							| TSXToken::Multiply,
						_
					))
				)
		};
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
	pub(crate) fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
		start: Option<TokenStart>,
	) -> crate::ParseResult<(Self, source_map::End)> {
		if let (true, Some(start), Some(Token(peek, at))) =
			(options.partial_syntax, start, reader.peek())
		{
			let next_is_not_location_like = peek.is_statement_or_declaration_start()
				&& state
					.line_starts
					.byte_indexes_on_different_lines(start.0 as usize, at.0 as usize);

			if next_is_not_location_like {
				return Ok((
					ImportLocation::Marker(state.new_partial_point_marker(*at)),
					source_map::End(start.0),
				));
			}
		}

		let token = reader.next().ok_or_else(parse_lexing_error)?;
		if let Token(TSXToken::StringLiteral(content, quoted), start) = token {
			let with_length = start.get_end_after(content.len() + 1);
			Ok((ImportLocation::Quoted(content, quoted), with_length))
		} else if options.interpolation_points
			&& matches!(&token.0, TSXToken::Identifier(i) if i == crate::marker::MARKER)
		{
			Ok((Self::Marker(state.new_partial_point_marker(token.1)), source_map::End(token.1 .0)))
		} else {
			Err(ParseError::new(
				ParseErrors::ExpectedStringLiteral { found: token.0 },
				token.1.with_length(0),
			))
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
	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> crate::ParseResult<Self> {
		// TODO assert decorators are used. If they exist but item is not `Decorated`
		// then need to throw a parse error
		let decorators = decorators::decorators_from_reader(reader, state, options)?;

		match reader.peek().ok_or_else(parse_lexing_error)?.0 {
			// Const can be either variable declaration or const enum
			TSXToken::Keyword(TSXKeyword::Const) => {
				let after_const = reader.peek_n(2);
				if let Some(Token(TSXToken::Keyword(TSXKeyword::Enum), _)) = after_const {
					EnumDeclaration::from_reader(reader, state, options)
						.map(|on| Declaration::Enum(Decorated::new(decorators, on)))
				} else {
					let declaration = VariableDeclaration::from_reader(reader, state, options)?;
					Ok(Declaration::Variable(declaration))
				}
			}
			TSXToken::Keyword(TSXKeyword::Let) => {
				let declaration = VariableDeclaration::from_reader(reader, state, options)?;
				Ok(Declaration::Variable(declaration))
			}
			TSXToken::Keyword(TSXKeyword::Enum) => {
				EnumDeclaration::from_reader(reader, state, options)
					.map(|on| Declaration::Enum(Decorated::new(decorators, on)))
			}
			#[cfg(feature = "extras")]
			TSXToken::Keyword(ref kw) if kw.is_special_function_header() => {
				let function = StatementFunction::from_reader(reader, state, options)?;
				Ok(Declaration::Function(Decorated::new(decorators, function)))
			}
			TSXToken::Keyword(TSXKeyword::Function | TSXKeyword::Async) => {
				let function = StatementFunction::from_reader(reader, state, options)?;
				Ok(Declaration::Function(Decorated::new(decorators, function)))
			}
			TSXToken::Keyword(TSXKeyword::Class) => {
				let Token(_, start) = reader.next().unwrap();
				state.append_keyword_at_pos(start.0, TSXKeyword::Class);
				ClassDeclaration::from_reader_sub_class_keyword(reader, state, options, start)
					.map(|on| Declaration::Class(Decorated::new(decorators, on)))
			}
			TSXToken::Keyword(TSXKeyword::Export) => {
				ExportDeclaration::from_reader(reader, state, options)
					.map(|on| Declaration::Export(Decorated::new(decorators, on)))
			}
			TSXToken::Keyword(TSXKeyword::Import) => {
				ImportDeclaration::from_reader(reader, state, options).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::Interface) if options.type_annotations => {
				InterfaceDeclaration::from_reader(reader, state, options)
					.map(|on| Declaration::Interface(Decorated::new(decorators, on)))
			}
			TSXToken::Keyword(TSXKeyword::Type) if options.type_annotations => {
				TypeAlias::from_reader(reader, state, options).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::Declare) if options.type_annotations => {
				let Token(_, start) = reader.next().unwrap();
				match reader.peek().ok_or_else(parse_lexing_error)?.0 {
					TSXToken::Keyword(TSXKeyword::Let | TSXKeyword::Const | TSXKeyword::Var) => {
						DeclareVariableDeclaration::from_reader_sub_declare(
							reader,
							state,
							options,
							Some(start),
							decorators,
						)
						.map(Into::into)
					}
					TSXToken::Keyword(TSXKeyword::Class) => {
						let mut class = ClassDeclaration::<StatementPosition>::from_reader(
							reader, state, options,
						)?;
						class.name.declare = true;
						class.position.start = start.0;
						Ok(Declaration::Class(Decorated::new(decorators, class)))
					}
					TSXToken::Keyword(TSXKeyword::Function) => {
						let mut function = StatementFunction::from_reader(reader, state, options)?;
						function.name.declare = true;
						function.position.start = start.0;
						Ok(Declaration::Function(Decorated::new(decorators, function)))
					}
					TSXToken::Keyword(TSXKeyword::Type) => {
						let mut alias = TypeAlias::from_reader(reader, state, options)?;
						alias.name.declare = true;
						alias.position.start = start.0;
						Ok(Declaration::TypeAlias(alias))
					}
					_ => throw_unexpected_token_with_token(
						reader.next().ok_or_else(parse_lexing_error)?,
						&[
							TSXToken::Keyword(TSXKeyword::Let),
							TSXToken::Keyword(TSXKeyword::Const),
							TSXToken::Keyword(TSXKeyword::Var),
							TSXToken::Keyword(TSXKeyword::Function),
							TSXToken::Keyword(TSXKeyword::Class),
							TSXToken::Keyword(TSXKeyword::Type),
						],
					),
				}
			}
			#[cfg(feature = "extras")]
			TSXToken::Keyword(TSXKeyword::From) => {
				ImportDeclaration::reversed_from_reader(reader, state, options).map(Into::into)
			}
			#[cfg(feature = "full-typescript")]
			TSXToken::Keyword(TSXKeyword::Namespace) => {
				crate::types::namespace::Namespace::from_reader(reader, state, options)
					.map(Into::into)
			}
			_ => throw_unexpected_token_with_token(
				reader.next().ok_or_else(parse_lexing_error)?,
				&[
					TSXToken::Keyword(TSXKeyword::Let),
					TSXToken::Keyword(TSXKeyword::Const),
					TSXToken::Keyword(TSXKeyword::Function),
					TSXToken::Keyword(TSXKeyword::Class),
					TSXToken::Keyword(TSXKeyword::Enum),
					TSXToken::Keyword(TSXKeyword::Type),
					TSXToken::Keyword(TSXKeyword::Declare),
					TSXToken::Keyword(TSXKeyword::Import),
					TSXToken::Keyword(TSXKeyword::Export),
					TSXToken::Keyword(TSXKeyword::Async),
					#[cfg(feature = "extras")]
					TSXToken::Keyword(TSXKeyword::Generator),
				],
			),
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
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub struct ImportExportPart<T: ImportOrExport> {
	pub just_type: bool,
	pub name: crate::VariableIdentifier,
	pub alias: Option<ImportExportName>,
	pub position: Span,
	#[visit_skip_field]
	pub _marker: std::marker::PhantomData<T>,
}

impl<T: ImportOrExport> crate::ListItem for ImportExportPart<T> {
	type LAST = ();
}

impl<U: ImportOrExport> crate::ASTNode for ImportExportPart<U> {
	fn get_position(&self) -> Span {
		*GetFieldByType::get(self)
	}

	// TODO also single line comments here
	fn from_reader(
		reader: &mut impl crate::TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> crate::ParseResult<Self> {
		let just_type =
			reader.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Type))).is_some();

		if U::PREFIX {
			let (alias, position) = ImportExportName::from_reader(reader, state, options)?;
			if reader.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::As))).is_some()
			{
				let name = crate::VariableIdentifier::from_reader(reader, state, options)?;
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
				crate::throw_unexpected_token(reader, &[TSXToken::Keyword(TSXKeyword::As)])
			}
		} else {
			let name = crate::VariableIdentifier::from_reader(reader, state, options)?;
			let mut position = name.get_position();
			let alias = if reader
				.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::As)))
				.is_some()
			{
				let (alias, end) = ImportExportName::from_reader(reader, state, options)?;
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
	pub(crate) fn from_reader(
		reader: &mut impl crate::TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> crate::ParseResult<(Self, Span)> {
		if let Some(Token(TSXToken::Comma, pos)) = reader.peek() {
			let marker = state.new_partial_point_marker(*pos);
			return Ok((ImportExportName::Marker(marker), pos.union(source_map::End(pos.0))));
		}
		let token = reader.next().unwrap();
		if let Token(TSXToken::StringLiteral(alias, quoted), start) = token {
			let with_length = start.with_length(alias.len() + 1);
			state.constant_imports.push(alias.clone());
			Ok((ImportExportName::Quoted(alias, quoted), with_length))
		} else {
			let (ident, pos) = crate::tokens::token_as_identifier(token, "import alias")?;
			if options.interpolation_points && ident == crate::marker::MARKER {
				Ok((ImportExportName::Marker(state.new_partial_point_marker(pos.get_start())), pos))
			} else {
				Ok((ImportExportName::Reference(ident), pos))
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
