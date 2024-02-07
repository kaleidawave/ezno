use derive_enum_from_into::{EnumFrom, EnumTryInto};
use get_field_by_type::GetFieldByType;
use source_map::Span;
use tokenizer_lib::{sized_tokens::TokenStart, Token};
use visitable_derive::Visitable;

use crate::{
	errors::parse_lexing_error, extensions::decorators, throw_unexpected_token_with_token,
	Decorated, Marker, ParseError, ParseErrors, ParseOptions, Quoted, StatementPosition,
	TSXKeyword, TSXToken, TypeDefinitionModuleDeclaration,
};

pub use self::{
	export::ExportDeclaration,
	variable::{VariableDeclaration, VariableDeclarationItem},
};

pub type StatementFunctionBase = crate::functions::GeneralFunctionBase<StatementPosition>;
pub type StatementFunction = crate::FunctionBase<StatementFunctionBase>;

#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
const TYPES_STATEMENT_FUNCTION: &str = r"
	export interface StatementFunction extends FunctionBase {
		header: FunctionHeader,
		body: Block,
		name: StatementPosition
	}
";
pub mod classes;
pub mod export;
pub mod import;
pub mod variable;

pub use super::types::{
	declares::*,
	enum_declaration::{EnumDeclaration, EnumMember},
	interface::InterfaceDeclaration,
	type_alias::TypeAlias,
};
pub use classes::ClassDeclaration;
pub use import::{ImportDeclaration, ImportExportName, ImportPart};

#[derive(
	Debug, Clone, Visitable, EnumFrom, EnumTryInto, PartialEq, get_field_by_type::GetFieldByType,
)]
#[get_field_by_type_target(Span)]
#[try_into_references(&, &mut)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub enum Declaration {
	Variable(VariableDeclaration),
	Function(Decorated<StatementFunction>),
	Class(Decorated<ClassDeclaration<StatementPosition>>),
	Enum(Decorated<EnumDeclaration>),
	Interface(Decorated<InterfaceDeclaration>),
	TypeAlias(TypeAlias),
	// Special TS only
	DeclareVariable(DeclareVariableDeclaration),
	DeclareFunction(DeclareFunctionDeclaration),
	#[from_ignore]
	DeclareInterface(InterfaceDeclaration),
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
					| TSXKeyword::Class | TSXKeyword::Import
					| TSXKeyword::Export
			) | TSXToken::At,
		);

		#[cfg(feature = "extras")]
		return result
			|| matches!(token, TSXToken::Keyword(kw) if options.custom_function_headers && kw.is_special_function_header())
			|| {
				let TSXToken::Keyword(token) = *token else { return false };
				let Some(Token(after, _)) = reader.peek_n(1) else { return false };

				matches!(
					token,
					TSXKeyword::Declare | TSXKeyword::Interface
					if options.type_annotations
				) || matches!(
					(token, after),
					(TSXKeyword::From, TSXToken::StringLiteral(..))
						| (TSXKeyword::Async, TSXToken::Keyword(TSXKeyword::Function))
				) || matches!(
					(token, after),
					(TSXKeyword::Async, TSXToken::Keyword(kw)) if options.custom_function_headers && kw.is_special_function_header()
				)
			};

		#[cfg(not(feature = "extras"))]
		return result;
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
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
			#[cfg(feature = "extras")]
			TSXToken::Keyword(TSXKeyword::From) => {
				ImportDeclaration::reversed_from_reader(reader, state, options).map(Into::into)
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
				crate::modules::parse_declare_item(reader, state, options, decorators, start)
					.and_then(|ty_def_mod_stmt| match ty_def_mod_stmt {
						TypeDefinitionModuleDeclaration::Variable(declare_var) => {
							Ok(Declaration::DeclareVariable(declare_var))
						}
						TypeDefinitionModuleDeclaration::Function(declare_func) => {
							Ok(Declaration::DeclareFunction(declare_func))
						}
						TypeDefinitionModuleDeclaration::Class(item) => Err(ParseError::new(
							ParseErrors::InvalidDeclareItem("class"),
							*item.get_position(),
						)),
						TypeDefinitionModuleDeclaration::Interface(item) => Err(ParseError::new(
							ParseErrors::InvalidDeclareItem("interface"),
							*item.get_position(),
						)),
						TypeDefinitionModuleDeclaration::TypeAlias(item) => Err(ParseError::new(
							ParseErrors::InvalidDeclareItem("type alias"),
							*item.get_position(),
						)),
						TypeDefinitionModuleDeclaration::Namespace(item) => Err(ParseError::new(
							ParseErrors::InvalidDeclareItem("namespace"),
							*item.get_position(),
						)),
						TypeDefinitionModuleDeclaration::LocalTypeAlias(_)
						| TypeDefinitionModuleDeclaration::LocalVariableDeclaration(_)
						| TypeDefinitionModuleDeclaration::Comment(_) => unreachable!(),
					})
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
			Declaration::Function(f) => f.to_string_from_buffer(buf, options, local),
			Declaration::Variable(var) => var.to_string_from_buffer(buf, options, local),
			Declaration::Class(cls) => cls.to_string_from_buffer(buf, options, local),
			Declaration::Import(is) => is.to_string_from_buffer(buf, options, local),
			Declaration::Export(es) => es.to_string_from_buffer(buf, options, local),
			Declaration::Interface(id) => id.to_string_from_buffer(buf, options, local),
			Declaration::TypeAlias(ta) => ta.to_string_from_buffer(buf, options, local),
			Declaration::Enum(r#enum) => r#enum.to_string_from_buffer(buf, options, local),
			// TODO should skip these under no types
			Declaration::DeclareFunction(dfd) => dfd.to_string_from_buffer(buf, options, local),
			Declaration::DeclareVariable(dvd) => dvd.to_string_from_buffer(buf, options, local),
			Declaration::DeclareInterface(did) => {
				buf.push_str("declare ");
				did.to_string_from_buffer(buf, options, local);
			}
		}
	}

	fn get_position(&self) -> &source_map::Span {
		self.get()
	}
}
