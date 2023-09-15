use derive_enum_from_into::{EnumFrom, EnumTryInto};
use tokenizer_lib::Token;
use visitable_derive::Visitable;

use crate::{
	errors::parse_lexing_error, extensions::decorators, throw_unexpected_token_with_token,
	Decorated, Keyword, ParseError, ParseErrors, StatementPosition, TSXKeyword, TSXToken,
	TypeDefinitionModuleDeclaration,
};

pub use self::{
	export::ExportDeclaration,
	variable::{VariableDeclaration, VariableDeclarationItem},
};

pub type StatementFunctionBase = crate::functions::GeneralFunctionBase<StatementPosition>;
pub type StatementFunction = crate::FunctionBase<StatementFunctionBase>;

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
pub use import::{ImportDeclaration, ImportPart, ImportStatementId};

#[derive(Debug, Clone, Visitable, EnumFrom, EnumTryInto, PartialEq)]
#[try_into_references(&, &mut)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
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
	// TODO strict mode, type etc can affect result
	pub(crate) fn is_declaration_start(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
	) -> bool {
		matches!(
			reader.peek(),
			Some(Token(
				TSXToken::Keyword(
					TSXKeyword::Let
						| TSXKeyword::Const | TSXKeyword::Function
						| TSXKeyword::Class | TSXKeyword::Enum
						| TSXKeyword::Type | TSXKeyword::Declare
						| TSXKeyword::Import | TSXKeyword::Export
						| TSXKeyword::Interface | TSXKeyword::Async
						| TSXKeyword::Generator
				) | TSXToken::At,
				_
			))
		)
	}
}

impl crate::ASTNode for Declaration {
	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseOptions,
	) -> crate::ParseResult<Self> {
		// TODO assert decorators are used. If they exist but item is not `Decorated`
		// then need to throw a parse error
		let decorators = decorators::decorators_from_reader(reader, state, settings)?;

		match reader.peek().ok_or_else(parse_lexing_error)?.0 {
			// Const can be either variable declaration or const enum
			TSXToken::Keyword(TSXKeyword::Const) => {
				let after_const = reader.peek_n(2);
				if let Some(Token(TSXToken::Keyword(TSXKeyword::Enum), _)) = after_const {
					EnumDeclaration::from_reader(reader, state, settings)
						.map(|on| Declaration::Enum(Decorated { decorators, on }))
				} else {
					let declaration = VariableDeclaration::from_reader(reader, state, settings)?;
					Ok(Declaration::Variable(declaration))
				}
			}
			TSXToken::Keyword(TSXKeyword::Let) => {
				let declaration = VariableDeclaration::from_reader(reader, state, settings)?;
				Ok(Declaration::Variable(declaration))
			}
			TSXToken::Keyword(TSXKeyword::Enum) => {
				EnumDeclaration::from_reader(reader, state, settings)
					.map(|on| Declaration::Enum(Decorated { decorators, on }))
			}
			TSXToken::Keyword(TSXKeyword::Generator) if settings.generator_keyword => {
				let function = StatementFunction::from_reader(reader, state, settings)?;
				Ok(Declaration::Function(Decorated { decorators, on: function }))
			}
			TSXToken::Keyword(TSXKeyword::Function | TSXKeyword::Async) => {
				let function = StatementFunction::from_reader(reader, state, settings)?;
				Ok(Declaration::Function(Decorated { decorators, on: function }))
			}
			TSXToken::Keyword(TSXKeyword::Class) => {
				let class_keyword = Keyword::new(reader.next().unwrap().get_span());
				ClassDeclaration::from_reader_sub_class_keyword(
					reader,
					state,
					settings,
					class_keyword,
				)
				.map(|on| Declaration::Class(Decorated { decorators, on }))
			}
			TSXToken::Keyword(TSXKeyword::Export) => {
				ExportDeclaration::from_reader(reader, state, settings)
					.map(|on| Declaration::Export(Decorated { decorators, on }))
			}
			TSXToken::Keyword(TSXKeyword::Import) => {
				ImportDeclaration::from_reader(reader, state, settings).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::Interface) => {
				InterfaceDeclaration::from_reader(reader, state, settings)
					.map(|on| Declaration::Interface(Decorated { decorators, on }))
			}
			TSXToken::Keyword(TSXKeyword::Type) => {
				TypeAlias::from_reader(reader, state, settings).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::Declare) => {
				let Token(_, start) = reader.next().unwrap();
				crate::modules::parse_declare_item(reader, state, settings, decorators, start)
					.and_then(|ty_def_mod_stmt| match ty_def_mod_stmt {
						TypeDefinitionModuleDeclaration::Variable(declare_var) => {
							Ok(Declaration::DeclareVariable(declare_var))
						}
						TypeDefinitionModuleDeclaration::Function(declare_func) => {
							Ok(Declaration::DeclareFunction(declare_func))
						}
						TypeDefinitionModuleDeclaration::Class(item) => Err(ParseError::new(
							ParseErrors::InvalidDeclareItem("class"),
							item.get_position().clone(),
						)),
						TypeDefinitionModuleDeclaration::Interface(item) => Err(ParseError::new(
							ParseErrors::InvalidDeclareItem("interface"),
							item.get_position().clone(),
						)),
						TypeDefinitionModuleDeclaration::TypeAlias(item) => Err(ParseError::new(
							ParseErrors::InvalidDeclareItem("type alias"),
							item.get_position().clone(),
						)),
						TypeDefinitionModuleDeclaration::Namespace(item) => Err(ParseError::new(
							ParseErrors::InvalidDeclareItem("namespace"),
							item.get_position().clone(),
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
					TSXToken::Keyword(TSXKeyword::Generator),
				],
			),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			Declaration::Function(f) => f.to_string_from_buffer(buf, settings, depth),
			Declaration::Variable(var) => var.to_string_from_buffer(buf, settings, depth),
			Declaration::Class(cls) => cls.to_string_from_buffer(buf, settings, depth),
			Declaration::Import(is) => is.to_string_from_buffer(buf, settings, depth),
			Declaration::Export(es) => es.to_string_from_buffer(buf, settings, depth),
			Declaration::Interface(id) => id.to_string_from_buffer(buf, settings, depth),
			Declaration::TypeAlias(ta) => ta.to_string_from_buffer(buf, settings, depth),
			Declaration::Enum(r#enum) => r#enum.to_string_from_buffer(buf, settings, depth),
			// TODO should skip these under no types
			Declaration::DeclareFunction(dfd) => dfd.to_string_from_buffer(buf, settings, depth),
			Declaration::DeclareVariable(dvd) => dvd.to_string_from_buffer(buf, settings, depth),
			Declaration::DeclareInterface(did) => {
				buf.push_str("declare ");
				did.to_string_from_buffer(buf, settings, depth)
			}
		}
	}

	fn get_position(&self) -> &source_map::Span {
		todo!()
	}
}
