pub mod classes;
pub mod import_export;
pub mod variable;

pub use import_export::{export, import};

pub use super::types::{
	declare_variable::DeclareVariableDeclaration,
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
	// TODO strict mode can affect result
	// TODO reuse
	// Warning expects skip to have been called
	pub(crate) fn is_declaration_start(reader: &crate::Lexer) -> bool {
		let mut declaration_keyword = reader.is_one_of_keywords(&[
			"let",
			"const",
			"class",
			"enum",
			"interface",
			"type",
			"namespace",
			"declare",
			"import",
			"export",
			// Extra
			"from",
		]);

		if let Some("from") = declaration_keyword {
			reader.get_options().reversed_imports
		} else if let Some(name @ ("import" | "export" | "namespace" | "type")) =
			declaration_keyword
		{
			let after_declaration_keyword = reader.get_current()[name.len()..].trim_start();

			// TODO more (is operator like?)
			let is_declaration_keyword_expression = after_declaration_keyword.starts_with('(')
				|| after_declaration_keyword.starts_with('.')
				|| after_declaration_keyword.starts_with('[')
				|| after_declaration_keyword.starts_with('(')
				|| after_declaration_keyword.starts_with('=');

			!is_declaration_keyword_expression
		} else if declaration_keyword.is_some() {
			true
		} else {
			crate::lexer::utilities::is_function_header(reader.get_current())
		}
	}
}

impl crate::ASTNode for Declaration {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::Lexer) -> crate::ParseResult<Self> {
		// TODO assert decorators are used. If they exist but item is not `Decorated`
		// then need to throw a parse error
		let decorators = decorators::decorators_from_reader(reader)?;

		if reader.is_keyword("const") {
			// Const can be either variable declaration or `const enum`
			if reader.get_current()["const".len()..].trim_start().starts_with("enum ") {
				EnumDeclaration::from_reader(reader)
					.map(|on| Declaration::Enum(Decorated::new(decorators, on)))
			} else {
				VariableDeclaration::from_reader(reader).map(Declaration::Variable)
			}
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
		} else if reader.is_keyword("import") {
			ImportDeclaration::from_reader(reader).map(Into::into)
		} else if reader.is_keyword("export") {
			ExportDeclaration::from_reader(reader)
				.map(|on| Declaration::Export(Decorated::new(decorators, on)))
		} else if reader.is_keyword("interface") {
			let interface = InterfaceDeclaration::from_reader(reader)?;
			crate::lexer::utilities::assert_type_annotations(reader, interface.get_position())?;
			Ok(Declaration::Interface(Decorated::new(decorators, interface)))
		} else if reader.is_keyword("type") {
			// options.type_annotations => {
			let alias = TypeAlias::from_reader(reader)?;
			crate::lexer::utilities::assert_type_annotations(reader, alias.get_position())?;
			Ok(alias.into())
		} else if reader.is_keyword("declare") {
			let start = reader.get_start();
			reader.advance("declare".len() as u32);
			reader.skip();
			if let Some(_keyword) = reader.is_one_of_keywords(&["let", "const", "var"]) {
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
				#[cfg(feature = "extras")]
				if reader.is_keyword("namespace") {
					let mut namespace = crate::types::namespace::Namespace::from_reader(reader)?;
					namespace.is_declare = true;
					namespace.position.start = start.0;
					return Ok(Declaration::Namespace(namespace));
				}

				Err(crate::lexer::utilities::expected_one_of_items(
					reader,
					&["let", "const", "var", "class", "type", "async", "function", "namespace"],
				))
			}
		} else if crate::lexer::utilities::is_function_header(reader.get_current()) {
			let function = StatementFunction::from_reader(reader)?;
			Ok(Declaration::Function(Decorated::new(decorators, function)))
		} else {
			#[cfg(feature = "extras")]
			if reader.is_keyword("from") {
				return ImportDeclaration::from_reader_reversed(reader).map(Declaration::Import);
			}

			#[cfg(feature = "full-typescript")]
			if reader.is_keyword("namespace") {
				return crate::types::namespace::Namespace::from_reader(reader).map(Into::into);
			}

			// #[cfg(feature = "extras")]
			// TSXToken::Keyword(ref kw) if kw.is_special_function_header() => {
			// }
			// TSXToken::Keyword(TSXKeyword::Function | TSXKeyword::Async) => {
			// 	let function = StatementFunction::from_reader(reader)?;
			// 	Ok(Declaration::Function(Decorated::new(decorators, function)))
			// }

			// TODO vary list on certain parameters
			Err(crate::lexer::utilities::expected_one_of_items(
				reader,
				&[
					"let",
					"const",
					"function",
					"class",
					"enum",
					"type",
					"declare",
					"import",
					"export",
					"async",
					"generator",
				],
			))
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
}
