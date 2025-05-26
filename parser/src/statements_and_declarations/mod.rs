pub mod classes;
pub mod control_flow;
pub mod import_export;
pub mod variable;
pub mod with_statement;

pub use import_export::{export, import};

pub use super::types::{
	declare_variable::DeclareVariableDeclaration,
	enum_declaration::{EnumDeclaration, EnumMember, EnumMemberValue},
	interface::InterfaceDeclaration,
	type_alias::TypeAlias,
};

use crate::{
	derive_ASTNode,
	extensions::decorators::{decorators_from_reader, Decorated},
	Marker, ParseError, ParseErrors, StatementPosition,
};
use derive_enum_from_into::{EnumFrom, EnumTryInto};
use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use std::fmt::Debug;
use visitable_derive::Visitable;

use super::{expressions::MultipleExpression, ASTNode, Block, ParseResult, Span};
pub use control_flow::for_statement::{
	ForLoopCondition, ForLoopStatement, ForLoopStatementInitialiser,
};
pub use control_flow::if_statement::*;
pub use control_flow::switch_statement::{SwitchBranch, SwitchStatement};
pub use control_flow::try_catch_statement::TryCatchStatement;
pub use control_flow::while_statement::{DoWhileStatement, WhileStatement};

pub use classes::ClassDeclaration;
pub use export::ExportDeclaration;
pub use import::ImportDeclaration;
pub use variable::{
	VarVariableStatement, VariableDeclaration, VariableDeclarationItem, VariableDeclarationKeyword,
};

pub use with_statement::WithStatement;

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

// TODO strict mode can affect result
// TODO reuse
// Warning expects skip to have been called
pub(crate) fn is_declaration_start(reader: &crate::Lexer) -> bool {
	let declaration_keyword = reader.is_one_of_keywords(&[
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
	} else if let Some(name @ ("import" | "export" | "namespace" | "type")) = declaration_keyword {
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

/// A statement. See [Declaration]s and [StatementOrDeclaration] for more
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, EnumFrom, EnumTryInto, PartialEqExtras, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[partial_eq_ignore_types(Span)]
#[try_into_references(&, &mut)]
pub enum StatementOrDeclaration {
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
	// statements
	Expression(MultipleExpression),
	/// { ... } statement
	Block(Block),
	Debugger(Span),
	// Loops and "condition-aries"
	If(IfStatement),
	ForLoop(ForLoopStatement),
	Switch(SwitchStatement),
	WhileLoop(WhileStatement),
	DoWhileLoop(DoWhileStatement),
	TryCatch(TryCatchStatement),
	// Control flow
	Return(ReturnStatement),
	// TODO maybe an actual label struct instead of `Option<String>`
	Continue(Option<String>, Span),
	// TODO maybe an actual label struct instead of `Option<String>`
	Break(Option<String>, Span),
	/// e.g `throw ...`
	Throw(ThrowStatement),
	// Comments
	Comment(String, Span),
	MultiLineComment(String, Span),
	Labelled {
		position: Span,
		name: String,
		statement: Box<Statement>,
	},
	VarVariable(VarVariableStatement),
	/// FUTURE under cfg?
	WithStatement(WithStatement),
	/// Lol
	AestheticSemiColon(Span),
	Empty(Span),
	/// For bundling
	Imported {
		moved: Box<StatementOrDeclaration>,
		/// from the import statement
		originally: Span,
		from: source_map::SourceId,
	},
	/// TODO under cfg
	#[cfg_attr(feature = "self-rust-tokenize", self_tokenize_field(0))]
	Marker(#[visit_skip_field] Marker<Statement>, Span),
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, PartialEqExtras, GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ReturnStatement(pub Option<MultipleExpression>, pub Span);

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, PartialEqExtras, GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ThrowStatement(pub Box<MultipleExpression>, pub Span);

impl Eq for StatementOrDeclaration {}

impl ASTNode for StatementOrDeclaration {
	fn get_position(&self) -> Span {
		*get_field_by_type::GetFieldByType::get(self)
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		if reader.after_identifier().starts_with(':') {
			let start = reader.get_start();
			let name = reader.parse_identifier("statement label", true)?.to_owned();
			let _ = reader.expect(':')?;
			let statement = Statement::from_reader(reader).map(Box::new)?;
			if statement.0.requires_semi_colon() {
				reader.expect_semi_colon()?;
			}
			// TODO check statement.can_be_labelled()
			let position = start.union(statement.get_position());
			return Ok(StatementOrDeclaration::Labelled { name, statement, position });
		}

		reader.skip();
		let start = reader.get_start();

		// TODO assert decorators are used. If they exist but item is not `Decorated`
		// then need to throw a parse error
		let decorators = decorators_from_reader(reader)?;

		if reader.is_keyword("const") {
			// Const can be either variable declaration or `const enum`
			if reader.get_current()["const".len()..].trim_start().starts_with("enum ") {
				EnumDeclaration::from_reader(reader)
					.map(|on| StatementOrDeclaration::Enum(Decorated::new(decorators, on)))
			} else {
				VariableDeclaration::from_reader(reader).map(StatementOrDeclaration::Variable)
			}
		} else if reader.is_keyword("let") {
			let declaration = VariableDeclaration::from_reader(reader)?;
			Ok(StatementOrDeclaration::Variable(declaration))
		} else if reader.is_keyword("enum") {
			EnumDeclaration::from_reader(reader)
				.map(|on| StatementOrDeclaration::Enum(Decorated::new(decorators, on)))
		} else if reader.is_keyword("class") {
			// state.append_keyword_at_pos(start.0, TSXKeyword::Class);
			ClassDeclaration::from_reader(reader)
				.map(|on| StatementOrDeclaration::Class(Decorated::new(decorators, on)))
		} else if reader.is_keyword("import") {
			ImportDeclaration::from_reader(reader).map(Into::into)
		} else if reader.is_keyword("export") {
			ExportDeclaration::from_reader(reader)
				.map(|on| StatementOrDeclaration::Export(Decorated::new(decorators, on)))
		} else if reader.is_keyword("interface") {
			let interface = InterfaceDeclaration::from_reader(reader)?;
			crate::lexer::utilities::assert_type_annotations(reader, interface.get_position())?;
			Ok(StatementOrDeclaration::Interface(Decorated::new(decorators, interface)))
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
				return Ok(StatementOrDeclaration::DeclareVariable(declare));
			} else if reader.is_keyword("class") {
				let mut class = ClassDeclaration::<StatementPosition>::from_reader(reader)?;
				class.name.is_declare = true;
				class.position.start = start.0;
				Ok(StatementOrDeclaration::Class(Decorated::new(decorators, class)))
			} else if reader.is_keyword("function") || reader.is_keyword("async") {
				let mut function = StatementFunction::from_reader(reader)?;
				function.name.is_declare = true;
				function.position.start = start.0;
				Ok(StatementOrDeclaration::Function(Decorated::new(decorators, function)))
			} else if reader.is_keyword("type") {
				let mut alias = TypeAlias::from_reader(reader)?;
				alias.name.is_declare = true;
				alias.position.start = start.0;
				Ok(StatementOrDeclaration::TypeAlias(alias))
			} else {
				#[cfg(feature = "extras")]
				if reader.is_keyword("namespace") {
					let mut namespace = crate::types::namespace::Namespace::from_reader(reader)?;
					namespace.is_declare = true;
					namespace.position.start = start.0;
					return Ok(StatementOrDeclaration::Namespace(namespace));
				}

				Err(crate::lexer::utilities::expected_one_of_items(
					reader,
					&["let", "const", "var", "class", "type", "async", "function", "namespace"],
				))
			}
		} else if crate::lexer::utilities::is_function_header(reader.get_current()) {
			let function = StatementFunction::from_reader(reader)?;
			Ok(StatementOrDeclaration::Function(Decorated::new(decorators, function)))
		} else if reader.is_keyword("if") {
			IfStatement::from_reader(reader).map(Into::into)
		} else if reader.is_keyword("for") {
			ForLoopStatement::from_reader(reader).map(Into::into)
		} else if reader.is_keyword("switch") {
			SwitchStatement::from_reader(reader).map(Into::into)
		} else if reader.is_keyword("while") {
			WhileStatement::from_reader(reader).map(Into::into)
		} else if reader.is_keyword("do") {
			DoWhileStatement::from_reader(reader).map(Into::into)
		} else if reader.is_keyword("try") {
			TryCatchStatement::from_reader(reader).map(Into::into)
		} else if reader.is_keyword("var") {
			VarVariableStatement::from_reader(reader).map(StatementOrDeclaration::VarVariable)
		} else if reader.is_keyword("with") {
			WithStatement::from_reader(reader).map(StatementOrDeclaration::WithStatement)
		} else if reader.starts_with('{') {
			Block::from_reader(reader).map(StatementOrDeclaration::Block)
		} else if reader.is_keyword_advance("debugger") {
			Ok(StatementOrDeclaration::Debugger(start.with_length("debugger".len())))
		} else if reader.is_keyword_advance("return") {
			if reader.is_semi_colon() {
				Ok(StatementOrDeclaration::Return(ReturnStatement(
					None,
					start.with_length("return".len()),
				)))
			} else {
				let multiple_expression = MultipleExpression::from_reader(reader)?;
				let position = start.union(multiple_expression.get_position());
				Ok(StatementOrDeclaration::Return(ReturnStatement(
					Some(multiple_expression),
					position,
				)))
			}
		} else if reader.is_keyword_advance("break") {
			if reader.is_semi_colon() {
				Ok(StatementOrDeclaration::Break(None, start.with_length("break".len())))
			} else {
				let start = reader.get_start();
				let label = reader.parse_identifier("break identifier", true)?;
				Ok(StatementOrDeclaration::Break(
					Some(label.to_owned()),
					start.union(reader.get_end()),
				))
			}
		} else if reader.is_keyword_advance("continue") {
			if reader.is_semi_colon() {
				Ok(StatementOrDeclaration::Continue(None, start.with_length("continue".len())))
			} else {
				let start = reader.get_start();
				let label = reader.parse_identifier("continue identifier", true)?;
				Ok(StatementOrDeclaration::Continue(
					Some(label.to_owned()),
					start.union(reader.get_end()),
				))
			}
		} else if reader.is_keyword_advance("throw") {
			let expression = MultipleExpression::from_reader(reader)?;
			let position = start.union(expression.get_position());
			Ok(StatementOrDeclaration::Throw(ThrowStatement(Box::new(expression), position)))
		} else if reader.is_operator_advance(";") {
			Ok(StatementOrDeclaration::AestheticSemiColon(start.with_length(1)))
		} else if reader.is_operator_advance("//") {
			let content = reader.parse_comment_literal(false)?;
			let position = start.with_length(2 + content.len());
			if reader.get_options().comments.should_add_comment(content) {
				Ok(StatementOrDeclaration::Comment(content.to_owned(), position))
			} else {
				Ok(StatementOrDeclaration::Empty(position))
			}
		} else if reader.is_operator_advance("/*") {
			let content = reader.parse_comment_literal(true)?;
			let position = start.with_length(4 + content.len());
			if reader.get_options().comments.should_add_comment(content) {
				Ok(StatementOrDeclaration::MultiLineComment(content.to_owned(), position))
			} else {
				Ok(StatementOrDeclaration::Empty(position))
			}
		} else if reader.get_options().partial_syntax && reader.starts_with_expression_delimiter() {
			// Prevents cycic recursion
			let (_found, position) = crate::lexer::utilities::next_item(reader);
			Err(ParseError::new(ParseErrors::ExpectedExpression, position))
		} else {
			#[cfg(feature = "extras")]
			if reader.is_keyword("from") {
				return ImportDeclaration::from_reader_reversed(reader)
					.map(StatementOrDeclaration::Import);
			}

			#[cfg(feature = "full-typescript")]
			if reader.is_keyword("namespace") {
				return crate::types::namespace::Namespace::from_reader(reader).map(Into::into);
			}

			// "let",
			// 		"const",
			// 		"function",
			// 		"class",
			// 		"enum",
			// 		"type",
			// 		"declare",
			// 		"import",
			// 		"export",
			// 		"async",
			// 		"generator",

			let expression = MultipleExpression::from_reader(reader)?;
			Ok(StatementOrDeclaration::Expression(expression))
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			// declarations
			StatementOrDeclaration::Variable(var) => var.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::Class(cls) => cls.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::Import(is) => is.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::Export(es) => es.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::Function(f) => f.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::Interface(id) => id.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::TypeAlias(ta) => ta.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::Enum(r#enum) => {
				r#enum.to_string_from_buffer(buf, options, local)
			}
			StatementOrDeclaration::DeclareVariable(dvd) => {
				dvd.to_string_from_buffer(buf, options, local)
			}
			#[cfg(feature = "full-typescript")]
			StatementOrDeclaration::Namespace(ns) => ns.to_string_from_buffer(buf, options, local),
			// statements
			StatementOrDeclaration::Empty(..) => {}
			StatementOrDeclaration::AestheticSemiColon(..) => buf.push(';'),
			StatementOrDeclaration::If(is) => is.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::ForLoop(fl) => fl.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::Switch(ss) => ss.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::WhileLoop(ws) => ws.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::DoWhileLoop(dws) => {
				dws.to_string_from_buffer(buf, options, local)
			}
			StatementOrDeclaration::TryCatch(tcs) => tcs.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::VarVariable(stmt) => {
				stmt.to_string_from_buffer(buf, options, local)
			}
			StatementOrDeclaration::WithStatement(stmt) => {
				stmt.to_string_from_buffer(buf, options, local)
			}
			StatementOrDeclaration::Return(ReturnStatement(expression, _)) => {
				buf.push_str("return");
				if let Some(expression) = expression {
					buf.push(' ');
					expression.to_string_from_buffer(buf, options, local);
				}
			}
			StatementOrDeclaration::Comment(comment, _) => {
				if options.should_add_comment(comment.as_str()) {
					buf.push_str("//");
					buf.push_str_contains_new_line(comment.as_str().trim_end());
				}
			}
			StatementOrDeclaration::MultiLineComment(comment, _) => {
				if options.should_add_comment(comment) {
					buf.push_str("/*");
					if options.pretty {
						// Perform indent correction
						for (idx, line) in comment.split('\n').enumerate() {
							if idx > 0 {
								buf.push_new_line();
							}
							options.add_indent(local.depth, buf);
							buf.push_str(line.trim());
						}
					} else {
						buf.push_str_contains_new_line(comment.as_str());
					}
					buf.push_str("*/");
				}
			}
			StatementOrDeclaration::Block(block) => {
				block.to_string_from_buffer(buf, options, local.next_level());
			}
			StatementOrDeclaration::Debugger(_) => buf.push_str("debugger"),
			StatementOrDeclaration::Continue(label, _) => {
				buf.push_str("continue");
				if let Some(label) = label {
					buf.push(' ');
					buf.push_str(label);
				}
			}
			StatementOrDeclaration::Break(label, _) => {
				buf.push_str("break");
				if let Some(label) = label {
					buf.push(' ');
					buf.push_str(label);
				}
			}
			StatementOrDeclaration::Expression(val) => {
				val.to_string_on_left(buf, options, local);
			}
			StatementOrDeclaration::Labelled { name, statement, .. } => {
				buf.push_str(name);
				buf.push_str(": ");

				if let StatementOrDeclaration::Empty(..) = statement.0 {
					buf.push(';');
				} else {
					// TODO new line?
					statement.to_string_from_buffer(buf, options, local);
					if statement.0.requires_semi_colon() {
						buf.push(';');
					}
				}
			}
			StatementOrDeclaration::Throw(ThrowStatement(thrown_expression, _)) => {
				buf.push_str("throw ");
				thrown_expression.to_string_from_buffer(buf, options, local);
			}
			StatementOrDeclaration::Marker(_, _) => {
				assert!(options.expect_markers, "Unexpected marker in AST");
			}
			StatementOrDeclaration::Imported { moved, from, originally: _ } => {
				moved.to_string_from_buffer(buf, options, local.change_source(*from));
			}
		}
	}
}

impl StatementOrDeclaration {
	/// Used for skipping in `to_string`
	#[must_use]
	pub fn is_comment(&self) -> bool {
		matches!(
			self,
			StatementOrDeclaration::Comment(..) | StatementOrDeclaration::MultiLineComment(..)
		)
	}

	pub(crate) fn requires_semi_colon(&self) -> bool {
		// TODO Self::Imported { moved, .. } => moved.requires_semi_colon(),
		matches!(
			self,
			StatementOrDeclaration::VarVariable(_)
				| StatementOrDeclaration::Expression(_)
				| StatementOrDeclaration::DoWhileLoop(_)
				| StatementOrDeclaration::Continue(..)
				| StatementOrDeclaration::Break(..)
				| StatementOrDeclaration::Return(..)
				| StatementOrDeclaration::Throw(..)
				| StatementOrDeclaration::Variable(..)
				| StatementOrDeclaration::DeclareVariable(..)
				| StatementOrDeclaration::Export(Decorated {
					on: import_export::export::ExportDeclaration::Default { .. }
						| import_export::export::ExportDeclaration::Item {
							exported: import_export::export::Exportable::ImportAll { .. }
								| import_export::export::Exportable::ImportParts { .. }
								| import_export::export::Exportable::Parts { .. },
							..
						},
					..
				}) | StatementOrDeclaration::Import(..)
				| StatementOrDeclaration::TypeAlias(..)
		)
	}

	pub fn is_declaration(&self) -> bool {
		matches!(
			self,
			StatementOrDeclaration::Variable(_)
				| StatementOrDeclaration::Function(_)
				| StatementOrDeclaration::Class(_)
				| StatementOrDeclaration::Enum(_)
				| StatementOrDeclaration::Interface(_)
				| StatementOrDeclaration::TypeAlias(_)
				| StatementOrDeclaration::DeclareVariable(_)
				| StatementOrDeclaration::Namespace(_)
		)
		// #[cfg(feature = "full-typescript")]
	}
}

#[apply(derive_ASTNode!)]
#[derive(Clone, PartialEq, Debug, Visitable)]
pub struct Statement(pub StatementOrDeclaration);

impl ASTNode for Statement {
	fn get_position(&self) -> Span {
		self.0.get_position()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let statement_or_declaration = StatementOrDeclaration::from_reader(reader)?;
		if statement_or_declaration.is_declaration() {
			todo!("error here")
		} else {
			Ok(Self(statement_or_declaration))
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		self.0.to_string_from_buffer(buf, options, local);
	}
}
