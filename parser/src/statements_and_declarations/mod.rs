pub mod classes;
pub mod control_flow;
pub mod import_export;
pub mod using;
pub mod variables;
pub mod with;

pub use import_export::{
	export::{self, Exportable},
	import,
};

pub use super::types::{
	declare_variable::DeclareVariableDeclaration,
	enum_declaration::{EnumDeclaration, EnumMember, EnumMemberValue},
	interface::InterfaceDeclaration,
	type_alias::TypeAlias,
};

use crate::extensions::decorators::{
	Decorated, possible_decorators_from_reader, warn_if_possible_decorators_unused,
};
use crate::{Marker, ParseError, ParseErrors, StatementPosition, derive_ASTNode};
use derive_enum_from_into::{EnumFrom, EnumTryInto};
use get_field_by_type::GetFieldByType;
use std::fmt::Debug;
use visitable_derive::Visitable;

use super::{ASTNode, Block, ParseResult, Span};
pub use control_flow::for_statement::{
	ForLoopCondition, ForLoopStatement, ForLoopStatementInitialiser,
};
pub use control_flow::if_statement::*;
pub use control_flow::switch_statement::{SwitchBranch, SwitchStatement};
pub use control_flow::try_catch_statement::TryCatchStatement;
pub use control_flow::while_statement::{DoWhileStatement, WhileStatement};

pub use classes::ClassDeclaration;
pub use export::ExportDeclaration;
use export::export_declaration_from_reader_after_export_keyword;
pub use import::ImportDeclaration;
pub use variables::{
	VarVariableStatement, VariableDeclaration, VariableDeclarationItem, VariableDeclarationKeyword,
};

pub use using::UsingDeclaration;
pub use with::WithStatement;

use crate::expressions::{Expression, MultipleExpression, parse_after_import};

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

#[cfg_attr(target_family = "wasm", tsify::declare)]
pub type ClassDeclarationStatement = ClassDeclaration<StatementPosition>;

/// A statement or declaration. See [Statement] which is a subset of items
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, EnumFrom, EnumTryInto, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[try_into_references(&, &mut)]
#[visit_self]
pub enum StatementOrDeclaration {
	Variable(Box<Exportable<VariableDeclaration>>),
	Function(Box<Decorated<Exportable<StatementFunction>>>),
	Class(Box<Decorated<Exportable<ClassDeclarationStatement>>>),
	Enum(Box<Decorated<Exportable<EnumDeclaration>>>),
	Interface(Box<Decorated<Exportable<InterfaceDeclaration>>>),
	TypeAlias(Box<Decorated<Exportable<TypeAlias>>>),
	/// Special TypeScript only
	DeclareVariable(DeclareVariableDeclaration),
	#[cfg(feature = "full-typescript")]
	Namespace(Exportable<crate::types::namespace::Namespace>),
	// Top level only
	Import(Box<ImportDeclaration>),
	Export(Box<Decorated<ExportDeclaration>>),
	// statement but also sort of declaration 🤷‍♂️
	VarVariable(Exportable<VarVariableStatement>),
	UsingDeclaration(UsingDeclaration),
	// statements
	Expression(MultipleExpression),
	/// { ... } statement
	Block(Block),
	Debugger(Span),
	// Loops and "condition-aries"
	If(Box<IfStatement>),
	ForLoop(Box<ForLoopStatement>),
	Switch(SwitchStatement),
	WhileLoop(WhileStatement),
	DoWhileLoop(DoWhileStatement),
	TryCatch(Box<TryCatchStatement>),
	// Control flow
	Return(ReturnStatement),
	// TODO maybe an actual label struct instead of `Option<String>`
	Continue(Option<String>, Span),
	// TODO maybe an actual label struct instead of `Option<String>`
	Break(Option<String>, Span),
	/// e.g `throw ...`
	Throw(ThrowStatement),
	// CommentsOption
	Comment(String, Span),
	MultiLineComment(String, Span),
	Labelled {
		position: Span,
		name: String,
		statement: Box<Statement>,
	},
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

/// Return with an optional expression
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ReturnStatement(pub Option<MultipleExpression>, pub Span);

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ThrowStatement(pub Box<MultipleExpression>, pub Span);

impl ASTNode for StatementOrDeclaration {
	fn get_position(&self) -> Span {
		*get_field_by_type::GetFieldByType::get(self)
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		reader.skip();
		let start = reader.get_start();

		// TODO assert possible_decorators are used. If they exist but item is not `Decorated`
		// then need to throw a parse error
		let mut possible_decorators = possible_decorators_from_reader(reader)?;

		// TODO
		let first_byte = reader.get_current().as_bytes().first().copied().unwrap_or(0);

		match first_byte {
			b'c' if reader.is_immediate_keyword_advance("const") => {
				// Const can be either variable declaration or `const enum`
				reader.skip_including_comments()?;
				if reader.is_keyword("enum") {
					let enum_declaration = EnumDeclaration::from_reader(reader)?;
					crate::lexer::utilities::assert_type_annotations(
						reader,
						enum_declaration.get_position(),
					)?;
					let declaration = Decorated::new(
						possible_decorators,
						Exportable::not_exported(enum_declaration),
					);
					Ok(StatementOrDeclaration::Enum(Box::new(declaration)))
				} else {
					warn_if_possible_decorators_unused("const", possible_decorators)?;
					let declaration = VariableDeclaration::parse_declarations_after_kind(
						(start, VariableDeclarationKeyword::Const),
						reader,
					)?;
					let declaration = Exportable::not_exported(declaration);
					Ok(StatementOrDeclaration::Variable(Box::new(declaration)))
				}
			}
			b'l' if reader.is_immediate_keyword_advance("let") => {
				warn_if_possible_decorators_unused("let", possible_decorators)?;
				reader.skip_including_comments()?;
				if reader.starts_with_expression_delimiter()
					|| reader.get_current().starts_with('(')
				{
					// , '{', '['])
					expression_statement_after(reader, start, "let")
				} else {
					VariableDeclaration::parse_declarations_after_kind(
						(start, VariableDeclarationKeyword::Let),
						reader,
					)
					.map(Exportable::not_exported)
					.map(Box::new)
					.map(StatementOrDeclaration::Variable)
				}
			}
			b'e' if reader.is_immediate_keyword("enum") => EnumDeclaration::from_reader(reader)
				.map(Exportable::not_exported)
				.map(|on| Decorated::new(possible_decorators, on))
				.map(Box::new)
				.map(StatementOrDeclaration::Enum),
			b'c' if reader.is_immediate_keyword("class") => ClassDeclaration::from_reader(reader)
				.map(Exportable::not_exported)
				.map(|on| Decorated::new(possible_decorators, on))
				.map(Box::new)
				.map(StatementOrDeclaration::Class),
			// This can be a expression start
			b'i' if reader.is_immediate_keyword_advance("interface") => {
				reader.skip_including_comments()?;
				if reader.starts_with_expression_delimiter_or_open_bracket() {
					expression_statement_after(reader, start, "interface")
				} else {
					let interface = InterfaceDeclaration::from_reader_after_keyword(reader, start)?;
					crate::lexer::utilities::assert_type_annotations(
						reader,
						interface.get_position(),
					)?;
					let exported = Exportable::not_exported(interface);
					let decorated = Decorated::new(possible_decorators, exported);
					let item = Box::new(decorated);
					Ok(StatementOrDeclaration::Interface(item))
				}
			}
			b't' if reader.is_immediate_keyword_advance("type") => {
				reader.skip_including_comments()?;
				if reader.starts_with_expression_delimiter_or_open_bracket() {
					expression_statement_after(reader, start, "type")
				} else {
					let alias = {
						let name: StatementPosition =
							crate::ExpressionOrStatementPosition::from_reader(reader)?;
						let parameters = if reader.is_operator_advance("<") {
							let (params, _) = crate::bracketed_items_from_reader(reader, ">")?;
							Some(params)
						} else {
							None
						};
						reader.expect('=')?;
						let references = crate::types::TypeAnnotation::from_reader(reader)?;
						let position = start.union(references.get_position());
						TypeAlias { name, parameters, references, position }
					};
					crate::lexer::utilities::assert_type_annotations(reader, alias.get_position())?;
					if !possible_decorators.is_empty() {
						todo!();
					}
					let alias = Exportable::not_exported(alias);
					let decorated = Decorated::new(possible_decorators, alias);
					let item = Box::new(decorated);
					Ok(StatementOrDeclaration::TypeAlias(item))
				}
			}
			// async or function (generator, server, worker or test)
			b'a' | b'f' | b'g' | b's' | b'w' | b't' if reader.starts_with_function_header() => {
				let mut header = crate::functions::FunctionHeader::from_reader_initial(reader)?;
				reader.skip_including_comments()?;
				if reader.is_immediate_keyword("function") {
					// `to_full` absorbs the function keyword
					let header = header.to_full(reader)?;
					let name: crate::StatementPosition =
						crate::ExpressionOrStatementPosition::from_reader(reader)?;
					let function =
						StatementFunction::from_reader_with_header_and_name(reader, header, name)?;
					let exported = Exportable::not_exported(function);
					let decorated = Decorated::new(possible_decorators, exported);
					let item = Box::new(decorated);
					Ok(StatementOrDeclaration::Function(item))
				} else if reader.starts_with_expression_delimiter()
					|| (!header.is_async() && reader.get_current().starts_with(['(', '{', '[']))
				{
					if let Ok(name) = header.into_expression() {
						expression_statement_after(reader, start, name)
					} else {
						// TODO expected `function`?
						// let (_found, position) = crate::lexer::utilities::next_item(reader);
						// return Err(ParseError::new(ParseErrors::ExpectedExpression, position));
						todo!("expected function");
					}
				} else {
					// `(async(a, b, c))` is valid non-strict syntax
					if !reader.strict_mode() && header.is_async_only() {
						expression_statement_after(reader, start, "async")
					} else {
						let function = StatementFunction::from_reader(reader)?;
						let exported = Exportable::not_exported(function);
						let decorated = Decorated::new(possible_decorators, exported);
						let item = Box::new(decorated);
						Ok(StatementOrDeclaration::Function(item))
					}
				}
			}
			b'e' if reader.is_immediate_keyword_advance("export") => {
				let mut possible_inner_decorators = possible_decorators_from_reader(reader)?;
				possible_decorators.append(&mut possible_inner_decorators);

				reader.skip();
				// TODO as match
				if reader.is_immediate_keyword_advance("const") {
					// Const can be either variable declaration or `const enum`
					reader.skip_including_comments()?;
					if reader.is_operator("enum") {
						let enum_declaration = EnumDeclaration::from_reader(reader)?;
						crate::lexer::utilities::assert_type_annotations(
							reader,
							enum_declaration.get_position(),
						)?;
						let declaration = Decorated::new(
							possible_decorators,
							Exportable::exported(enum_declaration),
						);
						Ok(StatementOrDeclaration::Enum(Box::new(declaration)))
					} else {
						warn_if_possible_decorators_unused("const", possible_decorators)?;
						let declaration = VariableDeclaration::parse_declarations_after_kind(
							(start, VariableDeclarationKeyword::Const),
							reader,
						)?;
						let declaration = Exportable::exported(declaration);
						Ok(StatementOrDeclaration::Variable(Box::new(declaration)))
					}
				} else if reader.is_immediate_keyword("let") {
					warn_if_possible_decorators_unused("let", possible_decorators)?;
					VariableDeclaration::from_reader(reader)
						.map(Exportable::exported)
						.map(Box::new)
						.map(StatementOrDeclaration::Variable)
				} else if reader.is_immediate_keyword("var") {
					warn_if_possible_decorators_unused("var", possible_decorators)?;
					VarVariableStatement::from_reader(reader)
						.map(Exportable::exported)
						.map(StatementOrDeclaration::VarVariable)
				} else if reader.starts_with_function_header() {
					let function =
						StatementFunction::from_reader(reader).map(Exportable::exported)?;
					let item = Decorated::new(possible_decorators, function);
					Ok(StatementOrDeclaration::Function(Box::new(item)))
				} else if reader.is_immediate_keyword("enum") {
					let enum_declaration = EnumDeclaration::from_reader(reader)?;
					crate::lexer::utilities::assert_type_annotations(
						reader,
						enum_declaration.get_position(),
					)?;
					let declaration = Decorated::new(
						possible_decorators,
						Exportable::not_exported(enum_declaration),
					);
					Ok(StatementOrDeclaration::Enum(Box::new(declaration)))
				} else if reader.is_immediate_keyword("class") {
					let declaration = ClassDeclaration::from_reader(reader)?;
					let declaration =
						Decorated::new(possible_decorators, Exportable::not_exported(declaration));
					Ok(StatementOrDeclaration::Class(Box::new(declaration)))
				} else if reader.is_immediate_keyword("interface") {
					let interface = InterfaceDeclaration::from_reader(reader)?;
					crate::lexer::utilities::assert_type_annotations(
						reader,
						interface.get_position(),
					)?;
					let exported = Exportable::exported(interface);
					let decorated = Decorated::new(possible_decorators, exported);
					let item = Box::new(decorated);
					Ok(StatementOrDeclaration::Interface(item))
				} else {
					if reader.is_immediate_keyword("type") {
						let type_alias_result = reader.try_parse(TypeAlias::from_reader);
						if let Ok(type_alias) = type_alias_result {
							crate::lexer::utilities::assert_type_annotations(
								reader,
								type_alias.get_position(),
							)?;
							let exported = Exportable::exported(type_alias);
							let decorated = Decorated::new(possible_decorators, exported);
							let item = Box::new(decorated);
							return Ok(StatementOrDeclaration::TypeAlias(item));
						}
					}
					let on = export_declaration_from_reader_after_export_keyword(start, reader)?;
					let export = Box::new(Decorated::new(possible_decorators, on));
					Ok(StatementOrDeclaration::Export(export))
				}
			}
			b'i' if reader.is_immediate_keyword_advance("import") => {
				// TODO could be parse initial keyword then continue
				warn_if_possible_decorators_unused("import", possible_decorators)?;

				reader.skip_including_comments()?;
				let after = reader.get_current().as_bytes().first();

				if let Some(b'.' | b'(') = after {
					let first = parse_after_import(reader, start)?;
					let expression = Expression::Import(first);
					let precedence = crate::expressions::precedence::COMMA_PRECEDENCE;
					let expression = Expression::from_reader_after_first_expression(
						reader, precedence, expression,
					)?;
					let expression = MultipleExpression::from_first_expression(reader, expression)?;
					Ok(StatementOrDeclaration::Expression(expression))
				} else {
					let parts =
						import::import_specifier_and_parts_from_reader_without_import(reader)?;
					let import = ImportDeclaration::from_reader_with_parts(reader, start, parts)?;
					Ok(StatementOrDeclaration::Import(Box::new(import)))
				}
			}
			b'd' if reader.is_immediate_keyword_advance("declare") => {
				// TODO match
				reader.skip();
				if reader.is_immediate_keyword("let")
					|| reader.is_immediate_keyword("const")
					|| reader.is_immediate_keyword("var")
				{
					let mut declare =
						DeclareVariableDeclaration::from_reader_without_declare(reader)?;
					// TODO pass these down
					declare.decorators = possible_decorators;
					Ok(StatementOrDeclaration::DeclareVariable(declare))
				} else if reader.is_immediate_keyword("class") {
					let mut class = ClassDeclaration::<StatementPosition>::from_reader(reader)?;
					class.name.is_declare = true;
					class.position.start = start.0;
					let class = Exportable::not_exported(class);
					let decorated = Decorated::new(possible_decorators, class);
					Ok(StatementOrDeclaration::Class(Box::new(decorated)))
				} else if reader.is_immediate_keyword("function")
					|| reader.is_immediate_keyword("async")
				{
					let mut function = StatementFunction::from_reader(reader)?;
					function.name.is_declare = true;
					function.position.start = start.0;
					let function = Exportable::not_exported(function);
					let decorated = Decorated::new(possible_decorators, function);
					Ok(StatementOrDeclaration::Function(Box::new(decorated)))
				} else if reader.is_immediate_keyword("type") {
					let mut alias = TypeAlias::from_reader(reader)?;
					alias.name.is_declare = true;
					alias.position.start = start.0;
					let alias = Exportable::not_exported(alias);
					let decorated = Decorated::new(possible_decorators, alias);
					let item = Box::new(decorated);
					Ok(StatementOrDeclaration::TypeAlias(item))
				} else {
					#[cfg(feature = "extras")]
					if reader.is_immediate_keyword("namespace") {
						let mut namespace =
							crate::types::namespace::Namespace::from_reader(reader)?;
						namespace.is_declare = true;
						namespace.position.start = start.0;
						let namespace = Exportable::not_exported(namespace);
						return Ok(StatementOrDeclaration::Namespace(namespace));
					}

					Err(crate::lexer::utilities::expected_one_of_items(
						reader,
						&["let", "const", "var", "class", "type", "async", "function", "namespace"],
					))
				}
			}
			b'u' if reader.is_immediate_keyword_advance("using") => {
				warn_if_possible_decorators_unused("using", possible_decorators)?;
				if reader.starts_with_expression_delimiter_or_open_bracket() {
					expression_statement_after(reader, start, "using")
				} else {
					UsingDeclaration::from_reader_after_keywords(reader, start, false)
						.map(StatementOrDeclaration::UsingDeclaration)
				}
			}
			b'a' if reader.is_immediate_keyword_advance("await") => {
				reader.skip_including_comments()?;
				if reader.is_immediate_keyword_advance("using") {
					reader.skip_including_comments()?;
					if reader.starts_with_expression_delimiter_or_open_bracket() {
						let expression =
							crate::expressions::parse_after_await(reader, start, true)?;
						Ok(StatementOrDeclaration::Expression(expression))
					} else {
						warn_if_possible_decorators_unused("using", possible_decorators)?;
						UsingDeclaration::from_reader_after_keywords(reader, start, true)
							.map(StatementOrDeclaration::UsingDeclaration)
					}
				} else if reader.is_operator_advance(":") {
					reader.skip_including_comments()?;
					let statement = Statement::from_reader(reader)?;
					check_semi_colon(&statement.0, reader)?;
					let position = start.union(statement.get_position());
					let statement = Box::new(statement);
					let name = "await".to_owned();
					Ok(StatementOrDeclaration::Labelled { name, statement, position })
				} else {
					warn_if_possible_decorators_unused("await", possible_decorators)?;
					let expression = crate::expressions::parse_after_await(reader, start, false)?;
					Ok(StatementOrDeclaration::Expression(expression))
				}
			}
			b'i' if reader.is_immediate_keyword("if") => {
				warn_if_possible_decorators_unused("if", possible_decorators)?;
				IfStatement::from_reader(reader).map(Box::new).map(Into::into)
			}
			b'f' if reader.is_immediate_keyword("for") => {
				warn_if_possible_decorators_unused("for", possible_decorators)?;
				ForLoopStatement::from_reader(reader).map(Box::new).map(Into::into)
			}
			b's' if reader.is_immediate_keyword("switch") => {
				warn_if_possible_decorators_unused("switch", possible_decorators)?;
				SwitchStatement::from_reader(reader).map(Into::into)
			}
			b'w' if reader.is_immediate_keyword("while") => {
				warn_if_possible_decorators_unused("while", possible_decorators)?;
				WhileStatement::from_reader(reader).map(Into::into)
			}
			b'd' if reader.is_immediate_keyword("do") => {
				warn_if_possible_decorators_unused("do", possible_decorators)?;
				DoWhileStatement::from_reader(reader).map(Into::into)
			}
			b't' if reader.is_immediate_keyword("try") => {
				warn_if_possible_decorators_unused("try", possible_decorators)?;
				TryCatchStatement::from_reader(reader).map(Box::new).map(Into::into)
			}
			b'v' if reader.is_immediate_keyword("var") => {
				warn_if_possible_decorators_unused("var", possible_decorators)?;
				VarVariableStatement::from_reader(reader)
					.map(Exportable::not_exported)
					.map(StatementOrDeclaration::VarVariable)
			}
			b'w' if reader.is_immediate_keyword("with") => {
				warn_if_possible_decorators_unused("with", possible_decorators)?;
				WithStatement::from_reader(reader).map(StatementOrDeclaration::WithStatement)
			}
			b'{' => {
				warn_if_possible_decorators_unused("block", possible_decorators)?;
				Block::from_reader(reader).map(StatementOrDeclaration::Block)
			}
			b'd' if reader.is_immediate_keyword_advance("debugger") => {
				warn_if_possible_decorators_unused("debugger", possible_decorators)?;
				Ok(StatementOrDeclaration::Debugger(start.with_length("debugger".len())))
			}
			b'r' if reader.is_immediate_keyword_advance("return") => {
				warn_if_possible_decorators_unused("return", possible_decorators)?;
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
			}
			b'b' if reader.is_immediate_keyword_advance("break") => {
				warn_if_possible_decorators_unused("break", possible_decorators)?;
				if reader.is_semi_colon() {
					Ok(StatementOrDeclaration::Break(None, start.with_length("break".len())))
				} else {
					let start = reader.get_start();
					let label = reader.parse_identifier("break identifier", true)?.into_owned();
					Ok(StatementOrDeclaration::Break(Some(label), start.union(reader.get_end())))
				}
			}
			b'c' if reader.is_immediate_keyword_advance("continue") => {
				warn_if_possible_decorators_unused("continue", possible_decorators)?;
				if reader.is_semi_colon() {
					// WIP fix
					reader.accept_semi_colon();
					Ok(StatementOrDeclaration::Continue(None, start.with_length("continue".len())))
				} else {
					let start = reader.get_start();
					let label = reader.parse_identifier("continue identifier", true)?.into_owned();
					Ok(StatementOrDeclaration::Continue(Some(label), start.union(reader.get_end())))
				}
			}
			b't' if reader.is_immediate_keyword_advance("throw") => {
				warn_if_possible_decorators_unused("throw", possible_decorators)?;
				let expression = MultipleExpression::from_reader(reader)?;
				let position = start.union(expression.get_position());
				Ok(StatementOrDeclaration::Throw(ThrowStatement(Box::new(expression), position)))
			}
			b';' => {
				reader.advance(1);
				warn_if_possible_decorators_unused("semi-colon", possible_decorators)?;
				Ok(StatementOrDeclaration::AestheticSemiColon(start.with_length(1)))
			}
			b'/' if reader.is_operator_advance("//") => {
				warn_if_possible_decorators_unused("comment", possible_decorators)?;
				let content = reader.parse_comment_literal(false)?;
				let position = start.with_length(2 + content.len());
				if reader.get_options().comments.should_add_comment(content) {
					Ok(StatementOrDeclaration::Comment(content.to_owned(), position))
				} else {
					Ok(StatementOrDeclaration::Empty(position))
				}
			}
			b'/' if reader.is_operator_advance("/*") => {
				warn_if_possible_decorators_unused("comment", possible_decorators)?;
				let content = reader.parse_comment_literal(true)?.to_owned();
				let position = start.with_length(4 + content.len());
				if reader.get_options().comments.should_add_comment(&content) {
					Ok(StatementOrDeclaration::MultiLineComment(content, position))
				} else {
					Ok(StatementOrDeclaration::Empty(position))
				}
			}
			b'<' if reader.starts_with_slice("<!--") => {
				let _ = reader.parse_html_comment_literal()?;
				Ok(StatementOrDeclaration::Empty(start.union(reader.get_end())))
			}
			b'-' if reader.starts_with_slice("-->") => {
				let _ = reader.parse_html_comment_literal()?;
				Ok(StatementOrDeclaration::Empty(start.union(reader.get_end())))
			}
			_ => {
				if reader.get_options().features.partial_syntax
					&& reader.starts_with_expression_delimiter()
				{
					// Prevents cycic recursion
					let (_found, position) = crate::lexer::utilities::next_item(reader);
					return Err(ParseError::new(ParseErrors::ExpectedExpression, position));
				} else if reader.get_options().features.interpolation_points
					&& reader.is_immediate_keyword_advance(crate::marker::MARKER)
				{
					let position = start.with_length(0);
					let marker_id = reader.new_partial_point_marker(position);
					return Ok(StatementOrDeclaration::Marker(marker_id, position));
				}

				// TODO what about markers here?
				#[cfg(feature = "extras")]
				if reader.is_immediate_keyword("from") {
					let reversed_import_result =
						reader.try_parse(ImportDeclaration::from_reader_reversed);
					if let Ok(reversed_import) = reversed_import_result {
						warn_if_possible_decorators_unused("import", possible_decorators)?;
						return Ok(StatementOrDeclaration::Import(Box::new(reversed_import)));
					}
				}

				#[cfg(feature = "full-typescript")]
				if reader.is_immediate_keyword("namespace") {
					warn_if_possible_decorators_unused("namespace", possible_decorators)?;
					let namespace = crate::types::namespace::Namespace::from_reader(reader)?;

					return Ok(StatementOrDeclaration::Namespace(Exportable::not_exported(
						namespace,
					)));
				}

				// "let" | "const" | "function" | "class" | "enum" | "type" | "declare" |
				// "import" | "export" | "async" | "generator"

				warn_if_possible_decorators_unused("expression", possible_decorators)?;
				let expression = MultipleExpression::from_reader(reader)?;

				if reader.is_operator_advance(":") {
					let position = expression.get_position();
					let inner = expression.get_inner();
					if let Ok((name, _pos)) = inner.as_identifier() {
						reader.skip_including_comments()?;
						let statement = Statement::from_reader(reader)?;
						check_semi_colon(&statement.0, reader)?;
						let position = start.union(statement.get_position());
						let statement = Box::new(statement);
						let name = name.to_owned();
						return Ok(StatementOrDeclaration::Labelled { name, statement, position });
					} else {
						return Err(ParseError::new(ParseErrors::InvalidStatementLabel, position));
					}
				}

				Ok(StatementOrDeclaration::Expression(expression))
			}
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
			StatementOrDeclaration::UsingDeclaration(ud) => {
				ud.to_string_from_buffer(buf, options, local);
			}
			StatementOrDeclaration::Enum(r#enum) => {
				r#enum.to_string_from_buffer(buf, options, local);
			}
			StatementOrDeclaration::DeclareVariable(dvd) => {
				dvd.to_string_from_buffer(buf, options, local);
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
				dws.to_string_from_buffer(buf, options, local);
			}
			StatementOrDeclaration::TryCatch(tcs) => tcs.to_string_from_buffer(buf, options, local),
			StatementOrDeclaration::VarVariable(stmt) => {
				stmt.to_string_from_buffer(buf, options, local);
			}
			StatementOrDeclaration::WithStatement(stmt) => {
				stmt.to_string_from_buffer(buf, options, local);
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

fn expression_statement_after(
	reader: &mut crate::Lexer,
	start: source_map::Start,
	identifier: &str,
) -> ParseResult<StatementOrDeclaration> {
	let initial =
		Expression::VariableReference(identifier.to_owned(), start.with_length(identifier.len()));
	crate::expressions::Expression::from_reader_after_first_expression(reader, 0, initial)
		.map(MultipleExpression)
		.map(StatementOrDeclaration::Expression)
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

	#[allow(clippy::match_same_arms)]
	pub(crate) fn requires_semi_colon(&self) -> bool {
		match self {
			// For some reason
			StatementOrDeclaration::Export(declaration) => {
				// use crate::ExpressionOrStatementPosition;
				if let ExportDeclaration::Default { expression, .. } = &declaration.on {
					if let crate::Expression::ClassExpression(_cls) = &**expression {
						false
					} else if let crate::Expression::ExpressionFunction(_func) = &**expression {
						/* && func.name.as_option_variable_identifier().is_some() && func.header.is_generator() */
						false
					} else {
						true
					}
				} else {
					true
				}
			}
			// Hmm esid: sec-rules-of-automatic-semicolon-insertion. maybe reader ends with ) ?
			StatementOrDeclaration::DoWhileLoop(_) => false,
			StatementOrDeclaration::VarVariable(_)
			| StatementOrDeclaration::Continue(..)
			| StatementOrDeclaration::Break(..)
			| StatementOrDeclaration::Return(..)
			| StatementOrDeclaration::Throw(..)
			| StatementOrDeclaration::Variable(..)
			| StatementOrDeclaration::DeclareVariable(..)
			| StatementOrDeclaration::Import(..)
			| StatementOrDeclaration::TypeAlias(..) => true,
			StatementOrDeclaration::Expression(_expr) => true,
			StatementOrDeclaration::Imported { moved, .. } => moved.requires_semi_colon(),
			_ => false,
		}
	}

	#[allow(clippy::match_same_arms)]
	#[must_use]
	pub fn is_declaration(&self) -> bool {
		match self {
			StatementOrDeclaration::Variable(_)
			// TODO strict mode | StatementOrDeclaration::Function(_)
			| StatementOrDeclaration::Class(_)
			| StatementOrDeclaration::Enum(_)
			| StatementOrDeclaration::Interface(_)
			| StatementOrDeclaration::TypeAlias(_)
			| StatementOrDeclaration::DeclareVariable(_) => true,
			#[cfg(feature = "full-typescript")]
			StatementOrDeclaration::Namespace(_) => true,
			_ => false,
		}
	}
}

#[apply(derive_ASTNode!)]
#[derive(Clone, Debug, Visitable)]
pub struct Statement(pub StatementOrDeclaration);

impl ASTNode for Statement {
	fn get_position(&self) -> Span {
		self.0.get_position()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		// TEMP fix
		let start = reader.get_start();
		if reader.is_immediate_keyword_advance("let") {
			return expression_statement_after(reader, start, "let").map(Statement);
		}

		let statement_or_declaration = StatementOrDeclaration::from_reader(reader)?;
		if statement_or_declaration.is_declaration() {
			Err(ParseError::new(
				ParseErrors::ExpectedStatement,
				statement_or_declaration.get_position(),
			))
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

pub(crate) fn check_semi_colon(
	item: &StatementOrDeclaration,
	reader: &mut crate::Lexer,
) -> ParseResult<()> {
	if item.requires_semi_colon() { reader.expect_semi_colon() } else { Ok(()) }
}
