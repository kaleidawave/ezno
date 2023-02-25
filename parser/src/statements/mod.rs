pub mod classes;
mod enum_statement;
mod export;
mod for_statement;
mod if_statement;
mod import;
mod interface;
mod switch_statement;
mod types;
mod variable;
mod while_statement;

use crate::tsx_keywords;
use derive_enum_from_into::{EnumFrom, EnumTryInto};
use derive_partial_eq_extras::PartialEqExtras;
use iterator_endiate::EndiateIteratorExt;
use std::{borrow::Cow, fmt::Debug};

pub type StatementFunctionBase = crate::functions::GeneralFunctionBase<StatementPosition>;
pub type StatementFunction = crate::FunctionBase<StatementFunctionBase>;

use super::{
	expressions::MultipleExpression, ASTNode, Block, BlockId, CursorId, Expression,
	GenericTypeConstraint, Keyword, ParseError, ParseResult, ParseSettings, Span, TSXKeyword,
	TSXToken, Token, TokenReader, TypeId, TypeReference, VariableId,
};
use crate::{
	errors::parse_lexing_error,
	extensions::decorators::{self, Decorated},
	extractor::{ExtractedFunction, GetFunction},
	StatementPosition,
};
pub use classes::ClassDeclaration;
pub use enum_statement::{EnumDeclaration, EnumMember};
pub use export::{ExportStatement, Exportable};
pub use for_statement::{ForLoopCondition, ForLoopStatement, ForLoopStatementInitializer};
pub use if_statement::*;
pub use import::{ImportPart, ImportStatement, ImportStatementId};
pub(crate) use interface::parse_interface_members;
pub use interface::*;
pub use switch_statement::{SwitchBranch, SwitchStatement};
pub use types::*;
pub use variable::{
	DeclarationExpression, VariableDeclaration, VariableKeyword, VariableStatement,
};
use visitable_derive::Visitable;
pub use while_statement::{DoWhileStatement, WhileStatement};

/// A statement
/// TODO Import and Export are only allowed in top level modules. Could there be a `TopLevelStatement` which
/// extends Statement and includes them, therefore retaining type safety
///
/// Throw is on [Expression] (non-standard)
#[derive(Debug, Clone, Visitable, EnumFrom, EnumTryInto, PartialEqExtras)]
#[try_into_references(&, &mut)]
#[partial_eq_ignore_types(Span)]
#[visit_self]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum Statement {
	Expression(MultipleExpression),
	/// { ... } statement
	Block(Block),
	Debugger(Span),
	// Declarations:
	VariableDeclaration(VariableStatement),
	FunctionDeclaration(Decorated<StatementFunction>),
	ExtractedFunction(ExtractedFunction<StatementFunctionBase>),
	ClassDeclaration(Decorated<ClassDeclaration<StatementPosition>>),
	EnumDeclaration(Decorated<EnumDeclaration>),
	InterfaceDeclaration(Decorated<InterfaceDeclaration>),
	TypeAlias(TypeAlias),
	// Special TS only
	DeclareVariableDeclaration(DeclareVariableDeclaration),
	DeclareFunctionDeclaration(DeclareFunctionDeclaration),
	#[from_ignore]
	DeclareInterfaceDeclaration(InterfaceDeclaration),
	// Loops and "condition-aries"
	IfStatement(IfStatement),
	ForLoopStatement(ForLoopStatement),
	SwitchStatement(SwitchStatement),
	WhileStatement(WhileStatement),
	DoWhileStatement(DoWhileStatement),
	// Control flow
	ReturnStatement(Keyword<tsx_keywords::Return>, Option<MultipleExpression>),
	// TODO labels on these:
	Continue(Span),
	Break(Span),
	// Comments
	Comment(String, Span),
	MultiLineComment(String, Span),
	// Top level only
	ImportStatement(ImportStatement),
	ExportStatement(Decorated<ExportStatement>),
	/// TODO under cfg
	#[self_tokenize_field(0)]
	Cursor(#[visit_skip_field] CursorId<Statement>, Span),
}

impl Eq for Statement {}

impl ASTNode for Statement {
	fn get_position(&self) -> Cow<Span> {
		match self {
			Statement::FunctionDeclaration(stmt) => stmt.get_position(),
			Statement::ImportStatement(stmt) => stmt.get_position(),
			Statement::ExportStatement(stmt) => stmt.get_position(),
			Statement::Expression(val) => val.get_position(),
			Statement::InterfaceDeclaration(dec) => dec.get_position(),
			Statement::VariableDeclaration(dec) => dec.get_position(),
			Statement::Debugger(pos)
			| Statement::Continue(pos)
			| Statement::Cursor(_, pos)
			| Statement::Break(pos)
			| Statement::Comment(_, pos)
			| Statement::MultiLineComment(_, pos) => Cow::Borrowed(pos),
			Statement::ReturnStatement(kw, expr) => {
				if let Some(expr) = expr {
					Cow::Owned(kw.1.union(&expr.get_position()))
				} else {
					Cow::Borrowed(&kw.1)
				}
			}
			Statement::ClassDeclaration(cd) => cd.get_position(),
			Statement::IfStatement(is) => is.get_position(),
			Statement::ForLoopStatement(fl) => fl.get_position(),
			Statement::SwitchStatement(ss) => ss.get_position(),
			Statement::WhileStatement(ws) => ws.get_position(),
			Statement::DoWhileStatement(dws) => dws.get_position(),
			Statement::TypeAlias(alias) => alias.get_position(),
			Statement::DeclareFunctionDeclaration(dfd) => dfd.get_position(),
			Statement::DeclareVariableDeclaration(dvd) => dvd.get_position(),
			Statement::DeclareInterfaceDeclaration(did) => did.get_position(),
			Statement::Block(blk) => blk.get_position(),
			Statement::EnumDeclaration(enum_declaration) => enum_declaration.get_position(),
			Statement::ExtractedFunction(..) => todo!(),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		// TODO assert decorators are used. If they exist but item is not `Decorated`
		// then need to throw a parse error
		let decorators = decorators::decorators_from_reader(reader, state, settings)?;

		let Token(token, _) = &reader.peek().ok_or_else(parse_lexing_error)?;
		match token {
			TSXToken::Cursor(_) => {
				if let Token(TSXToken::Cursor(cursor_id), span) = reader.next().unwrap() {
					Ok(Statement::Cursor(cursor_id.into_cursor(), span))
				} else {
					unreachable!()
				}
			}
			TSXToken::Keyword(TSXKeyword::Let) | TSXToken::Keyword(TSXKeyword::Var) => {
				VariableStatement::from_reader(reader, state, settings)
					.map(Statement::VariableDeclaration)
			}
			// Const can be either variable declaration or const enum
			TSXToken::Keyword(TSXKeyword::Const) => {
				// Lol I think there needs to be a change in tokenizer-lib
				let after_const = reader.scan(move |_, _| true);
				if let Some(Token(TSXToken::Keyword(TSXKeyword::Enum), _)) = after_const {
					EnumDeclaration::from_reader(reader, state, settings)
						.map(|on| Statement::EnumDeclaration(Decorated { decorators, on }))
				} else {
					VariableStatement::from_reader(reader, state, settings)
						.map(Statement::VariableDeclaration)
				}
			}
			TSXToken::Keyword(TSXKeyword::Enum) => {
				EnumDeclaration::from_reader(reader, state, settings)
					.map(|on| Statement::EnumDeclaration(Decorated { decorators, on }))
			}
			TSXToken::Keyword(TSXKeyword::Generator) if settings.generator_keyword => {
				let func = StatementFunction::from_reader(reader, state, settings)?;
				if !decorators.is_empty() {
					todo!();
				}
				let extracted = state.function_extractor.new_extracted_function(func);
				Ok(Statement::ExtractedFunction(extracted))
			}
			TSXToken::Keyword(TSXKeyword::Function | TSXKeyword::Async) => {
				let func = StatementFunction::from_reader(reader, state, settings)?;
				if !decorators.is_empty() {
					todo!();
				}
				let extracted = state.function_extractor.new_extracted_function(func);
				Ok(Statement::ExtractedFunction(extracted))
			}
			TSXToken::OpenBrace => {
				Block::from_reader(reader, state, settings).map(Statement::Block)
			}
			TSXToken::Keyword(TSXKeyword::Class) => {
				let Token(_, class_token_pos) = reader.next().unwrap();
				let class_keyword = Keyword::new(class_token_pos);
				ClassDeclaration::from_reader_sub_class_keyword(
					reader,
					state,
					settings,
					class_keyword,
				)
				.map(|on| Statement::ClassDeclaration(Decorated { decorators, on }))
			}
			TSXToken::Keyword(TSXKeyword::If) => {
				IfStatement::from_reader(reader, state, settings).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::For) => {
				ForLoopStatement::from_reader(reader, state, settings).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::Switch) => {
				SwitchStatement::from_reader(reader, state, settings).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::While) => {
				WhileStatement::from_reader(reader, state, settings).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::Do) => {
				todo!()
			}
			TSXToken::Keyword(TSXKeyword::Export) => {
				ExportStatement::from_reader_2(reader, state, settings)
					.map(|on| Statement::ExportStatement(Decorated { decorators, on }))
			}
			TSXToken::Keyword(TSXKeyword::Import) => {
				ImportStatement::from_reader(reader, state, settings).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::Interface) => {
				InterfaceDeclaration::from_reader(reader, state, settings)
					.map(|on| Statement::InterfaceDeclaration(Decorated { decorators, on }))
			}
			TSXToken::Keyword(TSXKeyword::Type) => {
				TypeAlias::from_reader(reader, state, settings).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::Return) => Ok({
				let Token(_, return_span) = reader.next().unwrap();
				let expression = if matches!(
					reader.peek(),
					Some(Token(TSXToken::SemiColon | TSXToken::CloseBrace, _))
				) {
					None
				} else {
					Some(MultipleExpression::from_reader(reader, state, settings)?)
				};
				Statement::ReturnStatement(Keyword::new(return_span), expression)
			}),
			TSXToken::Keyword(TSXKeyword::Debugger) => {
				let Token(_, span) = reader.next().unwrap();
				Ok(Statement::Debugger(span))
			}
			TSXToken::Keyword(TSXKeyword::Break) => {
				let Token(_, span) = reader.next().unwrap();
				Ok(Statement::Break(span))
			}
			TSXToken::Keyword(TSXKeyword::Continue) => {
				let Token(_, span) = reader.next().unwrap();
				Ok(Statement::Continue(span))
			}
			TSXToken::Comment(_) => Ok({
				let (comment, position) =
					if let Token(TSXToken::Comment(comment), position) = reader.next().unwrap() {
						(comment, position)
					} else {
						unreachable!()
					};
				Statement::Comment(comment, position)
			}),
			TSXToken::MultiLineComment(_) => {
				if let Token(TSXToken::MultiLineComment(comment), position) = reader.next().unwrap()
				{
					Ok(Statement::MultiLineComment(comment, position))
				} else {
					unreachable!()
				}
			}
			TSXToken::Keyword(TSXKeyword::Declare) => {
				let declare_span = reader.next().unwrap().1;
				crate::modules::parse_declare_item(
					reader,
					state,
					settings,
					decorators,
					declare_span,
				)
				.map(|ty_def_mod_stmt| match ty_def_mod_stmt {
					crate::TypeDefinitionModuleStatement::VariableDeclaration(
						declare_var_statement,
					) => Statement::DeclareVariableDeclaration(declare_var_statement),
					crate::TypeDefinitionModuleStatement::FunctionDeclaration(
						declare_func_statement,
					) => Statement::DeclareFunctionDeclaration(declare_func_statement),
					crate::TypeDefinitionModuleStatement::ClassDeclaration(_) => todo!("error"),
					crate::TypeDefinitionModuleStatement::InterfaceDeclaration(_) => todo!("error"),
					crate::TypeDefinitionModuleStatement::TypeAlias(_) => todo!("error"),
					crate::TypeDefinitionModuleStatement::Namespace(_) => todo!(),
					crate::TypeDefinitionModuleStatement::Comment(_) => unreachable!(),
					crate::TypeDefinitionModuleStatement::LocalTypeAlias(_) => todo!(),
					crate::TypeDefinitionModuleStatement::LocalVariableDeclaration(_) => {
						todo!()
					}
				})
			}
			// Finally ...!
			_ => {
				MultipleExpression::from_reader(reader, state, settings).map(Statement::Expression)
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		match self {
			Statement::Cursor(..) => {
				// TODO panic under setting
			}
			Statement::ReturnStatement(_, expression) => {
				// TODO check iife properties of expression
				buf.push_str("return");
				if let Some(expression) = expression {
					buf.push(' ');
					expression.to_string_from_buffer(buf, settings, depth);
				}
			}
			Statement::FunctionDeclaration(f) => f.to_string_from_buffer(buf, settings, depth),
			Statement::ExtractedFunction(func) => {
				if let Some(func) =
					GetFunction::<StatementFunctionBase>::get_function_ref(&settings.1, func.0)
				{
					func.to_string_from_buffer(buf, settings, depth)
				}
			}
			Statement::VariableDeclaration(var) => var.to_string_from_buffer(buf, settings, depth),
			Statement::ClassDeclaration(cls) => cls.to_string_from_buffer(buf, settings, depth),
			Statement::IfStatement(is) => is.to_string_from_buffer(buf, settings, depth),
			Statement::ForLoopStatement(fl) => fl.to_string_from_buffer(buf, settings, depth),
			Statement::SwitchStatement(ss) => ss.to_string_from_buffer(buf, settings, depth),
			Statement::WhileStatement(ws) => ws.to_string_from_buffer(buf, settings, depth),
			Statement::DoWhileStatement(dws) => dws.to_string_from_buffer(buf, settings, depth),
			Statement::ImportStatement(is) => is.to_string_from_buffer(buf, settings, depth),
			Statement::ExportStatement(es) => es.to_string_from_buffer(buf, settings, depth),
			Statement::InterfaceDeclaration(id) => id.to_string_from_buffer(buf, settings, depth),
			Statement::TypeAlias(ta) => ta.to_string_from_buffer(buf, settings, depth),
			Statement::Comment(comment, _) => {
				if settings.0.should_add_comment() {
					buf.push_str("//");
					buf.push_str_contains_new_line(comment.as_str().trim_end());
				}
			}
			Statement::MultiLineComment(comment, _) => {
				if settings.0.should_add_comment() {
					buf.push_str("/*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/");
				}
			}
			// TODO should skip these under no types
			Statement::DeclareFunctionDeclaration(dfd) => {
				dfd.to_string_from_buffer(buf, settings, depth)
			}
			Statement::DeclareVariableDeclaration(dvd) => {
				dvd.to_string_from_buffer(buf, settings, depth)
			}
			Statement::DeclareInterfaceDeclaration(did) => {
				buf.push_str("declare ");
				did.to_string_from_buffer(buf, settings, depth)
			}
			Statement::Block(block) => {
				block.to_string_from_buffer(buf, settings, depth + 1);
			}
			Statement::Debugger(_) => buf.push_str("debugger"),
			Statement::Continue(_) => buf.push_str("continue"),
			Statement::Break(_) => buf.push_str("break"),
			Statement::EnumDeclaration(enum_declaration) => {
				enum_declaration.to_string_from_buffer(buf, settings, depth)
			}
			Statement::Expression(val) => {
				if let Some(body) = val.is_iife(&settings.1) {
					match body {
						crate::expressions::ExpressionOrBlock::Expression(expression) => {
							expression.to_string_from_buffer(buf, settings, depth)
						}
						crate::expressions::ExpressionOrBlock::Block(block) => {
							// TODO if block returns then this will break:::
							block.to_string_from_buffer(buf, settings, depth);
						}
					}
				} else {
					val.to_string_from_buffer(buf, settings, depth);
				}
			}
		}
	}
}

impl Statement {
	/// Used for skipping in to_string
	pub fn is_ts_statement(&self) -> bool {
		matches!(
			self,
			Statement::TypeAlias(..)
				| Statement::InterfaceDeclaration(..)
				| Statement::DeclareFunctionDeclaration(..)
				| Statement::DeclareVariableDeclaration(..)
				| Statement::DeclareInterfaceDeclaration(..)
		)
	}

	/// Used for skipping in to_string
	pub fn is_comment(&self) -> bool {
		matches!(self, Statement::Comment(..) | Statement::MultiLineComment(..))
	}

	/// These need ; after wards for next statement
	pub(crate) fn needs_delimiter(&self) -> bool {
		matches!(
			self,
			Statement::Expression(..)
				| Statement::VariableDeclaration(..)
				| Statement::ReturnStatement(..)
				| Statement::ImportStatement(..)
				| Statement::TypeAlias(..)
				| Statement::Debugger(_)
		)
	}
}

/// Parse statements, regardless of bracing or not
pub(crate) fn parse_statements(
	reader: &mut impl TokenReader<TSXToken, Span>,
	state: &mut crate::ParsingState,
	settings: &ParseSettings,
) -> ParseResult<(Vec<Statement>, BlockId)> {
	let mut statements = Vec::new();
	let block_id = BlockId::new();
	while let Some(Token(token_type, _)) = reader.peek() {
		if let TSXToken::EOS | TSXToken::CloseBrace = token_type {
			break;
		}

		let value = Statement::from_reader(reader, state, settings)?;

		// Register hoisted functions here
		// TODO nested blocks? Interfaces...?
		if let Statement::ExtractedFunction(func) = &value {
			state.hoisted_functions.entry(block_id).or_default().push(func.0);
		}

		statements.push(value);

		// Skip over semi colons at the end of statements
		if let Some(Token(TSXToken::SemiColon, _)) = reader.peek() {
			reader.next();
		}
	}
	Ok((statements, block_id))
}

pub fn statements_to_string<T: source_map::ToString>(
	statements: &[Statement],
	buf: &mut T,
	settings: &crate::ToStringSettingsAndData,
	depth: u8,
) {
	for (at_end, statement) in statements.iter().endiate() {
		settings.0.add_indent(depth, buf);
		statement.to_string_from_buffer(buf, settings, depth);
		if !at_end {
			if statement.needs_delimiter() {
				buf.push(';');
			}
			let ignored_ts_stmt = !settings.0.include_types && statement.is_ts_statement();
			let ignored_comment = !settings.0.include_comments && statement.is_comment();
			if settings.0.pretty && !(ignored_ts_stmt || ignored_comment) {
				buf.push_new_line();
			}
		}
	}
}
