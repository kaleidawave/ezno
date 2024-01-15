mod for_statement;
mod if_statement;
mod switch_statement;
mod try_catch_statement;
mod while_statement;

use crate::{
	declarations::variable::{declarations_to_string, VariableDeclarationItem},
	tokens::token_as_identifier,
};
use derive_enum_from_into::{EnumFrom, EnumTryInto};
use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use std::fmt::Debug;

use super::{
	expressions::MultipleExpression, ASTNode, Block, CursorId, Expression, ParseOptions,
	ParseResult, Span, TSXKeyword, TSXToken, Token, TokenReader,
};
use crate::errors::parse_lexing_error;
pub use for_statement::{ForLoopCondition, ForLoopStatement, ForLoopStatementInitializer};
pub use if_statement::*;
pub use switch_statement::{SwitchBranch, SwitchStatement};
pub use try_catch_statement::TryCatchStatement;
use visitable_derive::Visitable;
pub use while_statement::{DoWhileStatement, WhileStatement};

/// A statement
#[derive(Debug, Clone, Visitable, EnumFrom, EnumTryInto, PartialEqExtras, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[try_into_references(&, &mut)]
#[partial_eq_ignore_types(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum Statement {
	Expression(MultipleExpression),
	/// { ... } statement
	Block(Block),
	// TODO as keyword
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
	// TODO maybe an actual label struct:
	Continue(Option<String>, Span),
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
	// TODO position
	Empty(Span),
	/// TODO under cfg
	#[cfg_attr(feature = "self-rust-tokenize", self_tokenize_field(0))]
	Cursor(#[visit_skip_field] CursorId<Statement>, Span),
}

#[derive(Debug, Clone, Visitable, PartialEqExtras, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct ReturnStatement(pub Option<MultipleExpression>, pub Span);

#[derive(Debug, Clone, Visitable, PartialEqExtras, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct ThrowStatement(pub Box<MultipleExpression>, pub Span);

impl Eq for Statement {}

impl ASTNode for Statement {
	fn get_position(&self) -> &Span {
		get_field_by_type::GetFieldByType::get(self)
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		if let Some(Token(TSXToken::Colon, _)) = reader.peek_n(1) {
			let (name, label_name_pos) = token_as_identifier(reader.next().unwrap(), "label name")?;
			let _colon = reader.next().unwrap();
			let statement = Statement::from_reader(reader, state, options).map(Box::new)?;
			let position = label_name_pos.union(statement.get_position());
			return Ok(Statement::Labelled { name, statement, position });
		}

		let Token(token, _) = &reader.peek().ok_or_else(parse_lexing_error)?;

		match token {
			TSXToken::Cursor(_) => {
				if let Token(TSXToken::Cursor(cursor_id), start) = reader.next().unwrap() {
					Ok(Statement::Cursor(cursor_id.into_cursor(), start.with_length(1)))
				} else {
					unreachable!()
				}
			}
			TSXToken::Keyword(TSXKeyword::Var) => {
				let stmt = VarVariableStatement::from_reader(reader, state, options)?;
				Ok(Statement::VarVariable(stmt))
			}
			TSXToken::Keyword(TSXKeyword::Throw) => {
				let Token(_, start) = reader.next().unwrap();
				let expression = MultipleExpression::from_reader(reader, state, options)?;
				let position = start.union(expression.get_position());
				Ok(Statement::Throw(ThrowStatement(Box::new(expression), position)))
			}
			TSXToken::Keyword(TSXKeyword::If) => {
				IfStatement::from_reader(reader, state, options).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::For) => {
				ForLoopStatement::from_reader(reader, state, options).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::Switch) => {
				SwitchStatement::from_reader(reader, state, options).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::While) => {
				WhileStatement::from_reader(reader, state, options).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::Do) => {
				DoWhileStatement::from_reader(reader, state, options).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::Try) => {
				TryCatchStatement::from_reader(reader, state, options).map(Into::into)
			}
			TSXToken::OpenBrace => Block::from_reader(reader, state, options).map(Statement::Block),
			TSXToken::Keyword(TSXKeyword::Return) => Ok({
				let Token(_, start) = reader.next().unwrap();
				state.add_keyword_at_pos(start.0, TSXKeyword::Return);
				if matches!(
					reader.peek(),
					Some(Token(TSXToken::SemiColon | TSXToken::CloseBrace, _))
				) {
					let position = start.with_length(TSXKeyword::Return.length() as usize);
					Statement::Return(ReturnStatement(None, position))
				} else {
					let multiple_expression =
						MultipleExpression::from_reader(reader, state, options)?;
					let position = start.union(multiple_expression.get_position());
					Statement::Return(ReturnStatement(Some(multiple_expression), position))
				}
			}),
			TSXToken::Keyword(TSXKeyword::Debugger) => {
				Ok(Statement::Debugger(reader.next().unwrap().get_span()))
			}
			TSXToken::Keyword(TSXKeyword::Break) => {
				let break_token = reader.next().unwrap();
				// TODO token is semi-colon
				let label = if matches!(
					reader.peek(),
					Some(Token(TSXToken::SemiColon | TSXToken::CloseBrace, _))
				) {
					None
				} else {
					Some(token_as_identifier(reader.next().unwrap(), "break label")?.0)
				};
				Ok(Statement::Break(label, break_token.get_span()))
			}
			TSXToken::Keyword(TSXKeyword::Continue) => {
				let continue_token = reader.next().unwrap();
				// TODO token is semi-colon
				let label = if matches!(
					reader.peek(),
					Some(Token(TSXToken::SemiColon | TSXToken::CloseBrace, _))
				) {
					None
				} else {
					Some(token_as_identifier(reader.next().unwrap(), "continue label")?.0)
				};
				Ok(Statement::Continue(label, continue_token.get_span()))
			}
			TSXToken::Comment(_) => {
				if let Token(TSXToken::Comment(comment), start) = reader.next().unwrap() {
					let position = start.with_length(comment.len() + 2);
					Ok(Statement::Comment(comment, position))
				} else {
					unreachable!()
				}
			}
			TSXToken::MultiLineComment(_) => {
				if let Token(TSXToken::MultiLineComment(comment), start) = reader.next().unwrap() {
					let position = start.with_length(comment.len() + 2);
					Ok(Statement::MultiLineComment(comment, position))
				} else {
					unreachable!()
				}
			}
			TSXToken::SemiColon => Ok(Statement::Empty(reader.next().unwrap().get_span())),
			// Finally ...!
			_ => {
				let expr = MultipleExpression::from_reader(reader, state, options)?;
				Ok(Statement::Expression(expr))
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			Statement::Cursor(..) => {
				assert!(options.expect_cursors, "tried to to-string cursor");
			}
			Statement::Empty(..) => {
				buf.push(';');
			}
			Statement::Return(ReturnStatement(expression, _)) => {
				buf.push_str("return");
				if let Some(expression) = expression {
					buf.push(' ');
					expression.to_string_from_buffer(buf, options, depth);
				}
			}
			Statement::If(is) => is.to_string_from_buffer(buf, options, depth),
			Statement::ForLoop(fl) => fl.to_string_from_buffer(buf, options, depth),
			Statement::Switch(ss) => ss.to_string_from_buffer(buf, options, depth),
			Statement::WhileLoop(ws) => ws.to_string_from_buffer(buf, options, depth),
			Statement::DoWhileLoop(dws) => dws.to_string_from_buffer(buf, options, depth),
			Statement::TryCatch(tcs) => tcs.to_string_from_buffer(buf, options, depth),
			Statement::Comment(comment, _) => {
				if options.should_add_comment(false) {
					buf.push_str("//");
					buf.push_str_contains_new_line(comment.as_str().trim_end());
				}
			}
			Statement::MultiLineComment(comment, _) => {
				if options.should_add_comment(comment.starts_with('*')) {
					buf.push_str("/*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/");
				}
			}
			Statement::Block(block) => {
				block.to_string_from_buffer(buf, options, depth + 1);
			}
			Statement::Debugger(_) => buf.push_str("debugger"),
			Statement::Continue(label, _) => {
				buf.push_str("continue");
				if let Some(label) = label {
					buf.push(' ');
					buf.push_str(label);
				}
			}
			Statement::Break(label, _) => {
				buf.push_str("break");
				if let Some(label) = label {
					buf.push(' ');
					buf.push_str(label);
				}
			}
			Statement::Expression(val) => {
				val.to_string_from_buffer(buf, options, depth);
			}
			Statement::Labelled { name, statement, .. } => {
				buf.push_str(name);
				buf.push(':');
				statement.to_string_from_buffer(buf, options, depth);
			}
			Statement::Throw(ThrowStatement(thrown_expression, _)) => {
				buf.push_str("throw ");
				thrown_expression.to_string_from_buffer(buf, options, depth);
			}
			Statement::VarVariable(var_stmt) => var_stmt.to_string_from_buffer(buf, options, depth),
		}
	}
}

impl Statement {
	/// Used for skipping in `to_string`
	#[must_use]
	pub fn is_comment(&self) -> bool {
		matches!(self, Statement::Comment(..) | Statement::MultiLineComment(..))
	}

	pub(crate) fn requires_semi_colon(&self) -> bool {
		matches!(
			self,
			Statement::VarVariable(_)
				| Statement::Expression(_)
				| Statement::DoWhileLoop(_)
				| Statement::Continue(..)
				| Statement::Break(..)
				| Statement::Return(..)
				| Statement::Throw(..)
		)
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct VarVariableStatement {
	declarations: Vec<VariableDeclarationItem<Option<Expression>>>,
	position: Span,
}

impl ASTNode for VarVariableStatement {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let Token(_, start) = reader.next().unwrap();
		let mut declarations = Vec::new();
		loop {
			let value =
				VariableDeclarationItem::<Option<Expression>>::from_reader(reader, state, options)?;
			declarations.push(value);
			if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
				reader.next();
			} else {
				break;
			}
		}
		Ok(VarVariableStatement {
			position: start.union(declarations.last().unwrap().get_position()),
			declarations,
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str("var ");
		declarations_to_string(&self.declarations, buf, options, depth);
	}
}
