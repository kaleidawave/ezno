mod for_statement;
mod if_statement;
mod switch_statement;
mod try_catch_statement;
mod while_statement;

use crate::{
	declarations::variable::{declarations_to_string, VariableDeclarationItem},
	tokens::token_as_identifier,
	tsx_keywords,
};
use derive_enum_from_into::{EnumFrom, EnumTryInto};
use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use std::fmt::Debug;

use super::{
	expressions::MultipleExpression, ASTNode, Block, CursorId, Expression, Keyword, ParseOptions,
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
#[visit_self]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum Statement {
	Expression(MultipleExpression),
	/// { ... } statement
	Block(Block),
	// TODO as keyword
	Debugger(Span),
	// Loops and "condition-aries"
	IfStatement(IfStatement),
	ForLoopStatement(ForLoopStatement),
	SwitchStatement(SwitchStatement),
	WhileStatement(WhileStatement),
	DoWhileStatement(DoWhileStatement),
	TryCatchStatement(TryCatchStatement),
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
	#[self_tokenize_field(0)]
	Cursor(#[visit_skip_field] CursorId<Statement>, Span),
}

#[derive(Debug, Clone, Visitable, PartialEqExtras, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct ReturnStatement(Keyword<tsx_keywords::Return>, Option<MultipleExpression>, Span);

#[derive(Debug, Clone, Visitable, PartialEqExtras, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct ThrowStatement(Keyword<tsx_keywords::Throw>, Box<MultipleExpression>, Span);

impl Eq for Statement {}

impl ASTNode for Statement {
	fn get_position(&self) -> &Span {
		get_field_by_type::GetFieldByType::get(self)
	}
	// 	match self {
	// 		Statement::Expression(val) => val.get_position(),
	// 		Statement::VarVariable(val) => val.get_position(),
	// 		Statement::Debugger(pos)
	// 		| Statement::Continue(_, pos)
	// 		| Statement::Break(_, pos)
	// 		| Statement::Cursor(_, pos)
	// 		| Statement::Comment(_, pos)
	// 		| Statement::Empty(pos)
	// 		| Statement::Labelled { position: pos, .. }
	// 		| Statement::MultiLineComment(_, pos) => pos,
	// 		Statement::Return(kw, expr) => {
	// 			if let Some(expr) = expr {
	// 				Cow::Owned(kw.1.union(&expr.get_position()))
	// 			} else {
	// 				&kw.1
	// 			}
	// 		}
	// 		Statement::Throw(kw, expr) => Cow::Owned(kw.1.union(&expr.get_position())),
	// 		Statement::IfStatement(is) => is.get_position(),
	// 		Statement::ForLoopStatement(fl) => fl.get_position(),
	// 		Statement::SwitchStatement(ss) => ss.get_position(),
	// 		Statement::WhileStatement(ws) => ws.get_position(),
	// 		Statement::DoWhileStatement(dws) => dws.get_position(),
	// 		Statement::TryCatchStatement(tcs) => tcs.get_position(),
	// 		Statement::Block(blk) => blk.get_position(),
	// 	}
	// }

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		if let Some(Token(TSXToken::Colon, _)) = reader.peek_n(1) {
			let (name, label_name_pos) = token_as_identifier(reader.next().unwrap(), "label name")?;
			let _colon = reader.next().unwrap();
			let statement = Statement::from_reader(reader, state, settings).map(Box::new)?;
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
				let stmt = VarVariableStatement::from_reader(reader, state, settings)?;
				Ok(Statement::VarVariable(stmt))
			}
			TSXToken::Keyword(TSXKeyword::Throw) => {
				let throw_pos = Keyword::new(reader.next().unwrap().get_span());
				let expression = MultipleExpression::from_reader(reader, state, settings)?;
				let position = throw_pos.get_position().union(expression.get_position());
				Ok(Statement::Throw(ThrowStatement(throw_pos, Box::new(expression), position)))
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
				DoWhileStatement::from_reader(reader, state, settings).map(Into::into)
			}
			TSXToken::Keyword(TSXKeyword::Try) => {
				TryCatchStatement::from_reader(reader, state, settings).map(Into::into)
			}
			TSXToken::OpenBrace => {
				Block::from_reader(reader, state, settings).map(Statement::Block)
			}
			TSXToken::Keyword(TSXKeyword::Return) => Ok({
				let return_keyword = Keyword::new(reader.next().unwrap().get_span());
				if matches!(
					reader.peek(),
					Some(Token(TSXToken::SemiColon | TSXToken::CloseBrace, _))
				) {
					let position = return_keyword.get_position().clone();
					Statement::Return(ReturnStatement(return_keyword, None, position))
				} else {
					let multiple_expression =
						MultipleExpression::from_reader(reader, state, settings)?;
					let position =
						return_keyword.get_position().union(multiple_expression.get_position());
					Statement::Return(ReturnStatement(
						return_keyword,
						Some(multiple_expression),
						position,
					))
				}
			}),
			TSXToken::Keyword(TSXKeyword::Debugger) => {
				Ok(Statement::Debugger(reader.next().unwrap().get_span()))
			}
			TSXToken::Keyword(TSXKeyword::Break) => {
				let break_token = reader.next().unwrap();
				// TODO token is semi-colon
				let label = if !matches!(
					reader.peek(),
					Some(Token(TSXToken::SemiColon | TSXToken::CloseBrace, _))
				) {
					Some(token_as_identifier(reader.next().unwrap(), "break label")?.0)
				} else {
					None
				};
				Ok(Statement::Break(label, break_token.get_span()))
			}
			TSXToken::Keyword(TSXKeyword::Continue) => {
				let continue_token = reader.next().unwrap();
				// TODO token is semi-colon
				let label = if !matches!(
					reader.peek(),
					Some(Token(TSXToken::SemiColon | TSXToken::CloseBrace, _))
				) {
					Some(token_as_identifier(reader.next().unwrap(), "continue label")?.0)
				} else {
					None
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
				let expr = MultipleExpression::from_reader(reader, state, settings)?;
				Ok(Statement::Expression(expr))
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			Statement::Cursor(..) => {
				if !settings.expect_cursors {
					panic!("tried to to-string cursor")
				}
			}
			Statement::Empty(..) => {
				buf.push(';');
			}
			Statement::Return(ReturnStatement(_, expression, _)) => {
				buf.push_str("return");
				if let Some(expression) = expression {
					buf.push(' ');
					expression.to_string_from_buffer(buf, settings, depth);
				}
			}
			Statement::IfStatement(is) => is.to_string_from_buffer(buf, settings, depth),
			Statement::ForLoopStatement(fl) => fl.to_string_from_buffer(buf, settings, depth),
			Statement::SwitchStatement(ss) => ss.to_string_from_buffer(buf, settings, depth),
			Statement::WhileStatement(ws) => ws.to_string_from_buffer(buf, settings, depth),
			Statement::DoWhileStatement(dws) => dws.to_string_from_buffer(buf, settings, depth),
			Statement::TryCatchStatement(tcs) => tcs.to_string_from_buffer(buf, settings, depth),
			Statement::Comment(comment, _) => {
				if settings.should_add_comment() {
					buf.push_str("//");
					buf.push_str_contains_new_line(comment.as_str().trim_end());
				}
			}
			Statement::MultiLineComment(comment, _) => {
				if settings.should_add_comment() {
					buf.push_str("/*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/");
				}
			}
			Statement::Block(block) => {
				block.to_string_from_buffer(buf, settings, depth + 1);
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
				val.to_string_from_buffer(buf, settings, depth);
			}
			Statement::Labelled { name, statement, .. } => {
				buf.push_str(name);
				buf.push(':');
				statement.to_string_from_buffer(buf, settings, depth);
			}
			Statement::Throw(ThrowStatement(_, thrown_expression, _)) => {
				buf.push_str("throw ");
				thrown_expression.to_string_from_buffer(buf, settings, depth);
			}
			Statement::VarVariable(var_stmt) => {
				var_stmt.to_string_from_buffer(buf, settings, depth)
			}
		}
	}
}

impl Statement {
	/// Used for skipping in to_string
	pub fn is_comment(&self) -> bool {
		matches!(self, Statement::Comment(..) | Statement::MultiLineComment(..))
	}

	pub(crate) fn requires_semi_colon(&self) -> bool {
		matches!(
			self,
			Statement::VarVariable(_)
				| Statement::Expression(_)
				| Statement::DoWhileStatement(_)
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
pub struct VarVariableStatement {
	keyword: Keyword<tsx_keywords::Var>,
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
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let keyword = Keyword::from_reader(reader)?;
		let mut declarations = Vec::new();
		loop {
			let value = VariableDeclarationItem::<Option<Expression>>::from_reader(
				reader, state, settings,
			)?;
			declarations.push(value);
			if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
				reader.next();
			} else {
				break;
			}
		}
		Ok(VarVariableStatement {
			position: keyword.get_position().union(declarations.last().unwrap().get_position()),
			keyword,
			declarations,
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str("var ");
		declarations_to_string(&self.declarations, buf, settings, depth);
	}
}
