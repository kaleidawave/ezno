mod for_statement;
mod if_statement;
mod switch_statement;
mod try_catch_statement;
mod while_statement;

use crate::{
	declarations::variable::{declarations_to_string, VariableDeclarationItem},
	derive_ASTNode, ParseError, ParseErrors,
};
use derive_enum_from_into::{EnumFrom, EnumTryInto};
use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use std::fmt::Debug;

use super::{
	expressions::MultipleExpression, ASTNode, Block, Expression, ParseOptions, ParseResult, Span,
};
pub use for_statement::{ForLoopCondition, ForLoopStatement, ForLoopStatementInitialiser};
pub use if_statement::*;
pub use switch_statement::{SwitchBranch, SwitchStatement};
pub use try_catch_statement::TryCatchStatement;
use visitable_derive::Visitable;
pub use while_statement::{DoWhileStatement, WhileStatement};

/// A statement. See [Declaration]s and [StatementAndDeclaration] for more
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, EnumFrom, EnumTryInto, PartialEqExtras, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[try_into_references(&, &mut)]
#[partial_eq_ignore_types(Span)]
pub enum Statement {
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
	Empty(Span),
	/// Lol
	AestheticSemiColon(Span),
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, PartialEqExtras, GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ReturnStatement(pub Option<MultipleExpression>, pub Span);

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, PartialEqExtras, GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ThrowStatement(pub Box<MultipleExpression>, pub Span);

impl Eq for Statement {}

impl ASTNode for Statement {
	fn get_position(&self) -> Span {
		*get_field_by_type::GetFieldByType::get(self)
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		if reader.after_identifier().starts_with(":") {
			let start = reader.get_start();
			let name = reader.parse_identifier()?.to_owned();
			let _ = reader.expect(':')?;
			let statement = Statement::from_reader(reader).map(Box::new)?;
			if statement.requires_semi_colon() {
				reader.expect_semi_colon();
			}
			// TODO check statement.can_be_labelled()
			let position = start.union(statement.get_position());
			return Ok(Statement::Labelled { name, statement, position });
		}

		reader.skip();
		let start = reader.get_start();

		if reader.is_keyword("var") {
			VarVariableStatement::from_reader(reader).map(Statement::VarVariable)
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
		} else if reader.starts_with('{') {
			Block::from_reader(reader).map(Statement::Block)
		} else if reader.is_keyword_advance("debugger") {
			Ok(Statement::Debugger(start.with_length("debugger".len())))
		} else if reader.is_keyword_advance("return") {
			if reader.is_semi_colon() {
				Ok(Statement::Return(ReturnStatement(None, start.with_length("return".len()))))
			} else {
				let multiple_expression = MultipleExpression::from_reader(reader)?;
				let position = start.union(multiple_expression.get_position());
				Ok(Statement::Return(ReturnStatement(Some(multiple_expression), position)))
			}
		} else if reader.is_keyword_advance("break") {
			if reader.is_semi_colon() {
				Ok(Statement::Break(None, start.with_length("break".len() as usize)))
			} else {
				let start = reader.get_start();
				let label = reader.parse_identifier()?;
				Ok(Statement::Break(Some(label.to_owned()), start.union(reader.get_end())))
			}
		} else if reader.is_keyword_advance("continue") {
			if reader.is_semi_colon() {
				Ok(Statement::Continue(None, start.with_length("continue".len() as usize)))
			} else {
				let start = reader.get_start();
				let label = reader.parse_identifier()?;
				Ok(Statement::Continue(Some(label.to_owned()), start.union(reader.get_end())))
			}
		} else if reader.is_keyword_advance("throw") {
			let expression = MultipleExpression::from_reader(reader)?;
			let position = start.union(expression.get_position());
			Ok(Statement::Throw(ThrowStatement(Box::new(expression), position)))
		} else if reader.is_operator_advance(";") {
			Ok(Statement::AestheticSemiColon(start.with_length(1)))
		} else if reader.is_operator_advance("//") {
			let content = reader.parse_until("\n").expect("TODO");
			Ok(Statement::Comment(content.to_owned(), start.with_length(2 + content.len())))
		} else if reader.is_operator_advance("/*") {
			let content = reader.parse_until("*/").expect("TODO");
			Ok(Statement::MultiLineComment(
				content.to_owned(),
				start.with_length(4 + content.len()),
			))
		} else {
			MultipleExpression::from_reader(reader).map(Statement::Expression)
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			Statement::Empty(..) => {}
			Statement::AestheticSemiColon(..) => buf.push(';'),
			Statement::Return(ReturnStatement(expression, _)) => {
				buf.push_str("return");
				if let Some(expression) = expression {
					buf.push(' ');
					expression.to_string_from_buffer(buf, options, local);
				}
			}
			Statement::If(is) => is.to_string_from_buffer(buf, options, local),
			Statement::ForLoop(fl) => fl.to_string_from_buffer(buf, options, local),
			Statement::Switch(ss) => ss.to_string_from_buffer(buf, options, local),
			Statement::WhileLoop(ws) => ws.to_string_from_buffer(buf, options, local),
			Statement::DoWhileLoop(dws) => dws.to_string_from_buffer(buf, options, local),
			Statement::TryCatch(tcs) => tcs.to_string_from_buffer(buf, options, local),
			Statement::Comment(comment, _) => {
				if options.should_add_comment(comment.as_str()) {
					buf.push_str("//");
					buf.push_str_contains_new_line(comment.as_str().trim_end());
				}
			}
			Statement::MultiLineComment(comment, _) => {
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
			Statement::Block(block) => {
				block.to_string_from_buffer(buf, options, local.next_level());
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
				val.to_string_on_left(buf, options, local);
			}
			Statement::Labelled { name, statement, .. } => {
				buf.push_str(name);
				buf.push_str(": ");

				if let Statement::Empty(..) = &**statement {
					buf.push(';');
				} else {
					// TODO new line?
					statement.to_string_from_buffer(buf, options, local);
					if statement.requires_semi_colon() {
						buf.push(';');
					}
				}
			}
			Statement::Throw(ThrowStatement(thrown_expression, _)) => {
				buf.push_str("throw ");
				thrown_expression.to_string_from_buffer(buf, options, local);
			}
			Statement::VarVariable(var_stmt) => var_stmt.to_string_from_buffer(buf, options, local),
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
				// | Statement::DoWhileLoop(_)
				| Statement::Continue(..)
				| Statement::Break(..)
				| Statement::Return(..)
				| Statement::Throw(..)
		)
	}
}

#[apply(derive_ASTNode)]
#[derive(Debug, PartialEq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct VarVariableStatement {
	pub declarations: Vec<VariableDeclarationItem<Option<Expression>>>,
	pub position: Span,
}

impl ASTNode for VarVariableStatement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		reader.expect_keyword("var").expect("TODO");
		let mut declarations = Vec::new();
		loop {
			let value = VariableDeclarationItem::<Option<Expression>>::from_reader(reader)?;
			if value.expression.is_none()
				&& !matches!(value.name.get_ast_ref(), crate::VariableField::Name(_))
			{
				todo!()
				// return Err(crate::ParseError::new(
				// 	crate::ParseErrors::DestructuringRequiresValue,
				// 	value.name.get_ast_ref().get_position(),
				// ));
			}
			declarations.push(value);
			if !reader.is_operator_advance(",") {
				break;
			}
		}

		let position = if let Some(last) = declarations.last() {
			start.union(last.get_position())
		} else {
			todo!();
			// let position = start.with_length(3);
			// if options.partial_syntax {
			// 	position
			// } else {
			// 	return Err(ParseError::new(ParseErrors::ExpectedDeclaration, position));
			// }
		};

		Ok(VarVariableStatement { declarations, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("var ");
		declarations_to_string(&self.declarations, buf, options, local, false);
	}
}

fn on_different_lines_or_line_end(// line_starts: &source_map::LineStarts,
	// keyword_position: crate::TokenStart,
	// Token(kind, next): &Token<TSXToken, crate::TokenStart>,
) -> bool {
	todo!()
	// matches!(kind, TSXToken::SemiColon | TSXToken::CloseBrace | TSXToken::EOS)
	// 	|| line_starts.byte_indexes_on_different_lines(keyword_position.0 as usize, next.0 as usize)
}
