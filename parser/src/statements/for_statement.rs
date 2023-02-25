use std::borrow::Cow;

use crate::{
	block::BlockOrSingleStatement, ParseSettings, TSXKeyword, VariableField,
	VariableFieldInSourceCode, WithComment,
};
use visitable_derive::Visitable;

use super::{
	variable::VariableKeyword, ASTNode, Expression, ParseResult, Span, TSXToken, Token,
	TokenReader, VariableStatement,
};

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct ForLoopStatement {
	pub condition: ForLoopCondition,
	pub inner: BlockOrSingleStatement,
	pub position: Span,
}

impl ASTNode for ForLoopStatement {
	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let start_pos = reader.expect_next(TSXToken::Keyword(TSXKeyword::For))?;
		let condition = ForLoopCondition::from_reader(reader, state, settings)?;
		let inner = BlockOrSingleStatement::from_reader(reader, state, settings)?;
		let position = start_pos.union(&inner.get_position());
		Ok(ForLoopStatement { condition, inner, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		buf.push_str("for");
		settings.0.add_gap(buf);
		self.condition.to_string_from_buffer(buf, settings, depth);
		settings.0.add_gap(buf);
		self.inner.to_string_from_buffer(buf, settings, depth + 1);
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum ForLoopStatementInitializer {
	Statement(VariableStatement),
	Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum ForLoopCondition {
	ForOf {
		keyword: VariableKeyword,
		variable: WithComment<VariableField<VariableFieldInSourceCode>>,
		// TODO box...?
		of: Expression,
	},
	ForIn {
		keyword: VariableKeyword,
		variable: WithComment<VariableField<VariableFieldInSourceCode>>,
		// TODO box...?
		in_condition: Expression,
	},
	Statements {
		initializer: ForLoopStatementInitializer,
		condition: Expression,
		final_expression: Expression,
	},
}

impl ASTNode for ForLoopCondition {
	fn get_position(&self) -> Cow<Span> {
		todo!()
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		reader.expect_next(TSXToken::OpenParentheses)?;
		// Figure out if after variable declaration there exists a "=", "in" or a "of"
		let mut destructuring_depth = 0;
		let mut ate_variable_specifier = false;
		let next = reader.scan(|token, _| {
			if ate_variable_specifier {
				match token {
					TSXToken::OpenBrace | TSXToken::OpenBracket => destructuring_depth += 1,
					TSXToken::CloseBrace | TSXToken::CloseBracket => destructuring_depth -= 1,
					_ => {}
				}
				destructuring_depth == 0
			} else {
				ate_variable_specifier = true;
				false
			}
		});

		let condition = match next.map(|Token(tok, _)| tok) {
			Some(TSXToken::Keyword(TSXKeyword::Of)) => {
				let keyword = VariableKeyword::from_reader(reader.next().unwrap())?;
				let variable = ASTNode::from_reader(reader, state, settings)?;
				reader.expect_next(TSXToken::Keyword(TSXKeyword::Of))?;
				let of = Expression::from_reader(reader, state, settings)?;
				Self::ForOf { variable, keyword, of }
			}
			Some(TSXToken::Keyword(TSXKeyword::In)) => {
				let keyword = VariableKeyword::from_reader(reader.next().unwrap())?;
				let variable = ASTNode::from_reader(reader, state, settings)?;
				reader.expect_next(TSXToken::Keyword(TSXKeyword::In))?;
				let in_condition = Expression::from_reader(reader, state, settings)?;
				Self::ForIn { variable, keyword, in_condition }
			}
			_ => {
				let initializer = if let Some(Token(
					TSXToken::Keyword(TSXKeyword::Const | TSXKeyword::Let | TSXKeyword::Var),
					_,
				)) = reader.peek()
				{
					VariableStatement::from_reader(reader, state, settings)
						.map(ForLoopStatementInitializer::Statement)
				} else {
					Expression::from_reader(reader, state, settings)
						.map(ForLoopStatementInitializer::Expression)
				}?;
				reader.expect_next(TSXToken::SemiColon)?;
				let condition = Expression::from_reader(reader, state, settings)?;
				reader.expect_next(TSXToken::SemiColon)?;
				let final_expression = Expression::from_reader(reader, state, settings)?;
				Self::Statements { initializer, condition, final_expression }
			}
		};
		reader.expect_next(TSXToken::CloseParentheses)?;
		Ok(condition)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		buf.push('(');
		match self {
			Self::ForOf { keyword, variable, of } => {
				buf.push_str(keyword.as_str());
				variable.to_string_from_buffer(buf, settings, depth);
				// TODO whitespace here if variable is array of object destructuring
				buf.push_str(" of ");
				of.to_string_from_buffer(buf, settings, depth);
			}
			Self::ForIn { keyword, variable, in_condition } => {
				buf.push_str(keyword.as_str());
				variable.to_string_from_buffer(buf, settings, depth);
				// TODO whitespace here if variable is array of object destructuring
				buf.push_str(" in ");
				in_condition.to_string_from_buffer(buf, settings, depth);
			}
			Self::Statements { initializer, condition, final_expression } => {
				match initializer {
					ForLoopStatementInitializer::Statement(stmt) => {
						stmt.to_string_from_buffer(buf, settings, depth)
					}
					ForLoopStatementInitializer::Expression(expr) => {
						expr.to_string_from_buffer(buf, settings, depth)
					}
				}
				buf.push(';');
				settings.0.add_gap(buf);
				condition.to_string_from_buffer(buf, settings, depth);
				buf.push(';');
				settings.0.add_gap(buf);
				final_expression.to_string_from_buffer(buf, settings, depth);
			}
		}
		buf.push(')');
	}
}
