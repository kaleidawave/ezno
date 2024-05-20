use crate::{
	ast::MultipleExpression, block::BlockOrSingleStatement,
	declarations::variable::VariableDeclaration, derive_ASTNode, ParseError, ParseErrors,
	ParseOptions, Statement, TSXKeyword, VariableField, VariableKeyword, WithComment,
};
use tokenizer_lib::sized_tokens::TokenReaderWithTokenEnds;
use visitable_derive::Visitable;

use super::{
	ASTNode, Expression, ParseResult, Span, TSXToken, Token, TokenReader, VarVariableStatement,
};

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ForLoopStatement {
	pub condition: ForLoopCondition,
	pub inner: BlockOrSingleStatement,
	pub position: Span,
}

impl ASTNode for ForLoopStatement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = state.expect_keyword(reader, TSXKeyword::For)?;
		let is_await = reader
			.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Await)))
			.is_some();
		let mut condition = ForLoopCondition::from_reader(reader, state, options)?;
		if is_await {
			if let ForLoopCondition::ForOf { is_await: ref mut a, .. } = condition {
				*a = is_await;
			} else {
				return Err(ParseError::new(
					ParseErrors::AwaitRequiresForOf,
					condition.get_position(),
				));
			}
		}
		let inner = BlockOrSingleStatement::from_reader(reader, state, options)?;
		let position = start.union(inner.get_position());
		Ok(ForLoopStatement { condition, inner, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("for");
		if let ForLoopCondition::ForOf { is_await: true, .. } = self.condition {
			buf.push_str(" await");
		}
		options.push_gap_optionally(buf);
		self.condition.to_string_from_buffer(buf, options, local);
		options.push_gap_optionally(buf);
		if let BlockOrSingleStatement::SingleStatement(ref statement) = self.inner {
			if let Statement::Empty(..) = &**statement {
				buf.push(';');
				return;
			}
		}
		self.inner.to_string_from_buffer(buf, options, local.next_level());
	}
}

#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub enum ForLoopStatementInitializer {
	VariableDeclaration(VariableDeclaration),
	VarStatement(VarVariableStatement),
	Expression(MultipleExpression),
}

#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub enum ForLoopCondition {
	ForOf {
		keyword: Option<VariableKeyword>,
		variable: WithComment<VariableField>,
		of: Expression,
		is_await: bool,
		position: Span,
	},
	ForIn {
		keyword: Option<VariableKeyword>,
		variable: WithComment<VariableField>,
		/// Yes `of` is single expression, `in` is multiple
		r#in: MultipleExpression,
		position: Span,
	},
	Statements {
		initialiser: Option<ForLoopStatementInitializer>,
		condition: Option<MultipleExpression>,
		afterthought: Option<MultipleExpression>,
		position: Span,
	},
}

impl ASTNode for ForLoopCondition {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
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
				!VariableKeyword::is_token_variable_keyword(token)
			}
		});

		let condition = match next {
			Some(Token(TSXToken::Keyword(TSXKeyword::Of), _)) => {
				let (start, keyword) = reader
					.conditional_next(VariableKeyword::is_token_variable_keyword)
					.map(|token| (token.1, VariableKeyword::from_reader(token).unwrap()))
					.unzip();

				let variable = WithComment::<VariableField>::from_reader(reader, state, options)?;

				let _ = state.expect_keyword(reader, TSXKeyword::Of)?;

				let of = Expression::from_reader(reader, state, options)?;
				let position = start
					.unwrap_or_else(|| variable.get_position().get_start())
					.union(of.get_position());

				// Not great, set from above
				Self::ForOf { variable, keyword, of, position, is_await: false }
			}
			Some(Token(TSXToken::Keyword(TSXKeyword::In), _)) => {
				let (start, keyword) = reader
					.conditional_next(VariableKeyword::is_token_variable_keyword)
					.map(|token| (token.1, VariableKeyword::from_reader(token).unwrap()))
					.unzip();

				let variable = WithComment::<VariableField>::from_reader(reader, state, options)?;

				let _ = state.expect_keyword(reader, TSXKeyword::In)?;

				let r#in = MultipleExpression::from_reader(reader, state, options)?;
				let position = start
					.unwrap_or_else(|| variable.get_position().get_start())
					.union(r#in.get_position());
				Self::ForIn { variable, keyword, r#in, position }
			}
			_ => {
				let peek = reader.peek();
				let initializer =
					if let Some(Token(TSXToken::Keyword(TSXKeyword::Const | TSXKeyword::Let), _)) =
						peek
					{
						let declaration = VariableDeclaration::from_reader(reader, state, options)?;
						Some(ForLoopStatementInitializer::VariableDeclaration(declaration))
					} else if let Some(Token(TSXToken::Keyword(TSXKeyword::Var), _)) = peek {
						let stmt = VarVariableStatement::from_reader(reader, state, options)?;
						Some(ForLoopStatementInitializer::VarStatement(stmt))
					} else if let Some(Token(TSXToken::SemiColon, _)) = peek {
						None
					} else {
						let expr = MultipleExpression::from_reader(reader, state, options)?;
						Some(ForLoopStatementInitializer::Expression(expr))
					};

				let semi_colon_one = reader.expect_next(TSXToken::SemiColon)?;
				let start = initializer.as_ref().map_or(semi_colon_one, |init| match init {
					ForLoopStatementInitializer::VariableDeclaration(item) => {
						item.get_position().get_start()
					}
					ForLoopStatementInitializer::VarStatement(item) => {
						item.get_position().get_start()
					}
					ForLoopStatementInitializer::Expression(item) => {
						item.get_position().get_start()
					}
				});

				let condition = if matches!(reader.peek(), Some(Token(TSXToken::SemiColon, _))) {
					None
				} else {
					Some(MultipleExpression::from_reader(reader, state, options)?)
				};
				let semi_colon_two = reader.expect_next_get_end(TSXToken::SemiColon)?;
				let afterthought =
					if matches!(reader.peek(), Some(Token(TSXToken::CloseParentheses, _))) {
						None
					} else {
						Some(MultipleExpression::from_reader(reader, state, options)?)
					};
				let end = afterthought
					.as_ref()
					.map_or(semi_colon_two, |expr| expr.get_position().get_end());
				let position = start.union(end);
				Self::Statements { initialiser: initializer, condition, afterthought, position }
			}
		};
		reader.expect_next(TSXToken::CloseParentheses)?;
		Ok(condition)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push('(');
		match self {
			Self::ForOf { keyword, variable, of, position: _, is_await: _ } => {
				if let Some(keyword) = keyword {
					buf.push_str(keyword.as_str());
				}
				variable.to_string_from_buffer(buf, options, local);
				// TODO whitespace here if variable is array of object destructuring
				buf.push_str(" of ");
				of.to_string_from_buffer(buf, options, local);
			}
			Self::ForIn { keyword, variable, r#in, position: _ } => {
				if let Some(keyword) = keyword {
					buf.push_str(keyword.as_str());
				}
				variable.to_string_from_buffer(buf, options, local);
				// TODO whitespace here if variable is array of object destructuring
				buf.push_str(" in ");
				r#in.to_string_from_buffer(buf, options, local);
			}
			Self::Statements { initialiser: initializer, condition, afterthought, position: _ } => {
				if let Some(initializer) = initializer {
					match initializer {
						ForLoopStatementInitializer::VariableDeclaration(stmt) => {
							stmt.to_string_from_buffer(buf, options, local);
						}
						ForLoopStatementInitializer::Expression(expr) => {
							expr.to_string_from_buffer(buf, options, local);
						}
						ForLoopStatementInitializer::VarStatement(stmt) => {
							stmt.to_string_from_buffer(buf, options, local);
						}
					}
				}
				buf.push(';');
				if let Some(condition) = condition {
					options.push_gap_optionally(buf);
					condition.to_string_from_buffer(buf, options, local);
				}
				buf.push(';');
				if let Some(afterthought) = afterthought {
					options.push_gap_optionally(buf);
					afterthought.to_string_from_buffer(buf, options, local);
				}
			}
		}
		buf.push(')');
	}

	fn get_position(&self) -> Span {
		match self {
			ForLoopCondition::ForOf { position, .. }
			| ForLoopCondition::ForIn { position, .. }
			| ForLoopCondition::Statements { position, .. } => *position,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::ForLoopCondition;
	use crate::{assert_matches_ast, statements::ForLoopStatement, ASTNode};

	#[test]
	fn condition_without_variable_keyword() {
		assert_matches_ast!("(k in x)", ForLoopCondition::ForIn { .. });
	}

	#[test]
	fn for_await() {
		assert_matches_ast!(
			"for await (let k of x) {}",
			ForLoopStatement { condition: ForLoopCondition::ForOf { is_await: true, .. }, .. }
		);
		assert_matches_ast!(
			"for (let k of x) {}",
			ForLoopStatement { condition: ForLoopCondition::ForOf { is_await: false, .. }, .. }
		);

		assert!(ForLoopStatement::from_string(
			"for await (let x = 0; x < 5; x++) {}".into(),
			Default::default()
		)
		.is_err());
	}
}
