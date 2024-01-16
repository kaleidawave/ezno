use crate::{
	ast::MultipleExpression, block::BlockOrSingleStatement,
	declarations::variable::VariableDeclaration, ParseOptions, TSXKeyword, VariableField,
	VariableFieldInSourceCode, VariableKeyword, WithComment,
};
use tokenizer_lib::sized_tokens::TokenReaderWithTokenEnds;
use visitable_derive::Visitable;

use super::{
	ASTNode, Expression, ParseResult, Span, TSXToken, Token, TokenReader, VarVariableStatement,
};

#[derive(Debug, Clone, PartialEq, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct ForLoopStatement {
	pub condition: ForLoopCondition,
	pub inner: BlockOrSingleStatement,
	pub position: Span,
}

impl ASTNode for ForLoopStatement {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = state.expect_keyword(reader, TSXKeyword::For)?;
		let condition = ForLoopCondition::from_reader(reader, state, options)?;
		let inner = BlockOrSingleStatement::from_reader(reader, state, options)?;
		let position = start.union(inner.get_position());
		Ok(ForLoopStatement { condition, inner, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str("for");
		options.add_gap(buf);
		self.condition.to_string_from_buffer(buf, options, depth);
		options.add_gap(buf);
		self.inner.to_string_from_buffer(buf, options, depth + 1);
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ForLoopStatementInitializer {
	VariableDeclaration(VariableDeclaration),
	VarStatement(VarVariableStatement),
	Expression(MultipleExpression),
}

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ForLoopCondition {
	ForOf {
		keyword: Option<VariableKeyword>,
		variable: WithComment<VariableField<VariableFieldInSourceCode>>,
		of: Expression,
		position: Span,
	},
	ForIn {
		keyword: Option<VariableKeyword>,
		variable: WithComment<VariableField<VariableFieldInSourceCode>>,
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
	// fn get_position(&self) -> &Span {
	// 	match self {
	// 		ForLoopCondition::ForOf { keyword, variable, of: rhs }
	// 		| ForLoopCondition::ForIn { keyword, variable, r#in: rhs } => Cow::Owned(
	// 			keyword
	// 				.as_ref()
	// 				.map(ForLoopVariableKeyword::get_position)
	// 				.unwrap_or_else(|| variable.get_position())
	// 				.union(&rhs.get_position()),
	// 		),
	// 		ForLoopCondition::Statements { initializer, condition: _, afterthought } => {
	// 			let initializer_position = match initializer.as_ref().expect("TODO what about None")
	// 			{
	// 				ForLoopStatementInitializer::VariableDeclaration(stmt) => stmt.get_position(),
	// 				ForLoopStatementInitializer::VarStatement(stmt) => stmt.get_position(),
	// 				ForLoopStatementInitializer::Expression(expr) => expr.get_position(),
	// 			};
	// 			Cow::Owned(
	// 				initializer_position.union(
	// 					&afterthought.as_ref().expect("TODO what about None").get_position(),
	// 				),
	// 			)
	// 		}
	// 	}
	// }

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

				let variable =
					WithComment::<VariableField<_>>::from_reader(reader, state, options)?;

				let _ = state.expect_keyword(reader, TSXKeyword::Of)?;

				let of = Expression::from_reader(reader, state, options)?;
				let position = start
					.unwrap_or_else(|| variable.get_position().get_start())
					.union(of.get_position());
				Self::ForOf { variable, keyword, of, position }
			}
			Some(Token(TSXToken::Keyword(TSXKeyword::In), _)) => {
				let (start, keyword) = reader
					.conditional_next(VariableKeyword::is_token_variable_keyword)
					.map(|token| (token.1, VariableKeyword::from_reader(token).unwrap()))
					.unzip();

				let variable =
					WithComment::<VariableField<_>>::from_reader(reader, state, options)?;

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
		depth: u8,
	) {
		buf.push('(');
		match self {
			Self::ForOf { keyword, variable, of, position: _ } => {
				if let Some(keyword) = keyword {
					buf.push_str(keyword.as_str());
				}
				variable.to_string_from_buffer(buf, options, depth);
				// TODO whitespace here if variable is array of object destructuring
				buf.push_str(" of ");
				of.to_string_from_buffer(buf, options, depth);
			}
			Self::ForIn { keyword, variable, r#in, position: _ } => {
				if let Some(keyword) = keyword {
					buf.push_str(keyword.as_str());
				}
				variable.to_string_from_buffer(buf, options, depth);
				// TODO whitespace here if variable is array of object destructuring
				buf.push_str(" in ");
				r#in.to_string_from_buffer(buf, options, depth);
			}
			Self::Statements { initialiser: initializer, condition, afterthought, position: _ } => {
				if let Some(initializer) = initializer {
					match initializer {
						ForLoopStatementInitializer::VariableDeclaration(stmt) => {
							stmt.to_string_from_buffer(buf, options, depth);
						}
						ForLoopStatementInitializer::Expression(expr) => {
							expr.to_string_from_buffer(buf, options, depth);
						}
						ForLoopStatementInitializer::VarStatement(stmt) => {
							stmt.to_string_from_buffer(buf, options, depth);
						}
					}
				}
				buf.push(';');
				if let Some(condition) = condition {
					options.add_gap(buf);
					condition.to_string_from_buffer(buf, options, depth);
				}
				buf.push(';');
				if let Some(afterthought) = afterthought {
					options.add_gap(buf);
					afterthought.to_string_from_buffer(buf, options, depth);
				}
			}
		}
		buf.push(')');
	}

	fn get_position(&self) -> &Span {
		match self {
			ForLoopCondition::ForOf { position, .. }
			| ForLoopCondition::ForIn { position, .. }
			| ForLoopCondition::Statements { position, .. } => position,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::ForLoopCondition;
	use crate::assert_matches_ast;

	#[test]
	fn condition_without_variable_keyword() {
		assert_matches_ast!("(k in x)", ForLoopCondition::ForIn { .. });
	}
}
