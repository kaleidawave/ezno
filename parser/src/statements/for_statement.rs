use crate::{
	ast::MultipleExpression, block::BlockOrSingleStatement,
	declarations::variable::VariableDeclaration, tsx_keywords, Keyword, ParseOptions, TSXKeyword,
	VariableField, VariableFieldInSourceCode, WithComment,
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
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let start_pos = reader.expect_next(TSXToken::Keyword(TSXKeyword::For))?;
		let condition = ForLoopCondition::from_reader(reader, state, settings)?;
		let inner = BlockOrSingleStatement::from_reader(reader, state, settings)?;
		let position = start_pos.union(inner.get_position());
		Ok(ForLoopStatement { condition, inner, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str("for");
		settings.add_gap(buf);
		self.condition.to_string_from_buffer(buf, settings, depth);
		settings.add_gap(buf);
		self.inner.to_string_from_buffer(buf, settings, depth + 1);
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

#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ForLoopVariableKeyword {
	Const(Keyword<tsx_keywords::Const>),
	Let(Keyword<tsx_keywords::Let>),
	Var(Keyword<tsx_keywords::Var>),
}

impl ForLoopVariableKeyword {
	pub fn is_token_variable_keyword(token: &TSXToken) -> bool {
		matches!(token, TSXToken::Keyword(TSXKeyword::Const | TSXKeyword::Let | TSXKeyword::Var))
	}

	pub(crate) fn from_reader(token: Token<TSXToken, crate::TokenStart>) -> ParseResult<Self> {
		match token {
			Token(TSXToken::Keyword(TSXKeyword::Const), _) => {
				Ok(Self::Const(Keyword::new(token.get_span())))
			}
			Token(TSXToken::Keyword(TSXKeyword::Let), _) => {
				Ok(Self::Let(Keyword::new(token.get_span())))
			}
			Token(TSXToken::Keyword(TSXKeyword::Var), _) => {
				Ok(Self::Var(Keyword::new(token.get_span())))
			}
			token => crate::throw_unexpected_token_with_token(
				token,
				&[
					TSXToken::Keyword(TSXKeyword::Const),
					TSXToken::Keyword(TSXKeyword::Let),
					TSXToken::Keyword(TSXKeyword::Var),
				],
			),
		}
	}

	pub fn as_str(&self) -> &str {
		match self {
			Self::Const(_) => "const ",
			Self::Let(_) => "let ",
			Self::Var(_) => "var ",
		}
	}

	pub fn get_position(&self) -> &Span {
		match self {
			Self::Const(kw) => kw.get_position(),
			Self::Let(kw) => kw.get_position(),
			Self::Var(kw) => kw.get_position(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ForLoopCondition {
	ForOf {
		keyword: Option<ForLoopVariableKeyword>,
		variable: WithComment<VariableField<VariableFieldInSourceCode>>,
		of: Expression,
		position: Span,
	},
	ForIn {
		keyword: Option<ForLoopVariableKeyword>,
		variable: WithComment<VariableField<VariableFieldInSourceCode>>,
		/// Yes `of` is single expression, `in` is multiple
		r#in: MultipleExpression,
		position: Span,
	},
	Statements {
		initializer: Option<ForLoopStatementInitializer>,
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
		settings: &ParseOptions,
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
				!ForLoopVariableKeyword::is_token_variable_keyword(token)
			}
		});

		let condition = match next {
			Some(Token(TSXToken::Keyword(TSXKeyword::Of), _)) => {
				let keyword = if let Some(token) =
					reader.conditional_next(ForLoopVariableKeyword::is_token_variable_keyword)
				{
					Some(ForLoopVariableKeyword::from_reader(token).unwrap())
				} else {
					None
				};

				let variable =
					WithComment::<VariableField<_>>::from_reader(reader, state, settings)?;

				reader.expect_next(TSXToken::Keyword(TSXKeyword::Of))?;
				let of = Expression::from_reader(reader, state, settings)?;
				let position = keyword
					.as_ref()
					.map_or(variable.get_position(), |kw| kw.get_position())
					.union(of.get_position());
				Self::ForOf { variable, keyword, of, position }
			}
			Some(Token(TSXToken::Keyword(TSXKeyword::In), _)) => {
				let keyword = if let Some(token) =
					reader.conditional_next(ForLoopVariableKeyword::is_token_variable_keyword)
				{
					Some(ForLoopVariableKeyword::from_reader(token).unwrap())
				} else {
					None
				};

				let variable =
					WithComment::<VariableField<_>>::from_reader(reader, state, settings)?;

				reader.expect_next(TSXToken::Keyword(TSXKeyword::In))?;
				let r#in = MultipleExpression::from_reader(reader, state, settings)?;
				let position = keyword
					.as_ref()
					.map_or(variable.get_position(), |kw| kw.get_position())
					.union(r#in.get_position());
				Self::ForIn { variable, keyword, r#in, position }
			}
			_ => {
				let peek = reader.peek();
				let initializer =
					if let Some(Token(TSXToken::Keyword(TSXKeyword::Const | TSXKeyword::Let), _)) =
						peek
					{
						let declaration =
							VariableDeclaration::from_reader(reader, state, settings)?;
						Some(ForLoopStatementInitializer::VariableDeclaration(declaration))
					} else if let Some(Token(TSXToken::Keyword(TSXKeyword::Var), _)) = peek {
						let stmt = VarVariableStatement::from_reader(reader, state, settings)?;
						Some(ForLoopStatementInitializer::VarStatement(stmt))
					} else if let Some(Token(TSXToken::SemiColon, _)) = peek {
						None
					} else {
						let expr = MultipleExpression::from_reader(reader, state, settings)?;
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

				let condition = if !matches!(reader.peek(), Some(Token(TSXToken::SemiColon, _))) {
					Some(MultipleExpression::from_reader(reader, state, settings)?)
				} else {
					None
				};
				let semi_colon_two = reader.expect_next_get_end(TSXToken::SemiColon)?;
				let afterthought =
					if !matches!(reader.peek(), Some(Token(TSXToken::CloseParentheses, _))) {
						Some(MultipleExpression::from_reader(reader, state, settings)?)
					} else {
						None
					};
				let end = afterthought
					.as_ref()
					.map_or(semi_colon_two, |expr| expr.get_position().get_end());
				let position = start.union(end);
				Self::Statements { initializer, condition, afterthought, position }
			}
		};
		reader.expect_next(TSXToken::CloseParentheses)?;
		Ok(condition)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push('(');
		match self {
			Self::ForOf { keyword, variable, of, position: _ } => {
				if let Some(keyword) = keyword {
					buf.push_str(keyword.as_str());
				}
				variable.to_string_from_buffer(buf, settings, depth);
				// TODO whitespace here if variable is array of object destructuring
				buf.push_str(" of ");
				of.to_string_from_buffer(buf, settings, depth);
			}
			Self::ForIn { keyword, variable, r#in, position: _ } => {
				if let Some(keyword) = keyword {
					buf.push_str(keyword.as_str());
				}
				variable.to_string_from_buffer(buf, settings, depth);
				// TODO whitespace here if variable is array of object destructuring
				buf.push_str(" in ");
				r#in.to_string_from_buffer(buf, settings, depth);
			}
			Self::Statements { initializer, condition, afterthought, position: _ } => {
				if let Some(initializer) = initializer {
					match initializer {
						ForLoopStatementInitializer::VariableDeclaration(stmt) => {
							stmt.to_string_from_buffer(buf, settings, depth)
						}
						ForLoopStatementInitializer::Expression(expr) => {
							expr.to_string_from_buffer(buf, settings, depth);
						}
						ForLoopStatementInitializer::VarStatement(stmt) => {
							stmt.to_string_from_buffer(buf, settings, depth)
						}
					}
				}
				buf.push(';');
				if let Some(condition) = condition {
					settings.add_gap(buf);
					condition.to_string_from_buffer(buf, settings, depth);
				}
				buf.push(';');
				if let Some(afterthought) = afterthought {
					settings.add_gap(buf);
					afterthought.to_string_from_buffer(buf, settings, depth);
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
		assert_matches_ast!("(k in x)", ForLoopCondition::ForIn { .. })
	}
}
