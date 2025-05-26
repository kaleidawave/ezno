use crate::{
	ast::MultipleExpression,
	block::BlockOrSingleStatement,
	derive_ASTNode,
	statements_and_declarations::variables::{
		VarVariableStatement, VariableDeclaration, VariableField, VariableKeyword,
	},
	ParseError, ParseErrors, WithComment,
};
use visitable_derive::Visitable;

use crate::{ASTNode, Expression, ParseResult, Span};

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

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("for")?;
		let is_await = reader.is_keyword_advance("await");
		let mut condition = ForLoopCondition::from_reader(reader)?;
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
		let inner = BlockOrSingleStatement::from_reader(reader)?;
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
		self.inner.to_string_from_buffer(buf, options, local.next_level());
	}
}

#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub enum ForLoopStatementInitialiser {
	VariableDeclaration(VariableDeclaration),
	VarStatement(VarVariableStatement),
	Expression(Box<MultipleExpression>),
}

#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub enum ForLoopCondition {
	ForOf {
		keyword: Option<VariableKeyword>,
		variable: WithComment<VariableField>,
		of: Box<Expression>,
		is_await: bool,
		position: Span,
	},
	ForIn {
		keyword: Option<VariableKeyword>,
		variable: WithComment<VariableField>,
		/// Yes `of` is single expression, `in` is multiple
		r#in: Box<MultipleExpression>,
		position: Span,
	},
	Statements {
		initialiser: Option<ForLoopStatementInitialiser>,
		condition: Option<Box<MultipleExpression>>,
		afterthought: Option<Box<MultipleExpression>>,
		position: Span,
	},
}

impl ASTNode for ForLoopCondition {
	fn get_position(&self) -> Span {
		match self {
			ForLoopCondition::ForOf { position, .. }
			| ForLoopCondition::ForIn { position, .. }
			| ForLoopCondition::Statements { position, .. } => *position,
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		reader.expect('(')?;
		reader.skip();

		let start = reader.get_start();

		// Figure out if after variable declaration there exists "in" or "of", or something assignment like
		// TODO temp `after_variable_start` implementation
		let after_stuff = reader.after_variable_start();

		let condition = if after_stuff.starts_with("in") {
			let keyword = if reader.is_keyword_advance("const") {
				Some(VariableKeyword::Const)
			} else if reader.is_keyword_advance("let") {
				Some(VariableKeyword::Let)
			} else if reader.is_keyword_advance("var") {
				Some(VariableKeyword::Var)
			} else {
				None
			};

			let variable = WithComment::<VariableField>::from_reader(reader)?;

			let _ = reader.expect_keyword("in")?;

			let r#in = MultipleExpression::from_reader(reader).map(Box::new)?;
			let position = start.union(r#in.get_position());
			Self::ForIn { variable, keyword, r#in, position }
		} else if after_stuff.starts_with("of") {
			let keyword = if reader.is_keyword_advance("const") {
				Some(VariableKeyword::Const)
			} else if reader.is_keyword_advance("let") {
				Some(VariableKeyword::Let)
			} else if reader.is_keyword_advance("var") {
				Some(VariableKeyword::Var)
			} else {
				None
			};

			let variable = WithComment::<VariableField>::from_reader(reader)?;

			let _ = reader.expect_keyword("of")?;

			let of = Expression::from_reader(reader).map(Box::new)?;
			let position = start.union(of.get_position());

			// Not great `is_await`, set from above
			Self::ForOf { variable, keyword, of, position, is_await: false }
		} else {
			let initialiser = if reader.is_one_of_keywords(&["const", "let"]).is_some() {
				let declaration = VariableDeclaration::from_reader(reader)?;
				Some(ForLoopStatementInitialiser::VariableDeclaration(declaration))
			} else if reader.is_keyword("var") {
				let stmt = VarVariableStatement::from_reader(reader)?;
				Some(ForLoopStatementInitialiser::VarStatement(stmt))
			} else if reader.is_operator(";") {
				None
			} else {
				let expr = MultipleExpression::from_reader(reader).map(Box::new)?;
				Some(ForLoopStatementInitialiser::Expression(expr))
			};

			let _semi_colon_one = reader.expect(';')?;
			let condition = if reader.is_operator(";") {
				None
			} else {
				Some(MultipleExpression::from_reader(reader).map(Box::new)?)
			};
			let _semi_colon_two = reader.expect(';')?;
			let afterthought = if reader.is_operator(")") {
				None
			} else {
				Some(MultipleExpression::from_reader(reader).map(Box::new)?)
			};

			let position = start.union(reader.get_end());
			Self::Statements { initialiser, condition, afterthought, position }
		};
		reader.expect(')')?;
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
			Self::Statements { initialiser, condition, afterthought, position: _ } => {
				let mut large = false;
				if options.enforce_limit_length_limit() && local.should_try_pretty_print {
					let room = options.max_line_length as usize;
					let mut buf = source_map::StringWithOptionalSourceMap {
						source: String::new(),
						source_map: None,
						quit_after: Some(room),
						since_new_line: 0,
					};

					if let Some(initialiser) = initialiser {
						initialiser_to_string(initialiser, &mut buf, options, local);
					}
					large = buf.source.len() > room;
					if !large {
						if let Some(condition) = condition {
							condition.to_string_from_buffer(&mut buf, options, local);
						}
						large = buf.source.len() > room;
						if !large {
							if let Some(afterthought) = afterthought {
								afterthought.to_string_from_buffer(&mut buf, options, local);
							}
							large = buf.source.len() > room;
						}
					}
				}
				let inner_local = if large { local.next_level() } else { local };

				if let Some(initialiser) = initialiser {
					if large {
						buf.push_new_line();
						options.add_indent(inner_local.depth, buf);
					}
					initialiser_to_string(initialiser, buf, options, inner_local);
				}
				buf.push(';');
				if let Some(condition) = condition {
					if large {
						buf.push_new_line();
						options.add_indent(inner_local.depth, buf);
					} else {
						options.push_gap_optionally(buf);
					}
					condition.to_string_from_buffer(buf, options, inner_local);
				}
				buf.push(';');
				if let Some(afterthought) = afterthought {
					if large {
						buf.push_new_line();
						options.add_indent(inner_local.depth, buf);
					} else {
						options.push_gap_optionally(buf);
					}
					afterthought.to_string_from_buffer(buf, options, inner_local);
				}
				if large {
					buf.push_new_line();
					options.add_indent(local.depth, buf);
				}
			}
		}
		buf.push(')');
	}
}

fn initialiser_to_string<T: source_map::ToString>(
	initialiser: &ForLoopStatementInitialiser,
	buf: &mut T,
	options: &crate::ToStringOptions,
	local: crate::LocalToStringInformation,
) {
	match initialiser {
		ForLoopStatementInitialiser::VariableDeclaration(stmt) => {
			stmt.to_string_from_buffer(buf, options, local);
		}
		ForLoopStatementInitialiser::Expression(expr) => {
			expr.to_string_from_buffer(buf, options, local);
		}
		ForLoopStatementInitialiser::VarStatement(stmt) => {
			stmt.to_string_from_buffer(buf, options, local);
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
