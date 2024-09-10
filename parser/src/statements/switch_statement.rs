use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use visitable_derive::Visitable;

use crate::{
	ast::MultipleExpression, derive_ASTNode, ASTNode, Expression, ParseOptions,
	StatementOrDeclaration,
};

#[apply(derive_ASTNode)]
#[derive(Debug, PartialEq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct SwitchStatement {
	pub case: MultipleExpression,
	pub branches: Vec<SwitchBranch>,
	pub position: Span,
}

#[derive(Debug, PartialEq, Clone, Visitable)]
#[apply(derive_ASTNode)]
pub enum SwitchBranch {
	Default(Vec<StatementOrDeclaration>),
	Case(Expression, Vec<StatementOrDeclaration>),
}

impl ASTNode for SwitchStatement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> Result<Self, crate::ParseError> {
		let _existing = r#"let start = state.expect_keyword(reader, TSXKeyword::Switch)?;

		reader.expect_next(crate::TSXToken::OpenParentheses)?;
		let case = MultipleExpression::from_reader(reader, state, options)?;
		reader.expect_next(crate::TSXToken::CloseParentheses)?;
		reader.expect_next(crate::TSXToken::OpenBrace)?;

		let mut branches = Vec::new();

		// TODO not great has this works
		let close_brace_pos: TokenEnd;

		loop {
			let case: Option<Expression> = match reader.next().ok_or_else(parse_lexing_error)? {
				Token(TSXToken::Keyword(TSXKeyword::Default), _) => {
					reader.expect_next(TSXToken::Colon)?;
					None
				}
				Token(TSXToken::Keyword(TSXKeyword::Case), _) => {
					let case = Expression::from_reader(reader, state, options)?;
					reader.expect_next(TSXToken::Colon)?;
					Some(case)
				}
				Token(TSXToken::CloseBrace, pos) => {
					close_brace_pos = TokenEnd::new(pos.0 + 1);
					break;
				}
				token => {
					return throw_unexpected_token_with_token(
						token,
						&[
							TSXToken::Keyword(TSXKeyword::Default),
							TSXToken::Keyword(TSXKeyword::Case),
							TSXToken::CloseBrace,
						],
					);
				}
			};

			// This is a modified form of Block::from_reader where `TSXKeyword::Case` and
			// `TSXKeyword::Default` are delimiters
			let mut items = Vec::new();
			loop {
				if let Some(Token(
					TSXToken::Keyword(TSXKeyword::Case | TSXKeyword::Default)
					| TSXToken::CloseBrace,
					_,
				)) = reader.peek()
				{
					break;
				}
				let value = StatementOrDeclaration::from_reader(reader, state, options)?;
				if value.requires_semi_colon() {
					let _ = crate::expect_semi_colon(
						reader,
						&state.line_starts,
						value.get_position().end,
						options,
					)?;
				}
				// Could skip over semi colons regardless. But they are technically empty statements 🤷‍♂️
				items.push(value);
			}
			if let Some(case) = case {
				branches.push(SwitchBranch::Case(case, items));
			} else {
				branches.push(SwitchBranch::Default(items));
			}
		}
		Ok(Self { case, branches, position: start.union(close_brace_pos) })"#;
		todo!();
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("switch");
		options.push_gap_optionally(buf);
		buf.push('(');
		self.case.to_string_from_buffer(buf, options, local);
		buf.push(')');
		options.push_gap_optionally(buf);
		buf.push('{');
		for branch in &self.branches {
			if options.pretty {
				buf.push_new_line();
				options.add_indent(local.depth + 1, buf);
			}
			let local = local.next_level();
			match branch {
				SwitchBranch::Default(statements) => {
					buf.push_str("default:");
					for (at_end, stmt) in statements.iter().endiate() {
						if options.pretty {
							buf.push_new_line();
							options.add_indent(local.depth + 1, buf);
						}
						stmt.to_string_from_buffer(buf, options, local.next_level());
						if stmt.requires_semi_colon() {
							buf.push(';');
						}
						if options.pretty && !at_end {
							buf.push_new_line();
						}
					}
				}
				SwitchBranch::Case(case, statements) => {
					buf.push_str("case ");
					case.to_string_from_buffer(buf, options, local);
					buf.push(':');
					for (at_end, stmt) in statements.iter().endiate() {
						if options.pretty {
							buf.push_new_line();
							options.add_indent(local.depth + 1, buf);
						}
						stmt.to_string_from_buffer(buf, options, local.next_level());
						if stmt.requires_semi_colon() {
							buf.push(';');
						}
						if options.pretty && !at_end {
							buf.push_new_line();
						}
					}
				}
			}
		}
		if options.pretty {
			buf.push_new_line();
			options.add_indent(local.depth, buf);
		}
		buf.push('}');
	}
}
