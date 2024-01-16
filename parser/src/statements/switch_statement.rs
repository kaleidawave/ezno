use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::{sized_tokens::TokenEnd, Token};
use visitable_derive::Visitable;

use crate::{
	ast::MultipleExpression, errors::parse_lexing_error, throw_unexpected_token_with_token,
	ASTNode, Expression, ParseOptions, StatementOrDeclaration, TSXKeyword, TSXToken,
};

#[derive(Debug, PartialEq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct SwitchStatement {
	pub case: MultipleExpression,
	pub branches: Vec<SwitchBranch>,
	pub position: Span,
}

#[derive(Debug, PartialEq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum SwitchBranch {
	Default(Vec<StatementOrDeclaration>),
	Case(Expression, Vec<StatementOrDeclaration>),
}

impl ASTNode for SwitchStatement {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> Result<Self, crate::ParseError> {
		let start = state.expect_keyword(reader, TSXKeyword::Switch)?;
		reader.expect_next(crate::TSXToken::OpenParentheses)?;
		let case = MultipleExpression::from_reader(reader, state, options)?;
		reader.expect_next(crate::TSXToken::CloseParentheses)?;
		reader.expect_next(crate::TSXToken::OpenBrace)?;
		let mut branches = Vec::new();
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
					// TODO bad
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
			let mut statements = Vec::new();
			loop {
				if let Some(Token(
					TSXToken::Keyword(TSXKeyword::Case | TSXKeyword::Default)
					| TSXToken::CloseBrace,
					_,
				)) = reader.peek()
				{
					break;
				}
				statements.push(StatementOrDeclaration::from_reader(reader, state, options)?);
				if let Some(Token(TSXToken::SemiColon, _)) = reader.peek() {
					reader.next();
				}
			}
			if let Some(case) = case {
				branches.push(SwitchBranch::Case(case, statements));
			} else {
				branches.push(SwitchBranch::Default(statements));
			}
		}
		Ok(Self { case, branches, position: start.union(close_brace_pos) })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str("switch");
		options.add_gap(buf);
		buf.push('(');
		self.case.to_string_from_buffer(buf, options, depth);
		buf.push(')');
		options.add_gap(buf);
		buf.push('{');
		for branch in &self.branches {
			if options.pretty {
				buf.push_new_line();
				options.add_indent(depth + 1, buf);
			}
			match branch {
				SwitchBranch::Default(statements) => {
					buf.push_str("default:");
					for (at_end, stmt) in statements.iter().endiate() {
						if options.pretty {
							buf.push_new_line();
							options.add_indent(depth + 2, buf);
						}
						stmt.to_string_from_buffer(buf, options, depth + 2);
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
					case.to_string_from_buffer(buf, options, depth);
					buf.push(':');
					for (at_end, stmt) in statements.iter().endiate() {
						if options.pretty {
							buf.push_new_line();
							options.add_indent(depth + 2, buf);
						}
						stmt.to_string_from_buffer(buf, options, depth + 2);
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
			options.add_indent(depth, buf);
		}
		buf.push('}');
	}
}
