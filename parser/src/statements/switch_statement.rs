use std::borrow::Cow;

use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::Token;
use visitable_derive::Visitable;

use crate::{
	errors::parse_lexing_error, ASTNode, Expression, ParseSettings, Statement, TSXKeyword, TSXToken,
};

#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct SwitchStatement {
	pub case: Expression,
	pub branches: Vec<SwitchBranch>,
	pub position: Span,
}

#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum SwitchBranch {
	Default(Vec<Statement>),
	Case(Expression, Vec<Statement>),
}

impl ASTNode for SwitchStatement {
	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> Result<Self, crate::ParseError> {
		let start_span = reader.expect_next(TSXToken::Keyword(TSXKeyword::Switch))?;
		reader.expect_next(crate::TSXToken::OpenParentheses)?;
		let case = Expression::from_reader(reader, state, settings)?;
		reader.expect_next(crate::TSXToken::CloseParentheses)?;
		reader.expect_next(crate::TSXToken::OpenBrace)?;
		let mut branches = Vec::new();
		let close_brace_pos: Span;
		loop {
			let Token(token_type, pos) = reader.next().ok_or_else(parse_lexing_error)?;
			let case: Option<Expression> = match token_type {
				TSXToken::Keyword(TSXKeyword::Default) => {
					reader.expect_next(TSXToken::Colon)?;
					None
				}
				TSXToken::Keyword(TSXKeyword::Case) => {
					let case = Expression::from_reader(reader, state, settings)?;
					reader.expect_next(TSXToken::Colon)?;
					Some(case)
				}
				TSXToken::CloseBrace => {
					close_brace_pos = pos;
					break;
				}
				_ => todo!(),
			};
			let mut statements = Vec::new();
			loop {
				if let Some(Token(
					TSXToken::Keyword(TSXKeyword::Case)
					| TSXToken::Keyword(TSXKeyword::Default)
					| TSXToken::CloseBrace,
					_,
				)) = reader.peek()
				{
					break;
				}
				statements.push(Statement::from_reader(reader, state, settings)?);
				if let Some(Token(TSXToken::SemiColon, _)) = reader.peek() {
					reader.next();
				}
			}
			if let Some(case) = case {
				branches.push(SwitchBranch::Case(case, statements))
			} else {
				branches.push(SwitchBranch::Default(statements))
			}
		}
		Ok(Self { case, branches, position: start_span.union(&close_brace_pos) })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		buf.push_str("switch");
		settings.add_gap(buf);
		buf.push('(');
		self.case.to_string_from_buffer(buf, settings, depth);
		buf.push(')');
		settings.add_gap(buf);
		buf.push('{');
		for branch in self.branches.iter() {
			if settings.pretty {
				buf.push_new_line();
				settings.add_indent(depth + 1, buf);
			}
			match branch {
				SwitchBranch::Default(statements) => {
					buf.push_str("default:");
					for (at_end, stmt) in statements.iter().endiate() {
						if settings.pretty {
							buf.push_new_line();
							settings.add_indent(depth + 2, buf);
						}
						stmt.to_string_from_buffer(buf, settings, depth + 2);
						if stmt.requires_semi_colon() {
							buf.push(';');
						}
						if settings.pretty && !at_end {
							buf.push_new_line();
						}
					}
				}
				SwitchBranch::Case(case, statements) => {
					buf.push_str("case ");
					case.to_string_from_buffer(buf, settings, depth);
					buf.push(':');
					for (at_end, stmt) in statements.iter().endiate() {
						if settings.pretty {
							buf.push_new_line();
							settings.add_indent(depth + 2, buf);
						}
						stmt.to_string_from_buffer(buf, settings, depth + 2);
						if stmt.requires_semi_colon() {
							buf.push(';');
						}
						if settings.pretty && !at_end {
							buf.push_new_line();
						}
					}
				}
			}
		}
		if settings.pretty {
			buf.push_new_line();
			settings.add_indent(depth, buf);
		}
		buf.push('}');
	}
}
