//! Contains wrappers for AST with comments

use super::{ASTNode, ParseError, Span, TSXToken, TokenReader};
use crate::ParseSettings;
use std::{borrow::Cow, mem};
use tokenizer_lib::Token;

#[derive(Debug, Clone, Eq)]
pub enum WithComment<T> {
	None(T),
	PrefixComment(String, T),
	PostfixComment(T, String),
}

// Ignore comments for now
#[cfg(feature = "self-rust-tokenize")]
impl<T> self_rust_tokenize::SelfRustTokenize for WithComment<T>
where
	T: self_rust_tokenize::SelfRustTokenize,
{
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		match self {
			WithComment::None(item)
			| WithComment::PrefixComment(_, item)
			| WithComment::PostfixComment(item, _) => {
				let inner = self_rust_tokenize::SelfRustTokenize::to_tokens(item);
				token_stream.extend(self_rust_tokenize::quote!(WithComment::None(#inner)))
			}
		}
	}
}

impl<T: PartialEq> PartialEq for WithComment<T> {
	fn eq(&self, other: &Self) -> bool {
		self.get_ast() == other.get_ast()
	}
}

impl<T> From<T> for WithComment<T> {
	fn from(item: T) -> Self {
		Self::None(item)
	}
}

impl<T> WithComment<T> {
	pub fn get_ast(&self) -> &T {
		match self {
			Self::None(ast) => ast,
			Self::PrefixComment(_, ast) => ast,
			Self::PostfixComment(ast, _) => ast,
		}
	}

	pub fn get_ast_mut(&mut self) -> &mut T {
		match self {
			Self::None(ast) => ast,
			Self::PrefixComment(_, ast) => ast,
			Self::PostfixComment(ast, _) => ast,
		}
	}

	// TODO not sure about location of this
	pub fn add_comment(&mut self, comment: &str) {
		match self {
			Self::None(t) => {
				let t = mem::replace(t, unsafe { mem::zeroed() });
				*self = Self::PrefixComment(comment.to_owned(), t);
			}
			Self::PrefixComment(prev_comment, _) | Self::PostfixComment(_, prev_comment) => {
				prev_comment.push_str(comment)
			}
		}
	}

	pub fn unwrap_ast(self) -> T {
		match self {
			Self::None(ast) | Self::PrefixComment(_, ast) | Self::PostfixComment(ast, _) => ast,
		}
	}

	pub fn as_ref_mut(&mut self) -> WithComment<&mut T> {
		match self {
			WithComment::None(t) => WithComment::None(t),
			WithComment::PrefixComment(_, _) => todo!(),
			WithComment::PostfixComment(_, _) => todo!(),
		}
	}

	pub fn map<U>(self, cb: impl FnOnce(T) -> U) -> WithComment<U> {
		match self {
			Self::None(item) => WithComment::None(cb(item)),
			Self::PrefixComment(_, _) => todo!(),
			Self::PostfixComment(_, _) => todo!(),
		}
	}
}

impl<T: ASTNode> ASTNode for WithComment<T> {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> Result<WithComment<T>, ParseError> {
		if matches!(reader.peek(), Some(Token(TSXToken::MultiLineComment(..), _))) {
			let comment = if let TSXToken::MultiLineComment(comment) = reader.next().unwrap().0 {
				comment
			} else {
				unreachable!();
			};
			Ok(Self::PrefixComment(comment, T::from_reader(reader, state, settings)?))
		} else {
			let key = T::from_reader(reader, state, settings)?;
			if matches!(reader.peek(), Some(Token(TSXToken::MultiLineComment(..), _))) {
				let comment = if let TSXToken::MultiLineComment(comment) = reader.next().unwrap().0
				{
					comment
				} else {
					unreachable!();
				};
				Ok(Self::PostfixComment(key, comment))
			} else {
				Ok(Self::None(key))
			}
		}
	}

	// TODO doesn't include comment space might be fine
	fn get_position(&self) -> Cow<Span> {
		match self {
			Self::None(ast) => ast.get_position(),
			Self::PrefixComment(_, ast) => ast.get_position(),
			Self::PostfixComment(ast, _) => ast.get_position(),
		}
	}

	fn to_string_from_buffer<U: source_map::ToString>(
		&self,
		buf: &mut U,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		match self {
			Self::None(ast) => ast.to_string_from_buffer(buf, settings, depth),
			Self::PrefixComment(comment, ast) => {
				if settings.should_add_comment() {
					buf.push_str("/*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/ ");
				}
				ast.to_string_from_buffer(buf, settings, depth);
			}
			Self::PostfixComment(ast, comment) => {
				ast.to_string_from_buffer(buf, settings, depth);
				if settings.should_add_comment() {
					buf.push_str(" /*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/");
				}
			}
		}
	}
}
