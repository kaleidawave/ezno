//! Contains wrappers for AST with comments

use super::{ASTNode, ParseError, Span, TSXToken, TokenReader};
use crate::{ParseOptions, Visitable};

use tokenizer_lib::Token;

#[derive(Debug, Clone, Eq)]
pub enum WithComment<T> {
	None(T),
	PrefixComment(String, T, Span),
	PostfixComment(T, String, Span),
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
			| WithComment::PrefixComment(_, item, _)
			| WithComment::PostfixComment(item, _, _) => {
				let inner = self_rust_tokenize::SelfRustTokenize::to_tokens(item);
				token_stream.extend(self_rust_tokenize::quote!(WithComment::None(#inner)))
			}
		}
	}
}

// TODO comments
#[cfg(feature = "serde-serialize")]
impl<T: serde::Serialize> serde::Serialize for WithComment<T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		self.get_ast_ref().serialize(serializer)
	}
}

impl<T: Visitable> Visitable for WithComment<T> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::VisitSettings,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		self.get_ast_ref().visit(visitors, data, options, chain)
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::VisitSettings,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		self.get_ast_mut().visit_mut(visitors, data, options, chain)
	}
}

impl<T: PartialEq> PartialEq for WithComment<T> {
	fn eq(&self, other: &Self) -> bool {
		self.get_ast_ref() == other.get_ast_ref()
	}
}

impl<T> From<T> for WithComment<T> {
	fn from(item: T) -> Self {
		Self::None(item)
	}
}

impl<T> WithComment<T> {
	pub fn get_ast(self) -> T {
		match self {
			Self::None(ast) | Self::PrefixComment(_, ast, _) | Self::PostfixComment(ast, _, _) => {
				ast
			}
		}
	}

	pub fn get_ast_ref(&self) -> &T {
		match self {
			Self::None(ast) | Self::PrefixComment(_, ast, _) | Self::PostfixComment(ast, _, _) => {
				ast
			}
		}
	}

	pub fn get_ast_mut(&mut self) -> &mut T {
		match self {
			Self::None(ast) | Self::PrefixComment(_, ast, _) | Self::PostfixComment(ast, _, _) => {
				ast
			}
		}
	}

	pub fn map<U>(self, cb: impl FnOnce(T) -> U) -> WithComment<U> {
		match self {
			Self::None(item) => WithComment::None(cb(item)),
			Self::PrefixComment(comment, item, position) => {
				WithComment::PrefixComment(comment, cb(item), position)
			}
			Self::PostfixComment(item, comment, position) => {
				WithComment::PostfixComment(cb(item), comment, position)
			}
		}
	}
}

impl<T: ASTNode> ASTNode for WithComment<T> {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> Result<WithComment<T>, ParseError> {
		if let Some(token) =
			reader.conditional_next(|t| matches!(t, TSXToken::MultiLineComment(..)))
		{
			let Token(TSXToken::MultiLineComment(comment), position) = token else {
				unreachable!();
			};
			let item = T::from_reader(reader, state, options)?;
			let position = position.union(item.get_position());
			Ok(Self::PrefixComment(comment, item, position))
		} else {
			let item = T::from_reader(reader, state, options)?;
			if let Some(token) =
				reader.conditional_next(|t| matches!(t, TSXToken::MultiLineComment(..)))
			{
				let end = token.get_span();
				let Token(TSXToken::MultiLineComment(comment), _) = token else {
					unreachable!();
				};
				let position = item.get_position().union(end);
				Ok(Self::PostfixComment(item, comment, position))
			} else {
				Ok(Self::None(item))
			}
		}
	}

	fn get_position(&self) -> &Span {
		match self {
			Self::None(ast) => ast.get_position(),
			Self::PostfixComment(_, _, position) | Self::PrefixComment(_, _, position) => position,
		}
	}

	fn to_string_from_buffer<U: source_map::ToString>(
		&self,
		buf: &mut U,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			Self::None(ast) => ast.to_string_from_buffer(buf, options, depth),
			Self::PrefixComment(comment, ast, _) => {
				if options.should_add_comment() {
					buf.push_str("/*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/ ");
				}
				ast.to_string_from_buffer(buf, options, depth);
			}
			Self::PostfixComment(ast, comment, _) => {
				ast.to_string_from_buffer(buf, options, depth);
				if options.should_add_comment() {
					buf.push_str(" /*");
					buf.push_str_contains_new_line(comment.as_str());
					buf.push_str("*/");
				}
			}
		}
	}
}
