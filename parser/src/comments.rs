//! Contains wrappers for AST with comments

use super::{ASTNode, Span};
use crate::ParseResult;
use visitable_derive::Visitable;

#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
#[derive(Debug, Clone, Visitable)]
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
		let inner = self_rust_tokenize::SelfRustTokenize::to_tokens(self.get_ast_ref());
		token_stream.extend(self_rust_tokenize::quote!(WithComment::None(#inner)));
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
	fn get_position(&self) -> Span {
		match self {
			Self::None(ast) => ast.get_position(),
			Self::PostfixComment(_, _, position) | Self::PrefixComment(_, _, position) => *position,
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		if reader.is_operator_advance("/*") {
			let comment = reader.parse_comment_literal(true)?.to_owned();
			let item = T::from_reader(reader)?;
			let position = start.union(item.get_position());
			Ok(Self::PrefixComment(comment, item, position))
		} else {
			let item = T::from_reader(reader)?;
			if reader.is_operator_advance("/*") {
				let comment = reader.parse_comment_literal(true)?.to_owned();
				let position = start.union(reader.get_end());
				Ok(Self::PostfixComment(item, comment, position))
			} else {
				Ok(Self::None(item))
			}
		}
	}

	fn to_string_from_buffer<U: source_map::ToString>(
		&self,
		buf: &mut U,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			Self::None(ast) => ast.to_string_from_buffer(buf, options, local),
			Self::PrefixComment(content, ast, _) => {
				if options.should_add_comment(content) {
					buf.push_str("/*");
					if options.pretty {
						// Perform indent correction
						// Have to use '\n' as `.lines` with it's handling of '\r'
						for (idx, line) in content.split('\n').enumerate() {
							if idx > 0 {
								buf.push_new_line();
							}
							options.add_indent(local.depth, buf);
							buf.push_str(line.trim());
						}
					} else {
						buf.push_str_contains_new_line(content.as_str());
					}
					buf.push_str("*/");
				}
				ast.to_string_from_buffer(buf, options, local);
			}
			Self::PostfixComment(ast, comment, _) => {
				ast.to_string_from_buffer(buf, options, local);
				if options.should_add_comment(comment) {
					buf.push_str("/*");
					if options.pretty {
						// Perform indent correction
						for (idx, line) in comment.split('\n').enumerate() {
							if idx > 0 {
								buf.push_new_line();
							}
							options.add_indent(local.depth, buf);
							buf.push_str(line.trim());
						}
					} else {
						buf.push_str_contains_new_line(comment.as_str());
					}
					buf.push_str("*/");
				}
			}
		}
	}
}
