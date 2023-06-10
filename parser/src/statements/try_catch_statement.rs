use std::borrow::Cow;

use crate::{
	ASTNode, Block, ParseError, ParseErrors, TSXKeyword, TSXToken, TypeReference, VariableField,
	VariableFieldInSourceCode, WithComment,
};
use source_map::Span;
use tokenizer_lib::Token;
use visitable_derive::Visitable;

// Note: Removing "WithComment" causes a trait bound error here
pub type ExceptionVarField = WithComment<VariableField<VariableFieldInSourceCode>>;

/// Try/catch/finally clauses can be used in 3 specific combinations.
///
/// See: https://developer.mozilla.org/en-US/docs/web/javascript/reference/statements/try...catch#description
#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum TryCatchStatement {
	TryCatch {
		try_inner: Block,
		catch_inner: Block,
		exception_var: Option<(ExceptionVarField, Option<TypeReference>)>,
		position: Span,
	},
	TryFinally {
		try_inner: Block,
		finally_inner: Block,
		position: Span,
	},
	TryCatchFinally {
		try_inner: Block,
		catch_inner: Block,
		exception_var: Option<(ExceptionVarField, Option<TypeReference>)>,
		finally_inner: Block,
		position: Span,
	},
}

impl ASTNode for TryCatchStatement {
	fn get_position(&self) -> Cow<Span> {
		match self {
			TryCatchStatement::TryCatch { position, .. }
			| TryCatchStatement::TryFinally { position, .. }
			| TryCatchStatement::TryCatchFinally { position, .. } => Cow::Borrowed(position),
		}
	}

	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseSettings,
	) -> Result<Self, crate::ParseError> {
		let start_span = reader.expect_next(TSXToken::Keyword(TSXKeyword::Try))?;
		let try_inner = Block::from_reader(reader, state, settings)?;

		let mut catch_inner: Option<Block> = None;
		let mut exception_var: Option<(ExceptionVarField, Option<TypeReference>)> = None;

		// Optional `catch` clause
		if let Some(Token(TSXToken::Keyword(TSXKeyword::Catch), _)) = reader.peek() {
			reader.expect_next(TSXToken::Keyword(TSXKeyword::Catch))?;

			// Optional exception variable field `catch (e)`
			if let Some(Token(TSXToken::OpenParentheses, _)) = reader.peek() {
				reader.expect_next(TSXToken::OpenParentheses)?;
				let variable_field =
					WithComment::<VariableField<VariableFieldInSourceCode>>::from_reader(
						reader, state, settings,
					)?;

				// Optional type reference `catch (e: type)`
				let mut exception_var_type: Option<TypeReference> = None;
				if let Some(Token(TSXToken::Colon, _)) = reader.peek() {
					reader.expect_next(TSXToken::Colon)?;
					exception_var_type = Some(TypeReference::from_reader(reader, state, settings)?);
				}
				exception_var = Some((variable_field, exception_var_type));

				reader.expect_next(TSXToken::CloseParentheses)?;
			}
		}

		// Optional `finally` clause
		let mut finally_inner: Option<Block> = None;
		if let Some(Token(TSXToken::Keyword(TSXKeyword::Finally), _)) = reader.peek() {
			reader.expect_next(TSXToken::Keyword(TSXKeyword::Finally))?;
			finally_inner = Some(Block::from_reader(reader, state, settings)?);
		}

		// Wrap in enum variant and return
		if let Some(catch_inner) = catch_inner {
			if let Some(finally_inner) = finally_inner {
				return Ok(Self::TryCatchFinally {
					position: start_span.union(&finally_inner.get_position()),
					try_inner,
					exception_var,
					catch_inner,
					finally_inner,
				});
			} else {
				Ok(Self::TryCatch {
					position: start_span.union(&catch_inner.get_position()),
					try_inner,
					exception_var,
					catch_inner,
				})
			}
		} else {
			if let Some(finally_inner) = finally_inner {
				Ok(Self::TryFinally {
					position: start_span.union(&finally_inner.get_position()),
					try_inner,
					finally_inner,
				})
			} else {
				Err(ParseError::new(ParseErrors::ExpectedCatchOrFinally, start_span))
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		// todo
		buf.push_str("try");
		settings.add_gap(buf);
		self.inner.to_string_from_buffer(buf, settings, depth + 1);
		settings.add_gap(buf);
		buf.push_str("catch");
		settings.add_gap(buf);
		buf.push('(');
		self.exception_var.to_string_from_buffer(buf, settings, depth);
		buf.push(')');
		settings.add_gap(buf);
		self.catch_inner.to_string_from_buffer(buf, settings, depth + 1);
	}
}
