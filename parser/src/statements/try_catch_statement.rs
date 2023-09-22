use crate::{
	ASTNode, Block, ParseError, ParseErrors, TSXKeyword, TSXToken, TypeAnnotation, VariableField,
	VariableFieldInSourceCode, WithComment,
};
use source_map::Span;
use tokenizer_lib::Token;
use visitable_derive::Visitable;

pub type ExceptionVarField = WithComment<VariableField<VariableFieldInSourceCode>>;

#[derive(Debug, PartialEq, Eq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct TryCatchStatement {
	pub try_inner: Block,
	pub catch_inner: Option<Block>,
	pub exception_var: Option<(ExceptionVarField, Option<TypeAnnotation>)>,
	pub finally_inner: Option<Block>,
	pub position: Span,
}

impl ASTNode for TryCatchStatement {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseOptions,
	) -> Result<Self, crate::ParseError> {
		let start = reader.expect_next(TSXToken::Keyword(TSXKeyword::Try))?;
		let try_inner = Block::from_reader(reader, state, settings)?;

		let mut catch_inner: Option<Block> = None;
		let mut exception_var: Option<(ExceptionVarField, Option<TypeAnnotation>)> = None;

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
				let mut exception_var_type: Option<TypeAnnotation> = None;
				if let Some(Token(TSXToken::Colon, _)) = reader.peek() {
					reader.expect_next(TSXToken::Colon)?;
					exception_var_type =
						Some(TypeAnnotation::from_reader(reader, state, settings)?);
				}
				exception_var = Some((variable_field, exception_var_type));

				reader.expect_next(TSXToken::CloseParentheses)?;
			}

			catch_inner = Some(Block::from_reader(reader, state, settings)?);
		}

		// Optional `finally` clause
		let mut finally_inner: Option<Block> = None;
		if let Some(Token(TSXToken::Keyword(TSXKeyword::Finally), _)) = reader.peek() {
			reader.expect_next(TSXToken::Keyword(TSXKeyword::Finally))?;
			finally_inner = Some(Block::from_reader(reader, state, settings)?);
		}

		// Determine span based on which clauses are present
		let position: Span = if let Some(finally_block) = &finally_inner {
			start.union(finally_block.get_position())
		} else if let Some(catch_block) = &catch_inner {
			start.union(catch_block.get_position())
		} else {
			// Parse error if neither catch nor finally clause is present
			return Err(ParseError::new(
				ParseErrors::ExpectedCatchOrFinally,
				reader.next().unwrap().get_span(),
			));
		};

		Ok(Self { position, try_inner, exception_var, catch_inner, finally_inner })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		// Required `try` block
		buf.push_str("try");
		settings.add_gap(buf);
		self.try_inner.to_string_from_buffer(buf, settings, depth + 1);

		// Optional `catch` block
		if let Some(catch) = &self.catch_inner {
			settings.add_gap(buf);
			buf.push_str("catch");
			settings.add_gap(buf);

			// Optional exception variable: `catch (e)`
			if let Some((exception_var, exception_var_type)) = &self.exception_var {
				buf.push('(');
				exception_var.to_string_from_buffer(buf, settings, depth);

				// Optional type annotation: `catch (e: any)`
				if let Some(exception_var_type) = exception_var_type {
					buf.push_str(": ");
					exception_var_type.to_string_from_buffer(buf, settings, depth);
				}
				buf.push(')');
				settings.add_gap(buf);
			}

			catch.to_string_from_buffer(buf, settings, depth + 1);
		}

		// Optional `finally` block
		if let Some(finally) = &self.finally_inner {
			settings.add_gap(buf);
			buf.push_str("finally");
			settings.add_gap(buf);
			finally.to_string_from_buffer(buf, settings, depth + 1);
		}
	}
}
