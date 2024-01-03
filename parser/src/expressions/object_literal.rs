use derive_partial_eq_extras::PartialEqExtras;
use iterator_endiate::EndiateIteratorExt;
use std::{fmt::Debug, mem};
use tokenizer_lib::sized_tokens::{TokenReaderWithTokenEnds, TokenStart};
use visitable_derive::Visitable;

use crate::{
	errors::parse_lexing_error,
	functions::{FunctionBased, MethodHeader},
	property_key::AlwaysPublic,
	throw_unexpected_token_with_token,
	visiting::Visitable,
	ASTNode, Block, Expression, FunctionBase, ParseOptions, ParseResult, PropertyKey, Span,
	TSXToken, Token, TokenReader, WithComment,
};

#[derive(Debug, Clone, Eq, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct ObjectLiteral {
	pub members: Vec<WithComment<ObjectLiteralMember>>,
	pub position: Span,
}

#[derive(Debug, Clone, PartialEqExtras)]
#[partial_eq_ignore_types(Span, VariableId)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ObjectLiteralMember {
	Spread(Expression, Span),
	Shorthand(String, Span),
	Property(PropertyKey<AlwaysPublic>, Expression, Span),
	Method(ObjectLiteralMethod),
}

impl crate::Visitable for ObjectLiteralMember {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		match self {
			ObjectLiteralMember::Shorthand(_, _)
			| ObjectLiteralMember::Property(_, _, _)
			| ObjectLiteralMember::Spread(_, _) => {}
			ObjectLiteralMember::Method(method) => method.visit(visitors, data, options, chain),
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		match self {
			ObjectLiteralMember::Property(_, _, _)
			| ObjectLiteralMember::Spread(_, _)
			| ObjectLiteralMember::Shorthand(_, _) => {}
			ObjectLiteralMember::Method(method) => method.visit_mut(visitors, data, options, chain),
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ObjectLiteralMethodBase;
pub type ObjectLiteralMethod = FunctionBase<ObjectLiteralMethodBase>;

impl FunctionBased for ObjectLiteralMethodBase {
	type Name = PropertyKey<AlwaysPublic>;
	type Header = MethodHeader;
	type Body = Block;

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<(Self::Header, Self::Name)> {
		Ok((MethodHeader::from_reader(reader), PropertyKey::from_reader(reader, state, options)?))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		header: &Self::Header,
		name: &Self::Name,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		header.to_string_from_buffer(buf);
		name.to_string_from_buffer(buf, options, depth);
	}

	fn header_left(header: &Self::Header) -> Option<source_map::Start> {
		header.get_start()
	}

	fn visit_name<TData>(
		name: &Self::Name,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::visiting::VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		name.visit(visitors, data, options, chain);
	}

	fn visit_name_mut<TData>(
		name: &mut Self::Name,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::visiting::VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		name.visit_mut(visitors, data, options, chain);
	}

	fn get_name(name: &Self::Name) -> Option<&str> {
		if let PropertyKey::Ident(name, ..) = name {
			Some(name.as_str())
		} else {
			None
		}
	}
}

impl Eq for ObjectLiteralMember {}

impl ASTNode for ObjectLiteral {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = reader.expect_next(TSXToken::OpenBrace)?;
		Self::from_reader_sub_open_curly(reader, state, options, start)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push('{');
		options.add_gap(buf);
		for (at_end, member) in self.members.iter().endiate() {
			member.to_string_from_buffer(buf, options, depth);
			if !at_end {
				buf.push(',');
				options.add_gap(buf);
			}
		}
		options.add_gap(buf);
		buf.push('}');
	}
}

impl ObjectLiteral {
	pub(crate) fn from_reader_sub_open_curly(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		start: TokenStart,
	) -> ParseResult<Self> {
		let mut members: Vec<WithComment<ObjectLiteralMember>> = Vec::new();
		loop {
			if matches!(reader.peek(), Some(Token(TSXToken::CloseBrace, _))) {
				break;
			}
			members.push(WithComment::from_reader(reader, state, options)?);
			if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
				reader.next();
			} else {
				break;
			}
		}
		let end = reader.expect_next_get_end(TSXToken::CloseBrace)?;
		Ok(ObjectLiteral { members, position: start.union(end) })
	}
}

impl ASTNode for ObjectLiteralMember {
	#[allow(clippy::similar_names)]
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		if let Some(Token(_, spread_start)) =
			reader.conditional_next(|tok| matches!(tok, TSXToken::Spread))
		{
			// TODO precedence okay?
			let expression = Expression::from_reader(reader, state, options)?;
			let position = spread_start.union(expression.get_position());
			return Ok(Self::Spread(expression, position));
		};

		// TODO this probably needs with comment here:
		while reader.conditional_next(TSXToken::is_comment).is_some() {}

		let mut header = MethodHeader::from_reader(reader);
		// Catch for named get or set :(
		let is_named_get_set_or_async = matches!(
			(reader.peek(), &header),
			(
				Some(Token(
					TSXToken::OpenParentheses
						| TSXToken::OpenChevron | TSXToken::Colon
						| TSXToken::Comma | TSXToken::CloseBrace,
					_
				)),
				MethodHeader::Get(..)
					| MethodHeader::Set(..)
					| MethodHeader::Regular { r#async: Some(_), .. }
			)
		);

		let key = if is_named_get_set_or_async {
			// Backtrack allowing `get` to be a key
			let (name, position) = match mem::take(&mut header) {
				MethodHeader::Get(kw) => ("get", kw.1),
				MethodHeader::Set(kw) => ("set", kw.1),
				MethodHeader::Regular { r#async: Some(kw), .. } => ("async", kw.1),
				MethodHeader::Regular { .. } => unreachable!(),
			};
			PropertyKey::Ident(name.to_owned(), position, ())
		} else {
			PropertyKey::from_reader(reader, state, options)?
		};

		let Token(token, _) = &reader.peek().ok_or_else(parse_lexing_error)?;
		match token {
			// Functions, (OpenChevron is for generic parameters)
			TSXToken::OpenParentheses | TSXToken::OpenChevron => {
				let method: ObjectLiteralMethod = FunctionBase::from_reader_with_header_and_name(
					reader, state, options, header, key,
				)?;

				Ok(Self::Method(method))
			}
			_ => {
				if header.is_some() {
					return crate::throw_unexpected_token(reader, &[TSXToken::OpenParentheses]);
				}
				if let Some(Token(TSXToken::Comma | TSXToken::CloseBrace, _)) = reader.peek() {
					if let PropertyKey::Ident(name, position, ()) = key {
						Ok(Self::Shorthand(name, position))
					} else {
						let token = reader.next().ok_or_else(parse_lexing_error)?;
						throw_unexpected_token_with_token(token, &[TSXToken::Colon])
					}
				} else {
					reader.expect_next(TSXToken::Colon)?;
					let expression = Expression::from_reader(reader, state, options)?;
					let position = key.get_position().union(expression.get_position());
					Ok(Self::Property(key, expression, position))
				}
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			Self::Property(name, expression, _) => {
				name.to_string_from_buffer(buf, options, depth);
				buf.push(':');
				options.add_gap(buf);
				expression.to_string_from_buffer(buf, options, depth);
			}
			Self::Shorthand(name, ..) => {
				buf.push_str(name.as_str());
			}
			Self::Method(func) => {
				func.to_string_from_buffer(buf, options, depth);
			}
			Self::Spread(spread_expr, _) => {
				buf.push_str("...");
				spread_expr.to_string_from_buffer(buf, options, depth);
			}
		};
	}

	fn get_position(&self) -> &Span {
		match self {
			Self::Method(method) => method.get_position(),
			Self::Shorthand(_, pos) | Self::Property(_, _, pos) | Self::Spread(_, pos) => pos,
		}
	}
}
