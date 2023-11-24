use crate::{
	visiting::{Chain, VisitOptions, Visitable},
	Quoted, TSXToken, WithComment,
};
use source_map::Span;
use std::fmt::Debug;
use temporary_annex::Annex;
use tokenizer_lib::{sized_tokens::TokenReaderWithTokenEnds, Token, TokenReader};

use crate::{
	errors::parse_lexing_error, tokens::token_as_identifier, ASTNode, Expression,
	NumberRepresentation, ParseOptions, ParseResult,
};

// pub enum GeneralPropertyKey {
// 	AlwaysPublicPropertyKey(PropertyKey<AlwaysPublic>),
// 	PublicOrPrivatePropertyKey(PropertyKey<PublicOrPrivate>),
// }

pub trait PropertyKeyKind: Debug + PartialEq + Eq + Clone {
	type Private: Debug + Sync + Send + Clone + Copy + PartialEq + Eq;

	fn parse_ident(
		first: Token<TSXToken, crate::TokenStart>,
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	) -> ParseResult<(String, Span, Self::Private)>;

	fn is_private(p: Self::Private) -> bool;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlwaysPublic;

impl PropertyKeyKind for AlwaysPublic {
	type Private = ();

	fn parse_ident(
		first: Token<TSXToken, crate::TokenStart>,
		_reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	) -> ParseResult<(String, Span, Self::Private)> {
		token_as_identifier(first, "property key").map(|(name, position)| (name, position, ()))
	}

	fn is_private(_p: Self::Private) -> bool {
		true
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PublicOrPrivate;

impl PropertyKeyKind for PublicOrPrivate {
	type Private = bool;

	fn parse_ident(
		first: Token<TSXToken, crate::TokenStart>,
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	) -> ParseResult<(String, Span, Self::Private)> {
		if matches!(first.0, TSXToken::HashTag) {
			token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "property key")
				.map(|(name, position)| (name, position, true))
		} else {
			token_as_identifier(first, "property key")
				.map(|(name, position)| (name, position, false))
		}
	}

	fn is_private(p: Self::Private) -> bool {
		p
	}
}

/// A key for a member in a class or object literal
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum PropertyKey<T: PropertyKeyKind> {
	Ident(String, Span, T::Private),
	StringLiteral(String, Quoted, Span),
	NumberLiteral(NumberRepresentation, Span),
	/// Includes anything in the `[...]` maybe a symbol
	Computed(Box<Expression>, Span),
}

impl<U: PropertyKeyKind> PropertyKey<U> {
	pub fn get_position(&self) -> &Span {
		match self {
			PropertyKey::Ident(_, pos, _)
			| PropertyKey::StringLiteral(_, _, pos)
			| PropertyKey::NumberLiteral(_, pos)
			| PropertyKey::Computed(_, pos) => pos,
		}
	}

	pub fn is_private(&self) -> bool {
		match self {
			PropertyKey::Ident(_, _, p) => U::is_private(*p),
			_ => false,
		}
	}
}

impl<U: PropertyKeyKind> PartialEq<str> for PropertyKey<U> {
	fn eq(&self, other: &str) -> bool {
		match self {
			PropertyKey::Ident(name, _, _) | PropertyKey::StringLiteral(name, _, _) => {
				name == other
			}
			PropertyKey::NumberLiteral(_, _) | PropertyKey::Computed(_, _) => false,
		}
	}
}

impl<U: PropertyKeyKind + 'static> ASTNode for PropertyKey<U> {
	fn get_position(&self) -> &Span {
		match self {
			PropertyKey::Ident(_, pos, _)
			| PropertyKey::StringLiteral(_, _, pos)
			| PropertyKey::NumberLiteral(_, pos)
			| PropertyKey::Computed(_, pos) => pos,
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		match reader.next().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::StringLiteral(content, quoted), start) => {
				let position = start.with_length(content.len() + 2);
				Ok(Self::StringLiteral(content, quoted, position))
			}
			Token(TSXToken::NumberLiteral(value), start) => {
				let position = start.with_length(value.len());
				Ok(Self::NumberLiteral(value.parse().unwrap(), position))
			}
			Token(TSXToken::OpenBracket, start) => {
				let expression = Expression::from_reader(reader, state, options)?;
				let end = reader.expect_next_get_end(TSXToken::CloseBracket)?;
				Ok(Self::Computed(Box::new(expression), start.union(end)))
			}
			token => {
				let (name, position, private) = U::parse_ident(token, reader)?;
				Ok(Self::Ident(name, position, private))
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
			Self::Ident(ident, _pos, _) => buf.push_str(ident.as_str()),
			Self::NumberLiteral(number, _) => buf.push_str(&number.to_string()),
			Self::StringLiteral(string, quoted, _) => {
				buf.push(quoted.as_char());
				buf.push_str(string.as_str());
				buf.push(quoted.as_char());
			}
			Self::Computed(expression, _) => {
				buf.push('[');
				expression.to_string_from_buffer(buf, options, depth);
				buf.push(']');
			}
		}
	}
}

impl Visitable for WithComment<PropertyKey<PublicOrPrivate>> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		_options: &VisitOptions,
		chain: &mut Annex<Chain>,
	) {
		visitors.visit_variable(
			&crate::visiting::ImmutableVariableOrProperty::ClassPropertyKey(self),
			data,
			chain,
		);
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		_options: &VisitOptions,
		chain: &mut Annex<Chain>,
	) {
		visitors.visit_variable_mut(
			&mut crate::visiting::MutableVariableOrProperty::ClassPropertyKey(self),
			data,
			chain,
		);
	}
}

impl Visitable for WithComment<PropertyKey<AlwaysPublic>> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		_options: &VisitOptions,
		chain: &mut Annex<Chain>,
	) {
		visitors.visit_variable(
			&crate::visiting::ImmutableVariableOrProperty::ObjectPropertyKey(self),
			data,
			chain,
		);
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		_options: &VisitOptions,
		chain: &mut Annex<Chain>,
	) {
		visitors.visit_variable_mut(
			&mut crate::visiting::MutableVariableOrProperty::ObjectPropertyKey(self),
			data,
			chain,
		);
	}
}
