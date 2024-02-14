use crate::{
	derive_ASTNode,
	visiting::{Chain, VisitOptions, Visitable},
	Quoted, TSXToken,
};
use source_map::Span;
use std::fmt::Debug;
use temporary_annex::Annex;
use tokenizer_lib::{sized_tokens::TokenReaderWithTokenEnds, Token, TokenReader};

use crate::{
	errors::parse_lexing_error, tokens::token_as_identifier, ASTNode, Expression,
	NumberRepresentation, ParseOptions, ParseResult,
};

pub trait PropertyKeyKind: Debug + PartialEq + Eq + Clone + Sized + Send + Sync + 'static {
	fn parse_ident(
		first: Token<TSXToken, crate::TokenStart>,
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	) -> ParseResult<(String, Span, Self)>;

	fn is_private(&self) -> bool;

	/// TODO temp
	fn new_public() -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub struct AlwaysPublic;

// #[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
// const TYPES_ALWAYS_PUBLIC: &str = r"
// 	export type AlwaysPublic = false;
// ";

impl PropertyKeyKind for AlwaysPublic {
	fn parse_ident(
		first: Token<TSXToken, crate::TokenStart>,
		_reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	) -> ParseResult<(String, Span, Self)> {
		token_as_identifier(first, "property key")
			.map(|(name, position)| (name, position, Self::new_public()))
	}

	fn is_private(&self) -> bool {
		false
	}

	fn new_public() -> Self {
		AlwaysPublic
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub enum PublicOrPrivate {
	Public,
	Private,
}

// #[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
// const TYPES_PUBLIC_OR_PRIVATE: &str = r"
// 	export type PublicOrPrivate = boolean;
// ";

impl PropertyKeyKind for PublicOrPrivate {
	fn parse_ident(
		first: Token<TSXToken, crate::TokenStart>,
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	) -> ParseResult<(String, Span, Self)> {
		if matches!(first.0, TSXToken::HashTag) {
			token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "property key")
				.map(|(name, position)| (name, position, Self::Private))
		} else {
			token_as_identifier(first, "property key")
				.map(|(name, position)| (name, position, Self::Public))
		}
	}

	fn is_private(&self) -> bool {
		matches!(self, Self::Private)
	}

	fn new_public() -> Self {
		Self::Public
	}
}

/// A key for a member in a class or object literal
#[derive(Debug, PartialEq, Eq, Clone)]
#[apply(derive_ASTNode)]
pub enum PropertyKey<T: PropertyKeyKind> {
	Ident(String, Span, T),
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
			PropertyKey::Ident(_, _, p) => U::is_private(p),
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

impl<U: PropertyKeyKind> ASTNode for PropertyKey<U> {
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
				if token.0.is_comment() {
					// TODO could add marker?
					Self::from_reader(reader, state, options)
				} else {
					let (name, position, private) = U::parse_ident(token, reader)?;
					Ok(Self::Ident(name, position, private))
				}
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
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
				expression.to_string_from_buffer(buf, options, local);
				buf.push(']');
			}
		}
	}
}

// TODO visit expression?
impl Visitable for PropertyKey<PublicOrPrivate> {
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

impl Visitable for PropertyKey<AlwaysPublic> {
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
