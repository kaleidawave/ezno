use std::borrow::Cow;

use crate::TSXToken;
use source_map::Span;
use tokenizer_lib::{Token, TokenReader};

use crate::{
	errors::parse_lexing_error, tokens::token_as_identifier, ASTNode, Expression, NumberStructure,
	ParseOptions, ParseResult,
};

/// A key for a member in a class or object literal
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum PropertyKey {
	Ident(String, Span),
	StringLiteral(String, Span),
	NumberLiteral(NumberStructure, Span),
	/// Includes anything in the `[...]` maybe a symbol
	Computed(Box<Expression>, Span),
}

impl PropertyKey {
	pub fn get_position(&self) -> Cow<Span> {
		match self {
			PropertyKey::Ident(_, pos)
			| PropertyKey::StringLiteral(_, pos)
			| PropertyKey::NumberLiteral(_, pos)
			| PropertyKey::Computed(_, pos) => Cow::Borrowed(pos),
		}
	}
}

impl PartialEq<str> for PropertyKey {
	fn eq(&self, other: &str) -> bool {
		match self {
			PropertyKey::Ident(name, _) | PropertyKey::StringLiteral(name, _) => name == other,
			PropertyKey::NumberLiteral(_, _) | PropertyKey::Computed(_, _) => false,
		}
	}
}

impl ASTNode for PropertyKey {
	fn get_position(&self) -> Cow<Span> {
		match self {
			PropertyKey::Ident(_, pos)
			| PropertyKey::StringLiteral(_, pos)
			| PropertyKey::NumberLiteral(_, pos)
			| PropertyKey::Computed(_, pos) => Cow::Borrowed(pos),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		match reader.next().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::DoubleQuotedStringLiteral(content), position)
			| Token(TSXToken::SingleQuotedStringLiteral(content), position) => {
				Ok(Self::StringLiteral(content, position))
			}
			Token(TSXToken::NumberLiteral(value), position) => {
				Ok(Self::NumberLiteral(value.parse().unwrap(), position))
			}
			Token(TSXToken::OpenBracket, start_pos) => {
				let expression = Expression::from_reader(reader, state, settings)?;
				let end_pos = reader.expect_next(TSXToken::CloseBracket)?;
				Ok(Self::Computed(Box::new(expression), start_pos.union(&end_pos)))
			}
			token => {
				let (name, position) = token_as_identifier(token, "property key")?;
				Ok(Self::Ident(name, position))
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			Self::Ident(ident, _pos) => buf.push_str(ident.as_str()),
			Self::NumberLiteral(number, _) => buf.push_str(&number.to_string()),
			Self::StringLiteral(string, _) => {
				buf.push('"');
				buf.push_str(string.as_str());
				buf.push('"');
			}
			Self::Computed(expression, _) => {
				buf.push('[');
				expression.to_string_from_buffer(buf, settings, depth);
				buf.push(']');
			}
		}
	}
}

// TODO
// impl WithComment<PropertyKey> {
// 	pub(crate) fn visit<TData>(
// 		&self,
// 		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
// 		data: &mut TData,
// 		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
// 		location: PropertyKeyLocation,
// 	) {
// 		visitors.visit_variable(
// 			&ImmutableVariableOrPropertyPart::PropertyKey(self, location),
// 			data,
// 			chain,
// 		);
// 	}

// 	pub(crate) fn visit_mut<TData>(
// 		&mut self,
// 		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
// 		data: &mut TData,
// 		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
// 		location: PropertyKeyLocation,
// 	) {
// 		visitors.visit_variable_mut(
// 			&mut MutableVariablePart::PropertyKey(self, location),
// 			data,
// 			chain,
// 		);
// 	}
// }
