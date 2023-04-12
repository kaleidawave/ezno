use std::borrow::Cow;

use crate::TSXToken;
use source_map::Span;
use tokenizer_lib::{Token, TokenReader};

use crate::{
	errors::parse_lexing_error, tokens::token_as_identifier, ASTNode, Expression, NumberStructure,
	ParseResult, ParseSettings,
};

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct PropertyId(u16);

impl PropertyId {
	pub fn new() -> Self {
		use std::sync::atomic::{AtomicU16, Ordering};

		static PROPERTY_ID_COUNTER: AtomicU16 = AtomicU16::new(2000);
		Self(PROPERTY_ID_COUNTER.fetch_add(1, Ordering::SeqCst))
	}

	pub fn new_known(id: u16) -> Self {
		Self(id)
	}

	pub fn unwrap_counter(&self) -> u16 {
		self.0
	}
}

// TODO not sure
#[cfg(feature = "self-rust-tokenize")]
impl self_rust_tokenize::SelfRustTokenize for PropertyId {
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		token_stream.extend(self_rust_tokenize::quote!(PropertyId::new()))
	}
}

/// A key for a member in a class or object literal
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum PropertyKey {
	Ident(String, PropertyId, Span),
	StringLiteral(String, PropertyId, Span),
	NumberLiteral(NumberStructure, PropertyId, Span),
	/// Includes anything in the `[...]` maybe a symbol
	Computed(Box<Expression>, PropertyId, Span),
}

impl PropertyKey {
	pub fn get_property_id(&self) -> PropertyId {
		match self {
			PropertyKey::Ident(_, variable_id, _)
			| PropertyKey::StringLiteral(_, variable_id, _)
			| PropertyKey::NumberLiteral(_, variable_id, _)
			| PropertyKey::Computed(_, variable_id, _) => *variable_id,
		}
	}

	pub fn get_position(&self) -> Cow<Span> {
		match self {
			PropertyKey::Ident(_, _, pos)
			| PropertyKey::StringLiteral(_, _, pos)
			| PropertyKey::NumberLiteral(_, _, pos)
			| PropertyKey::Computed(_, _, pos) => Cow::Borrowed(pos),
		}
	}
}

impl PartialEq<str> for PropertyKey {
	fn eq(&self, other: &str) -> bool {
		match self {
			PropertyKey::Ident(name, _, _) | PropertyKey::StringLiteral(name, _, _) => {
				name == other
			}
			PropertyKey::NumberLiteral(_, _, _) | PropertyKey::Computed(_, _, _) => false,
		}
	}
}

impl ASTNode for PropertyKey {
	fn get_position(&self) -> Cow<Span> {
		match self {
			PropertyKey::Ident(_, _, pos)
			| PropertyKey::StringLiteral(_, _, pos)
			| PropertyKey::NumberLiteral(_, _, pos)
			| PropertyKey::Computed(_, _, pos) => Cow::Borrowed(pos),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		match reader.next().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::DoubleQuotedStringLiteral(content), position)
			| Token(TSXToken::SingleQuotedStringLiteral(content), position) => {
				Ok(Self::StringLiteral(content, PropertyId::new(), position))
			}
			Token(TSXToken::NumberLiteral(value), position) => {
				Ok(Self::NumberLiteral(value.parse().unwrap(), PropertyId::new(), position))
			}
			Token(TSXToken::OpenBracket, start_pos) => {
				let expression = Expression::from_reader(reader, state, settings)?;
				let end_pos = reader.expect_next(TSXToken::CloseBracket)?;
				Ok(Self::Computed(
					Box::new(expression),
					PropertyId::new(),
					start_pos.union(&end_pos),
				))
			}
			token => {
				let (name, position) = token_as_identifier(token, "property key")?;
				Ok(Self::Ident(name, PropertyId::new(), position))
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		match self {
			Self::Ident(ident, _, _pos) => buf.push_str(ident.as_str()),
			Self::NumberLiteral(number, _, _) => buf.push_str(&number.to_string()),
			Self::StringLiteral(string, _, _) => {
				buf.push('"');
				buf.push_str(string.as_str());
				buf.push('"');
			}
			Self::Computed(expression, _, _) => {
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
