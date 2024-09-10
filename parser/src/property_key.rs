use crate::{
	derive_ASTNode,
	visiting::{Chain, VisitOptions, Visitable},
	Quoted,
};
use get_field_by_type::GetFieldByType;
use source_map::Span;
use std::fmt::Debug;
use temporary_annex::Annex;

use crate::{
	errors::parse_lexing_error, number::NumberRepresentation, ASTNode, Expression, ParseOptions,
	ParseResult,
};

pub trait PropertyKeyKind: Debug + PartialEq + Eq + Clone + Sized + Send + Sync + 'static {
	fn parse_identifier(reader: &mut crate::new::Lexer) -> ParseResult<(String, Span, Self)>;

	fn is_private(&self) -> bool;

	/// TODO temp
	fn new_public() -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub struct AlwaysPublic;

// #[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
#[allow(dead_code)]
// const TYPES_ALWAYS_PUBLIC: &str = r"
// 	export type AlwaysPublic = false;
// ";

impl PropertyKeyKind for AlwaysPublic {
	fn parse_identifier(reader: &mut crate::new::Lexer) -> ParseResult<(String, Span, Self)> {
		let start = reader.get_start();
		let name = reader.parse_identifier().expect("TODO");
		Ok((name.to_owned(), start.with_length(name.len()), Self::new_public()))
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
#[allow(dead_code)]
// const TYPES_PUBLIC_OR_PRIVATE: &str = r"
// 	export type PublicOrPrivate = boolean;
// ";

impl PropertyKeyKind for PublicOrPrivate {
	fn parse_identifier(reader: &mut crate::new::Lexer) -> ParseResult<(String, Span, Self)> {
		let start = reader.get_start();
		if reader.is_operator_advance("#") {
			let name = reader.parse_identifier().expect("TODO");
			Ok((name.to_owned(), start.with_length(name.len()), Self::Private))
		} else {
			let name = reader.parse_identifier().expect("TODO");
			Ok((name.to_owned(), start.with_length(name.len()), Self::Public))
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
#[apply(derive_ASTNode)]
#[derive(Debug, PartialEq, Eq, Clone, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum PropertyKey<T: PropertyKeyKind> {
	Identifier(String, Span, T),
	StringLiteral(String, Quoted, Span),
	NumberLiteral(NumberRepresentation, Span),
	/// Includes anything in the `[...]` maybe a symbol
	Computed(Box<Expression>, Span),
}

impl<U: PropertyKeyKind> PropertyKey<U> {
	pub fn is_private(&self) -> bool {
		match self {
			PropertyKey::Identifier(_, _, p) => U::is_private(p),
			_ => false,
		}
	}
}

impl<U: PropertyKeyKind> PartialEq<str> for PropertyKey<U> {
	fn eq(&self, other: &str) -> bool {
		match self {
			PropertyKey::Identifier(name, _, _) | PropertyKey::StringLiteral(name, _, _) => {
				name == other
			}
			PropertyKey::NumberLiteral(_, _) | PropertyKey::Computed(_, _) => false,
		}
	}
}

impl<U: PropertyKeyKind> ASTNode for PropertyKey<U> {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		if reader.starts_with('"') || reader.starts_with('\'') {
			let (content, quoted) = reader.parse_string_literal().expect("TODO");
			let position = start.with_length(content.len() + 2);
			Ok(Self::StringLiteral(content.to_owned(), quoted, position))
		} else if reader.starts_with_number() {
			let (value, length) = reader.parse_number_literal().expect("TODO");
			let position = start.with_length(length as usize);
			Ok(Self::NumberLiteral(value, position))
		} else if reader.is_operator_advance("[") {
			let expression = Expression::from_reader(reader)?;
			let end = reader.expect(']')?;
			Ok(Self::Computed(Box::new(expression), start.union(end)))
		} else {
			let (name, position, private) = U::parse_identifier(reader)?;
			Ok(Self::Identifier(name, position, private))
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			Self::Identifier(ident, _pos, _) => buf.push_str(ident.as_str()),
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
