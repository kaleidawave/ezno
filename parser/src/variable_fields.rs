/// Contains:
/// - [VariableId] given to variable declaring items
/// - [VariableField] for destructuring things and its nested derivatives + visiting behavior + tests for self
use std::fmt::Debug;

use crate::{
	errors::parse_lexing_error, parse_bracketed, property_key::PropertyKey,
	throw_unexpected_token_with_token, tokens::token_as_identifier, ASTNode, CursorId, Expression,
	ImmutableVariableOrProperty, MutableVariableOrProperty, ParseError, ParseOptions, ParseResult,
	Span, TSXToken, Token, VisitOptions, Visitable, WithComment,
};

use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use tokenizer_lib::TokenReader;

#[derive(Debug, PartialEqExtras, Eq, Clone, GetFieldByType)]
#[partial_eq_ignore_types(Span)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum VariableIdentifier {
	Standard(String, Span),
	#[cfg_attr(feature = "self-rust-tokenize", self_tokenize_field(0))]
	Cursor(CursorId<Self>, Span),
}

impl ASTNode for VariableIdentifier {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		_state: &mut crate::ParsingState,
		_options: &ParseOptions,
	) -> ParseResult<Self> {
		let token = reader.next().ok_or_else(parse_lexing_error)?;
		Ok(if let Token(TSXToken::Cursor(id), start) = token {
			Self::Cursor(id.into_cursor(), start.with_length(0))
		} else {
			let (ident, span) = token_as_identifier(token, "variable identifier")?;
			Self::Standard(ident, span)
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		_options: &crate::ToStringOptions,
		_depth: u8,
	) {
		buf.push_str(self.as_str());
	}

	fn get_position(&self) -> &Span {
		self.get()
	}
}

impl VariableIdentifier {
	/// TODO temp
	pub fn as_str(&self) -> &str {
		match self {
			VariableIdentifier::Standard(name, _) => name.as_str(),
			VariableIdentifier::Cursor(_, _) => "",
		}
	}
}

/// A variable declaration name, used in variable declarations and function parameters.
/// See [destructuring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment)
#[derive(Debug, Clone)]
pub enum VariableField<T: VariableFieldKind> {
	/// `x`
	Name(VariableIdentifier),
	/// `[x, y, z]`
	/// TODO spread last
	Array(Vec<ArrayDestructuringField<T>>, Span),
	/// `{ x, y: z }`.
	/// TODO spread last
	Object(Vec<WithComment<ObjectDestructuringField<T>>>, Span),
}

#[cfg(feature = "self-rust-tokenize")]
impl<T: VariableFieldKind> self_rust_tokenize::SelfRustTokenize for VariableField<T>
where
	T::OptionalExpression: self_rust_tokenize::SelfRustTokenize,
{
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		use self_rust_tokenize::{quote, SelfRustTokenize};
		let tokens = match self {
			VariableField::Name(identifier) => {
				let tokens = identifier.to_tokens();
				quote!(VariableField::Name(#tokens))
			}
			VariableField::Array(value, span) => {
				let (value, span) =
					(SelfRustTokenize::to_tokens(value), SelfRustTokenize::to_tokens(span));
				quote!(VariableField::Array(#value, #span))
			}
			VariableField::Object(value, span) => {
				let (value, span) =
					(SelfRustTokenize::to_tokens(value), SelfRustTokenize::to_tokens(span));
				quote!(VariableField::Object(#value, #span))
			}
		};
		token_stream.extend(tokens);
	}
}

#[cfg(feature = "serde-serialize")]
impl<T: VariableFieldKind> serde::Serialize for VariableField<T>
where
	T: serde::Serialize,
	T::OptionalExpression: serde::Serialize,
{
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		match self {
			VariableField::Name(name) => {
				serializer.serialize_newtype_variant("VariableField", 0, "Name", name)
			}
			VariableField::Array(items, _) => {
				serializer.serialize_newtype_variant("VariableField", 0, "Array", items)
			}
			VariableField::Object(items, _) => {
				serializer.serialize_newtype_variant("VariableField", 0, "Object", items)
			}
		}
	}
}

impl<T: VariableFieldKind> From<VariableIdentifier> for VariableField<T> {
	fn from(value: VariableIdentifier) -> Self {
		Self::Name(value)
	}
}

impl<T: VariableFieldKind + PartialEq> PartialEq for VariableField<T> {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Name(l0), Self::Name(r0)) => l0 == r0,
			(Self::Array(l0, _), Self::Array(r0, _)) => l0 == r0,
			(Self::Object(l0, _), Self::Object(r0, _)) => l0 == r0,
			_ => false,
		}
	}
}

impl<T: VariableFieldKind> Eq for VariableField<T> {}

/// Variable field can be used in type annotations but cannot have a value
///
/// TODO value assignment this is VariableOrFieldAccess thingy
///
/// TODO could have get_optional_expression_as_option(&Self::OptionalExpression) -> Option<Expression>
pub trait VariableFieldKind: PartialEq + Eq + Debug + Clone + 'static {
	type OptionalExpression: PartialEq + Eq + Debug + Clone + Sync + Send;

	fn optional_expression_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> Result<Self::OptionalExpression, ParseError>;

	fn optional_expression_to_string_from_buffer<T: source_map::ToString>(
		optional_expression: &Self::OptionalExpression,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	);

	fn optional_expression_get_position(
		optional_expression: &Self::OptionalExpression,
	) -> Option<&Span>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct VariableFieldInSourceCode;

impl VariableFieldKind for VariableFieldInSourceCode {
	type OptionalExpression = Option<Expression>;

	fn optional_expression_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> Result<Self::OptionalExpression, ParseError> {
		Ok(if reader.conditional_next(|tok| matches!(tok, TSXToken::Assign)).is_some() {
			Some(Expression::from_reader(reader, state, options)?)
		} else {
			None
		})
	}

	fn optional_expression_to_string_from_buffer<T: source_map::ToString>(
		optional_expression: &Self::OptionalExpression,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		if let Some(optional_expression) = optional_expression {
			buf.push_str(if options.pretty { " = " } else { "=" });
			optional_expression.to_string_from_buffer(buf, options, depth)
		}
	}

	fn optional_expression_get_position(
		optional_expression: &Self::OptionalExpression,
	) -> Option<&Span> {
		optional_expression.as_ref().map(|expr| expr.get_position())
	}
}

/// For function type references
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct VariableFieldInTypeAnnotation;

impl VariableFieldKind for VariableFieldInTypeAnnotation {
	type OptionalExpression = ();

	fn optional_expression_from_reader(
		_reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		_state: &mut crate::ParsingState,
		_options: &ParseOptions,
	) -> Result<Self::OptionalExpression, ParseError> {
		Ok(())
	}

	fn optional_expression_to_string_from_buffer<T: source_map::ToString>(
		_optional_expression: &Self::OptionalExpression,
		_buf: &mut T,
		_options: &crate::ToStringOptions,
		_depth: u8,
	) {
	}

	fn optional_expression_get_position(
		_optional_expression: &Self::OptionalExpression,
	) -> Option<&Span> {
		None
	}
}

impl<U: VariableFieldKind> ASTNode for VariableField<U> {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		match reader.peek().ok_or_else(parse_lexing_error)?.0 {
			TSXToken::OpenBrace => {
				let Token(_, start_pos) = reader.next().unwrap();
				let (members, last_pos) =
					parse_bracketed(reader, state, options, None, TSXToken::CloseBrace)?;
				Ok(Self::Object(members, start_pos.union(last_pos)))
			}
			TSXToken::OpenBracket => {
				let Token(_, start_pos) = reader.next().unwrap();
				let (items, end_pos) =
					parse_bracketed(reader, state, options, None, TSXToken::CloseBracket)?;
				Ok(Self::Array(items, start_pos.union(end_pos)))
			}
			_ => Ok(Self::Name(VariableIdentifier::from_reader(reader, state, options)?)),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			Self::Name(identifier) => buf.push_str(identifier.as_str()),
			Self::Array(members, _) => {
				buf.push('[');
				for (at_end, member) in members.iter().endiate() {
					member.to_string_from_buffer(buf, options, depth);
					if !at_end {
						buf.push(',');
						options.add_gap(buf);
					}
				}
				buf.push(']');
			}
			Self::Object(members, _) => {
				buf.push('{');
				options.add_gap(buf);
				for (at_end, member) in members.iter().endiate() {
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
	}

	fn get_position(&self) -> &Span {
		match self {
			VariableField::Array(_, position) | VariableField::Object(_, position) => position,
			VariableField::Name(id) => id.get_position(),
		}
	}
}

#[derive(Debug, Clone, PartialEqExtras, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[partial_eq_ignore_types(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ObjectDestructuringField<T: VariableFieldKind> {
	/// `{ x }`
	Name(VariableIdentifier, T::OptionalExpression, Span),
	/// `{ ...x }`
	Spread(VariableIdentifier, Span),
	/// `{ x: y }`
	Map {
		from: PropertyKey<crate::property_key::AlwaysPublic>,
		name: WithComment<VariableField<T>>,
		default_value: T::OptionalExpression,
		position: Span,
	},
}

impl<U: VariableFieldKind> ASTNode for ObjectDestructuringField<U> {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		match reader.peek().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::Spread, _) => {
				let token = reader.next().unwrap();
				let identifier = VariableIdentifier::from_reader(reader, state, options)?;
				let position = token.get_span().union(identifier.get_position());
				Ok(Self::Spread(identifier, position))
			}
			_ => {
				let key = PropertyKey::from_reader(reader, state, options)?;
				if matches!(reader.peek(), Some(Token(TSXToken::Colon, _))) {
					reader.next();
					let variable_name =
						WithComment::<VariableField<U>>::from_reader(reader, state, options)?;
					let default_value = U::optional_expression_from_reader(reader, state, options)?;
					let position =
						if let Some(pos) = U::optional_expression_get_position(&default_value) {
							key.get_position().union(pos)
						} else {
							key.get_position().clone()
						};
					Ok(Self::Map { from: key, name: variable_name, default_value, position })
				} else if let PropertyKey::Ident(name, key_pos, _) = key {
					let default_value = U::optional_expression_from_reader(reader, state, options)?;
					let standard = VariableIdentifier::Standard(name, key_pos);
					let position =
						if let Some(pos) = U::optional_expression_get_position(&default_value) {
							standard.get_position().union(pos)
						} else {
							standard.get_position().clone()
						};
					Ok(Self::Name(standard, default_value, position))
				} else {
					let token = reader.next().ok_or_else(parse_lexing_error)?;
					throw_unexpected_token_with_token(token, &[TSXToken::Colon])
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
			Self::Spread(name, _) => {
				buf.push_str("...");
				buf.push_str(name.as_str());
			}
			Self::Name(name, default_value, _) => {
				buf.push_str(name.as_str());
				U::optional_expression_to_string_from_buffer(default_value, buf, options, depth)
			}
			Self::Map { from, name: variable_name, default_value, .. } => {
				from.to_string_from_buffer(buf, options, depth);
				buf.push(':');
				variable_name.to_string_from_buffer(buf, options, depth);
				U::optional_expression_to_string_from_buffer(default_value, buf, options, depth)
			}
		}
	}

	fn get_position(&self) -> &Span {
		self.get()
	}
}

/// TODO not sure about the positions here, is potential duplication if T::OptionalExpression is none
#[derive(Debug, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ArrayDestructuringField<T: VariableFieldKind> {
	Spread(Span, VariableIdentifier),
	Name(WithComment<VariableField<T>>, T::OptionalExpression),
	None,
}

impl<T: VariableFieldKind + PartialEq> PartialEq for ArrayDestructuringField<T> {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Spread(_, l0), Self::Spread(_, r0)) => l0 == r0,
			(Self::Name(l0, l1), Self::Name(r0, r1)) => l0 == r0 && l1 == r1,
			_ => core::mem::discriminant(self) == core::mem::discriminant(other),
		}
	}
}

impl<T: VariableFieldKind> Eq for ArrayDestructuringField<T> {}

impl<U: VariableFieldKind> ASTNode for ArrayDestructuringField<U> {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		match reader.peek().ok_or_else(parse_lexing_error)?.0 {
			TSXToken::Spread => {
				let token = reader.next().unwrap();
				Ok(Self::Spread(
					token.get_span(),
					VariableIdentifier::from_reader(reader, state, options)?,
				))
			}
			TSXToken::Comma | TSXToken::CloseBracket => Ok(Self::None),
			_ => {
				let name = WithComment::<VariableField<U>>::from_reader(reader, state, options)?;
				let expression = U::optional_expression_from_reader(reader, state, options)?;
				Ok(Self::Name(name, expression))
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
			Self::Spread(_, name) => {
				buf.push_str("...");
				buf.push_str(name.as_str());
			}
			Self::Name(name, default_value) => {
				name.to_string_from_buffer(buf, options, depth);
				U::optional_expression_to_string_from_buffer(default_value, buf, options, depth)
			}
			Self::None => {}
		}
	}

	fn get_position(&self) -> &Span {
		todo!()
	}
}

/// For object literals and things with computable or literal keys
impl Visitable for VariableField<VariableFieldInSourceCode> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		// TODO map
		match self {
			VariableField::Name(id) => {
				if let VariableIdentifier::Standard(name, pos) = id {
					let item = ImmutableVariableOrProperty::VariableFieldName(name, pos);
					visitors.visit_variable(&item, data, chain);
				}
			}
			VariableField::Array(array_destructuring_fields, _) => {
				for field in array_destructuring_fields.iter() {
					visitors.visit_variable(
						&ImmutableVariableOrProperty::ArrayDestructuringMember(field),
						data,
						chain,
					);
					match field {
						ArrayDestructuringField::Spread(_, _id) => todo!(),
						ArrayDestructuringField::None => {}
						ArrayDestructuringField::Name(variable_field, expression) => {
							variable_field.visit(visitors, data, options, chain);
							expression.visit(visitors, data, options, chain);
						}
					}
				}
			}
			VariableField::Object(object_destructuring_fields, _) => {
				for field in object_destructuring_fields.iter() {
					visitors.visit_variable(
						&ImmutableVariableOrProperty::ObjectDestructuringMember(field),
						data,
						chain,
					);
					match field.get_ast_ref() {
						ObjectDestructuringField::Spread(_name, _) => {}
						ObjectDestructuringField::Name(_name, default_value, _) => {
							default_value.visit(visitors, data, options, chain);
						}
						ObjectDestructuringField::Map {
							name: variable_name,
							default_value,
							..
						} => {
							variable_name.visit(visitors, data, options, chain);
							default_value.visit(visitors, data, options, chain);
						}
					}
				}
			}
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		match self {
			VariableField::Name(identifier) => {
				if let VariableIdentifier::Standard(name, _span) = identifier {
					visitors.visit_variable_mut(
						&mut MutableVariableOrProperty::VariableFieldName(name),
						data,
						chain,
					);
				}
			}
			VariableField::Array(array_destructuring_fields, _) => {
				for field in array_destructuring_fields.iter_mut() {
					visitors.visit_variable_mut(
						&mut MutableVariableOrProperty::ArrayDestructuringMember(field),
						data,
						chain,
					);
					match field {
						ArrayDestructuringField::Spread(_, _id) => todo!(),
						ArrayDestructuringField::None => {}
						ArrayDestructuringField::Name(variable_field, default_value) => {
							variable_field.visit_mut(visitors, data, options, chain);
							default_value.visit_mut(visitors, data, options, chain);
						}
					}
				}
			}
			VariableField::Object(object_destructuring_fields, _) => {
				for field in object_destructuring_fields.iter_mut() {
					visitors.visit_variable_mut(
						&mut MutableVariableOrProperty::ObjectDestructuringMember(field),
						data,
						chain,
					);
					match field.get_ast_mut() {
						ObjectDestructuringField::Spread(_id, _) => {}
						ObjectDestructuringField::Name(_id, default_value, _) => {
							default_value.visit_mut(visitors, data, options, chain);
						}
						ObjectDestructuringField::Map {
							name: variable_name,
							default_value,
							..
						} => {
							variable_name.visit_mut(visitors, data, options, chain);
							default_value.visit_mut(visitors, data, options, chain);
						}
					}
				}
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::{assert_matches_ast, span};

	type VariableField = super::VariableField<VariableFieldInSourceCode>;

	#[test]
	fn name() {
		assert_matches_ast!(
			"x",
			VariableField::Name(VariableIdentifier::Standard(Deref @ "x", Span { start: 0, end: 1, .. }))
		);
	}

	#[test]
	fn array() {
		assert_matches_ast!(
			"[x, y, z]",
			VariableField::Array(
				Deref @ [ArrayDestructuringField::Name(
					WithComment::None(VariableField::Name(VariableIdentifier::Standard(
						Deref @ "x",
						span!(1, 2),
					))),
					None,
				), ArrayDestructuringField::Name(
					WithComment::None(VariableField::Name(VariableIdentifier::Standard(
						Deref @ "y",
						span!(4, 5),
					))),
					None,
				), ArrayDestructuringField::Name(
					WithComment::None(VariableField::Name(VariableIdentifier::Standard(
						Deref @ "z",
						span!(7, 8),
					))),
					None,
				)],
				_,
			)
		);

		assert_matches_ast!(
			"[x,, z]",
			VariableField::Array(
				Deref @ [ArrayDestructuringField::Name(
					WithComment::None(VariableField::Name(VariableIdentifier::Standard(
						Deref @ "x",
						span!(1, 2),
					))),
					None,
				), ArrayDestructuringField::None, ArrayDestructuringField::Name(
					WithComment::None(VariableField::Name(VariableIdentifier::Standard(
						Deref @ "z",
						span!(5, 6),
					))),
					None,
				)],
				span!(0, 7),
			)
		);
	}

	#[test]
	fn object() {
		assert_matches_ast!(
			"{x, y, z}",
			VariableField::Object(
				Deref @ [WithComment::None(ObjectDestructuringField::Name(
					VariableIdentifier::Standard(Deref @ "x", span!(1, 2)),
					None,
					span!(1, 2),
				)), WithComment::None(ObjectDestructuringField::Name(
					VariableIdentifier::Standard(Deref @ "y", span!(4, 5)),
					None,
					span!(4, 5),
				)), WithComment::None(ObjectDestructuringField::Name(
					VariableIdentifier::Standard(Deref @ "z", span!(7, 8)),
					None,
					span!(7, 8),
				))],
				span!(0, 9),
			)
		);
	}

	#[test]
	fn name_with_default() {
		assert_matches_ast!(
			"{ x = 2 }",
			VariableField::Object(
				Deref @ [WithComment::None(ObjectDestructuringField::Name(
					VariableIdentifier::Standard(Deref @ "x", span!(2, 3)),
					Some(Expression::NumberLiteral(
						crate::NumberRepresentation::Number { .. },
						span!(6, 7),
					)),
					span!(2, 7),
				))],
				span!(0, 9),
			)
		);
	}

	#[test]
	fn array_spread() {
		assert_matches_ast!(
			"[x, ...y]",
			VariableField::Array(
				Deref @ [ArrayDestructuringField::Name(
					WithComment::None(VariableField::Name(VariableIdentifier::Standard(
						Deref @ "x",
						span!(1, 2),
					))),
					None,
				), ArrayDestructuringField::Spread(
					span!(4, 7),
					VariableIdentifier::Standard(Deref @ "y", span!(7, 8)),
				)],
				span!(0, 9),
			)
		);
	}
}
