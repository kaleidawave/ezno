/// Contains:
/// - [VariableId] given to variable declaring items
/// - [VariableField] for destructuring things and its nested derivatives + visiting behavior + tests for self
use std::{
	borrow::Cow,
	fmt::Debug,
	sync::atomic::{AtomicU16, Ordering},
};

use crate::{
	errors::parse_lexing_error, extractor::ExtractedFunctions, parse_bracketed,
	property_key::PropertyKey, tokens::token_as_identifier, ASTNode, CursorId, Expression,
	ImmutableVariableOrPropertyPart, MutableVariablePart, ParseError, ParseErrors, ParseResult,
	ParseSettings, Span, TSXToken, Token, VisitSettings, Visitable, WithComment,
};

use derive_debug_extras::DebugExtras;
use derive_partial_eq_extras::PartialEqExtras;
use iterator_endiate::EndiateIteratorExt;
use self_rust_tokenize::SelfRustTokenize;
use tokenizer_lib::TokenReader;

static VARIABLE_COUNTER: AtomicU16 = AtomicU16::new(1);

/// Id given to AST that declares a variable
#[derive(PartialEq, Eq, Clone, Copy, DebugExtras, Hash)]
pub struct VariableId(u16);

#[cfg(feature = "self-rust-tokenize")]
impl self_rust_tokenize::SelfRustTokenize for VariableId {
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		token_stream.extend(self_rust_tokenize::quote!(VariableId::new()))
	}
}

impl VariableId {
	pub fn new() -> Self {
		Self(VARIABLE_COUNTER.fetch_add(1, Ordering::SeqCst))
	}

	/// Only use if you know what you are not causing clashing
	pub const fn new_from_id(id: u16) -> Self {
		Self(id)
	}

	/// TODO temp
	pub fn unwrap_counter(&self) -> u16 {
		self.0
	}

	pub fn set_counter_bad(value: u16) {
		VARIABLE_COUNTER.store(value, Ordering::SeqCst)
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum VariableIdentifier {
	Standard(String, VariableId, Span),
	#[self_tokenize_field(0)]
	Cursor(CursorId<Self>),
}

impl ASTNode for VariableIdentifier {
	fn get_position(&self) -> Cow<Span> {
		match self {
			VariableIdentifier::Standard(_, _, span) => Cow::Borrowed(span),
			VariableIdentifier::Cursor(_) => todo!(),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		_state: &mut crate::ParsingState,
		_settings: &ParseSettings,
	) -> ParseResult<Self> {
		let token = reader.next().ok_or_else(parse_lexing_error)?;
		Ok(if let Token(TSXToken::Cursor(id), _) = token {
			Self::Cursor(id.into_cursor())
		} else {
			let (ident, span) = token_as_identifier(token, "variable identifier")?;
			Self::Standard(ident, VariableId::new(), span)
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		_settings: &crate::ToStringSettingsAndData,
		_depth: u8,
	) {
		buf.push_str(self.as_str());
	}
}

impl VariableIdentifier {
	pub fn as_str(&self) -> &str {
		match self {
			VariableIdentifier::Standard(name, _, _) => name.as_str(),
			VariableIdentifier::Cursor(_) => "",
		}
	}

	fn try_get_id(&self) -> Option<VariableId> {
		match self {
			VariableIdentifier::Standard(_, id, _) => Some(*id),
			VariableIdentifier::Cursor(_) => None,
		}
	}
}

/// A variable declaration name, used in variable declarations and function parameters.
/// See [destructuring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment)
#[derive(Debug, Clone)]
pub enum VariableField<T: VariableFieldTypes> {
	/// `x`
	Name(VariableIdentifier),
	/// `[x, y, z]`
	Array(Vec<ArrayDestructuringField<T>>, Span),
	/// `{ x, y: z }`.
	Object(Vec<WithComment<ObjectDestructuringField<T>>>, Span),
}

#[cfg(feature = "self-rust-tokenize")]
impl<T: VariableFieldTypes> self_rust_tokenize::SelfRustTokenize for VariableField<T>
where
	T::OptionalExpression: self_rust_tokenize::SelfRustTokenize,
{
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		use self_rust_tokenize::quote;
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

impl<T: VariableFieldTypes> From<VariableIdentifier> for VariableField<T> {
	fn from(value: VariableIdentifier) -> Self {
		Self::Name(value)
	}
}

impl<T: VariableFieldTypes + PartialEq> PartialEq for VariableField<T> {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Name(l0), Self::Name(r0)) => l0 == r0,
			(Self::Array(l0, _), Self::Array(r0, _)) => l0 == r0,
			(Self::Object(l0, _), Self::Object(r0, _)) => l0 == r0,
			_ => false,
		}
	}
}

impl<T: VariableFieldTypes> Eq for VariableField<T> {}

/// Variable field can be used in type annotations but cannot have a value
///
/// TODO value assignment this is VariableOrFieldAccess thingy
///
/// TODO could have get_optional_expression_as_option(&Self::OptionalExpression) -> Option<Expression>
pub trait VariableFieldTypes: PartialEq + Eq + Debug + Clone + 'static {
	type OptionalExpression: PartialEq + Eq + Debug + Clone + Sync + Send;

	fn optional_expression_from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> Result<Self::OptionalExpression, ParseError>;

	fn optional_expression_to_string_from_buffer<T: source_map::ToString>(
		optional_expression: &Self::OptionalExpression,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	);

	fn optional_expression_get_position(
		optional_expression: &Self::OptionalExpression,
	) -> Option<Cow<Span>>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableFieldInSourceCode;

impl VariableFieldTypes for VariableFieldInSourceCode {
	type OptionalExpression = Option<Expression>;

	fn optional_expression_from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> Result<Self::OptionalExpression, ParseError> {
		Ok(if reader.conditional_next(|tok| matches!(tok, TSXToken::Assign)).is_some() {
			Some(Expression::from_reader(reader, state, settings)?)
		} else {
			None
		})
	}

	fn optional_expression_to_string_from_buffer<T: source_map::ToString>(
		optional_expression: &Self::OptionalExpression,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		if let Some(optional_expression) = optional_expression {
			buf.push_str(if settings.0.pretty { " = " } else { "=" });
			optional_expression.to_string_from_buffer(buf, settings, depth)
		}
	}

	fn optional_expression_get_position(
		optional_expression: &Self::OptionalExpression,
	) -> Option<Cow<Span>> {
		optional_expression.as_ref().map(|expr| expr.get_position())
	}
}

/// For function type references
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableFieldInTypeReference;

impl VariableFieldTypes for VariableFieldInTypeReference {
	type OptionalExpression = ();

	fn optional_expression_from_reader(
		_reader: &mut impl TokenReader<TSXToken, Span>,
		_state: &mut crate::ParsingState,
		_settings: &ParseSettings,
	) -> Result<Self::OptionalExpression, ParseError> {
		Ok(())
	}

	fn optional_expression_to_string_from_buffer<T: source_map::ToString>(
		_optional_expression: &Self::OptionalExpression,
		_buf: &mut T,
		_settings: &crate::ToStringSettingsAndData,
		_depth: u8,
	) {
	}

	fn optional_expression_get_position(
		_optional_expression: &Self::OptionalExpression,
	) -> Option<Cow<Span>> {
		None
	}
}

impl<U: VariableFieldTypes> ASTNode for VariableField<U> {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		match reader.peek().ok_or_else(parse_lexing_error)?.0 {
			TSXToken::OpenBrace => {
				let Token(_, start_pos) = reader.next().unwrap();
				let (members, last_pos) =
					parse_bracketed(reader, state, settings, None, TSXToken::CloseBrace)?;
				Ok(Self::Object(members, start_pos.union(&last_pos)))
			}
			TSXToken::OpenBracket => {
				let Token(_, start_pos) = reader.next().unwrap();
				let (items, end_pos) =
					parse_bracketed(reader, state, settings, None, TSXToken::CloseBracket)?;
				Ok(Self::Array(items, start_pos.union(&end_pos)))
			}
			_ => Ok(Self::Name(VariableIdentifier::from_reader(reader, state, settings)?)),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		match self {
			Self::Name(identifier) => buf.push_str(identifier.as_str()),
			Self::Array(members, _) => {
				buf.push('[');
				for (at_end, member) in members.iter().endiate() {
					member.to_string_from_buffer(buf, settings, depth);
					if !at_end {
						buf.push(',');
						settings.0.add_gap(buf);
					}
				}
				buf.push(']');
			}
			Self::Object(members, _) => {
				buf.push('{');
				settings.0.add_gap(buf);
				for (at_end, member) in members.iter().endiate() {
					member.to_string_from_buffer(buf, settings, depth);
					if !at_end {
						buf.push(',');
						settings.0.add_gap(buf);
					}
				}
				settings.0.add_gap(buf);
				buf.push('}');
			}
		}
	}

	fn get_position(&self) -> Cow<Span> {
		match self {
			VariableField::Array(_, position) | VariableField::Object(_, position) => {
				Cow::Borrowed(position)
			}
			VariableField::Name(id) => id.get_position(),
		}
	}
}

impl<T: VariableFieldTypes> VariableField<T> {
	pub fn try_get_id(&self) -> Option<VariableId> {
		match self {
			VariableField::Name(identifier) => identifier.try_get_id(),
			VariableField::Array(_, _) => None,
			VariableField::Object(_, _) => None,
		}
	}
}

#[derive(Debug, Clone, PartialEqExtras)]
#[partial_eq_ignore_types(Span, T::Id)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum ObjectDestructuringField<T: VariableFieldTypes> {
	/// `{ ...x }`
	Spread(Span, VariableIdentifier),
	/// `{ x }`
	Name(VariableIdentifier, T::OptionalExpression),
	/// `{ x: y }`
	Map {
		from: PropertyKey,
		variable_name: WithComment<VariableField<T>>,
		default_value: T::OptionalExpression,
		position: Span,
	},
}

impl<U: VariableFieldTypes> ASTNode for ObjectDestructuringField<U> {
	fn get_position(&self) -> Cow<Span> {
		match self {
			// TODO account for `...` tokens
			ObjectDestructuringField::Spread(_, name) => name.get_position(),
			ObjectDestructuringField::Name(name, optional_expression) => {
				let name_position = name.get_position();
				if let Some(ref expr_pos) = U::optional_expression_get_position(optional_expression)
				{
					Cow::Owned(name_position.union(expr_pos))
				} else {
					name_position
				}
			}
			ObjectDestructuringField::Map { position, .. } => Cow::Borrowed(position),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		match reader.peek().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::Spread, _) => {
				let Token(_, spread_pos) = reader.next().unwrap();
				Ok(Self::Spread(
					spread_pos,
					VariableIdentifier::from_reader(reader, state, settings)?,
				))
			}
			_ => {
				let key = PropertyKey::from_reader(reader, state, settings)?;
				if matches!(reader.peek(), Some(Token(TSXToken::Colon, _))) {
					reader.next();
					let variable_name =
						WithComment::<VariableField<U>>::from_reader(reader, state, settings)?;
					let default_value =
						U::optional_expression_from_reader(reader, state, settings)?;
					let position =
						if let Some(pos) = U::optional_expression_get_position(&default_value) {
							key.get_position().union(&pos)
						} else {
							key.get_position().into_owned()
						};
					Ok(Self::Map { from: key, variable_name, default_value, position })
				} else if let PropertyKey::Ident(name, _, key_pos) = key {
					let default_value =
						U::optional_expression_from_reader(reader, state, settings)?;
					let position =
						if let Some(pos) = U::optional_expression_get_position(&default_value) {
							key_pos.union(&pos)
						} else {
							key_pos
						};
					let standard = VariableIdentifier::Standard(name, VariableId::new(), position);
					Ok(Self::Name(standard, default_value))
				} else {
					let Token(token, pos) = reader.next().unwrap();
					Err(ParseError::new(
						ParseErrors::UnexpectedToken { expected: &[TSXToken::Colon], found: token },
						pos,
					))
				}
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		match self {
			Self::Spread(_, name) => {
				buf.push_str("...");
				buf.push_str(name.as_str());
			}
			Self::Name(name, default_value) => {
				buf.push_str(name.as_str());
				U::optional_expression_to_string_from_buffer(default_value, buf, settings, depth)
			}
			Self::Map { from, variable_name, default_value, .. } => {
				from.to_string_from_buffer(buf, settings, depth);
				buf.push(':');
				variable_name.to_string_from_buffer(buf, settings, depth);
				U::optional_expression_to_string_from_buffer(default_value, buf, settings, depth)
			}
		}
	}
}

/// TODO not sure about the positions here, is potential duplication if T::OptionalExpression is none
#[derive(Debug, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum ArrayDestructuringField<T: VariableFieldTypes> {
	Spread(Span, VariableIdentifier),
	Name(WithComment<VariableField<T>>, T::OptionalExpression),
	None,
}

impl<T: VariableFieldTypes + PartialEq> PartialEq for ArrayDestructuringField<T> {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Spread(_, l0), Self::Spread(_, r0)) => l0 == r0,
			(Self::Name(l0, l1), Self::Name(r0, r1)) => l0 == r0 && l1 == r1,
			_ => core::mem::discriminant(self) == core::mem::discriminant(other),
		}
	}
}

impl<T: VariableFieldTypes> Eq for ArrayDestructuringField<T> {}

impl<U: VariableFieldTypes> ASTNode for ArrayDestructuringField<U> {
	fn get_position(&self) -> Cow<Span> {
		match self {
			ArrayDestructuringField::Spread(spread_pos, ident) => {
				Cow::Owned(spread_pos.union(&ident.get_position()))
			}
			ArrayDestructuringField::Name(name, optional_expression) => {
				let name_position = name.get_position();
				if let Some(ref expr_pos) = U::optional_expression_get_position(optional_expression)
				{
					Cow::Owned(name_position.union(expr_pos))
				} else {
					name_position
				}
			}
			ArrayDestructuringField::None => todo!(),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		match reader.peek().ok_or_else(parse_lexing_error)?.0 {
			TSXToken::Spread => {
				let Token(_, spread_pos) = reader.next().unwrap();
				Ok(Self::Spread(
					spread_pos,
					VariableIdentifier::from_reader(reader, state, settings)?,
				))
			}
			TSXToken::Comma | TSXToken::CloseBracket => Ok(Self::None),
			_ => {
				let name = WithComment::<VariableField<U>>::from_reader(reader, state, settings)?;
				let expression = U::optional_expression_from_reader(reader, state, settings)?;
				Ok(Self::Name(name, expression))
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		match self {
			Self::Spread(_, name) => {
				buf.push_str("...");
				buf.push_str(name.as_str());
			}
			Self::Name(name, default_value) => {
				name.to_string_from_buffer(buf, settings, depth);
				U::optional_expression_to_string_from_buffer(default_value, buf, settings, depth)
			}
			Self::None => {}
		}
	}
}

impl<T: VariableFieldTypes> ArrayDestructuringField<T> {
	pub fn try_get_id(&self) -> Option<VariableId> {
		match self {
			ArrayDestructuringField::Spread(_, id) => id.try_get_id(),
			ArrayDestructuringField::Name(variable_field, _) => {
				variable_field.get_ast().try_get_id()
			}
			ArrayDestructuringField::None => None,
		}
	}
}

/// For object literals and things with computable or literal keys
impl Visitable for WithComment<VariableField<VariableFieldInSourceCode>> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &VisitSettings,
		functions: &mut ExtractedFunctions,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		// TODO map
		match self.get_ast() {
			VariableField::Name(id) => {
				if let VariableIdentifier::Standard(name, variable_id, pos) = id {
					let item =
						ImmutableVariableOrPropertyPart::VariableFieldName(name, *variable_id, pos);
					visitors.visit_variable(&item, data, functions, chain);
				}
			}
			VariableField::Array(array_destructuring_fields, _) => {
				for field in array_destructuring_fields.iter() {
					visitors.visit_variable(
						&ImmutableVariableOrPropertyPart::ArrayDestructuringMember(field),
						data,
						functions,
						chain,
					);
					match field {
						ArrayDestructuringField::Spread(_, _id) => todo!(),
						ArrayDestructuringField::None => {}
						ArrayDestructuringField::Name(variable_field, expression) => {
							variable_field.visit(visitors, data, settings, functions, chain);
							expression.visit(visitors, data, settings, functions, chain);
						}
					}
				}
			}
			VariableField::Object(object_destructuring_fields, _) => {
				for field in object_destructuring_fields.iter() {
					visitors.visit_variable(
						&ImmutableVariableOrPropertyPart::ObjectDestructuringMember(field),
						data,
						functions,
						chain,
					);
					match field.get_ast() {
						ObjectDestructuringField::Spread(_, _name) => {}
						ObjectDestructuringField::Name(_name, default_value) => {
							default_value.visit(visitors, data, settings, functions, chain);
						}
						ObjectDestructuringField::Map { variable_name, default_value, .. } => {
							variable_name.visit(visitors, data, settings, functions, chain);
							default_value.visit(visitors, data, settings, functions, chain);
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
		settings: &VisitSettings,
		functions: &mut ExtractedFunctions,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		match self.get_ast_mut() {
			VariableField::Name(identifier) => {
				if let VariableIdentifier::Standard(name, variable_id, _span) = identifier {
					visitors.visit_variable_mut(
						&mut MutableVariablePart::VariableFieldName(name, *variable_id),
						data,
						functions,
						chain,
					);
				}
			}
			VariableField::Array(array_destructuring_fields, _) => {
				for field in array_destructuring_fields.iter_mut() {
					visitors.visit_variable_mut(
						&mut MutableVariablePart::ArrayDestructuringMember(field),
						data,
						functions,
						chain,
					);
					match field {
						ArrayDestructuringField::Spread(_, _id) => todo!(),
						ArrayDestructuringField::None => {}
						ArrayDestructuringField::Name(variable_field, default_value) => {
							variable_field.visit_mut(visitors, data, settings, functions, chain);
							default_value.visit_mut(visitors, data, settings, functions, chain);
						}
					}
				}
			}
			VariableField::Object(object_destructuring_fields, _) => {
				for field in object_destructuring_fields.iter_mut() {
					visitors.visit_variable_mut(
						&mut MutableVariablePart::ObjectDestructuringMember(field),
						data,
						functions,
						chain,
					);
					match field.get_ast_mut() {
						ObjectDestructuringField::Spread(_, _id) => {}
						ObjectDestructuringField::Name(_id, default_value) => {
							default_value.visit_mut(visitors, data, settings, functions, chain);
						}
						ObjectDestructuringField::Map { variable_name, default_value, .. } => {
							variable_name.visit_mut(visitors, data, settings, functions, chain);
							default_value.visit_mut(visitors, data, settings, functions, chain);
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
			VariableField::Name(VariableIdentifier::Standard(Deref @ "x", _, Span { start: 0, end: 1, .. }))
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
						_,
						span!(1, 2),
					))),
					None,
				), ArrayDestructuringField::Name(
					WithComment::None(VariableField::Name(VariableIdentifier::Standard(
						Deref @ "y",
						_,
						span!(4, 5),
					))),
					None,
				), ArrayDestructuringField::Name(
					WithComment::None(VariableField::Name(VariableIdentifier::Standard(
						Deref @ "z",
						_,
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
						_,
						span!(1, 2),
					))),
					None,
				), ArrayDestructuringField::None, ArrayDestructuringField::Name(
					WithComment::None(VariableField::Name(VariableIdentifier::Standard(
						Deref @ "z",
						_,
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
					VariableIdentifier::Standard(Deref @ "x", _, span!(1, 2)),
					None,
				)), WithComment::None(ObjectDestructuringField::Name(
					VariableIdentifier::Standard(Deref @ "y", _, span!(4, 5)),
					None,
				)), WithComment::None(ObjectDestructuringField::Name(
					VariableIdentifier::Standard(Deref @ "z", _, span!(7, 8)),
					None,
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
					VariableIdentifier::Standard(Deref @ "x", _, span!(2, 7)),
					Some(Expression::NumberLiteral(
						crate::NumberStructure::Number(_),
						span!(6, 7),
						_,
					)),
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
						_,
						span!(1, 2),
					))),
					None,
				), ArrayDestructuringField::Spread(
					span!(4, 7),
					VariableIdentifier::Standard(Deref @ "y", _, span!(7, 8)),
				)],
				span!(0, 9),
			)
		);
	}
}
