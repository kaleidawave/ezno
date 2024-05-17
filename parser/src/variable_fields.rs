/// Contains:
/// - [`VariableId`] given to variable declaring items
/// - [`VariableField`] for destructuring things and its nested derivatives + visiting behavior + tests for self
use std::fmt::Debug;

use crate::{
	derive_ASTNode,
	errors::parse_lexing_error,
	parse_bracketed,
	property_key::PropertyKey,
	throw_unexpected_token_with_token,
	tokens::token_as_identifier,
	visiting::{ImmutableVariableOrProperty, MutableVariableOrProperty},
	ASTNode, Expression, ListItem, Marker, ParseError, ParseErrors, ParseOptions, ParseResult,
	Span, TSXToken, Token, VisitOptions, Visitable, WithComment,
};

use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use tokenizer_lib::TokenReader;

#[apply(derive_ASTNode)]
#[derive(Debug, PartialEqExtras, Clone, GetFieldByType)]
#[partial_eq_ignore_types(Span)]
#[get_field_by_type_target(Span)]
pub enum VariableIdentifier {
	Standard(String, Span),
	// TODO does this need Span
	#[cfg_attr(feature = "self-rust-tokenize", self_tokenize_field(0))]
	Marker(
		#[cfg_attr(target_family = "wasm", tsify(type = "VariableIdentifier"))] Marker<Self>,
		Span,
	),
}

impl ASTNode for VariableIdentifier {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let (ident, span) = token_as_identifier(reader.next().unwrap(), "variable identifier")?;
		if ident == "let" {
			return Err(ParseError::new(ParseErrors::ReservedIdentifier, span));
		}
		Ok(if options.interpolation_points && ident == crate::marker::MARKER {
			Self::Marker(state.new_partial_point_marker(span.get_start()), span)
		} else {
			Self::Standard(ident, span)
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		_local: crate::LocalToStringInformation,
	) {
		match self {
			VariableIdentifier::Standard(name, _) => buf.push_str(name),
			VariableIdentifier::Marker(_, _) => {
				assert!(!options.expect_markers, "variable marker attempted to convert to string");
			}
		}
	}

	fn get_position(&self) -> Span {
		*self.get()
	}
}

impl VariableIdentifier {
	#[must_use]
	pub fn as_option_str(&self) -> Option<&str> {
		match self {
			VariableIdentifier::Standard(s, _) => Some(s.as_str()),
			VariableIdentifier::Marker(_, _) => None,
		}
	}
}

/// A variable declaration name, used in variable declarations and function parameters.
/// See [destructuring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment)
#[derive(Debug, Clone, PartialEq)]
#[apply(derive_ASTNode)]
pub enum VariableField {
	/// `x`
	Name(VariableIdentifier),
	/// `[x, y, z]`
	/// TODO spread last
	Array(Vec<WithComment<ArrayDestructuringField<Self>>>, Span),
	/// `{ x, y: z }`.
	/// TODO spread last
	Object(Vec<WithComment<ObjectDestructuringField<Self>>>, Span),
}

impl ASTNode for VariableField {
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
				let (items, end) =
					parse_bracketed(reader, state, options, None, TSXToken::CloseBracket)?;
				Ok(Self::Array(items, start_pos.union(end)))
			}
			_ => Ok(Self::Name(VariableIdentifier::from_reader(reader, state, options)?)),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			Self::Name(identifier) => {
				buf.add_mapping(&identifier.get_position().with_source(local.under));
				identifier.to_string_from_buffer(buf, options, local);
			}
			Self::Array(members, _) => {
				buf.push('[');
				for (at_end, member) in members.iter().endiate() {
					member.to_string_from_buffer(buf, options, local);
					if !at_end {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
				}
				buf.push(']');
			}
			Self::Object(members, _) => {
				buf.push('{');
				options.push_gap_optionally(buf);
				for (at_end, member) in members.iter().endiate() {
					member.to_string_from_buffer(buf, options, local);
					if !at_end {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
				}
				options.push_gap_optionally(buf);
				buf.push('}');
			}
		}
	}

	fn get_position(&self) -> Span {
		match self {
			VariableField::Array(_, position) | VariableField::Object(_, position) => *position,
			VariableField::Name(id) => id.get_position(),
		}
	}
}

pub trait DestructuringFieldInto: ASTNode {
	// This in an extra
	type TypeAnnotation: Clone + PartialEq + Debug + Sync + Send + 'static;

	fn type_annotation_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self::TypeAnnotation>;
}

impl DestructuringFieldInto for VariableField {
	type TypeAnnotation = Option<crate::TypeAnnotation>;

	fn type_annotation_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self::TypeAnnotation> {
		if let (true, Some(Token(TSXToken::Colon, _))) =
			(options.destructuring_type_annotation, reader.peek())
		{
			reader.next();
			crate::TypeAnnotation::from_reader(reader, state, options).map(Some)
		} else {
			Ok(None)
		}
	}
}

impl DestructuringFieldInto for crate::ast::LHSOfAssignment {
	type TypeAnnotation = ();

	fn type_annotation_from_reader(
		_reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		_state: &mut crate::ParsingState,
		_options: &ParseOptions,
	) -> ParseResult<Self::TypeAnnotation> {
		Ok(())
	}
}

/// For
/// - declarations: `T = VariableField`
/// - expressions: `T = LHSOfAssignment`
#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub enum ArrayDestructuringField<T: DestructuringFieldInto> {
	Spread(T, Span),
	Name(T, T::TypeAnnotation, Option<Box<Expression>>),
	Comment { content: String, is_multiline: bool, position: Span },
	None,
}

impl<T: DestructuringFieldInto> ListItem for WithComment<ArrayDestructuringField<T>> {
	const EMPTY: Option<Self> = Some(WithComment::None(ArrayDestructuringField::None));

	fn allow_comma_after(&self) -> bool {
		!matches!(self.get_ast_ref(), ArrayDestructuringField::Spread(..))
	}
}

impl<T: DestructuringFieldInto> ASTNode for ArrayDestructuringField<T> {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let Token(token, _start) = reader.peek().ok_or_else(parse_lexing_error)?;
		if let TSXToken::Spread = token {
			let token = reader.next().unwrap();
			let to = T::from_reader(reader, state, options)?;
			let position = token.get_span().union(to.get_position());
			Ok(Self::Spread(to, position))
		} else if matches!(token, TSXToken::Comma | TSXToken::CloseBracket) {
			Ok(Self::None)
		} else {
			let name = T::from_reader(reader, state, options)?;
			let annotation = T::type_annotation_from_reader(reader, state, options)?;
			let default_value = reader
				.conditional_next(|t| matches!(t, TSXToken::Assign))
				.is_some()
				.then(|| ASTNode::from_reader(reader, state, options).map(Box::new))
				.transpose()?;

			// let position =
			// 	if let Some(ref pos) = default_value {
			// 		key.get_position().union(pos)
			// 	} else {
			// 		*key.get_position()
			// 	};
			Ok(Self::Name(name, annotation, default_value))
		}
	}

	fn to_string_from_buffer<U: source_map::ToString>(
		&self,
		buf: &mut U,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			Self::Spread(name, _) => {
				buf.push_str("...");
				name.to_string_from_buffer(buf, options, local);
			}
			Self::Name(name, _annotation, default_value) => {
				name.to_string_from_buffer(buf, options, local);
				if let Some(default_value) = default_value {
					options.push_gap_optionally(buf);
					buf.push('=');
					options.push_gap_optionally(buf);
					default_value.to_string_from_buffer(buf, options, local);
				}
			}
			Self::Comment { content, is_multiline, position: _ } => {
				if options.should_add_comment(*is_multiline && content.starts_with('*')) {
					buf.push_str("/*");
					buf.push_str(content);
					buf.push_str("*/");
				}
			}
			Self::None => {}
		}
	}

	fn get_position(&self) -> Span {
		match self {
			ArrayDestructuringField::Comment { position, .. }
			| ArrayDestructuringField::Spread(_, position) => *position,
			// TODO misses out optional expression
			ArrayDestructuringField::Name(vf, ..) => vf.get_position(),
			ArrayDestructuringField::None => source_map::Nullable::NULL,
		}
	}
}

/// For
/// - declarations: `T = VariableField`
/// - expressions: `T = LHSOfAssignment`
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[partial_eq_ignore_types(Span)]
pub enum ObjectDestructuringField<T: DestructuringFieldInto> {
	/// `{ x }` and (annoyingly) `{ x = 2 }`
	Name(VariableIdentifier, T::TypeAnnotation, Option<Box<Expression>>, Span),
	/// `{ ...x }`
	Spread(T, Span),
	/// `{ x: y }`
	Map {
		from: PropertyKey<crate::property_key::AlwaysPublic>,
		annotation: T::TypeAnnotation,
		name: WithComment<T>,
		default_value: Option<Box<Expression>>,
		position: Span,
	},
}

impl<T: DestructuringFieldInto> ListItem for WithComment<ObjectDestructuringField<T>> {}

impl<T: DestructuringFieldInto> ASTNode for ObjectDestructuringField<T> {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		if let Token(TSXToken::Spread, _) = reader.peek().ok_or_else(parse_lexing_error)? {
			let token = reader.next().unwrap();
			let name = T::from_reader(reader, state, options)?;
			let position = token.get_span().union(name.get_position());
			Ok(Self::Spread(name, position))
		} else {
			let key = PropertyKey::from_reader(reader, state, options)?;
			if reader.peek().is_some_and(|Token(t, _)| is_destructuring_into_marker(t, options)) {
				reader.next();
				let name = WithComment::<T>::from_reader(reader, state, options)?;
				let annotation = T::type_annotation_from_reader(reader, state, options)?;

				let default_value = reader
					.conditional_next(|t| matches!(t, TSXToken::Assign))
					.is_some()
					.then(|| Expression::from_reader(reader, state, options).map(Box::new))
					.transpose()?;

				let position = if let Some(ref dv) = default_value {
					key.get_position().union(dv.get_position())
				} else {
					key.get_position()
				};

				Ok(Self::Map { from: key, annotation, name, default_value, position })
			} else if let PropertyKey::Ident(name, key_pos, _) = key {
				let default_value = reader
					.conditional_next(|t| matches!(t, TSXToken::Assign))
					.is_some()
					.then(|| Expression::from_reader(reader, state, options).map(Box::new))
					.transpose()?;

				let standard = VariableIdentifier::Standard(name, key_pos);
				let annotation = T::type_annotation_from_reader(reader, state, options)?;
				let position = if let Some(ref dv) = default_value {
					key_pos.union(dv.get_position())
				} else {
					key_pos
				};

				Ok(Self::Name(standard, annotation, default_value, position))
			} else {
				let token = reader.next().ok_or_else(parse_lexing_error)?;
				throw_unexpected_token_with_token(token, &[TSXToken::Colon])
			}
		}
	}

	fn to_string_from_buffer<U: source_map::ToString>(
		&self,
		buf: &mut U,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			Self::Spread(name, _) => {
				buf.push_str("...");
				name.to_string_from_buffer(buf, options, local);
			}
			Self::Name(name, _annotation, default_value, ..) => {
				name.to_string_from_buffer(buf, options, local);
				if let Some(default_value) = default_value {
					options.push_gap_optionally(buf);
					buf.push('=');
					options.push_gap_optionally(buf);
					default_value.to_string_from_buffer(buf, options, local);
				}
			}
			Self::Map { from, annotation: _, name: variable_name, default_value, .. } => {
				from.to_string_from_buffer(buf, options, local);
				buf.push(':');
				options.push_gap_optionally(buf);
				variable_name.to_string_from_buffer(buf, options, local);
				if let Some(default_value) = default_value {
					options.push_gap_optionally(buf);
					buf.push('=');
					options.push_gap_optionally(buf);
					default_value.to_string_from_buffer(buf, options, local);
				}
			}
		}
	}

	fn get_position(&self) -> Span {
		*self.get()
	}
}

/// For object literals and things with computable or literal keys
impl Visitable for VariableField {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		match self {
			VariableField::Name(id) => {
				if let VariableIdentifier::Standard(name, pos) = id {
					let item = ImmutableVariableOrProperty::VariableFieldName(name, pos);
					visitors.visit_variable(&item, data, chain);
				}
			}
			VariableField::Array(array_destructuring_fields, _) => array_destructuring_fields
				.iter()
				.for_each(|f| f.visit(visitors, data, options, chain)),
			VariableField::Object(object_destructuring_fields, _) => object_destructuring_fields
				.iter()
				.for_each(|f| f.visit(visitors, data, options, chain)),
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
			VariableField::Array(array_destructuring_fields, _) => array_destructuring_fields
				.iter_mut()
				.for_each(|f| f.visit_mut(visitors, data, options, chain)),
			VariableField::Object(object_destructuring_fields, _) => object_destructuring_fields
				.iter_mut()
				.for_each(|f| f.visit_mut(visitors, data, options, chain)),
		}
	}
}

impl Visitable for WithComment<ArrayDestructuringField<VariableField>> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		let field = self.get_ast_ref();
		let array_destructuring_member =
			ImmutableVariableOrProperty::ArrayDestructuringMember(field);
		visitors.visit_variable(&array_destructuring_member, data, chain);
		match field {
			// TODO should be okay, no nesting here
			ArrayDestructuringField::Comment { .. }
			| ArrayDestructuringField::Spread(..)
			| ArrayDestructuringField::None => {}
			ArrayDestructuringField::Name(variable_field, _, expression) => {
				variable_field.visit(visitors, data, options, chain);
				expression.visit(visitors, data, options, chain);
			}
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		let mut array_destructuring_member =
			MutableVariableOrProperty::ArrayDestructuringMember(self.get_ast_mut());
		visitors.visit_variable_mut(&mut array_destructuring_member, data, chain);
		match self.get_ast_mut() {
			ArrayDestructuringField::Spread(_, _id) => {
				// TODO should be okay, no nesting here
			}
			ArrayDestructuringField::Comment { .. } | ArrayDestructuringField::None => {}
			ArrayDestructuringField::Name(variable_field, _, default_value) => {
				variable_field.visit_mut(visitors, data, options, chain);
				default_value.visit_mut(visitors, data, options, chain);
			}
		}
	}
}

impl Visitable for WithComment<ArrayDestructuringField<crate::ast::LHSOfAssignment>> {
	fn visit<TData>(
		&self,
		_visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		_data: &mut TData,
		_options: &VisitOptions,
		_chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		todo!()
	}

	fn visit_mut<TData>(
		&mut self,
		_visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		_data: &mut TData,
		_options: &VisitOptions,
		_chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		todo!()
	}
}

impl Visitable for WithComment<ObjectDestructuringField<VariableField>> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		visitors.visit_variable(
			&ImmutableVariableOrProperty::ObjectDestructuringMember(self),
			data,
			chain,
		);
		match self.get_ast_ref() {
			ObjectDestructuringField::Spread(_name, _) => {}
			ObjectDestructuringField::Name(_name, _, default_value, _) => {
				default_value.visit(visitors, data, options, chain);
			}
			ObjectDestructuringField::Map {
				name: variable_name,
				annotation: _,
				default_value,
				..
			} => {
				variable_name.visit(visitors, data, options, chain);
				default_value.visit(visitors, data, options, chain);
			}
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		visitors.visit_variable_mut(
			&mut MutableVariableOrProperty::ObjectDestructuringMember(self),
			data,
			chain,
		);
		match self.get_ast_mut() {
			ObjectDestructuringField::Spread(_id, _) => {}
			ObjectDestructuringField::Name(_id, _, default_value, _) => {
				default_value.visit_mut(visitors, data, options, chain);
			}
			ObjectDestructuringField::Map {
				name: variable_name,
				annotation: _,
				default_value,
				..
			} => {
				variable_name.visit_mut(visitors, data, options, chain);
				default_value.visit_mut(visitors, data, options, chain);
			}
		}
	}
}
impl Visitable for WithComment<ObjectDestructuringField<crate::ast::LHSOfAssignment>> {
	fn visit<TData>(
		&self,
		_visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		_data: &mut TData,
		_options: &VisitOptions,
		_chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		todo!()
	}

	fn visit_mut<TData>(
		&mut self,
		_visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		_data: &mut TData,
		_options: &VisitOptions,
		_chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		todo!()
	}
}

#[cfg(not(feature = "extras"))]
fn is_destructuring_into_marker(t: &TSXToken, _options: &ParseOptions) -> bool {
	matches!(t, TSXToken::Colon)
}

#[cfg(feature = "extras")]
fn is_destructuring_into_marker(t: &TSXToken, options: &ParseOptions) -> bool {
	if options.destructuring_type_annotation {
		matches!(t, TSXToken::Keyword(crate::TSXKeyword::As))
	} else {
		matches!(t, TSXToken::Colon)
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::{assert_matches_ast, span};

	#[test]
	fn name() {
		assert_matches_ast!(
			"x",
			VariableField::Name(VariableIdentifier::Standard(
				Deref @ "x",
				Span { start: 0, end: 1, .. },
			))
		);
	}

	#[test]
	fn array() {
		assert_matches_ast!(
			"[x, y, z]",
			VariableField::Array(
				Deref @ [WithComment::None(ArrayDestructuringField::Name(
					VariableField::Name(VariableIdentifier::Standard(Deref @ "x", span!(1, 2))),
					None,
					None,
				)), WithComment::None(ArrayDestructuringField::Name(
					VariableField::Name(VariableIdentifier::Standard(Deref @ "y", span!(4, 5))),
					None,
					None,
				)), WithComment::None(ArrayDestructuringField::Name(
					VariableField::Name(VariableIdentifier::Standard(Deref @ "z", span!(7, 8))),
					None,
					None,
				))],
				_,
			)
		);

		assert_matches_ast!(
			"[x,,z]",
			VariableField::Array(
				Deref @ [WithComment::None(ArrayDestructuringField::Name(
					VariableField::Name(VariableIdentifier::Standard(Deref @ "x", span!(1, 2))),
					None,
					None,
				)), WithComment::None(ArrayDestructuringField::None), WithComment::None(ArrayDestructuringField::Name(
					VariableField::Name(VariableIdentifier::Standard(Deref @ "z", span!(4, 5))),
					None,
					None,
				))],
				span!(0, 6),
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
					None,
					span!(1, 2),
				)), WithComment::None(ObjectDestructuringField::Name(
					VariableIdentifier::Standard(Deref @ "y", span!(4, 5)),
					None,
					None,
					span!(4, 5),
				)), WithComment::None(ObjectDestructuringField::Name(
					VariableIdentifier::Standard(Deref @ "z", span!(7, 8)),
					None,
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
					None,
					Some(
						Deref @ Expression::NumberLiteral(
							crate::NumberRepresentation::Number { .. },
							span!(6, 7),
						),
					),
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
				Deref @ [WithComment::None(ArrayDestructuringField::Name(
					VariableField::Name(VariableIdentifier::Standard(Deref @ "x", span!(1, 2))),
					None,
					None,
				)), WithComment::None(ArrayDestructuringField::Spread(
					VariableField::Name(VariableIdentifier::Standard(Deref @ "y", span!(7, 8))),
					span!(4, 8),
				))],
				span!(0, 9),
			)
		);
	}
}
