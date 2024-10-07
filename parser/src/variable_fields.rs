/// Contains:
/// - [`VariableId`] given to variable declaring items
/// - [`VariableField`] for destructuring things and its nested derivatives + visiting behavior + tests for self
use std::fmt::Debug;

use crate::{
	bracketed_items_from_reader, derive_ASTNode,
	property_key::PropertyKey,
	visiting::{ImmutableVariableOrProperty, MutableVariableOrProperty},
	ASTNode, Expression, ListItem, Marker, ParseError, ParseErrors, ParseOptions, ParseResult,
	Span, VisitOptions, Visitable, WithComment,
};

use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;

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
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let identifier = reader.parse_identifier()?;
		let position = start.with_length(identifier.len());
		Ok(Self::Standard(identifier.to_owned(), position))
		// TODO
		// if ident == "let" {
		// 	return Err(ParseError::new(ParseErrors::ReservedIdentifier, span));
		// }
		// Ok(if options.interpolation_points && ident == crate::marker::MARKER {
		// 	Self::Marker(state.new_partial_point_marker(span.get_start()), span)
		// } else {
		// })
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
	Array {
		members: Vec<WithComment<ArrayDestructuringField<VariableField>>>,
		spread: Option<SpreadDestructuringField<VariableField>>,
		position: Span,
	},
	/// `{ x, y: z }`.
	Object {
		members: Vec<WithComment<ObjectDestructuringField<VariableField>>>,
		spread: Option<SpreadDestructuringField<VariableField>>,
		position: Span,
	},
}

impl ASTNode for VariableField {
	fn get_position(&self) -> Span {
		match self {
			VariableField::Array { position, .. } | VariableField::Object { position, .. } => {
				*position
			}
			VariableField::Name(id) => id.get_position(),
		}
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		reader.skip();
		let start = reader.get_start();
		if reader.is_operator_advance("{") {
			let (members, spread) = bracketed_items_from_reader(reader, "}")?;
			Ok(Self::Object { members, spread, position: start.union(reader.get_end()) })
		} else if reader.is_operator_advance("[") {
			let (members, spread) = bracketed_items_from_reader(reader, "]")?;
			Ok(Self::Array { members, spread, position: start.union(reader.get_end()) })
		} else {
			Ok(Self::Name(VariableIdentifier::from_reader(reader)?))
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
			Self::Array { members, spread, position: _ } => {
				buf.push('[');
				for (at_end, member) in members.iter().endiate() {
					member.to_string_from_buffer(buf, options, local);
					if !at_end {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
				}
				if let Some(ref spread) = spread {
					if !members.is_empty() {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
					buf.push_str("...");
					spread.0.to_string_from_buffer(buf, options, local);
				}
				buf.push(']');
			}
			Self::Object { members, spread, position: _ } => {
				buf.push('{');
				options.push_gap_optionally(buf);
				for (at_end, member) in members.iter().endiate() {
					member.to_string_from_buffer(buf, options, local);
					if !at_end {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
				}
				if let Some(ref spread) = spread {
					if !members.is_empty() {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
					buf.push_str("...");
					spread.0.to_string_from_buffer(buf, options, local);
				}
				options.push_gap_optionally(buf);
				buf.push('}');
			}
		}
	}
}

pub trait DestructuringFieldInto: ASTNode {
	// This in an extra
	type TypeAnnotation: Clone + PartialEq + Debug + Sync + Send + 'static;

	fn type_annotation_from_reader(
		reader: &mut crate::new::Lexer,
	) -> ParseResult<Self::TypeAnnotation>;
}

impl DestructuringFieldInto for VariableField {
	type TypeAnnotation = Option<crate::TypeAnnotation>;

	fn type_annotation_from_reader(
		reader: &mut crate::new::Lexer,
	) -> ParseResult<Self::TypeAnnotation> {
		if reader.get_options().destructuring_type_annotation && reader.is_operator_advance(":") {
			crate::TypeAnnotation::from_reader(reader).map(Some)
		} else {
			Ok(None)
		}
	}
}

impl DestructuringFieldInto for crate::ast::LHSOfAssignment {
	type TypeAnnotation = ();

	fn type_annotation_from_reader(
		_reader: &mut crate::new::Lexer,
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
	Name(T, T::TypeAnnotation, Option<Box<Expression>>),
	None,
}

/// Covers [`ArrayDestructuring`] AND [`ObjectDestructuringField`]
#[derive(Debug, Clone, PartialEq, Eq, visitable_derive::Visitable)]
#[apply(derive_ASTNode)]
pub struct SpreadDestructuringField<T: DestructuringFieldInto>(pub Box<T>, pub Span);

impl<T: DestructuringFieldInto> ListItem for WithComment<ArrayDestructuringField<T>> {
	const EMPTY: Option<Self> = Some(WithComment::None(ArrayDestructuringField::None));

	const LAST_PREFIX: Option<&'static str> = Some("...");

	type LAST = SpreadDestructuringField<T>;

	fn parse_last_item(reader: &mut crate::new::Lexer) -> ParseResult<Self::LAST> {
		let start = reader.get_start();
		reader.expect_operator("...")?;
		let node = T::from_reader(reader)?;
		let position = start.union(node.get_position());
		Ok(SpreadDestructuringField(Box::new(node), position))
	}
}

impl<T: DestructuringFieldInto> ASTNode for ArrayDestructuringField<T> {
	fn get_position(&self) -> Span {
		match self {
			// TODO misses out optional expression
			ArrayDestructuringField::Name(vf, ..) => vf.get_position(),
			ArrayDestructuringField::None => source_map::Nullable::NULL,
		}
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		// Allowed
		if reader.is_operator(",") || reader.is_operator("}") {
			Ok(Self::None)
		} else {
			let name = T::from_reader(reader)?;
			let annotation = T::type_annotation_from_reader(reader)?;
			let default_value = if reader.is_operator_advance("=") {
				Some(ASTNode::from_reader(reader).map(Box::new)?)
			} else {
				None
			};
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
			Self::Name(name, _annotation, default_value) => {
				name.to_string_from_buffer(buf, options, local);
				if let Some(default_value) = default_value {
					options.push_gap_optionally(buf);
					buf.push('=');
					options.push_gap_optionally(buf);
					default_value.to_string_from_buffer(buf, options, local);
				}
			}
			Self::None => {}
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
	/// `{ x: y }`
	Map {
		from: PropertyKey<crate::property_key::AlwaysPublic>,
		annotation: T::TypeAnnotation,
		name: WithComment<T>,
		default_value: Option<Box<Expression>>,
		position: Span,
	},
}

impl<T: DestructuringFieldInto> ListItem for WithComment<ObjectDestructuringField<T>> {
	const LAST_PREFIX: Option<&'static str> = Some("...");

	type LAST = SpreadDestructuringField<T>;

	fn parse_last_item(reader: &mut crate::new::Lexer) -> ParseResult<Self::LAST> {
		let start = reader.get_start();
		reader.expect_operator("...")?;
		let node = T::from_reader(reader)?;
		let position = start.union(node.get_position());
		Ok(SpreadDestructuringField(Box::new(node), position))
	}
}

impl<T: DestructuringFieldInto> ASTNode for ObjectDestructuringField<T> {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		// #[cfg(not(feature = "extras"))]
		// fn is_destructuring_into_marker(t: &TSXToken, _options: &ParseOptions) -> bool {
		// 	matches!(t, TSXToken::Colon)
		// }

		// #[cfg(feature = "extras")]
		// fn is_destructuring_into_marker(t: &TSXToken, options: &ParseOptions) -> bool {
		// 	if options.destructuring_type_annotation {
		// 		matches!(t, TSXToken::Keyword(crate::TSXKeyword::As))
		// 	} else {
		// 		matches!(t, TSXToken::Colon)
		// 	}
		// }

		let key = PropertyKey::from_reader(reader)?;
		if reader.is_operator_advance(":") {
			let name = WithComment::<T>::from_reader(reader)?;
			let annotation = T::type_annotation_from_reader(reader)?;

			let default_value = reader
				.is_operator_advance("=")
				.then(|| Expression::from_reader(reader).map(Box::new))
				.transpose()?;

			let position = if let Some(ref dv) = default_value {
				key.get_position().union(dv.get_position())
			} else {
				key.get_position()
			};

			Ok(Self::Map { from: key, annotation, name, default_value, position })
		} else if let PropertyKey::Identifier(name, key_pos, _) = key {
			let default_value = reader
				.is_operator_advance("=")
				.then(|| Expression::from_reader(reader).map(Box::new))
				.transpose()?;

			let standard = VariableIdentifier::Standard(name, key_pos);
			let annotation = T::type_annotation_from_reader(reader)?;
			let position = if let Some(ref dv) = default_value {
				key_pos.union(dv.get_position())
			} else {
				key_pos
			};

			Ok(Self::Name(standard, annotation, default_value, position))
		} else {
			todo!("expect colon")
		}
	}

	fn to_string_from_buffer<U: source_map::ToString>(
		&self,
		buf: &mut U,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
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
			VariableField::Array { members, spread: _, .. } => {
				members.iter().for_each(|f| f.visit(visitors, data, options, chain));
			}
			VariableField::Object { members, spread: _, .. } => {
				members.iter().for_each(|f| f.visit(visitors, data, options, chain));
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
			VariableField::Array { members, spread: _, .. } => {
				members.iter_mut().for_each(|f| f.visit_mut(visitors, data, options, chain));
			}
			VariableField::Object { members, spread: _, .. } => {
				members.iter_mut().for_each(|f| f.visit_mut(visitors, data, options, chain));
			}
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
			ArrayDestructuringField::None => {}
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
			ArrayDestructuringField::None => {}
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
			VariableField::Array {
				members: Deref @ [WithComment::None(ArrayDestructuringField::Name(
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
				spread: _,
				position: _
			}
		);

		assert_matches_ast!(
			"[x,,z]",
			VariableField::Array {
				members:
				Deref @ [WithComment::None(ArrayDestructuringField::Name(
					VariableField::Name(VariableIdentifier::Standard(Deref @ "x", span!(1, 2))),
					None,
					None,
				)), WithComment::None(ArrayDestructuringField::None), WithComment::None(ArrayDestructuringField::Name(
					VariableField::Name(VariableIdentifier::Standard(Deref @ "z", span!(4, 5))),
					None,
					None,
				))],
				spread: None,
				position: span!(0, 6),
			}
		);
	}

	#[test]
	fn object() {
		assert_matches_ast!(
			"{x, y, z}",
			VariableField::Object {
				members: Deref @ [WithComment::None(ObjectDestructuringField::Name(
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
				spread: None,
				position: span!(0, 9),
			}
		);
	}

	#[test]
	fn name_with_default() {
		assert_matches_ast!(
			"{ x = 2 }",
			VariableField::Object {
				members:
				Deref @ [WithComment::None(ObjectDestructuringField::Name(
					VariableIdentifier::Standard(Deref @ "x", span!(2, 3)),
					None,
					Some(
						Deref @ Expression::NumberLiteral(
							crate::number::NumberRepresentation::Number { .. },
							span!(6, 7),
						),
					),
					span!(2, 7),
				))],
				spread: None,
				position: span!(0, 9),
			}
		);
	}

	#[test]
	fn array_spread() {
		assert_matches_ast!(
			"[x, ...y]",
			VariableField::Array {
				members:Deref @ [WithComment::None(ArrayDestructuringField::Name(
					VariableField::Name(VariableIdentifier::Standard(Deref @ "x", span!(1, 2))),
					None,
					None,
				))],
				spread: Some(SpreadDestructuringField( Deref @ VariableField::Name(VariableIdentifier::Standard(Deref @ "y", span!(7, 8))), span!(4, 8))),
				position: span!(0, 9)
			}
		);
	}
}
