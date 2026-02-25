/// Contains:
/// - [`VariableField`] for destructuring things and its nested derivatives + visiting behavior + tests for self
use std::fmt::Debug;

use crate::{
	ASTNode, Expression, ListItem, Marker, ParseError, ParseErrors, ParseResult, Span,
	bracketed_items_from_reader, derive_ASTNode, property_key::PropertyKey,
};

use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, GetFieldByType)]
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

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		if reader.get_options().features.partial_syntax && reader.starts_with_expression_delimiter()
		{
			let span = start.with_length(0);
			Ok(Self::Marker(reader.new_partial_point_marker(span), span))
		} else {
			let enforce = false;
			let identifier = reader.parse_identifier("variable identifier", enforce)?;
			let position = start.with_length(identifier.len());

			// if !spread_name && !non_strict && identifier == "let" {
			// 	return Err(ParseError::new(ParseErrors::ReservedIdentifier, start.with_length(3)));
			// }

			if reader.get_options().features.interpolation_points
				&& identifier == crate::marker::MARKER
			{
				let span = start.with_length(0);
				Ok(Self::Marker(reader.new_partial_point_marker(span), span))
			} else {
				Ok(Self::Standard(identifier.into_owned(), position))
			}
		}
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

impl PartialEq<&str> for VariableIdentifier {
	fn eq(&self, s: &&str) -> bool {
		self.as_option_str().is_some_and(|name| name == *s)
	}
}

/// A variable declaration name, used in variable declarations and function parameters.
/// See [destructuring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment)
#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub enum VariableField {
	/// `x`
	Name(VariableIdentifier),
	/// `[x, y, z]`
	ArrayDestructuring {
		members: Vec<ArrayDestructuringField<VariableField>>,
		spread: Option<SpreadDestructuringField<VariableField>>,
		position: Span,
	},
	/// `{ x, y: z }`.
	ObjectDestructuring {
		#[cfg(feature = "extras")]
		class_name: Option<String>,
		members: Vec<ObjectDestructuringField<VariableField>>,
		spread: Option<SpreadDestructuringField<VariableField>>,
		position: Span,
	},
}

impl ASTNode for VariableField {
	fn get_position(&self) -> Span {
		match self {
			VariableField::ArrayDestructuring { position, .. }
			| VariableField::ObjectDestructuring { position, .. } => *position,
			VariableField::Name(id) => id.get_position(),
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		if reader.is_operator_advance("{") {
			let (members, spread) = bracketed_items_from_reader(reader, "}")?;
			Ok(Self::ObjectDestructuring {
				members,
				spread,
				position: start.union(reader.get_end()),
				#[cfg(feature = "extras")]
				class_name: None,
			})
		} else if reader.is_operator_advance("[") {
			let (members, spread) = bracketed_items_from_reader(reader, "]")?;
			Ok(Self::ArrayDestructuring {
				members,
				spread,
				position: start.union(reader.get_end()),
			})
		} else {
			#[cfg(feature = "extras")]
			if reader.get_options().extras.destructuring_type_annotation
				&& reader.after_identifier().starts_with('{')
			{
				let start = reader.get_start();
				let class_name =
					reader.parse_identifier("class name in destructuring label", true)?;
				let _ = reader.expect_chr('{')?;
				let (members, spread) = bracketed_items_from_reader(reader, "}")?;
				return Ok(Self::ObjectDestructuring {
					class_name: Some(class_name.into_owned()),
					members,
					spread,
					position: start.union(reader.get_end()),
				});
			}

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
			Self::ArrayDestructuring { members, spread, position: _ } => {
				buf.push('[');
				for (at_end, member) in members.iter().endiate() {
					member.to_string_from_buffer(buf, options, local);
					if !at_end {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
				}
				if let Some(spread) = spread {
					if !members.is_empty() {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
					buf.push_str("...");
					spread.0.to_string_from_buffer(buf, options, local);
				}
				buf.push(']');
			}
			Self::ObjectDestructuring { members, spread, position: _, .. } => {
				#[cfg(feature = "extras")]
				if let Self::ObjectDestructuring { class_name: Some(class_name), .. } = self {
					buf.push_str(class_name);
					options.push_gap_optionally(buf);
				}

				buf.push('{');
				options.push_gap_optionally(buf);
				for (at_end, member) in members.iter().endiate() {
					member.to_string_from_buffer(buf, options, local);
					if !at_end {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
				}
				if let Some(spread) = spread {
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

impl From<VariableIdentifier> for VariableField {
	fn from(on: VariableIdentifier) -> Self {
		VariableField::Name(on)
	}
}

pub trait DestructuringFieldInto: ASTNode {
	// This in an extra
	type TypeAnnotation: Clone + Debug + Sync + Send + 'static;

	fn type_annotation_from_reader(reader: &mut crate::Lexer) -> ParseResult<Self::TypeAnnotation>;
}

impl DestructuringFieldInto for VariableField {
	type TypeAnnotation = Option<crate::TypeAnnotation>;

	fn type_annotation_from_reader(reader: &mut crate::Lexer) -> ParseResult<Self::TypeAnnotation> {
		if reader.get_options().extras.destructuring_type_annotation
			&& reader.is_operator_advance(":")
		{
			crate::TypeAnnotation::from_reader(reader).map(Some)
		} else {
			Ok(None)
		}
	}
}

impl DestructuringFieldInto for crate::ast::LHSOfAssignment {
	type TypeAnnotation = ();

	fn type_annotation_from_reader(
		_reader: &mut crate::Lexer,
	) -> ParseResult<Self::TypeAnnotation> {
		Ok(())
	}
}

/// For
/// - declarations: `T = VariableField`
/// - expressions: `T = LHSOfAssignment`
#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub enum ArrayDestructuringField<T: DestructuringFieldInto> {
	Name(T, T::TypeAnnotation, Option<Box<Expression>>),
	None,
}

/// Covers [`ArrayDestructuring`] AND [`ObjectDestructuringField`]
#[derive(Debug, Clone, visitable_derive::Visitable)]
#[apply(derive_ASTNode)]
pub struct SpreadDestructuringField<T: DestructuringFieldInto>(pub Box<T>, pub Span);

impl<T: DestructuringFieldInto> ASTNode for ArrayDestructuringField<T> {
	fn get_position(&self) -> Span {
		match self {
			// TODO misses out optional expression
			ArrayDestructuringField::Name(vf, ..) => vf.get_position(),
			ArrayDestructuringField::None => source_map::Nullable::NULL,
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		// Allowed
		if reader.get_current().starts_with([',', ']']) {
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
			// 	if let Some(pos) = default_value {
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

impl<T: DestructuringFieldInto> ListItem for ArrayDestructuringField<T> {
	const LAST_PREFIX: Option<&'static str> = Some("...");

	type LAST = SpreadDestructuringField<T>;

	fn parse_last_item(reader: &mut crate::Lexer) -> ParseResult<Self::LAST> {
		let start = reader.get_start();
		reader.expect_operator("...")?;
		let node = T::from_reader(reader)?;
		let position = start.union(node.get_position());
		Ok(SpreadDestructuringField(Box::new(node), position))
	}

	fn skip_trailing() -> bool {
		false
	}
}

/// For
/// - declarations: `T = VariableField`
/// - expressions: `T = LHSOfAssignment`
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum ObjectDestructuringField<T: DestructuringFieldInto> {
	/// `{ x }` and (annoyingly) `{ x = 2 }`
	Name(VariableIdentifier, T::TypeAnnotation, Option<Box<Expression>>, Span),
	/// `{ x: y }`
	Map {
		from: PropertyKey<crate::property_key::AlwaysPublic>,
		annotation: T::TypeAnnotation,
		name: T,
		default_value: Option<Box<Expression>>,
		position: Span,
	},
}

impl<T: DestructuringFieldInto> ListItem for ObjectDestructuringField<T> {
	const LAST_PREFIX: Option<&'static str> = Some("...");

	type LAST = SpreadDestructuringField<T>;

	fn parse_last_item(reader: &mut crate::Lexer) -> ParseResult<Self::LAST> {
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

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
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
			let name = T::from_reader(reader)?;
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
			let (found, position) = crate::lexer::utilities::next_item(reader);
			let error = ParseErrors::ExpectedOperator { expected: ";", found };
			Err(ParseError::new(error, position))
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

pub mod visiting {
	use super::{
		ArrayDestructuringField, ObjectDestructuringField, VariableField, VariableIdentifier,
	};
	use crate::visiting::{
		ImmutableVariableOrProperty, MutableVariableOrProperty, VisitOptions, Visitable,
	};

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
				VariableField::ArrayDestructuring { members, spread: _, .. } => {
					for f in members {
						f.visit(visitors, data, options, chain);
					}
				}
				VariableField::ObjectDestructuring { members, spread: _, .. } => {
					for f in members {
						f.visit(visitors, data, options, chain);
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
				VariableField::ArrayDestructuring { members, spread: _, .. } => {
					for f in members {
						f.visit_mut(visitors, data, options, chain);
					}
				}
				VariableField::ObjectDestructuring { members, spread: _, .. } => {
					for f in members {
						f.visit_mut(visitors, data, options, chain);
					}
				}
			}
		}
	}

	impl Visitable for ArrayDestructuringField<VariableField> {
		fn visit<TData>(
			&self,
			visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
			data: &mut TData,
			options: &VisitOptions,
			chain: &mut temporary_annex::Annex<crate::Chain>,
		) {
			let field = self;
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
				MutableVariableOrProperty::ArrayDestructuringMember(self);
			visitors.visit_variable_mut(&mut array_destructuring_member, data, chain);
			match self {
				ArrayDestructuringField::None => {}
				ArrayDestructuringField::Name(variable_field, _, default_value) => {
					variable_field.visit_mut(visitors, data, options, chain);
					default_value.visit_mut(visitors, data, options, chain);
				}
			}
		}
	}

	impl Visitable for ArrayDestructuringField<crate::ast::LHSOfAssignment> {
		fn visit<TData>(
			&self,
			_visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
			_data: &mut TData,
			_options: &VisitOptions,
			_chain: &mut temporary_annex::Annex<crate::Chain>,
		) {
			todo!("visit array destructuring field")
		}

		fn visit_mut<TData>(
			&mut self,
			_visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
			_data: &mut TData,
			_options: &VisitOptions,
			_chain: &mut temporary_annex::Annex<crate::Chain>,
		) {
			todo!("visit array destructuring field")
		}
	}

	impl Visitable for ObjectDestructuringField<VariableField> {
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
			match self {
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
			match self {
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

	impl Visitable for ObjectDestructuringField<crate::ast::LHSOfAssignment> {
		fn visit<TData>(
			&self,
			_visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
			_data: &mut TData,
			_options: &VisitOptions,
			_chain: &mut temporary_annex::Annex<crate::Chain>,
		) {
			todo!("visit object destructuring field")
		}

		fn visit_mut<TData>(
			&mut self,
			_visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
			_data: &mut TData,
			_options: &VisitOptions,
			_chain: &mut temporary_annex::Annex<crate::Chain>,
		) {
			todo!("visit object destructuring field")
		}
	}

	// Lower level visiting for parsing
	impl VariableField {
		pub fn visit_names(&self, cb: &mut impl FnMut(&str)) {
			match self {
				VariableField::Name(name) => {
					if let Some(name) = name.as_option_str() {
						cb(name);
					}
				}
				VariableField::ArrayDestructuring { members, spread, position: _ } => {
					for member in members {
						if let super::ArrayDestructuringField::Name(name, ..) = member {
							name.visit_names(cb);
						}
					}
					if let Some(spread) = spread {
						spread.0.visit_names(cb);
					}
				}
				VariableField::ObjectDestructuring { members, spread, .. } => {
					for member in members {
						match member {
							super::ObjectDestructuringField::Name(name, ..) => {
								if let Some(name) = name.as_option_str() {
									cb(name);
								}
							}
							super::ObjectDestructuringField::Map { name, .. } => {
								name.visit_names(cb);
							}
						}
					}
					if let Some(spread) = spread {
						spread.0.visit_names(cb);
					}
				}
			}
		}
	}
}

impl TryFrom<Expression> for VariableField {
	type Error = ParseError;

	fn try_from(value: Expression) -> Result<Self, Self::Error> {
		match value {
			Expression::ArrayLiteral(members, position) => {
				let mut new_members: Vec<ArrayDestructuringField<VariableField>> =
					Vec::with_capacity(members.len());
				let mut iter = members.into_iter();
				for member in iter.by_ref() {
					if let Some(member) = member.0 {
						let (spread, expression) = member.value_and_spread();

						if spread {
							return if let Some(next) = iter.next() {
								Err(ParseError::new(
									ParseErrors::CannotHaveRegularMemberAfterSpread,
									next.get_position(),
								))
							} else {
								let inner: VariableField = expression.try_into()?;
								Ok(Self::ArrayDestructuring {
									members: new_members,
									spread: Some(SpreadDestructuringField(
										Box::new(inner),
										position,
									)),
									position,
								})
							};
						} else {
							match expression {
								Expression::Assignment { lhs, rhs, position: _ } => {
									new_members.push(ArrayDestructuringField::Name(
										lhs.try_into()?,
										None,
										Some(rhs),
									));
								}
								expression => {
									new_members.push(ArrayDestructuringField::Name(
										expression.try_into()?,
										None,
										None,
									));
								}
							}
						}
					} else {
						new_members.push(ArrayDestructuringField::None);
					}
				}
				Ok(Self::ArrayDestructuring { members: new_members, spread: None, position })
			}
			Expression::ObjectLiteral(crate::expressions::ObjectLiteral { members, position }) => {
				let mut new_members = Vec::with_capacity(members.len());
				let mut iter = members.into_iter();
				for member in iter.by_ref() {
					let new_member: ObjectDestructuringField<Self> = match member {
						crate::expressions::object_literal::ObjectLiteralMember::Spread(
							expression,
							span,
						) => {
							return if let Some(next) = iter.next() {
								Err(ParseError::new(
									ParseErrors::CannotHaveRegularMemberAfterSpread,
									next.get_position(),
								))
							} else {
								let inner: Self = expression.try_into()?;
								Ok(Self::ObjectDestructuring {
									class_name: None,
									members: new_members,
									spread: Some(SpreadDestructuringField(Box::new(inner), span)),
									position,
								})
							};
						}
						crate::expressions::object_literal::ObjectLiteralMember::Shorthand(
							name,
							pos,
						) => ObjectDestructuringField::Name(
							crate::VariableIdentifier::Standard(name, pos),
							None,
							None,
							pos,
						),
						crate::expressions::object_literal::ObjectLiteralMember::Property {
							assignment,
							key,
							position,
							value,
						} => {
							if assignment {
								if let PropertyKey::Identifier(name, pos, _) = key {
									ObjectDestructuringField::Name(
										crate::VariableIdentifier::Standard(name, pos),
										None,
										Some(Box::new(value)),
										pos,
									)
								} else {
									return Err(ParseError::new(
										crate::ParseErrors::InvalidLHSAssignment,
										position,
									));
								}
							} else {
								let (name, default_value) =
									if let Expression::Assignment { lhs, rhs, position: _ } = value
									{
										(lhs, Some(rhs))
									} else {
										(value.try_into()?, None)
									};

								ObjectDestructuringField::Map {
									from: key.into(),
									annotation: None,
									name: name.try_into()?,
									default_value,
									position,
								}
							}
						}
						crate::expressions::object_literal::ObjectLiteralMember::Method(_) => {
							return Err(ParseError::new(
								crate::ParseErrors::InvalidLHSAssignment,
								position,
							));
						}
						crate::expressions::object_literal::ObjectLiteralMember::Comment(..) => {
							continue;
						}
					};
					new_members.push(new_member);
				}
				Ok(Self::ObjectDestructuring {
					#[cfg(feature = "extras")]
					class_name: None,
					members: new_members,
					spread: None,
					position,
				})
			}
			expression => {
				Err(ParseError::new(ParseErrors::InvalidVariableField, expression.get_position()))
			}
		}
	}
}

impl TryFrom<crate::expressions::LHSOfAssignment> for VariableField {
	type Error = ParseError;

	#[allow(unused)]
	fn try_from(value: crate::expressions::LHSOfAssignment) -> Result<Self, Self::Error> {
		match value {
			crate::expressions::LHSOfAssignment::VariableOrPropertyAccess(
				variable_or_property_access,
			) => {
				if let crate::expressions::VariableOrPropertyAccess::Variable(name, position) =
					variable_or_property_access
				{
					Ok(VariableField::Name(VariableIdentifier::Standard(name, position)))
				} else {
					Err(ParseError::new(
						ParseErrors::InvalidVariableField,
						variable_or_property_access.get_position(),
					))
				}
			}
			crate::expressions::LHSOfAssignment::ArrayDestructuring {
				members,
				spread,
				position,
			} => {
				todo!();
			}
			crate::expressions::LHSOfAssignment::ObjectDestructuring {
				members,
				spread,
				position,
			} => {
				todo!();
			}
		}
	}
}
