use crate::{
	ASTNode, ParseError, ParseErrors, ParseResult,
	ast::{
		ArrayDestructuringField, Expression, ObjectDestructuringField, PropertyKey, PropertyLike,
		PropertyReference, SpreadDestructuringField, SuperReference,
		object_literal::{ObjectLiteral, ObjectLiteralMember},
	},
	derive_ASTNode,
};
use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use visitable_derive::Visitable;

use super::MultipleExpression;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum VariableOrPropertyAccess {
	Variable(String, Span),
	PropertyAccess {
		parent: Box<Expression>,
		property: PropertyReference,
		position: Span,
	},
	/// Using `x[y]`
	Index {
		indexee: Box<Expression>,
		indexer: Box<MultipleExpression>,
		position: Span,
	},
	PropertyOnSuper(PropertyLike, Span),
	/// Allowed under strict mode
	/// For bad property assignment
	Neither(Box<Expression>),
	#[cfg(feature = "full-typescript")]
	NonNullAssertion(Box<VariableOrPropertyAccess>, Span),
}

impl ASTNode for VariableOrPropertyAccess {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		// I think this is correct
		let precedence = super::precedence::INDEX_PRECEDENCE - 1;
		Expression::from_reader_with_precedence(reader, precedence)?.try_into()
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			VariableOrPropertyAccess::Variable(name, ..) => {
				buf.push_str(name);
			}
			VariableOrPropertyAccess::PropertyAccess { parent, property, .. } => {
				if let Expression::NumberLiteral(..)
				| Expression::ObjectLiteral(..)
				| Expression::ArrowFunction(..) = parent.get_non_parenthesised()
				{
					buf.push('(');
					parent.to_string_from_buffer(buf, options, local);
					buf.push(')');
				} else {
					parent.to_string_from_buffer(buf, options, local);
				}
				buf.push('.');
				if let PropertyReference::Standard { property, is_private } = property {
					if *is_private {
						buf.push('#');
					}
					buf.push_str(property);
				} else if !options.expect_markers {
					panic!("found marker");
				}
			}
			VariableOrPropertyAccess::Index { indexee, indexer, .. } => {
				indexee.to_string_from_buffer(buf, options, local);
				buf.push('[');
				indexer.to_string_from_buffer(buf, options, local);
				buf.push(']');
			}
			VariableOrPropertyAccess::Neither(expression) => {
				expression.to_string_from_buffer(buf, options, local);
			}
			VariableOrPropertyAccess::PropertyOnSuper(PropertyLike::Fixed(name), _) => {
				buf.push_str("super.");
				buf.push_str(name);
			}
			VariableOrPropertyAccess::PropertyOnSuper(PropertyLike::Computed(indexer), _) => {
				buf.push_str("super[");
				indexer.to_string_from_buffer(buf, options, local);
				buf.push(']');
			}
			#[cfg(feature = "full-typescript")]
			VariableOrPropertyAccess::NonNullAssertion(on, _position) => {
				on.to_string_from_buffer(buf, options, local);
				if options.include_type_annotations {
					buf.push('!');
				}
			}
		}
	}
}

impl TryFrom<Expression> for VariableOrPropertyAccess {
	type Error = ParseError;

	fn try_from(expression: Expression) -> ParseResult<Self> {
		match expression {
			Expression::VariableReference(name, position) => Ok(Self::Variable(name, position)),
			Expression::PropertyAccess { parent, position, property, is_optional } => {
				if is_optional {
					// Still a proposal :(
					Err(ParseError::new(crate::ParseErrors::InvalidLHSAssignment, position))
				} else {
					Ok(Self::PropertyAccess { parent, position, property })
				}
			}
			Expression::Index { indexer, position, indexee, is_optional: false } => {
				Ok(Self::Index { indexer, position, indexee })
			}
			Expression::SuperExpression(SuperReference::PropertyAccess(property), position) => {
				Ok(Self::PropertyOnSuper(property, position))
			}
			// Yah weird and recursion is fine here
			Expression::Parenthesised(inner, _) => TryFrom::try_from(inner.0),
			// TODO checked under strict mode...?
			Expression::FunctionCall { .. } => Ok(Self::Neither(Box::new(expression))),
			#[cfg(feature = "full-typescript")]
			Expression::SpecialOperators(
				super::SpecialOperators::NonNullAssertion(on),
				position,
			) => TryFrom::try_from(*on)
				.map(|value| Self::NonNullAssertion(Box::new(value), position)),
			expression => {
				Err(ParseError::new(
					crate::ParseErrors::InvalidLHSAssignment,
					expression.get_position(),
				))
				// TODO
				// Ok(Self::Neither(Box::new(expression)))
			}
		}
	}
}

impl From<VariableOrPropertyAccess> for Expression {
	fn from(this: VariableOrPropertyAccess) -> Self {
		match this {
			VariableOrPropertyAccess::Variable(variable, position) => {
				Expression::VariableReference(variable, position)
			}
			VariableOrPropertyAccess::Index { indexee, indexer, position } => {
				Expression::Index { indexee, indexer, position, is_optional: false }
			}
			VariableOrPropertyAccess::PropertyAccess { parent, position, property } => {
				Expression::PropertyAccess { parent, position, property, is_optional: false }
			}
			VariableOrPropertyAccess::PropertyOnSuper(property, position) => {
				Expression::SuperExpression(SuperReference::PropertyAccess(property), position)
			}
			VariableOrPropertyAccess::Neither(expression) => *expression,
			#[cfg(feature = "full-typescript")]
			VariableOrPropertyAccess::NonNullAssertion(on, position) => Expression::SpecialOperators(
				super::SpecialOperators::NonNullAssertion(Box::new((*on).into())),
				position,
			),
		}
	}
}

impl VariableOrPropertyAccess {
	#[must_use]
	pub fn get_parent(&self) -> Option<&Expression> {
		match self {
			VariableOrPropertyAccess::Variable(..)
			| VariableOrPropertyAccess::PropertyOnSuper(..)
			| VariableOrPropertyAccess::Neither(..) => None,
			VariableOrPropertyAccess::PropertyAccess { parent, .. }
			| VariableOrPropertyAccess::Index { indexee: parent, .. } => Some(parent),
			#[cfg(feature = "full-typescript")]
			VariableOrPropertyAccess::NonNullAssertion(on, _) => on.get_parent(),
		}
	}

	pub fn get_parent_mut(&mut self) -> Option<&mut Expression> {
		match self {
			VariableOrPropertyAccess::Variable(..)
			| VariableOrPropertyAccess::PropertyOnSuper(..)
			| VariableOrPropertyAccess::Neither(..) => None,
			VariableOrPropertyAccess::PropertyAccess { parent, .. }
			| VariableOrPropertyAccess::Index { indexee: parent, .. } => Some(parent),
			#[cfg(feature = "full-typescript")]
			VariableOrPropertyAccess::NonNullAssertion(on, _) => on.get_parent_mut(),
		}
	}
}

/// TODO visitable is current skipped...
///
/// Includes [Destructuring assignment](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment)
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, derive_enum_from_into::EnumFrom)]
pub enum LHSOfAssignment {
	VariableOrPropertyAccess(VariableOrPropertyAccess),
	ArrayDestructuring {
		#[visit_skip_field]
		members: Vec<ArrayDestructuringField<LHSOfAssignment>>,
		spread: Option<SpreadDestructuringField<LHSOfAssignment>>,
		position: Span,
	},
	ObjectDestructuring {
		#[visit_skip_field]
		members: Vec<ObjectDestructuringField<LHSOfAssignment>>,
		spread: Option<SpreadDestructuringField<LHSOfAssignment>>,
		position: Span,
	},
}

impl ASTNode for LHSOfAssignment {
	fn get_position(&self) -> Span {
		match self {
			LHSOfAssignment::ObjectDestructuring { position, .. }
			| LHSOfAssignment::ArrayDestructuring { position, .. } => *position,
			LHSOfAssignment::VariableOrPropertyAccess(var_prop_access) => {
				var_prop_access.get_position()
			}
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		if reader.is_keyword_advance("await") {
			if reader.starts_with_expression_delimiter() {
				let inner =
					VariableOrPropertyAccess::Variable("await".to_owned(), start.with_length(5));
				Ok(Self::VariableOrPropertyAccess(inner))
			} else {
				let expression = super::parse_after_await(reader, start, false)?;
				let inner = VariableOrPropertyAccess::Neither(Box::new(expression.0));
				Ok(Self::VariableOrPropertyAccess(inner))
			}
		} else {
			Expression::from_reader(reader).and_then(TryInto::try_into)
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			LHSOfAssignment::ObjectDestructuring { members, spread, position: _ } => {
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
			LHSOfAssignment::ArrayDestructuring { members, spread, position: _ } => {
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
			LHSOfAssignment::VariableOrPropertyAccess(variable_or_property_access) => {
				variable_or_property_access.to_string_from_buffer(buf, options, local);
			}
		}
	}
}

impl TryFrom<Expression> for LHSOfAssignment {
	type Error = ParseError;

	fn try_from(value: Expression) -> Result<Self, Self::Error> {
		match value {
			Expression::ArrayLiteral(members, position) => {
				let mut new_members = Vec::with_capacity(members.len());
				let mut iter = members.into_iter();
				for member in iter.by_ref() {
					let position = member.get_position();
					if let Some(member) = member.0 {
						let (expression, spread) = member.value_and_spread();

						if spread {
							return if let Some(next) = iter.next() {
								Err(ParseError::new(
									ParseErrors::CannotHaveRegularMemberAfterSpread,
									next.get_position(),
								))
							} else {
								let inner: LHSOfAssignment = expression.try_into()?;
								Ok(Self::ArrayDestructuring {
									members: new_members,
									spread: Some(SpreadDestructuringField(
										Box::new(inner),
										position,
									)),
									// TODO
									position,
								})
							};
						}

						match expression {
							Expression::Assignment { lhs, rhs, position: _ } => {
								new_members.push(ArrayDestructuringField::Name(lhs, (), Some(rhs)));
							}
							expression => {
								new_members.push(ArrayDestructuringField::Name(
									expression.try_into()?,
									(),
									None,
								));
							}
						}
					} else {
						new_members.push(ArrayDestructuringField::None);
					}
				}
				Ok(Self::ArrayDestructuring { members: new_members, spread: None, position })
			}
			Expression::ObjectLiteral(ObjectLiteral { members, position }) => {
				let mut new_members = Vec::with_capacity(members.len());
				let mut iter = members.into_iter();
				for member in iter.by_ref() {
					let new_member: ObjectDestructuringField<LHSOfAssignment> = match member {
						ObjectLiteralMember::Spread(expression, span) => {
							return if let Some(next) = iter.next() {
								Err(ParseError::new(
									ParseErrors::CannotHaveRegularMemberAfterSpread,
									next.get_position(),
								))
							} else {
								let inner: LHSOfAssignment = expression.try_into()?;
								Ok(Self::ObjectDestructuring {
									members: new_members,
									spread: Some(SpreadDestructuringField(Box::new(inner), span)),
									position,
								})
							};
						}
						ObjectLiteralMember::Shorthand(name) => {
							let PropertyKey::Identifier(name, pos, _) = name.0 else {
								panic!();
							};
							ObjectDestructuringField::Name(
								crate::VariableIdentifier::Standard(name, pos),
								(),
								None,
								pos,
							)
						}
						ObjectLiteralMember::Property { assignment, key, position, value } => {
							if assignment {
								if let PropertyKey::Identifier(name, pos, _) = key {
									ObjectDestructuringField::Name(
										crate::VariableIdentifier::Standard(name, pos),
										(),
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
									from: key,
									annotation: (),
									name,
									default_value,
									position,
								}
							}
						}
						ObjectLiteralMember::Method(_) => {
							return Err(ParseError::new(
								crate::ParseErrors::InvalidLHSAssignment,
								position,
							));
						}
						ObjectLiteralMember::Comment(..) => {
							continue;
						}
					};
					new_members.push(new_member);
				}
				Ok(Self::ObjectDestructuring { members: new_members, spread: None, position })
			}
			expression => VariableOrPropertyAccess::try_from(expression)
				.map(LHSOfAssignment::VariableOrPropertyAccess),
		}
	}
}
