use crate::{
	ast::{object_literal::ObjectLiteralMember, FunctionArgument},
	derive_ASTNode, PropertyKey, PropertyReference, TSXToken,
};
use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::TokenReader;
use visitable_derive::Visitable;

use crate::{
	ASTNode, ArrayDestructuringField, Expression, ObjectDestructuringField, ParseError,
	ParseResult, WithComment,
};

use super::MultipleExpression;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[partial_eq_ignore_types(Span)]
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
}

impl ASTNode for VariableOrPropertyAccess {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> ParseResult<Self> {
		Expression::from_reader(reader, state, options)?.try_into()
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
				| Expression::ArrowFunction(..) = parent.get_non_parenthesized()
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
		}
	}
}

impl VariableOrPropertyAccess {
	pub(crate) fn from_reader_with_precedence(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
		return_precedence: u8,
	) -> ParseResult<Self> {
		Expression::from_reader_with_precedence(reader, state, options, return_precedence, None)?
			.try_into()
	}
}

impl TryFrom<Expression> for VariableOrPropertyAccess {
	type Error = ParseError;

	fn try_from(expression: Expression) -> Result<Self, Self::Error> {
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
			// Yah weird. Recursion is fine
			Expression::ParenthesizedExpression(inner, _) => {
				if let MultipleExpression::Single(expression) = *inner {
					TryFrom::try_from(expression)
				} else {
					Err(ParseError::new(
						crate::ParseErrors::InvalidLHSAssignment,
						inner.get_position(),
					))
				}
			}
			expression => Err(ParseError::new(
				crate::ParseErrors::InvalidLHSAssignment,
				expression.get_position(),
			)),
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
		}
	}
}

impl VariableOrPropertyAccess {
	#[must_use]
	pub fn get_parent(&self) -> Option<&Expression> {
		match self {
			VariableOrPropertyAccess::Variable(..) => None,
			VariableOrPropertyAccess::PropertyAccess { parent, .. }
			| VariableOrPropertyAccess::Index { indexee: parent, .. } => Some(parent),
		}
	}

	pub fn get_parent_mut(&mut self) -> Option<&mut Expression> {
		match self {
			VariableOrPropertyAccess::Variable(..) => None,
			VariableOrPropertyAccess::PropertyAccess { parent, .. }
			| VariableOrPropertyAccess::Index { indexee: parent, .. } => Some(parent),
		}
	}
}

/// TODO visitable is current skipped...
///
/// Includes [Destructuring assignment](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment)
#[apply(derive_ASTNode)]
#[derive(PartialEqExtras, Debug, Clone, Visitable, derive_enum_from_into::EnumFrom)]
#[partial_eq_ignore_types(Span)]
pub enum LHSOfAssignment {
	VariableOrPropertyAccess(VariableOrPropertyAccess),
	ArrayDestructuring(#[visit_skip_field] Vec<WithComment<ArrayDestructuringField>>, Span),
	ObjectDestructuring(#[visit_skip_field] Vec<WithComment<ObjectDestructuringField>>, Span),
}

impl LHSOfAssignment {
	#[must_use]
	pub fn get_position(&self) -> Span {
		match self {
			LHSOfAssignment::ObjectDestructuring(_, pos)
			| LHSOfAssignment::ArrayDestructuring(_, pos) => *pos,
			LHSOfAssignment::VariableOrPropertyAccess(var_prop_access) => {
				var_prop_access.get_position()
			}
		}
	}

	pub(crate) fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			LHSOfAssignment::ObjectDestructuring(members, _) => {
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
			LHSOfAssignment::ArrayDestructuring(members, _) => {
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
			Expression::ArrayLiteral(inner, position) => {
				let mut members = Vec::with_capacity(inner.len());
				for member in inner {
					let new_member = match member.0 {
						Some(FunctionArgument::Comment { content, is_multiline: _, position }) => {
							WithComment::PrefixComment(
								content,
								ArrayDestructuringField::None,
								position,
							)
						}
						Some(FunctionArgument::Spread(expression, span)) => {
							let lhs: LHSOfAssignment = expression.try_into()?;
							let lhs: crate::VariableField = lhs_to_variable_field(lhs)?;
							WithComment::None(ArrayDestructuringField::Spread(lhs, span))
						}
						Some(FunctionArgument::Standard(expression)) => {
							WithComment::None(match expression {
								Expression::Assignment { lhs, rhs, position: _ } => {
									let lhs: crate::VariableField = lhs_to_variable_field(lhs)?;
									ArrayDestructuringField::Name(lhs, Some(rhs))
								}
								Expression::ArrayLiteral(..) | Expression::ObjectLiteral(..) => {
									let lhs: LHSOfAssignment = expression.try_into()?;
									let lhs: crate::VariableField = lhs_to_variable_field(lhs)?;
									ArrayDestructuringField::Name(lhs, None)
								}
								Expression::VariableReference(reference, pos) => {
									let name = crate::VariableIdentifier::Standard(reference, pos);
									ArrayDestructuringField::Name(
										crate::VariableField::Name(name),
										None,
									)
								}
								_ => {
									return Err(ParseError::new(
										crate::ParseErrors::InvalidLHSAssignment,
										expression.get_position(),
									))
								}
							})
						}
						None => WithComment::None(ArrayDestructuringField::None),
					};
					members.push(new_member);
				}
				Ok(Self::ArrayDestructuring(members, position))
			}
			Expression::ObjectLiteral(inner) => {
				let mut members = Vec::with_capacity(inner.members.len());
				for member in inner.members {
					let new_member: ObjectDestructuringField = match member {
						ObjectLiteralMember::Spread(expression, _) => {
							if let Expression::VariableReference(reference, pos) = expression {
								ObjectDestructuringField::Spread(
									crate::VariableIdentifier::Standard(reference, pos),
									pos,
								)
							} else {
								return Err(ParseError::new(
									crate::ParseErrors::InvalidLHSAssignment,
									expression.get_position(),
								));
							}
						}
						ObjectLiteralMember::Shorthand(name, pos) => {
							ObjectDestructuringField::Name(
								crate::VariableIdentifier::Standard(name, pos),
								None,
								pos,
							)
						}
						ObjectLiteralMember::Property { assignment, key, position, value } => {
							if assignment {
								if let PropertyKey::Ident(name, pos, _) = key.get_ast() {
									ObjectDestructuringField::Name(
										crate::VariableIdentifier::Standard(name, pos),
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
										let lhs: crate::VariableField = lhs_to_variable_field(lhs)?;
										(lhs, Some(rhs))
									} else {
										let lhs: LHSOfAssignment = value.try_into()?;
										let lhs: crate::VariableField = lhs_to_variable_field(lhs)?;
										(lhs, None)
									};

								ObjectDestructuringField::Map {
									from: key.get_ast(),
									name: WithComment::None(name),
									default_value,
									position,
								}
							}
						}
						ObjectLiteralMember::Method(_) => {
							return Err(ParseError::new(
								crate::ParseErrors::InvalidLHSAssignment,
								inner.position,
							))
						}
					};
					members.push(WithComment::None(new_member));
				}
				Ok(Self::ObjectDestructuring(members, inner.position))
			}
			expression => VariableOrPropertyAccess::try_from(expression)
				.map(LHSOfAssignment::VariableOrPropertyAccess),
		}
	}
}

fn lhs_to_variable_field(lhs: LHSOfAssignment) -> Result<crate::VariableField, ParseError> {
	match lhs {
		LHSOfAssignment::VariableOrPropertyAccess(VariableOrPropertyAccess::Variable(
			name,
			pos,
		)) => Ok(crate::VariableField::Name(crate::VariableIdentifier::Standard(name, pos))),
		LHSOfAssignment::ArrayDestructuring(fields, pos) => {
			Ok(crate::VariableField::Array(fields, pos))
		}
		LHSOfAssignment::ObjectDestructuring(fields, pos) => {
			Ok(crate::VariableField::Object(fields, pos))
		}
		LHSOfAssignment::VariableOrPropertyAccess(a) => {
			return Err(ParseError::new(
				crate::ParseErrors::InvalidLHSAssignment,
				a.get_position(),
			));
		}
	}
}
