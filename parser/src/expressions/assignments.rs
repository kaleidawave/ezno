use std::borrow::Cow;

use crate::{PropertyReference, TSXToken};
use derive_partial_eq_extras::PartialEqExtras;
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::TokenReader;
use visitable_derive::Visitable;

use crate::{
	ASTNode, ArrayDestructuringField, Expression, ObjectDestructuringField, ParseError,
	ParseResult, VariableFieldInSourceCode, WithComment,
};

use super::{ExpressionId, MultipleExpression};

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum VariableOrPropertyAccess {
	Variable(String, Span, ExpressionId),
	PropertyAccess {
		parent: Box<Expression>,
		property: PropertyReference,
		position: Span,
		expression_id: ExpressionId,
	},
	/// Using `x[y]`
	Index {
		indexee: Box<Expression>,
		indexer: Box<MultipleExpression>,
		position: Span,
		expression_id: ExpressionId,
	},
}

impl ASTNode for VariableOrPropertyAccess {
	fn get_position(&self) -> Cow<Span> {
		match self {
			VariableOrPropertyAccess::Variable(_, position, _)
			| VariableOrPropertyAccess::PropertyAccess { position, .. }
			| VariableOrPropertyAccess::Index { position, .. } => Cow::Borrowed(position),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseSettings,
	) -> ParseResult<Self> {
		Expression::from_reader(reader, state, settings)?.try_into()
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		match self {
			VariableOrPropertyAccess::Variable(name, ..) => {
				buf.push_str(name);
			}
			VariableOrPropertyAccess::PropertyAccess { parent, property, .. } => {
				parent.to_string_from_buffer(buf, settings, depth);
				buf.push('.');
				if let PropertyReference::Standard(property) = property {
					buf.push_str(property);
				} else if !settings.0.expect_cursors {
					panic!("found cursor");
				}
			}
			VariableOrPropertyAccess::Index { indexee, indexer, .. } => {
				indexee.to_string_from_buffer(buf, settings, depth);
				buf.push('[');
				indexer.to_string_from_buffer(buf, settings, depth);
				buf.push(']');
			}
		}
	}
}

impl TryFrom<Expression> for VariableOrPropertyAccess {
	type Error = ParseError;

	fn try_from(expression: Expression) -> Result<Self, Self::Error> {
		match expression {
			Expression::VariableReference(name, position, expression_id) => {
				Ok(Self::Variable(name, position, expression_id))
			}
			Expression::PropertyAccess {
				parent,
				position,
				expression_id,
				property,
				is_optional,
			} => {
				if is_optional {
					todo!()
				}
				Ok(Self::PropertyAccess { parent, position, expression_id, property })
			}
			Expression::Index { expression_id, indexer, position, indexee } => {
				Ok(Self::Index { indexer, position, indexee, expression_id })
			}
			expression => Err(ParseError::new(
				crate::ParseErrors::InvalidLHSAssignment,
				expression.get_position().into_owned(),
			)),
		}
	}
}

impl From<VariableOrPropertyAccess> for Expression {
	fn from(this: VariableOrPropertyAccess) -> Self {
		match this {
			VariableOrPropertyAccess::Variable(variable, position, expression_id) => {
				Expression::VariableReference(variable, position, expression_id)
			}
			VariableOrPropertyAccess::Index { expression_id, indexee, indexer, position } => {
				Expression::Index { indexee, indexer, position, expression_id }
			}
			VariableOrPropertyAccess::PropertyAccess {
				expression_id,
				parent,
				position,
				property,
			} => Expression::PropertyAccess {
				expression_id,
				parent,
				position,
				property,
				is_optional: false,
			},
		}
	}
}

impl VariableOrPropertyAccess {
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

/// TODO should be different from VariableFieldInSourceCode here
/// TODO visitable is current skipped...
/// TODO cursor
#[derive(PartialEqExtras, Debug, Clone, Visitable, derive_enum_from_into::EnumFrom)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[partial_eq_ignore_types(Span, ExpressionId)]
pub enum LHSOfAssignment {
	ObjectDestructuring(
		#[visit_skip_field] Vec<WithComment<ObjectDestructuringField<VariableFieldInSourceCode>>>,
		Span,
		ExpressionId,
	),
	ArrayDestructuring(
		#[visit_skip_field] Vec<ArrayDestructuringField<VariableFieldInSourceCode>>,
		Span,
		ExpressionId,
	),
	VariableOrPropertyAccess(VariableOrPropertyAccess),
}

impl LHSOfAssignment {
	pub fn get_position(&self) -> Cow<Span> {
		match self {
			LHSOfAssignment::ObjectDestructuring(_, _, _) => todo!(),
			LHSOfAssignment::ArrayDestructuring(_, _, _) => todo!(),
			LHSOfAssignment::VariableOrPropertyAccess(var_prop_access) => {
				var_prop_access.get_position()
			}
		}
	}

	pub(crate) fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		match self {
			LHSOfAssignment::ObjectDestructuring(members, _, _) => {
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
			LHSOfAssignment::ArrayDestructuring(members, _, _) => {
				buf.push('[');
				for (at_end, member) in members.iter().endiate() {
					member.to_string_from_buffer(buf, settings, depth);
					if !at_end {
						buf.push(',');
					}
				}
				buf.push(']');
			}
			LHSOfAssignment::VariableOrPropertyAccess(variable_or_property_access) => {
				variable_or_property_access.to_string_from_buffer(buf, settings, depth)
			}
		}
	}
}
