use source_map::{Span, SpanWithSource};

use crate::{
	context::facts::PublicityKind, types::properties::PropertyKey, CheckingData, Environment,
	TypeId,
};

use super::operations::{Logical, MathematicalAndBitwise};

pub enum Assignable {
	Reference(Reference),
	ObjectDestructuring(Vec<(PropertyKey<'static>, Assignable)>),
	ArrayDestructuring(Vec<Option<Assignable>>),
}

// TODO derive copy, when span derives copy
// TODO reference
#[derive(Clone)]
pub enum Reference {
	Variable(String, SpanWithSource),
	Property {
		on: TypeId,
		with: PropertyKey<'static>,
		publicity: PublicityKind,
		span: SpanWithSource,
	},
}

/// Increment and decrement are are not binary add subtract as they cast their lhs to number
pub enum AssignmentKind {
	Assign,
	PureUpdate(MathematicalAndBitwise),
	ConditionalUpdate(Logical),
	IncrementOrDecrement(IncrementOrDecrement, AssignmentReturnStatus),
}

pub enum IncrementOrDecrement {
	Increment,
	Decrement,
}

pub enum AssignmentReturnStatus {
	Previous,
	New,
}

impl Reference {
	pub fn get_position(&self) -> SpanWithSource {
		match self {
			Reference::Variable(_, span) | Reference::Property { span, .. } => span.clone(),
		}
	}
}
