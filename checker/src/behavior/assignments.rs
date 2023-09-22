use source_map::{Span, SpanWithSource};

use crate::{CheckingData, Environment, TypeId};

use super::operations::{Logical, MathematicalAndBitwise};

pub enum Assignable {
	Reference(Reference),
	ObjectDestructuring(Vec<(TypeId, Reference)>),
	ArrayDestructuring(Vec<Reference>),
}

// TODO copy, when span copy
#[derive(Clone)]
pub enum Reference {
	Variable(String, SpanWithSource),
	Property { on: TypeId, with: TypeId, span: SpanWithSource },
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

// TODO
pub trait SynthesizableExpression {
	fn synthesize_expression<U: crate::FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<U>,
	) -> TypeId;

	fn get_position(&self) -> &Span;
}
