use source_map::Span;

use crate::{structures::operators, CheckingData, Environment, TypeId};

pub enum Assignable {
	Reference(Reference),
	ObjectDestructuring(Vec<(TypeId, Reference)>),
	ArrayDestructuring(Vec<Reference>),
}

// TODO copy, when span copy
#[derive(Clone)]
pub enum Reference {
	Variable(String, Span),
	Property { on: TypeId, with: TypeId, span: Span },
}

/// Increment and decrement are are not binary add subtract as they cast their lhs to number
pub enum AssignmentKind {
	Assign,
	Update(operators::BinaryOperator),
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
	pub fn get_position(&self) -> Span {
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

	fn get_position(&self) -> Span;
}
