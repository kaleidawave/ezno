use parser::{Expression, VariableIdentifier, WithComment};
use source_map::SpanWithSource;

use crate::{
	context::information::Publicity, synthesis::EznoParser, types::properties::PropertyKey, TypeId,
};

use super::operations::{LogicalOperator, MathematicalAndBitwise};

pub enum Assignable<A: crate::ASTImplementation = EznoParser> {
	Reference(Reference),
	ObjectDestructuring(Vec<AssignableObjectDestructuringField<A>>),
	ArrayDestructuring(Vec<AssignableArrayDestructuringField<A>>),
}

// TODO derive copy, when span derives copy
// TODO reference
#[derive(Clone)]
pub enum Reference {
	Variable(String, SpanWithSource),
	Property { on: TypeId, with: PropertyKey<'static>, publicity: Publicity, span: SpanWithSource },
}

pub enum AssignableObjectDestructuringField<A: crate::ASTImplementation> {
	/// `{ x: y }`
	Mapped {
		on: PropertyKey<'static>,
		name: Assignable<A>,
		default_value: Option<Box<A::Expression<'static>>>,
		position: SpanWithSource,
	},
	/// `{ ...x }`
	Spread(VariableIdentifier, SpanWithSource),
}

pub enum AssignableArrayDestructuringField<A: crate::ASTImplementation> {
	Spread(Assignable<A>, SpanWithSource),
	Name(Assignable<A>, Option<Box<A::Expression<'static>>>),
	Comment { content: String, is_multiline: bool, position: SpanWithSource },
	None,
}

/// Increment and decrement are are not binary add subtract as they cast their lhs to number
pub enum AssignmentKind {
	Assign,
	PureUpdate(MathematicalAndBitwise),
	ConditionalUpdate(LogicalOperator),
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
	#[must_use]
	pub fn get_position(&self) -> SpanWithSource {
		match self {
			Reference::Variable(_, span) | Reference::Property { span, .. } => *span,
		}
	}
}
