use source_map::SpanWithSource;

use crate::{
	types::properties::{PropertyKey, Publicity},
	TypeId,
};

use super::operations::{LogicalOperator, MathematicalOrBitwiseOperation};

/// A single or multiple items to assign to
pub enum Assignable<A: crate::ASTImplementation> {
	Reference(Reference),
	ObjectDestructuring(Vec<AssignableObjectDestructuringField<A>>, Option<AssignableSpread<A>>),
	ArrayDestructuring(Vec<AssignableArrayDestructuringField<A>>, Option<AssignableSpread<A>>),
}

/// TODO Can this use lifetimes?
#[derive(Clone)]
pub enum Reference {
	Variable(String, SpanWithSource),
	Property {
		on: TypeId,
		with: PropertyKey<'static>,
		publicity: Publicity,
		position: SpanWithSource,
	},
}

pub enum AssignableObjectDestructuringField<A: crate::ASTImplementation> {
	/// `{ x: y }`
	Mapped {
		key: PropertyKey<'static>,
		name: Assignable<A>,
		default_value: Option<Box<A::Expression<'static>>>,
		position: SpanWithSource,
	},
}

pub struct AssignableSpread<A: crate::ASTImplementation>(
	pub Box<Assignable<A>>,
	pub SpanWithSource,
);

pub enum AssignableArrayDestructuringField<A: crate::ASTImplementation> {
	Name(Assignable<A>, Option<Box<A::Expression<'static>>>),
	Comment { content: String, is_multiline: bool, position: SpanWithSource },
	None,
}

/// Increment and decrement are are not binary add subtract as they cast their lhs to number
pub enum AssignmentKind {
	Assign,
	PureUpdate(MathematicalOrBitwiseOperation),
	ConditionalUpdate(LogicalOperator),
	IncrementOrDecrement(IncrementOrDecrement, AssignmentReturnStatus),
}

pub enum IncrementOrDecrement {
	Increment,
	Decrement,
}

/// Used for example for `++x` returns the new value, whereas `x++` returns the previous value (yay for *incredible useful and clear* semantics)
pub enum AssignmentReturnStatus {
	Previous,
	New,
}

impl Reference {
	#[must_use]
	pub fn get_position(&self) -> SpanWithSource {
		match self {
			Reference::Variable(_, position) | Reference::Property { position, .. } => *position,
		}
	}

	/// `is_empty` => for when edit in progress in playground / LSP
	#[must_use]
	pub fn is_empty(&self) -> bool {
		match self {
			Reference::Variable(name, _) => name.is_empty(),
			Reference::Property { with, .. } => {
				matches!(with, PropertyKey::String(n) if n.is_empty())
			}
		}
	}

	/// for LSP
	#[must_use]
	pub fn new_empty_variable_reference(position: SpanWithSource) -> Self {
		Self::Variable(String::new(), position)
	}
}
