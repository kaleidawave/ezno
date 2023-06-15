use source_map::Span;

use crate::{structures::operators, CheckingData, Environment, TypeId};

pub enum ResultOfAssignment {
	NewValue(TypeId),
	NewValueAndAReturnValue(TypeId, TypeId),
}

pub enum Assignable {
	Reference(Reference),
	ObjectDestructuring(Vec<(TypeId, Reference)>),
	ArrayDestructuring(Vec<Reference>),
}

pub enum Reference {
	Variable(String),
	Property { on: TypeId, with: TypeId },
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

pub(super) struct BinaryAssignment<'a, T> {
	// None == straight assignment... TODO better way
	pub operator: Option<operators::BinaryOperator>,
	pub rhs: &'a mut T,
}

pub(crate) struct PostfixUnaryAssignment {
	pub operator: operators::UnaryOperator,
}

pub(crate) struct PrefixUnaryAssignment {
	pub operator: operators::UnaryOperator,
}
