use super::{GenericArguments, SliceArguments, TypeId};
use crate::features::objects::Proxy;

/// Wraps logic
#[derive(Debug, Clone)]
pub enum Logical<T> {
	Pure(T),
	/// Note this uses [`PossibleLogical<T>`] rather than [`Logical<T>`].
	Or {
		/// This can be [`TypeId::BOOLEAN_TYPE`] for unknown left-right-ness
		condition: TypeId,
		left: Box<LogicalOrValid<T>>,
		right: Box<LogicalOrValid<T>>,
	},
	/// Passes down [`GenericArguments`] found trying to get to source
	Implies {
		on: Box<Self>,
		antecedent: GenericArguments,
	},
	/// WIP mainly for mapped type properties
	BasedOnKey(LeftRight<T>),
}

#[derive(Debug, Clone)]
pub enum LeftRight<T> {
	Left { value: Box<Logical<T>>, key_arguments: SliceArguments },
	Right { on: TypeId, key: TypeId },
}

/// TODO where does Error type fit in here?
#[derive(Debug, Clone)]
pub enum LogicalOrValid<T> {
	Logical(Logical<T>),
	NeedsCalculation(NeedsCalculation),
}

impl<T> From<Logical<T>> for LogicalOrValid<T> {
	fn from(l: Logical<T>) -> LogicalOrValid<T> {
		LogicalOrValid::Logical(l)
	}
}

impl<T> From<NeedsCalculation> for LogicalOrValid<T> {
	fn from(l: NeedsCalculation) -> LogicalOrValid<T> {
		LogicalOrValid::NeedsCalculation(l)
	}
}

// /// TODO split up
// #[derive(Debug, Clone)]
// pub enum MissingOrToCalculate {
// 	/// Doesn't contain request
// 	Missing,
// 	/// From [`TypeId::ERROR_TYPE`]
// 	Error,
// 	/// From [`TypeId::ANY_TYPE`]
// 	Infer { on: TypeId },
// 	/// Proxies require extra work in some cases
// 	Proxy(Proxy),
// }

#[derive(Debug, Clone)]
pub enum NeedsCalculation {
	/// From [`TypeId::ANY_TYPE`]
	Infer { on: TypeId },
	/// Proxies require extra work in some cases. TypeId points to proxy
	Proxy(Proxy, TypeId),
}

#[derive(Debug, Clone, Copy)]
pub struct Invalid(pub TypeId);

impl Invalid {
	fn _is_null(self) -> bool {
		self.0 == TypeId::NULL_TYPE
	}
}

/// TODO explain
pub type PossibleLogical<T> = Result<LogicalOrValid<T>, Invalid>;
// pub type PossibleLogical2<T> = Result<Logical<T>, ()>;
