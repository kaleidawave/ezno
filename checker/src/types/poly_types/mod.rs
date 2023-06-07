//! Note that this not the conventional "dependent type theory" **in dependent means dependent on a term**.
//!
//! Here dependent means the type depends on another type or condition (sometimes called type constructors)

pub mod generics;
pub mod specialization;

pub use generics::*;

use std::fmt::Debug;

use super::TypeId;

#[derive(Debug, Clone)]
pub enum ParameterDependentType {
	SourceGeneric,
	SourceTyped,
	Untyped,
}

#[derive(Clone, Debug)]
pub struct ClosureGenericType {
	pub variable_id: crate::context::VariableId,
	// These will need to also be stored in type mapping
	// TODO mutable thing
	pub constraint: TypeId,
}
