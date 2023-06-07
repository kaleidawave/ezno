use crate::types::TypeId;
use source_map::Span;
use std::fmt::Debug;

/// A variable, that can be referenced. Can be a including class (prototypes) and functions
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct Variable {
	/// Whether can be reassigned and what to
	pub mutability: VariableMutability,
	/// Location where variable is defined **ALSO UNIQUELY IDENTIFIES THE VARIABLE**
	pub declared_at: Span,
}

impl Variable {
	pub(crate) fn get_id(&self) -> crate::context::VariableId {
		crate::context::VariableId(self.declared_at.clone())
	}
}

/// Shallow mutability
#[derive(Copy, Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum VariableMutability {
	Constant,
	Mutable { reassignment_constraint: TypeId },
}

#[derive(Clone, Debug)]
pub struct VariableWithValue(pub Variable, pub TypeId);
