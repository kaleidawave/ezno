use crate::{types::TypeId, VariableId};
use source_map::{Span, SpanWithSource};
use std::fmt::Debug;

/// A variable, that can be referenced. Can be a including class (prototypes) and functions
///
/// TODO constant variables with a fixed value
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum VariableOrImport {
	Variable {
		/// Whether can be reassigned and what to
		mutability: VariableMutability,
		/// Location where variable is defined **ALSO UNIQUELY IDENTIFIES THE VARIABLE** as can
		/// be turned into a [VariableId]
		declared_at: SpanWithSource,
	},
	Import {
		of: VariableId,
		constant: bool,
		import_specified_at: SpanWithSource,
	},
}

impl VariableOrImport {
	pub(crate) fn get_id(&self) -> VariableId {
		match self {
			VariableOrImport::Variable { mutability: _, declared_at } => {
				VariableId(declared_at.source, declared_at.start)
			}
			VariableOrImport::Import { import_specified_at, .. } => {
				VariableId(import_specified_at.source, import_specified_at.start)
			}
		}
	}

	pub(crate) fn get_mutability(&self) -> VariableMutability {
		match self {
			VariableOrImport::Variable { mutability, declared_at } => *mutability,
			VariableOrImport::Import { of, constant, import_specified_at } => {
				VariableMutability::Constant
			}
		}
	}

	pub(crate) fn get_origin_variable_id(&self) -> VariableId {
		match self {
			VariableOrImport::Variable { declared_at, .. } => {
				VariableId(declared_at.source, declared_at.start)
			}
			VariableOrImport::Import { of, .. } => *of,
		}
	}
}

/// Shallow mutability
#[derive(Copy, Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum VariableMutability {
	Constant,
	Mutable { reassignment_constraint: Option<TypeId> },
}

#[derive(Clone, Debug)]
pub struct VariableWithValue(pub VariableOrImport, pub TypeId);
