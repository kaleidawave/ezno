use source_map::{Span, SpanWithSource};

use crate::context::{environment::ContextSpecifierTemp, AssignmentError};
use crate::{types::TypeId, CheckingData, VariableId};
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
		context: ContextSpecifierTemp,
	},
	MutableImport {
		of: VariableId,
		constant: bool,
		import_specified_at: SpanWithSource,
	},
	ConstantImport {
		to: Option<VariableId>,
		import_specified_at: SpanWithSource,
	},
}

impl VariableOrImport {
	pub(crate) fn get_id(&self) -> VariableId {
		match self {
			VariableOrImport::Variable { mutability: _, declared_at, .. } => {
				VariableId(declared_at.source, declared_at.start)
			}
			VariableOrImport::MutableImport { import_specified_at, .. }
			| VariableOrImport::ConstantImport { import_specified_at, .. } => {
				VariableId(import_specified_at.source, import_specified_at.start)
			}
		}
	}

	pub(crate) fn get_mutability(&self) -> VariableMutability {
		match self {
			VariableOrImport::Variable { mutability, declared_at, .. } => *mutability,
			VariableOrImport::MutableImport { of, constant, import_specified_at } => {
				VariableMutability::Constant
			}
			VariableOrImport::ConstantImport { to, import_specified_at } => {
				VariableMutability::Constant
			}
		}
	}

	pub(crate) fn get_origin_variable_id(&self) -> VariableId {
		match self {
			VariableOrImport::Variable { declared_at: pos, .. }
			| VariableOrImport::ConstantImport { import_specified_at: pos, .. } => {
				VariableId(pos.source, pos.start)
			}
			VariableOrImport::MutableImport { of, .. } => *of,
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

pub fn check_variable_initialization<T: crate::ReadFromFS, M: crate::ASTImplementation>(
	(variable_declared_type, variable_declared_pos): (TypeId, SpanWithSource),
	(expression_type, expression_declared_pos): (TypeId, SpanWithSource),
	environment: &mut crate::context::Environment,
	checking_data: &mut CheckingData<T, M>,
) {
	use crate::types::subtyping::{type_is_subtype, BasicEquality, SubTypeResult};

	let position = variable_declared_pos.clone();
	let mut basic_subtyping = BasicEquality { add_property_restrictions: true, position };

	let type_is_subtype = type_is_subtype(
		variable_declared_type,
		expression_type,
		&mut basic_subtyping,
		environment,
		&checking_data.types,
	);

	if let SubTypeResult::IsNotSubType(matches) = type_is_subtype {
		let error = crate::diagnostics::TypeCheckError::AssignmentError(
			AssignmentError::DoesNotMeetConstraint {
				variable_type: crate::diagnostics::TypeStringRepresentation::from_type_id(
					variable_declared_type,
					&environment.as_general_context(),
					&checking_data.types,
					checking_data.options.debug_types,
				),
				variable_site: basic_subtyping.position,
				value_type: crate::diagnostics::TypeStringRepresentation::from_type_id(
					expression_type,
					&environment.as_general_context(),
					&checking_data.types,
					checking_data.options.debug_types,
				),
				value_site: expression_declared_pos.clone(),
			},
		);

		checking_data.diagnostics_container.add_error(error);
	}
}
