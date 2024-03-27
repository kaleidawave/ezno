use source_map::{Span, SpanWithSource};

use crate::context::information::{get_property_unbound, Publicity};
use crate::context::VariableRegisterArguments;
use crate::context::{environment::ContextLocation, AssignmentError};
use crate::diagnostics::{PropertyRepresentation, TypeCheckError, TypeStringRepresentation};
use crate::types::printing::print_type;
use crate::types::properties::PropertyKey;
use crate::{types::TypeId, CheckingData, VariableId};
use crate::{Environment, Instance, Logical};
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
		context: ContextLocation,
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
			VariableOrImport::Variable { mutability, .. } => *mutability,
			// TODO I think ?
			VariableOrImport::MutableImport { of: _, constant: _, import_specified_at: _ }
			| VariableOrImport::ConstantImport { to: _, import_specified_at: _ } => {
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

pub fn check_variable_initialization<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	(variable_declared_type, variable_declared_pos): (TypeId, SpanWithSource),
	(expression_type, expression_declared_pos): (TypeId, SpanWithSource),
	environment: &mut crate::context::Environment,
	checking_data: &mut CheckingData<T, A>,
) {
	use crate::types::subtyping::{type_is_subtype, BasicEquality, SubTypeResult};

	let mut basic_subtyping = BasicEquality {
		add_property_restrictions: true,
		position: variable_declared_pos,
		object_constraints: Default::default(),
		allow_errors: true,
	};

	let type_is_subtype = type_is_subtype(
		variable_declared_type,
		expression_type,
		&mut basic_subtyping,
		environment,
		&checking_data.types,
	);

	environment
		.add_object_constraints(basic_subtyping.object_constraints, &mut checking_data.types);

	if let SubTypeResult::IsNotSubType(_matches) = type_is_subtype {
		let error = crate::diagnostics::TypeCheckError::AssignmentError(
			AssignmentError::DoesNotMeetConstraint {
				variable_type: crate::diagnostics::TypeStringRepresentation::from_type_id(
					variable_declared_type,
					environment,
					&checking_data.types,
					checking_data.options.debug_types,
				),
				variable_site: basic_subtyping.position,
				value_type: crate::diagnostics::TypeStringRepresentation::from_type_id(
					expression_type,
					environment,
					&checking_data.types,
					checking_data.options.debug_types,
				),
				value_site: expression_declared_pos,
			},
		);

		checking_data.diagnostics_container.add_error(error);
	}
}

pub fn get_new_register_argument_under<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	on: &VariableRegisterArguments,
	under: &PropertyKey,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
	at: Span,
) -> VariableRegisterArguments {
	let position = at.with_source(environment.get_source());

	let space = on.space.map(|space| {
		let property_constraint = get_property_unbound(
			space,
			Publicity::Public,
			under,
			&checking_data.types,
			environment,
		);
		if let Ok(value) = property_constraint {
			match value {
				Logical::Pure(crate::PropertyValue::Value(value)) => value,
				Logical::Pure(_) => todo!(),
				Logical::Or { .. } => todo!(),
				Logical::Implies { .. } => todo!(),
			}
		} else {
			checking_data.diagnostics_container.add_error(TypeCheckError::PropertyDoesNotExist {
				property: match under.clone() {
					PropertyKey::String(s) => PropertyRepresentation::StringKey(s.to_string()),
					PropertyKey::Type(t) => PropertyRepresentation::Type(print_type(
						t,
						&checking_data.types,
						environment,
						false,
					)),
				},
				on: TypeStringRepresentation::from_type_id(
					space,
					environment,
					&checking_data.types,
					false,
				),
				site: position,
			});
			TypeId::ERROR_TYPE
		}
	});

	let initial_value = on.initial_value.map(|initial_value| {
		environment
			.get_property_handle_errors(
				initial_value,
				Publicity::Public,
				under,
				checking_data,
				position,
				true,
			)
			.map_or(TypeId::ERROR_TYPE, Instance::get_value)
	});

	VariableRegisterArguments { constant: on.constant, space, initial_value }
}
