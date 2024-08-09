//! More code at [`crate::context`] and [`Environment`]

use source_map::{Span, SpanWithSource};

use crate::context::{environment::ContextLocation, AssignmentError, VariableRegisterArguments};
use crate::diagnostics::{PropertyKeyRepresentation, TypeCheckError, TypeStringRepresentation};
use crate::subtyping::{type_is_subtype_object, SubTypeResult};
use crate::{
	types::{
		logical::{Logical, LogicalOrValid},
		properties::{
			get_property_key_names_on_a_single_type, get_property_unbound, PropertyKey, Publicity,
		},
		TypeId,
	},
	CheckingData, VariableId,
};
use crate::{Environment, Instance};
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
		/// be turned into a [`VariableId`]
		declared_at: SpanWithSource,
		context: ContextLocation,
		allow_reregistration: bool,
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

	/// Whether can be reassigned
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

/// Returns whether valid
pub fn check_variable_initialization<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	(variable_declared_type, variable_declared_pos): (TypeId, SpanWithSource),
	(expression_type, expression_declared_pos): (TypeId, SpanWithSource),
	environment: &mut crate::context::Environment,
	checking_data: &mut CheckingData<T, A>,
) -> bool {
	let type_is_subtype = type_is_subtype_object(
		variable_declared_type,
		expression_type,
		environment,
		&mut checking_data.types,
	);

	if let SubTypeResult::IsNotSubType(_matches) = type_is_subtype {
		let error = crate::diagnostics::TypeCheckError::AssignmentError(
			AssignmentError::DoesNotMeetConstraint {
				variable_type: crate::diagnostics::TypeStringRepresentation::from_type_id(
					variable_declared_type,
					environment,
					&checking_data.types,
					checking_data.options.debug_types,
				),
				variable_position: variable_declared_pos,
				value_type: crate::diagnostics::TypeStringRepresentation::from_type_id(
					expression_type,
					environment,
					&checking_data.types,
					checking_data.options.debug_types,
				),
				value_position: expression_declared_pos,
			},
		);

		checking_data.diagnostics_container.add_error(error);
		false
	} else {
		true
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
			(space, None),
			(Publicity::Public, under, None),
			false,
			environment,
			&checking_data.types,
		);
		if let Ok(value) = property_constraint {
			if let LogicalOrValid::Logical(value) = value {
				match value {
					Logical::Pure(crate::PropertyValue::Value(value)) => value,
					Logical::Pure(_) => todo!(),
					Logical::Or { .. } => todo!(),
					Logical::Implies { .. } => todo!(),
					Logical::BasedOnKey { .. } => todo!(),
				}
			} else {
				TypeId::ERROR_TYPE
			}
		} else {
			let keys;
			let possibles = if let PropertyKey::String(s) = under {
				keys = get_property_key_names_on_a_single_type(
					space,
					&checking_data.types,
					environment,
				);
				let mut possibles =
					crate::get_closest(keys.iter().map(AsRef::as_ref), s).unwrap_or(vec![]);
				possibles.sort_unstable();
				possibles
			} else {
				Vec::new()
			};
			checking_data.diagnostics_container.add_error(TypeCheckError::PropertyDoesNotExist {
				property: PropertyKeyRepresentation::new(under, environment, &checking_data.types),
				on: TypeStringRepresentation::from_type_id(
					space,
					environment,
					&checking_data.types,
					false,
				),
				position,
				possibles,
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
				crate::types::properties::AccessMode::Regular,
			)
			.map_or(TypeId::ERROR_TYPE, Instance::get_value)
	});

	VariableRegisterArguments {
		constant: on.constant,
		space,
		initial_value,
		allow_reregistration: on.allow_reregistration,
	}
}
