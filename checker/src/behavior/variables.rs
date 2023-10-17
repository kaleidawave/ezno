use source_map::{Span, SpanWithSource};

use crate::context::AssignmentError;
use crate::{CheckingData, TypeId};

pub fn check_variable_initialization<T: crate::ReadFromFS, M: crate::SynthesisableModule>(
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
