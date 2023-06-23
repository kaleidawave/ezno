use source_map::Span;

use crate::context::AssignmentError;
use crate::{CheckingData, TypeId};

pub fn check_variable_initialization<T: crate::FSResolver>(
	(variable_declared_type, variable_declared_pos): (TypeId, std::borrow::Cow<Span>),
	(expression_type, expression_declared_pos): (TypeId, std::borrow::Cow<Span>),
	environment: &mut crate::context::Environment,
	checking_data: &mut CheckingData<T>,
) {
	use crate::types::subtyping::{type_is_subtype, BasicEquality, SubTypeResult};

	let position = variable_declared_pos.into_owned();
	let mut basic_subtyping = BasicEquality { add_property_restrictions: true, position };

	let type_is_subtype = type_is_subtype(
		variable_declared_type,
		expression_type,
		None,
		&mut basic_subtyping,
		environment,
		&checking_data.types,
	);

	if let SubTypeResult::IsNotSubType(matches) = type_is_subtype {
		let error = crate::diagnostics::TypeCheckError::AssignmentError(
			AssignmentError::InvalidDeclaration {
				variable_type: crate::diagnostics::TypeStringRepresentation::from_type_id(
					variable_declared_type,
					&environment.into_general_environment(),
					&checking_data.types,
					checking_data.settings.debug_types,
				),
				variable_site: basic_subtyping.position,
				value_type: crate::diagnostics::TypeStringRepresentation::from_type_id(
					expression_type,
					&environment.into_general_environment(),
					&checking_data.types,
					checking_data.settings.debug_types,
				),
				value_site: expression_declared_pos.into_owned(),
			},
		);

		checking_data.diagnostics_container.add_error(error);
	}
}
