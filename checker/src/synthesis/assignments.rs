use parser::{ast::LHSOfAssignment, expressions::assignments::VariableOrPropertyAccess, ASTNode};

use crate::{
	behavior::assignments::{Assignable, Reference},
	context::Environment,
	synthesis::expressions::synthesize_expression,
	types::Constant,
	CheckingData,
};

use super::expressions::synthesize_multiple_expression;

pub(super) fn synthesize_lhs_of_assignment_to_reference<T: crate::FSResolver>(
	lhs: &LHSOfAssignment,
	environment: &mut Environment,
	checking_data: &mut CheckingData<'_, T>,
) -> Assignable {
	match lhs {
		LHSOfAssignment::ObjectDestructuring(_, _) => todo!(),
		LHSOfAssignment::ArrayDestructuring(_, _) => todo!(),
		LHSOfAssignment::VariableOrPropertyAccess(access) => Assignable::Reference(
			synthesize_access_to_reference(access, environment, checking_data),
		),
	}
}

pub(crate) fn synthesize_access_to_reference<T: crate::FSResolver>(
	variable_or_property_access: &VariableOrPropertyAccess,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) -> Reference {
	match variable_or_property_access {
		VariableOrPropertyAccess::Variable(ident, _) => Reference::Variable(
			ident.clone(),
			variable_or_property_access.get_position().into_owned(),
		),
		VariableOrPropertyAccess::PropertyAccess { parent, property, position } => {
			let parent_ty = synthesize_expression(&parent, environment, checking_data);
			let key_ty = match property {
				parser::PropertyReference::Standard(prop) => {
					checking_data.types.new_constant_type(Constant::String(prop.clone()))
				}
				parser::PropertyReference::Cursor(_) => todo!(),
			};
			Reference::Property { on: parent_ty, with: key_ty, span: position.clone() }
		}
		VariableOrPropertyAccess::Index { indexee, indexer, position } => {
			let parent_ty = synthesize_expression(&indexee, environment, checking_data);
			let key_ty = synthesize_multiple_expression(&indexer, environment, checking_data);
			Reference::Property { on: parent_ty, with: key_ty, span: position.clone() }
		}
	}
}
