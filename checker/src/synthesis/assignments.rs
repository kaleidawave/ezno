use parser::{expressions::assignments::VariableOrPropertyAccess, Chain, Expression, Span};
use temporary_annex::Annex;

use crate::{
	behavior::assignments::AssignmentBehavior, context::Environment, errors::TypeCheckError,
	structures::variables::VariableWithValue, types::Constant, CheckingData, TypeId,
};

use super::{expressions::synthesize_multiple_expression, synthesize_expression};

/// TODO conditional assignments as well:
/// where RHS only runs if x is falsy, maybe encode via behavior
/// ```
/// x ||= console.log(4),2;
/// ```
pub(super) fn synthesize_assignment<
	T: crate::FSResolver,
	U: AssignmentBehavior<parser::Expression>,
>(
	variable_or_property_access: &mut VariableOrPropertyAccess,
	behavior: U,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
	assignment_position: Span,
) -> TypeId {
	match variable_or_property_access {
		VariableOrPropertyAccess::Variable(name, position, _) => {
			// TODO get_variable ...? A lot of this is handled by reassign variable
			let (id, reassignment_constraint, old_value) =
				if let Ok(VariableWithValue(variable, value)) =
					environment.get_variable_or_alternatives(&name, &mut checking_data.types)
				{
					match variable.mutability {
						crate::structures::variables::VariableMutability::Constant => {
							let error = TypeCheckError::CannotAssignToConstant {
								variable_name: name,
								variable_position: variable.declared_at.clone(),
								assignment_position,
							};
							checking_data.diagnostics_container.add_error(error);
							return TypeId::ERROR_TYPE;
						}
						crate::structures::variables::VariableMutability::Mutable {
							reassignment_constraint,
						} => (variable.id, reassignment_constraint.clone(), value),
					}
				} else {
					crate::utils::notify!("Variable not found error, could declare to `window`?");
					return TypeId::ERROR_TYPE;
				};

			todo!();

			// let result = U::get_new_value(behavior, old_value, environment, checking_data);

			// let return_type = returned_value.unwrap_or(new_value);

			// let result = environment.assign_variable(name, new_value, &checking_data.types);

			// if let Err(reassignment_error) = result {
			// 	// TODO abstract
			// 	match reassignment_error {
			// 		ReassignmentError::Constant => {
			// 			checking_data.diagnostics_container.add_error(
			// 				TypeCheckError::CannotAssignToConstant {
			// 					variable_name: todo!(),
			// 					variable_position: todo!(),
			// 					assignment_position: todo!(),
			// 				},
			// 			);
			// 		}
			// 		ReassignmentError::DoesNotMatchRestrictionType => {
			// 			checking_data.diagnostics_container.add_error(
			// 				TypeCheckError::InvalidAssignmentOrDeclaration {
			// 					variable_type: todo!(),
			// 					value_type: todo!(),
			// 					variable_site: todo!(),
			// 					value_site: todo!(),
			// 				},
			// 			);
			// 		}
			// 		ReassignmentError::VariableNotFound { variable } => {
			// 			checking_data.diagnostics_container.add_error(
			// 				TypeCheckError::CouldNotFindVariable {
			// 					variable,
			// 					possibles: Vec::new(),
			// 					position: position.clone(),
			// 				},
			// 			);
			// 		}
			// 	}
			// }

			// return_type
		}
		VariableOrPropertyAccess::Index { indexee, indexer, position, expression_id } => {
			let indexee_ty = synthesize_expression(indexee, environment, checking_data, chain);
			let indexer_ty =
				synthesize_multiple_expression(indexer, environment, checking_data, chain);

			crate::utils::notify!(
				"indexee_ty={}; indexer_ty={}",
				environment.debug_type(indexee_ty, &checking_data.types),
				environment.debug_type(indexer_ty, &checking_data.types)
			);

			assign_to_property(
				environment,
				indexee_ty,
				indexer_ty,
				checking_data,
				behavior,
				chain,
				position.clone(),
				Some(assignment_position),
			)
		}
		VariableOrPropertyAccess::PropertyAccess { parent, property, position, expression_id } => {
			let parent_type = synthesize_expression(parent, environment, checking_data, chain);
			let property = if let parser::PropertyReference::Standard(name) = property {
				checking_data.types.new_constant_type(Constant::String(name.clone()))
			} else {
				todo!("cursor")
			};

			assign_to_property(
				environment,
				parent_type,
				property,
				checking_data,
				behavior,
				chain,
				position.clone(),
				Some(assignment_position),
			)
		}
	}
}

/// TODO check readonly
fn assign_to_property<U: AssignmentBehavior<Expression>, T: crate::FSResolver>(
	environment: &mut Environment,
	parent_type: TypeId,
	property: TypeId,
	checking_data: &mut CheckingData<T>,
	behavior: U,
	chain: &mut Annex<Chain>,
	variable_or_property_access_position: Span,
	expression_pos: Option<Span>,
) -> TypeId {
	// TODO check readonly/frozen and things
	// match parent_type {
	// 	ref ty @ TypeId::Term(Term::ObjectReference(object_id)) => {
	// 		// Check restriction
	// 		let current_value = {
	// 			let existing_object =
	// 				checking_data.memory.objects.get(&object_id).unwrap();

	// 			notify!("Check restriction");
	// 			// match &existing_object.shape_restriction {
	// 			// 	crate::memory::ObjectReassignment::Frozen => {
	// 			// 		checking_data
	// 			// 			.error_warning_info_handler
	// 			// 			.add_error(TypeCheckError::CannotAssignToFrozen {
	// 			// 			variable_type:
	// 			// 				crate::errors::TypeStringRepresentation::from_type(
	// 			// 					todo!("ty"),
	// 			// 					&checking_data.memory,
	// 			// 					checking_data.settings.debug_types,
	// 			// 				),
	// 			// 			assignment_position: assignment_position
	// 			// 				.clone()
	// 			// 				.unwrap(),
	// 			// 		});
	// 			// 	}
	// 			// 	crate::memory::ObjectReassignment::FixedByConstraint(
	// 			// 		constraint,
	// 			// 	) => {
	// 			// 		todo!("check constraint");
	// 			// 		// let _todo = constraint.can_assign_to_property(
	// 			// 		//     &key,
	// 			// 		//     &checking_data.memory,
	// 			// 		//     &value,
	// 			// 		// );
	// 			// 	}
	// 			// 	crate::memory::ObjectReassignment::Free => {}
	// 			// };
	// 			// TODO unwrap. and isn't needed always
	// 			existing_object
	// 				.properties
	// 				.get(&key)
	// 				.cloned()
	// 				.map(|a| a.invoke_getter(checking_data).unwrap())
	// 				.unwrap_or(Term::Undefined.into())
	// 		};
	// 		let (new_value, returned_value) = behavior.get_new_value(
	// 			&current_value,
	// 			environment,
	// 			checking_data,
	// 			chain,
	// 		);
	// 		let return_type = returned_value.unwrap_or_else(|| new_value.clone());
	// 		environment.update_property(ty.clone(), key, new_value);
	// 		return_type
	// 	}
	// 	_ => {
	// 		todo!("{:?} probably error", parent_type);
	// 	}
	// }

	let old_value = if behavior.based_off_existing() {
		environment
			.get_property(parent_type, property, checking_data, None)
			.map(TypeId::from)
			.unwrap_or(TypeId::UNDEFINED_TYPE)
	} else {
		TypeId::ERROR_TYPE
	};

	todo!()

	// let result =
	// 	U::get_new_value(behavior, old_value, environment, checking_data, |_, _, _| todo!());

	// match result {}

	// let set_property = environment.set_property(parent_type, property, new_value, checking_data);
	// match set_property {
	// 	Ok(set_return) => set_return.or(returned_value).unwrap_or(new_value),
	// 	Err(error) => {
	// 		let error = match error {
	// 			crate::context::SetPropertyError::NotWriteable => TypeCheckError::NotWritable {
	// 				pos: variable_or_property_access_position.clone(),
	// 			},
	// 			crate::context::SetPropertyError::DoesNotMeetConstraint(_) => {
	// 				TypeCheckError::ValueDoesNotMeetConstraint {
	// 					pos: variable_or_property_access_position.clone(),
	// 					value_pos: expression_pos.unwrap(),
	// 				}
	// 			}
	// 		};

	// 		checking_data.diagnostics_container.add_error(error);

	// 		TypeId::ERROR_TYPE
	// 	}
	// }
}
