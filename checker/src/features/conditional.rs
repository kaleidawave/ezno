use crate::{
	context::{
		environment::ReturnState, information::merge_info, Context, LocalInformation, Syntax,
	},
	types::{is_type_truthy_falsy, TypeStore},
	CheckingData, Decidable, Environment, Scope, TypeId,
};
use source_map::Span;

pub trait ConditionalResult {
	fn non_result_result() -> Self;

	fn new_condition(condition: TypeId, left: Self, right: Self, types: &mut TypeStore) -> Self;
}

impl ConditionalResult for () {
	fn non_result_result() -> Self {}

	fn new_condition(
		_condition: TypeId,
		_left: Self,
		_right: Self,
		_types: &mut TypeStore,
	) -> Self {
	}
}

impl ConditionalResult for TypeId {
	fn non_result_result() -> Self {
		TypeId::UNDEFINED_TYPE
	}

	fn new_condition(condition: TypeId, left: Self, right: Self, types: &mut TypeStore) -> Self {
		types.new_conditional_type(condition, left, right)
	}
}

/// For top level checking
pub fn new_conditional_context<T, A, R>(
	environment: &mut Environment,
	(condition, position): (TypeId, Span),
	then_evaluate: impl FnOnce(&mut Environment, &mut CheckingData<T, A>) -> R,
	else_evaluate: Option<impl FnOnce(&mut Environment, &mut CheckingData<T, A>) -> R>,
	checking_data: &mut CheckingData<T, A>,
) -> R
where
	A: crate::ASTImplementation,
	R: ConditionalResult,
	T: crate::ReadFromFS,
{
	if let Decidable::Known(result) = is_type_truthy_falsy(condition, &checking_data.types) {
		// TODO could be better
		checking_data.diagnostics_container.add_warning(
			crate::diagnostics::TypeCheckWarning::DeadBranch {
				expression_span: position.with_source(environment.get_source()),
				expression_value: result,
			},
		);

		return if result {
			then_evaluate(environment, checking_data)
		} else if let Some(else_evaluate) = else_evaluate {
			else_evaluate(environment, checking_data)
		} else {
			R::non_result_result()
		};
	}

	let (truthy_return_state, truthy_result, mut truthy_info) = {
		let mut truthy_environment = environment
			.new_lexical_environment(Scope::Conditional { antecedent: condition, is_switch: None });

		let values = super::narrowing::narrow_based_on_expression_into_vec(
			condition,
			false,
			environment,
			&mut checking_data.types,
		);

		truthy_environment.info.narrowed_values = values;

		let result = then_evaluate(&mut truthy_environment, checking_data);

		let Context {
			context_type: Syntax { free_variables, closed_over_references, state, .. },
			info,
			possibly_mutated_objects,
			possibly_mutated_variables,
			..
		} = truthy_environment;

		environment.context_type.free_variables.extend(free_variables);
		environment.context_type.closed_over_references.extend(closed_over_references);

		environment.possibly_mutated_objects.extend(possibly_mutated_objects);
		environment.possibly_mutated_variables.extend(possibly_mutated_variables);

		(state, result, info)
	};

	let (otherwise_return_state, otherwise_result, mut otherwise_info) =
		if let Some(else_evaluate) = else_evaluate {
			let mut otherwise_environment =
				environment.new_lexical_environment(Scope::Conditional {
					antecedent: checking_data.types.new_logical_negation_type(condition),
					is_switch: None,
				});

			let values = super::narrowing::narrow_based_on_expression_into_vec(
				condition,
				true,
				environment,
				&mut checking_data.types,
			);

			otherwise_environment.info.narrowed_values = values;

			let result = else_evaluate(&mut otherwise_environment, checking_data);

			let Context {
				context_type: Syntax { free_variables, closed_over_references, state, .. },
				info,
				possibly_mutated_objects,
				possibly_mutated_variables,
				..
			} = otherwise_environment;

			environment.context_type.free_variables.extend(free_variables);
			environment.context_type.closed_over_references.extend(closed_over_references);

			environment.possibly_mutated_objects.extend(possibly_mutated_objects);
			environment.possibly_mutated_variables.extend(possibly_mutated_variables);

			(state, result, Some(info))
		} else {
			(ReturnState::None, R::non_result_result(), None)
		};

	let combined_result =
		R::new_condition(condition, truthy_result, otherwise_result, &mut checking_data.types);

	let position = position.with_source(environment.get_source());

	// Add contional events
	{
		use crate::events::Event;

		let truthy: &mut LocalInformation = &mut truthy_info;
		let otherwise: Option<&mut LocalInformation> = otherwise_info.as_mut();
		let onto: &mut LocalInformation = &mut environment.info;

		let truthy_events = truthy.events.len() as u32;
		let otherwise_events = otherwise.as_ref().map_or(0, |f| f.events.len() as u32);

		if truthy_events + otherwise_events != 0 {
			onto.events.push(Event::Conditionally {
				condition,
				truthy_events,
				otherwise_events,
				position,
			});

			onto.events.append(&mut truthy.events);
			if let Some(otherwise) = otherwise {
				onto.events.append(&mut otherwise.events);
			}

			onto.events.push(Event::EndOfControlFlow(truthy_events + otherwise_events));
		}
	}

	match (truthy_return_state.is_finished(), otherwise_return_state.is_finished()) {
		(true, true) => {
			// Don't have to do anything here
		}
		(false, true) => {
			environment.info.extend(truthy_info, None);
		}
		(true, false) => {
			if let Some(otherwise_info) = otherwise_info {
				environment.info.extend(otherwise_info, None);
			} else {
				// Could negate existing, but starting again handles types better
				let values = crate::features::narrowing::narrow_based_on_expression_into_vec(
					condition,
					true,
					environment,
					&mut checking_data.types,
				);

				environment.info.narrowed_values = values;
			}
		}
		(false, false) => match environment.context_type.parent {
			crate::GeneralContext::Syntax(parent) => {
				merge_info(
					parent,
					&mut environment.info,
					condition,
					truthy_info,
					otherwise_info,
					&mut checking_data.types,
				);
			}
			crate::GeneralContext::Root(parent) => {
				merge_info(
					parent,
					&mut environment.info,
					condition,
					truthy_info,
					otherwise_info,
					&mut checking_data.types,
				);
			}
		},
	}

	let new =
		truthy_return_state.merge(otherwise_return_state, condition, &mut checking_data.types);
	environment.context_type.state.merge_unconditionally(new, &mut checking_data.types);

	combined_result
}
