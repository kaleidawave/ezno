use crate::{
	context::{information::merge_info, Context, Syntax},
	types::is_type_truthy_falsy,
	CheckingData, Decidable, Environment, Scope, TypeId,
};
use source_map::Span;

pub fn new_conditional_context<T, A, R>(
	environment: &mut Environment,
	(condition, pos): (TypeId, Span),
	then_evaluate: impl FnOnce(&mut Environment, &mut CheckingData<T, A>) -> R,
	else_evaluate: Option<impl FnOnce(&mut Environment, &mut CheckingData<T, A>) -> R>,
	checking_data: &mut CheckingData<T, A>,
) -> R
where
	A: crate::ASTImplementation,
	R: crate::types::TypeCombinable,
	T: crate::ReadFromFS,
{
	if let Decidable::Known(result) = is_type_truthy_falsy(condition, &checking_data.types) {
		// TODO could be better
		checking_data.diagnostics_container.add_warning(
			crate::diagnostics::TypeCheckWarning::DeadBranch {
				expression_span: pos.with_source(environment.get_source()),
				expression_value: result,
			},
		);

		return if result {
			then_evaluate(environment, checking_data)
		} else if let Some(else_evaluate) = else_evaluate {
			else_evaluate(environment, checking_data)
		} else {
			R::default()
		};
	}

	let (truthy_result, truthy_info) = {
		let mut truthy_environment = environment
			.new_lexical_environment(Scope::Conditional { antecedent: condition, is_switch: None });

		let result = then_evaluate(&mut truthy_environment, checking_data);

		let Context {
			context_type: Syntax { free_variables, closed_over_references, .. },
			info,
			..
		} = truthy_environment;

		environment.context_type.free_variables.extend(free_variables);
		environment.context_type.closed_over_references.extend(closed_over_references);

		(result, info)
	};

	let (falsy_result, falsy_info) = if let Some(else_evaluate) = else_evaluate {
		let mut falsy_environment = environment.new_lexical_environment(Scope::Conditional {
			antecedent: checking_data.types.new_logical_negation_type(condition),
			is_switch: None,
		});

		let result = else_evaluate(&mut falsy_environment, checking_data);

		let Context {
			context_type: Syntax { free_variables, closed_over_references, .. },
			info,
			..
		} = falsy_environment;

		environment.context_type.free_variables.extend(free_variables);
		environment.context_type.closed_over_references.extend(closed_over_references);

		(result, Some(info))
	} else {
		(R::default(), None)
	};

	let combined_result =
		R::combine(condition, truthy_result, falsy_result, &mut checking_data.types);

	match environment.context_type.parent {
		crate::GeneralContext::Syntax(syn) => {
			merge_info(
				syn,
				&mut environment.info,
				condition,
				truthy_info,
				falsy_info,
				&mut checking_data.types,
			);
		}
		crate::GeneralContext::Root(root) => {
			merge_info(
				root,
				&mut environment.info,
				condition,
				truthy_info,
				falsy_info,
				&mut checking_data.types,
			);
		}
	}

	combined_result
}
