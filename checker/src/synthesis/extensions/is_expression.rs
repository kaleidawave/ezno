use crate::{
	context::{Environment, Scope},
	synthesis::{
		expressions::synthesise_multiple_expression, functions::SynthesisableFunctionBody,
		type_annotations::synthesise_type_annotation,
	},
	CheckingData, TypeId,
};

pub(crate) fn synthesise_is_expression<T: crate::ReadFromFS>(
	is_expression: &parser::is_expression::IsExpression,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, crate::synthesis::EznoParser>,
) -> TypeId {
	let matcher = synthesise_multiple_expression(
		&is_expression.matcher,
		environment,
		checking_data,
		TypeId::ANY_TYPE,
	);

	let mut returned = None;

	// TODO expecting
	for (condition, code) in &is_expression.branches {
		// TODO need to test subtyping and subtype here
		// TODO move proofs here

		environment.new_lexical_environment_fold_into_parent(
			Scope::Block {},
			checking_data,
			|environment, checking_data| {
				let requirement = synthesise_type_annotation(condition, environment, checking_data);
				// todo!("disjoint");
				// TODO extract named members as variables

				let narrowed = checking_data.types.new_narrowed(matcher, requirement);
				environment.info.narrowed_values.insert(matcher, narrowed);

				code.synthesise_function_body(environment, checking_data);

				// TODO this should be done outside
				let result = environment.info.state.clone().get_returned(&mut checking_data.types);

				returned = if let Some(existing) = returned {
					// TODO new conditional
					Some(checking_data.types.new_or_type(existing, result))
				} else {
					Some(result)
				};
			},
		);
	}

	// TODO check every case covered

	returned.unwrap_or(TypeId::UNDEFINED_TYPE)
}
