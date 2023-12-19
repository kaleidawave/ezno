use crate::{
	context::Environment,
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
	// TODO expecting
	let _matcher = synthesise_multiple_expression(
		&is_expression.matcher,
		environment,
		checking_data,
		TypeId::ANY_TYPE,
	);

	let returned = TypeId::UNDEFINED_TYPE;
	for (condition, code) in &is_expression.branches {
		let _requirement = synthesise_type_annotation(condition, environment, checking_data);

		// TODO need to test subtyping and subtype here
		// TODO move proofs here

		code.synthesise_function_body(environment, checking_data);

		// let code_returns = code.synthesise_function_body(environment, checking_data);

		// let on = todo!("Need to turn Type into binary operation?");

		// let ty = Type::Constructor(crate::types::Constructor::ConditionalTernary {
		// 	on,
		// 	true_res: code_returns,
		// 	false_res: returned,
		// 	result_union: todo!(),
		// });

		// returned = checking_data.types.register_type(ty);
	}

	returned
}
