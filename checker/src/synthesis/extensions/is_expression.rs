use crate::{
	context::Environment,
	synthesis::{
		expressions::synthesize_multiple_expression, functions::SynthesizableFunctionBody,
		type_annotations::synthesize_type_annotation,
	},
	CheckingData, TypeId,
};

pub(crate) fn synthesize_is_expression<T: crate::FSResolver>(
	is_expression: &parser::is_expression::IsExpression,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) -> TypeId {
	let matcher =
		synthesize_multiple_expression(&is_expression.matcher, environment, checking_data);

	let mut returned = TypeId::UNDEFINED_TYPE;
	for (condition, code) in is_expression.branches.iter() {
		let requirement = synthesize_type_annotation(&condition, environment, checking_data);

		// TODO need to test subtyping and subtype here
		// TODO move proofs here

		let result = code.synthesize_function_body(environment, checking_data);

		// let code_returns = code.synthesize_function_body(environment, checking_data);

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
