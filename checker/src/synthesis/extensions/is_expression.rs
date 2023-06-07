use parser::Chain;
use temporary_annex::Annex;

use crate::{
	context::Environment,
	synthesis::{functions::SynthesizableFunctionBody, synthesize_multiple_expression},
	types::Type,
	CheckingData, TypeId,
};

pub fn synthesize_is_expression<T: crate::FSResolver>(
	is_expression: &mut parser::is_expression::IsExpression,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) -> TypeId {
	let matcher = synthesize_multiple_expression(
		&mut is_expression.matcher,
		environment,
		checking_data,
		chain,
	);

	let mut returned = TypeId::UNDEFINED_TYPE;
	for (condition, code) in is_expression.branches.iter_mut() {
		let requirement = environment.get_type_handle_errors(&condition, checking_data);

		// TODO need to test subtyping and subtype here
		// TODO move properties here

		let code_returns = code.synthesize_function_body(environment, checking_data, chain);

		let on = todo!("Need to turn Type into binary operation?");

		let r#type = Type::Constructor(crate::types::Constructor::ConditionalTernary {
			on,
			t_res: code_returns,
			f_res: returned,
		});

		returned = checking_data.types.new_type(r#type);
	}

	returned
}
