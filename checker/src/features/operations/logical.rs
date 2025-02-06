use super::{
	super::{conditional, narrowing},
	relation::is_null_or_undefined,
};
use crate::{Type, TypeId};

#[derive(Copy, Clone, Debug)]
pub enum LogicalOperator {
	And,
	Or,
	/// TODO is this canonical?
	NullCoalescing,
}

/// TODO strict casts!
pub fn evaluate_logical_operation_with_expression<
	'a,
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
>(
	lhs: (TypeId, source_map::Span),
	operator: LogicalOperator,
	rhs: &'a A::Expression<'a>,
	checking_data: &mut crate::CheckingData<T, A>,
	environment: &mut crate::Environment,
	expecting: TypeId,
) -> Result<TypeId, ()> {
	match operator {
		LogicalOperator::And => Ok(conditional::new_conditional_context(
			environment,
			lhs,
			|env: &mut crate::Environment, data: &mut crate::CheckingData<T, A>| {
				A::synthesise_expression(rhs, expecting, env, data)
			},
			Some(|env: &mut crate::Environment, checking_data: &mut crate::CheckingData<T, A>| {
				if let Some(constraint) = crate::types::get_constraint(lhs.0, &checking_data.types)
				{
					let mut result = Vec::new();
					narrowing::build_union_from_filter(
						constraint,
						narrowing::FALSY,
						&mut result,
						env,
						&checking_data.types,
					);
					let narrowed_to = checking_data.types.new_or_type_from_iterator(result);
					checking_data.types.register_type(Type::Narrowed { from: lhs.0, narrowed_to })
				} else {
					lhs.0
				}
			}),
			checking_data,
		)),
		LogicalOperator::Or => Ok(conditional::new_conditional_context(
			environment,
			lhs,
			|env: &mut crate::Environment, checking_data: &mut crate::CheckingData<T, A>| {
				if let Some(constraint) = crate::types::get_constraint(lhs.0, &checking_data.types)
				{
					let mut result = Vec::new();
					narrowing::build_union_from_filter(
						constraint,
						narrowing::NOT_FALSY,
						&mut result,
						env,
						&checking_data.types,
					);
					let narrowed_to = checking_data.types.new_or_type_from_iterator(result);
					checking_data.types.register_type(Type::Narrowed { from: lhs.0, narrowed_to })
				} else {
					lhs.0
				}
			},
			Some(|env: &mut crate::Environment, data: &mut crate::CheckingData<T, A>| {
				A::synthesise_expression(rhs, expecting, env, data)
			}),
			checking_data,
		)),
		LogicalOperator::NullCoalescing => {
			let is_lhs_null_or_undefined =
				is_null_or_undefined(lhs.0, environment, &mut checking_data.types);
			// Equivalent to: `(lhs is null or undefined) ? lhs : rhs`
			Ok(conditional::new_conditional_context(
				environment,
				(is_lhs_null_or_undefined, lhs.1),
				|env: &mut crate::Environment, checking_data: &mut crate::CheckingData<T, A>| {
					if let Some(constraint) =
						crate::types::get_constraint(lhs.0, &checking_data.types)
					{
						let mut result = Vec::new();
						narrowing::build_union_from_filter(
							constraint,
							narrowing::NOT_NULL_OR_UNDEFINED,
							&mut result,
							env,
							&checking_data.types,
						);
						let narrowed_to = checking_data.types.new_or_type_from_iterator(result);
						checking_data
							.types
							.register_type(Type::Narrowed { from: lhs.0, narrowed_to })
					} else {
						lhs.0
					}
				},
				Some(|env: &mut crate::Environment, data: &mut crate::CheckingData<T, A>| {
					A::synthesise_expression(rhs, expecting, env, data)
				}),
				checking_data,
			))
		}
	}
}
