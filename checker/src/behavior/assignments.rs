use crate::{structures::operators, CheckingData, Environment, TypeId};

pub enum ResultOfAssignment {
	NewValue(TypeId),
	NewValueAndAReturnValue(TypeId, TypeId),
}

pub trait AssignmentBehavior<T> {
	/// Returns two values:
	/// 1) the new value of the variable and returns
	/// 2) second value returned from the expression. None = first type
	/// TODO use [ResultOfAssignment]
	fn get_new_value<U: crate::FSResolver>(
		self,
		assignee_type: TypeId,
		environment: &mut Environment,
		checking_data: &mut CheckingData<U>,
		other: impl SynthesizeExpression<T>,
	) -> ResultOfAssignment;

	/// Bool whether to evaluate LHS if updating expression. e.g. `x++` or `x -= 2` are true
	fn based_off_existing(&self) -> bool;
}

// TODO
pub trait SynthesizeExpression<T> {
	fn synthesize_expression<U: crate::FSResolver>(
		item: T,
		environment: &mut Environment,
		checking_data: &mut CheckingData<U>,
	) -> TypeId;
}

pub(super) struct BinaryAssignment<'a, T> {
	// None == straight assignment... TODO better way
	pub operator: Option<operators::BinaryOperator>,
	pub rhs: &'a mut T,
}

impl<'a, U> AssignmentBehavior<U> for BinaryAssignment<'a, U> {
	fn get_new_value<T: crate::FSResolver>(
		self,
		assignee_type: TypeId,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
		other: impl SynthesizeExpression<U>,
	) -> ResultOfAssignment {
		todo!()
		// let value = synthesize_expression(self.rhs, environment, checking_data, chain);
		// let new_value = if let Some(operator) = self.operator {
		// 	let operator: operators::BinaryOperator = operator.into();
		// 	let operator = super::parser_binary_operator_to_others(operator);
		// 	let evaluate_binary_operator = evaluate_binary_operator(
		// 		operator,
		// 		assignee_type,
		// 		value,
		// 		environment,
		// 		checking_data.settings.strict_casts,
		// 		&mut checking_data.types,
		// 	);

		// 	if let Ok(value) = evaluate_binary_operator {
		// 		value
		// 	} else {
		// 		todo!("Add error to checking_data")
		// 	}
		// } else {
		// 	value
		// };
		// (new_value, None)
	}

	fn based_off_existing(&self) -> bool {
		self.operator.is_some()
	}
}

pub(crate) struct PostfixUnaryAssignment {
	pub operator: operators::UnaryOperator,
}

pub(crate) struct PrefixUnaryAssignment {
	pub operator: operators::UnaryOperator,
}

impl<U> AssignmentBehavior<U> for PrefixUnaryAssignment {
	fn get_new_value<T: crate::FSResolver>(
		self,
		assignee_type: TypeId,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
		other: impl SynthesizeExpression<U>,
	) -> ResultOfAssignment {
		todo!()

		// let new_value = match self.operator {
		// 	UnaryPrefixAssignmentOperator::Invert => evaluate_unary_operator(
		// 		operators::UnaryOperator::LogicalNegation,
		// 		assignee_type,
		// 		environment,
		// 		checking_data.settings.strict_casts,
		// 		&mut checking_data.types,
		// 	)
		// 	.unwrap(),
		// 	UnaryPrefixAssignmentOperator::IncrementOrDecrement(direction) => {
		// 		// TODO want to down level this += or -= 1; but that function takes an expression not a type.
		// 		// TODO error handling
		// 		let rhs =
		// 			checking_data.types.new_constant_type(Constant::Number(1.try_into().unwrap()));
		// 		let operator = match direction {
		// 			parser::operators::IncrementOrDecrement::Increment => {
		// 				operators::BinaryOperator::Add
		// 			}
		// 			parser::operators::IncrementOrDecrement::Decrement => todo!(),
		// 		};
		// 		evaluate_binary_operator(
		// 			operator,
		// 			assignee_type,
		// 			rhs,
		// 			environment,
		// 			checking_data.settings.strict_casts,
		// 			&mut checking_data.types,
		// 		)
		// 		.unwrap()
		// 	}
		// };

		// (new_value, Some(new_value))
	}

	fn based_off_existing(&self) -> bool {
		true
	}
}

impl<U> AssignmentBehavior<U> for PostfixUnaryAssignment {
	fn get_new_value<T: crate::FSResolver>(
		self,
		assignee_type: TypeId,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
		other: impl SynthesizeExpression<U>,
	) -> ResultOfAssignment {
		todo!()
		// let update = AssignmentBehavior::get_new_value(
		// 	PrefixUnaryAssignment { operator: todo!("self.operator.0,") },
		// 	assignee_type,
		// 	environment,
		// 	checking_data,
		// 	chain,
		// );

		// (update.0, Some(assignee_type))
	}

	fn based_off_existing(&self) -> bool {
		true
	}
}
