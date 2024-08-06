pub enum Accumulator<T, C> {
	Finished(T),
	Partial { condition: C, value: T },
	None,
}

impl<T, C> Accumulator<T, C> {
	pub fn or_else(self) -> Self {
		match self {
			v @ Accumulator::Finished(..) => v,
			_ => todo!(),
		}
		// match (truthy_result, otherwise_result) {
		// 	(Some(truthy_result), Some(otherwise_result)) => {
		// 		return Some(ApplicationResult::Or {
		// 			on: condition,
		// 			truthy_result: Box::new(truthy_result),
		// 			otherwise_result: Box::new(otherwise_result),
		// 		});
		// 	}
		// 	(None, Some(right)) => {
		// 		let negated_condition = types.new_logical_negation_type(condition);
		// 		trailing = Some(match trailing {
		// 			Some((existing_condition, existing)) => (
		// 				negated_condition,
		// 				ApplicationResult::Or {
		// 					on: existing_condition,
		// 					truthy_result: Box::new(existing),
		// 					otherwise_result: Box::new(right),
		// 				},
		// 			),
		// 			None => (negated_condition, right),
		// 		});
		// 	}
		// 	(Some(left), None) => {
		// 		trailing = Some(match trailing {
		// 			Some((existing_condition, existing)) => (
		// 				condition,
		// 				ApplicationResult::Or {
		// 					on: existing_condition,
		// 					truthy_result: Box::new(existing),
		// 					otherwise_result: Box::new(left),
		// 				},
		// 			),
		// 			None => (condition, left),
		// 		});
		// 	}
		// 	(None, None) => {}
		// }
	}
}
