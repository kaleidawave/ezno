use super::facts::Facts;
use crate::{events::EventResult, Environment, FunctionId};

/// For anything that might involve a call, including gets, sets and actual calls
pub(crate) trait CallCheckingBehavior {
	// TODO
	const CHECK_PARAMETERS: bool;

	fn get_latest_facts<'a>(&'a mut self, environment: &'a mut Environment) -> &'a mut Facts;

	fn in_recursive_cycle(&self, function_id: FunctionId) -> bool;

	fn new_function_target<T>(
		&mut self,
		function_id: FunctionId,
		cb: impl for<'a> FnOnce(&'a mut Target) -> T,
	) -> T;
}

pub struct CheckThings;

impl CallCheckingBehavior for CheckThings {
	const CHECK_PARAMETERS: bool = true;

	fn get_latest_facts<'a>(&'a mut self, environment: &'a mut Environment) -> &'a mut Facts {
		&mut environment.facts
	}

	fn in_recursive_cycle(&self, _function_id: FunctionId) -> bool {
		// cannot get in a loop from checking
		false
	}

	fn new_function_target<T>(
		&mut self,
		function_id: FunctionId,
		cb: impl for<'a> FnOnce(&'a mut Target) -> T,
	) -> T {
		let mut target = Target(vec![TargetKind::Function(function_id)]);
		cb(&mut target)
	}
}

pub(crate) struct Target(Vec<TargetKind>);

pub(crate) enum TargetKind {
	Conditional(Facts),
	Function(FunctionId),
}

impl CallCheckingBehavior for Target {
	const CHECK_PARAMETERS: bool = false;

	fn get_latest_facts<'b>(&'b mut self, environment: &'b mut Environment) -> &'b mut Facts {
		self.0
			.iter_mut()
			.rev()
			.find_map(
				|kind| if let TargetKind::Conditional(facts) = kind { Some(facts) } else { None },
			)
			.unwrap_or(&mut environment.facts)
	}

	fn in_recursive_cycle(&self, function_id: FunctionId) -> bool {
		self.0.iter().any(|kind| matches!(kind, TargetKind::Function(id) if function_id == *id))
	}

	fn new_function_target<T>(
		&mut self,
		function_id: FunctionId,
		cb: impl for<'a> FnOnce(&'a mut Target) -> T,
	) -> T {
		self.0.push(TargetKind::Function(function_id));
		let value = cb(self);
		self.0.pop();
		value
	}
}

impl Target {
	/// TODO temp for loop unrolling
	pub(crate) fn new_default() -> Self {
		Target(Vec::new())
	}

	pub(crate) fn new_conditional_target(
		&mut self,
		cb: impl for<'a> FnOnce(&'a mut Target) -> Option<EventResult>,
	) -> (Facts, Option<EventResult>) {
		self.0.push(TargetKind::Conditional(Facts::default()));
		let result = cb(self);
		if let Some(TargetKind::Conditional(facts)) = self.0.pop() {
			(facts, result)
		} else {
			unreachable!()
		}
	}
}
