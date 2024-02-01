//! When a function is called (or a group of events like function such as a iteration block) it creates a mini-environment for which events are applied into

use super::information::LocalInformation;
use crate::{events::FinalEvent, Environment, FunctionId};

/// For anything that might involve a call, including gets, sets and actual calls
pub trait CallCheckingBehavior {
	// TODO
	const CHECK_PARAMETERS: bool;

	fn get_latest_info<'a>(
		&'a mut self,
		environment: &'a mut Environment,
	) -> &'a mut LocalInformation;

	fn in_recursive_cycle(&self, function_id: FunctionId) -> bool;

	fn new_function_context<T>(
		&mut self,
		function_id: FunctionId,
		cb: impl for<'a> FnOnce(&'a mut InvocationContext) -> T,
	) -> T;
}

pub struct CheckThings;

impl CallCheckingBehavior for CheckThings {
	const CHECK_PARAMETERS: bool = true;

	fn get_latest_info<'a>(
		&'a mut self,
		environment: &'a mut Environment,
	) -> &'a mut LocalInformation {
		&mut environment.info
	}

	fn in_recursive_cycle(&self, _function_id: FunctionId) -> bool {
		// cannot get in a loop from checking
		false
	}

	fn new_function_context<T>(
		&mut self,
		function_id: FunctionId,
		cb: impl for<'a> FnOnce(&'a mut InvocationContext) -> T,
	) -> T {
		let mut target = InvocationContext(vec![InvocationKind::Function(function_id)]);
		cb(&mut target)
	}
}

pub struct InvocationContext(Vec<InvocationKind>);

pub(crate) enum InvocationKind {
	Conditional(LocalInformation),
	Function(FunctionId),
	LoopIteration,
}

impl CallCheckingBehavior for InvocationContext {
	const CHECK_PARAMETERS: bool = false;

	fn get_latest_info<'b>(
		&'b mut self,
		environment: &'b mut Environment,
	) -> &'b mut LocalInformation {
		self.0
			.iter_mut()
			.rev()
			.find_map(
				|kind| {
					if let InvocationKind::Conditional(info) = kind {
						Some(info)
					} else {
						None
					}
				},
			)
			.unwrap_or(&mut environment.info)
	}

	fn in_recursive_cycle(&self, function_id: FunctionId) -> bool {
		self.0.iter().any(|kind| matches!(kind, InvocationKind::Function(id) if function_id == *id))
	}

	fn new_function_context<T>(
		&mut self,
		function_id: FunctionId,
		cb: impl for<'a> FnOnce(&'a mut InvocationContext) -> T,
	) -> T {
		self.0.push(InvocationKind::Function(function_id));
		let value = cb(self);
		self.0.pop();
		value
	}
}

impl InvocationContext {
	/// TODO temp for loop unrolling
	pub(crate) fn new_empty() -> Self {
		InvocationContext(Vec::new())
	}

	pub(crate) fn new_conditional_target(
		&mut self,
		cb: impl for<'a> FnOnce(&'a mut InvocationContext) -> Option<FinalEvent>,
	) -> (LocalInformation, Option<FinalEvent>) {
		self.0.push(InvocationKind::Conditional(LocalInformation::default()));
		let result = cb(self);
		if let Some(InvocationKind::Conditional(info)) = self.0.pop() {
			(info, result)
		} else {
			unreachable!()
		}
	}

	pub(crate) fn new_loop_iteration<T>(
		&mut self,
		cb: impl for<'a> FnOnce(&'a mut InvocationContext) -> T,
	) -> T {
		self.0.push(InvocationKind::LoopIteration);
		let value = cb(self);
		self.0.pop();
		value
	}

	pub(crate) fn get_iteration_depth(&self) -> u8 {
		let depth =
			self.0.iter().filter(|p| matches!(p, InvocationKind::LoopIteration)).count() as u8;
		// TODO can this every go > 1
		crate::utils::notify!("Iteration depth {}", depth);
		depth
	}
}
