//! When a function is called (or a group of events like function such as a iteration block) it creates a mini-environment for which events are applied into

use source_map::SpanWithSource;

use super::LocalInformation;
use crate::{
	context::information::merge_info, events::ApplicationResult, types::TypeStore, Environment,
	FunctionId, TypeId,
};

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

	fn debug_types(&self) -> bool {
		false
	}
}

/// Top level. Evaluating directly, rather than deep in event application
pub struct CheckThings {
	pub debug_types: bool,
}

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

	fn debug_types(&self) -> bool {
		self.debug_types
	}
}

pub struct InvocationContext(Vec<InvocationKind>);

/// TODO want to have type arguments on each of these
pub(crate) enum InvocationKind {
	Conditional(LocalInformation),
	/// *Unconditional*
	///
	/// TODO does this need [`LocalInformation`]??
	AlwaysTrue,
	Function(FunctionId),
	LoopIteration,
	Unknown,
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

	pub(crate) fn in_unknown(&self) -> bool {
		self.0.iter().any(|c| matches!(c, InvocationKind::Unknown))
	}

	/// WIP
	pub(crate) fn new_unknown_target<T>(
		&mut self,
		cb: impl for<'a> FnOnce(&'a mut InvocationContext) -> T,
	) -> T {
		self.0.push(InvocationKind::Unknown);
		let result = cb(self);
		self.0.pop();
		result
	}

	fn new_conditional_target<T>(
		&mut self,
		cb: impl for<'a> FnOnce(&'a mut InvocationContext) -> T,
	) -> (LocalInformation, T) {
		self.0.push(InvocationKind::Conditional(LocalInformation::default()));
		let result = cb(self);
		if let Some(InvocationKind::Conditional(info)) = self.0.pop() {
			(info, result)
		} else {
			unreachable!()
		}
	}

	pub(crate) fn new_unconditional_target(
		&mut self,
		cb: impl for<'a> FnOnce(&'a mut InvocationContext) -> Option<ApplicationResult>,
	) -> Option<ApplicationResult> {
		self.0.push(InvocationKind::AlwaysTrue);
		let result = cb(self);
		self.0.pop();
		result
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
		crate::utilities::notify!("Iteration depth {}", depth);
		depth
	}

	pub(crate) fn in_unconditional(&self) -> bool {
		self.0.iter().any(|mem| matches!(mem, InvocationKind::AlwaysTrue))
	}

	/// TODO maybe take result -> R
	/// TODO move to trait
	pub(crate) fn evaluate_conditionally<I, T, R>(
		&mut self,
		top_environment: &mut Environment,
		types: &mut TypeStore,
		(condition, condition_position): (TypeId, SpanWithSource),
		(input_left, input_right): (T, T),
		mut data: I,
		cb: impl for<'a> Fn(
			&'a mut Environment,
			&'a mut TypeStore,
			&'a mut InvocationContext,
			T,
			&'a mut I,
		) -> R,
	) -> (I, (R, R)) {
		let (truthy_info, truthy_result) =
			self.new_conditional_target(|target: &mut InvocationContext| {
				cb(top_environment, types, target, input_left, &mut data)
			});

		let (otherwise_info, otherwise_result) =
			self.new_conditional_target(|target: &mut InvocationContext| {
				cb(top_environment, types, target, input_right, &mut data)
			});

		// TODO all things that are
		// - variable and property values (these aren't read from events)
		// - immutable, mutable, prototypes etc
		// let info = self.get_latest_info(top_environment);

		if self.0.iter().any(|x| matches!(x, InvocationKind::Conditional(_))) {
			todo!("nested, get latest")
		// let local_information = &mut top_environment.info;
		// match top_environment.context_type.parent {
		// 	crate::GeneralContext::Syntax(env) => {
		// 		merge_info(
		// 			env,
		// 			local_information,
		// 			condition,
		// 			truthy_info,
		// 			Some(otherwise_info),
		// 			types,
		// 			condition_position,
		// 		);
		// 	}
		// 	crate::GeneralContext::Root(_) => todo!(),
		// }
		} else {
			let local_information = &mut top_environment.info;
			match top_environment.context_type.parent {
				crate::GeneralContext::Syntax(env) => {
					merge_info(
						env,
						local_information,
						condition,
						truthy_info,
						Some(otherwise_info),
						types,
						condition_position,
					);
				}
				crate::GeneralContext::Root(_) => {
					crate::utilities::notify!("Top environment is root?");
				}
			}
		}

		(data, (truthy_result, otherwise_result))
	}
}
