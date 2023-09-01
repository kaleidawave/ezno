use super::Event;
use crate::{
	types::{new_logical_or_type, TypeStore},
	TypeId,
};

pub(crate) enum ReturnedTypeFromBlock {
	// aka no return
	ContinuedExecution,
	// Aka can continue execution if when doesn't hold
	ReturnedIf { when: TypeId, returns: TypeId },
	Returned(TypeId),
}

pub(crate) fn get_return_from_events<'a>(
	iter: &mut (impl Iterator<Item = &'a Event> + ExactSizeIterator),
	types: &mut TypeStore,
) -> ReturnedTypeFromBlock {
	while let Some(event) = iter.next() {
		if let Event::Return { returned } = event {
			return ReturnedTypeFromBlock::Returned(*returned);
		} else if let Event::Conditionally { condition: on, events_if_truthy, else_events } = event
		{
			let return_if_truthy = get_return_from_events(&mut events_if_truthy.iter(), types);
			let else_return = get_return_from_events(&mut else_events.iter(), types);

			return match (return_if_truthy, else_return) {
				(
					ReturnedTypeFromBlock::ContinuedExecution,
					ReturnedTypeFromBlock::ContinuedExecution,
				) => {
					continue;
				}
				(
					ReturnedTypeFromBlock::ContinuedExecution,
					ReturnedTypeFromBlock::ReturnedIf { when, returns },
				) => {
					todo!("Expend rest of iterator")
					// ReturnedTypeFromBlock::ReturnedIf {
					// 	when: types.new_logical_negation_type(when),
					// 	returns,
					// }
				}
				(ReturnedTypeFromBlock::ContinuedExecution, ReturnedTypeFromBlock::Returned(_)) => {
					todo!()
				}
				(
					v @ ReturnedTypeFromBlock::ReturnedIf { .. },
					ReturnedTypeFromBlock::ContinuedExecution,
				) => {
					todo!()
					// return v
				}
				(
					ReturnedTypeFromBlock::ReturnedIf { when, returns },
					ReturnedTypeFromBlock::ReturnedIf { .. },
				) => todo!(),
				(
					ReturnedTypeFromBlock::ReturnedIf { when, returns },
					ReturnedTypeFromBlock::Returned(_),
				) => todo!(),
				(
					ReturnedTypeFromBlock::Returned(true_returned),
					ReturnedTypeFromBlock::ReturnedIf { when, returns },
				) => {
					todo!("expend iterator")
					// TODO could be simpler, aka a chain
					// ReturnedTypeFromBlock::ReturnedIf {
					// 	when: types.new_logical_or_type(lhs, rhs),
					// 	returns: types.new_conditional_type(when, returns, true_returned),
					// }
				}
				(ReturnedTypeFromBlock::Returned(true_returns), right) => {
					// if (a) { return 2 }
					// else { if (b) { return 3 } }
					// return 4
					let right = if iter.len() == 0
						&& !matches!(right, ReturnedTypeFromBlock::Returned(..))
					{
						get_return_from_events(iter, types)
					} else {
						right
					};
					match right {
						ReturnedTypeFromBlock::ContinuedExecution => {
							ReturnedTypeFromBlock::ReturnedIf { when: *on, returns: true_returns }
						}
						ReturnedTypeFromBlock::ReturnedIf { when, returns: false_returns } => {
							ReturnedTypeFromBlock::ReturnedIf {
								when: new_logical_or_type(*on, when, types),
								returns: types.new_conditional_type(
									when,
									false_returns,
									true_returns,
								),
							}
						}
						ReturnedTypeFromBlock::Returned(false_returns) => {
							ReturnedTypeFromBlock::Returned(types.new_conditional_type(
								*on,
								true_returns,
								false_returns,
							))
						}
					}
				}
			};
		}
	}
	ReturnedTypeFromBlock::ContinuedExecution
}

/// TODO improve
///
/// This actually removes the events as they are caught
pub(crate) fn extract_throw_events(events: Vec<Event>, thrown: &mut Vec<TypeId>) -> Vec<Event> {
	let mut new_events = Vec::new();
	for event in events.into_iter() {
		if let Event::Throw(value) = event {
			thrown.push(value);
		} else {
			// TODO nested grouping
			new_events.push(event)
		}
	}
	new_events
}
