use super::{Event, FinalEvent};
use crate::{types::new_logical_or_type, CheckingData, Environment, TypeId};

pub(crate) enum ReturnedTypeFromBlock {
	// aka no return
	ContinuedExecution,
	// Aka can continue execution if when doesn't hold
	ReturnedIf { when: TypeId, returns: TypeId },
	Returned(TypeId),
}

/// TODO will cover move, like yield events and stuff
pub(crate) fn get_return_from_events<'a, T: crate::ReadFromFS, A: crate::ASTImplementation>(
	iter: &mut (impl Iterator<Item = &'a Event> + ExactSizeIterator),
	checking_data: &mut CheckingData<T, A>,
	environment: &mut Environment,
	expected_return_type: Option<(TypeId, source_map::SpanWithSource)>,
) -> ReturnedTypeFromBlock {
	while let Some(event) = iter.next() {
		match event {
			Event::FinalEvent(FinalEvent::Return { returned, returned_position }) => {
				if let Some((expected_return_type, annotation_span)) = expected_return_type {
					let mut behavior = crate::subtyping::BasicEquality {
						add_property_restrictions: true,
						position: annotation_span,
					};

					let result = crate::subtyping::type_is_subtype(
						expected_return_type,
						*returned,
						&mut behavior,
						environment,
						&checking_data.types,
					);

					if let crate::subtyping::SubTypeResult::IsNotSubType(_) = result {
						checking_data.diagnostics_container.add_error(
							crate::diagnostics::TypeCheckError::ReturnedTypeDoesNotMatch {
								expected_return_type:
									crate::diagnostics::TypeStringRepresentation::from_type_id(
										expected_return_type,
										&environment.as_general_context(),
										&checking_data.types,
										false,
									),
								returned_type:
									crate::diagnostics::TypeStringRepresentation::from_type_id(
										*returned,
										&environment.as_general_context(),
										&checking_data.types,
										false,
									),
								annotation_position: annotation_span,
								returned_position: *returned_position,
							},
						);
					}
				}
				return ReturnedTypeFromBlock::Returned(*returned);
			}
			// TODO and for
			Event::Conditionally { condition: on, true_events, else_events, position: _ } => {
				let return_if_truthy = get_return_from_events(
					&mut true_events.iter(),
					checking_data,
					environment,
					expected_return_type,
				);
				let else_return = get_return_from_events(
					&mut else_events.iter(),
					checking_data,
					environment,
					expected_return_type,
				);

				return match (return_if_truthy, else_return) {
					(
						ReturnedTypeFromBlock::ContinuedExecution,
						ReturnedTypeFromBlock::ContinuedExecution,
					) => {
						continue;
					}
					(
						ReturnedTypeFromBlock::ContinuedExecution,
						ReturnedTypeFromBlock::ReturnedIf { when: _, returns: _ },
					) => {
						todo!("Expend rest of iterator")
						// ReturnedTypeFromBlock::ReturnedIf {
						// 	when: types.new_logical_negation_type(when),
						// 	returns,
						// }
					}
					(
						ReturnedTypeFromBlock::ContinuedExecution,
						ReturnedTypeFromBlock::Returned(_),
					) => {
						todo!()
					}
					(
						_v @ ReturnedTypeFromBlock::ReturnedIf { .. },
						ReturnedTypeFromBlock::ContinuedExecution,
					) => {
						todo!()
						// return v
					}
					(
						ReturnedTypeFromBlock::ReturnedIf { when: _, returns: _ },
						ReturnedTypeFromBlock::ReturnedIf { .. },
					) => todo!(),
					(
						ReturnedTypeFromBlock::ReturnedIf { when: _, returns: _ },
						ReturnedTypeFromBlock::Returned(_),
					) => todo!(),
					(
						ReturnedTypeFromBlock::Returned(_true_returned),
						ReturnedTypeFromBlock::ReturnedIf { when: _, returns: _ },
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
							get_return_from_events(
								iter,
								checking_data,
								environment,
								expected_return_type,
							)
						} else {
							right
						};
						match right {
							ReturnedTypeFromBlock::ContinuedExecution => {
								ReturnedTypeFromBlock::ReturnedIf {
									when: *on,
									returns: true_returns,
								}
							}
							ReturnedTypeFromBlock::ReturnedIf { when, returns: false_returns } => {
								ReturnedTypeFromBlock::ReturnedIf {
									when: new_logical_or_type(*on, when, &mut checking_data.types),
									returns: checking_data.types.new_conditional_type(
										when,
										false_returns,
										true_returns,
									),
								}
							}
							ReturnedTypeFromBlock::Returned(false_returns) => {
								ReturnedTypeFromBlock::Returned(
									checking_data.types.new_conditional_type(
										*on,
										true_returns,
										false_returns,
									),
								)
							}
						}
					}
				};
			}
			Event::FinalEvent(FinalEvent::Throw { .. }) => {
				// TODO ReturnedTypeFromBlock::Thrown? however this does work
				return ReturnedTypeFromBlock::Returned(TypeId::NEVER_TYPE);
			}
			_ => (),
		}
	}
	ReturnedTypeFromBlock::ContinuedExecution
}

/// TODO improve
///
/// This actually removes the events as they are caught
pub(crate) fn extract_throw_events(events: Vec<Event>, thrown: &mut TypeId) -> Vec<Event> {
	let mut new_events = Vec::new();
	for event in events {
		if let Event::FinalEvent(FinalEvent::Throw { thrown: value, position: _ }) = event {
			*thrown = value;
		} else if let Event::Conditionally { .. } = event {
			todo!()
		} else if let Event::Iterate { .. } = event {
			todo!()
		} else {
			// TODO nested grouping
			new_events.push(event);
		}
	}
	new_events
}
