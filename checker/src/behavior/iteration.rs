use std::collections::{HashMap, HashSet};

use source_map::{BaseSpan, SpanWithSource};

use crate::{
	behavior::{operations::CanonicalEqualityAndInequality, variables::VariableOrImport},
	context::get_value_of_variable,
	events::{application::apply_event_unknown, apply_event, Event, EventResult, RootReference},
	types::{
		poly_types::{generic_type_arguments::StructureGenericArguments, FunctionTypeArguments},
		Constructor, PolyNature, TypeArguments, TypeStore,
	},
	CheckingData, Constant, Environment, Facts, Scope, Type, TypeId, VariableId,
};

/// TODO for of and for in
pub enum IterationBehavior<'a, A: crate::ASTImplementation> {
	While(&'a A::MultipleExpression<'a>),
	DoWhile(&'a A::MultipleExpression<'a>),
	For {
		initialiser: &'a Option<A::ForStatementInitiliser<'a>>,
		condition: &'a Option<A::MultipleExpression<'a>>,
		afterthought: &'a Option<A::MultipleExpression<'a>>,
	},
}

pub fn synthesise_iteration<'a, T: crate::ReadFromFS, A: crate::ASTImplementation>(
	behavior: IterationBehavior<'a, A>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
	loop_body: impl FnOnce(&mut Environment, &mut CheckingData<T, A>),
) {
	let is_do_while = matches!(behavior, IterationBehavior::DoWhile(..));

	let (condition, result, loop_variables) = match behavior {
		IterationBehavior::While(condition) => {
			let (condition, events, ..) = environment.new_lexical_environment_fold_into_parent(
				Scope::Looping {},
				checking_data,
				|environment, checking_data| {
					let condition = A::synthesise_multiple_expression(
						condition,
						TypeId::ANY_TYPE,
						environment,
						checking_data,
					);

					// TODO not always needed
					let break_event = Event::Conditionally {
						condition,
						events_if_truthy: Default::default(),
						else_events: Box::new([Event::Break { position: None, label: None }]),
						position: None,
					};
					environment.facts.events.push(break_event);

					loop_body(environment, checking_data);

					condition
				},
			);
			(condition, events, None)
		}
		IterationBehavior::DoWhile(condition) => {
			// Same as above but condition is evaluated at end. Don't know whether events should be evaluated once...?
			let (condition, events, ..) = environment.new_lexical_environment_fold_into_parent(
				Scope::Looping {},
				checking_data,
				|environment, checking_data| {
					loop_body(environment, checking_data);

					let condition = A::synthesise_multiple_expression(
						condition,
						TypeId::ANY_TYPE,
						environment,
						checking_data,
					);

					// TODO not always needed
					let break_event = Event::Conditionally {
						condition,
						events_if_truthy: Default::default(),
						else_events: Box::new([Event::Break { position: None, label: None }]),
						position: None,
					};
					environment.facts.events.push(break_event);

					condition
				},
			);
			(condition, events, None)
		}
		IterationBehavior::For { initialiser, condition, afterthought } => {
			// 99% of the time need to do this, so doing here anyway
			let (result, ..) = environment.new_lexical_environment_fold_into_parent(
				Scope::Block {},
				checking_data,
				|environment, checking_data| {
					let dependent_variables_initial_values: HashMap<VariableId, TypeId> =
						if let Some(initialiser) = initialiser {
							A::synthesise_for_loop_initialiser(
								initialiser,
								environment,
								checking_data,
							);

							environment
								.variables
								.values()
								.map(|v| {
									let id = v.get_id();
									let end_value = environment
										.facts
										.variable_current_value
										.get(&id)
										.expect("loop variable with no initial value");
									(id, *end_value)
								})
								.collect()
						} else {
							Default::default()
						};

					let ((condition, dependent_variables), events, ..) = environment
						.new_lexical_environment_fold_into_parent(
							Scope::Looping {},
							checking_data,
							|environment, checking_data| {
								let condition = if let Some(condition) = condition {
									A::synthesise_multiple_expression(
										condition,
										TypeId::ANY_TYPE,
										environment,
										checking_data,
									)
								} else {
									TypeId::TRUE
								};

								// TODO not always needed
								let break_event = Event::Conditionally {
									condition,
									events_if_truthy: Default::default(),
									else_events: Box::new([Event::Break {
										position: None,
										label: None,
									}]),
									position: None,
								};
								environment.facts.events.push(break_event);

								loop_body(environment, checking_data);

								// Just want to observe events that happen here
								if let Some(afterthought) = afterthought {
									let _ = A::synthesise_multiple_expression(
										afterthought,
										TypeId::ANY_TYPE,
										environment,
										checking_data,
									);
								}

								let dependent_variables: HashMap<VariableId, (TypeId, TypeId)> =
									dependent_variables_initial_values
										.into_iter()
										.map(|(id, start_value)| {
											let end_value = environment
												.facts
												.variable_current_value
												.get(&id)
												.expect("loop variable with no initial value");
											(id, (start_value, *end_value))
										})
										.collect();

								(condition, dependent_variables)
							},
						);

					// TODO copy value of variables between things, or however it works

					(condition, events, Some(dependent_variables))
				},
			);

			result
		}
	};

	let (Facts { variable_current_value, current_properties, mut events, .. }, _closes_over) =
		result.unwrap();

	let fixed_iterations = calculate_maximum_iterations(
		condition,
		&loop_variables,
		environment,
		&checking_data.types,
		Values { variables: variable_current_value, properties: current_properties },
	);

	// let mut buf = String::new();
	// crate::types::printing::debug_effects(
	// 	&mut buf,
	// 	&events,
	// 	&checking_data.types,
	// 	&environment.as_general_context(),
	// 	true,
	// );
	// crate::utils::notify!("events in iteration = {}", buf);

	// TODO abstract for event application
	if let Some(mut iterations) = fixed_iterations {
		// These bodies always run at least once. TODO is there a better way?
		if is_do_while {
			iterations += 1;
		}

		// TODO temp fix
		if let Some(loop_variables) = loop_variables {
			for (var, (start, _)) in loop_variables {
				environment.facts.variable_current_value.insert(var, start);
			}
		}

		let result = evaluate_iterations(
			iterations,
			&events,
			TypeArguments::new(),
			None,
			environment,
			&mut checking_data.types,
		);

		if let Some(result) = result {
			match result {
				EventResult::Return(returned, returned_position) => {
					environment.return_value(returned, returned_position);
				}
				// Already added
				EventResult::Throw => {}
				EventResult::Break { label } => todo!(),
				EventResult::Continue { label } => todo!(),
			}
		}
	} else {
		let mut arguments = FunctionTypeArguments {
			structure_arguments: Default::default(),
			local_arguments: map_vec::Map::new(),
			closure_id: Default::default(),
		};

		// TODO maybe treat the same way as closures
		let mut initial = map_vec::Map::new();
		let mut filtered_events = Vec::new();
		for event in events.clone() {
			// TODO also nested events right?
			if let Event::ReadsReference { reference, reflects_dependency, position } = event {
				if let Some(reflects_dependency) = reflects_dependency {
					if let RootReference::Variable(id) = reference {
						let value = get_value_of_variable(
							environment.facts_chain(),
							id,
							None::<&crate::types::poly_types::FunctionTypeArguments>,
						)
						.unwrap();
						initial.insert(reflects_dependency, (value, position));
					}
				}
			} else {
				filtered_events.push(event);
			}
		}

		environment
			.facts
			.events
			.push(Event::Iterate { initial, iterate_over: filtered_events.into_boxed_slice() });
		
		// TODO can skip if at the end of a function
		for event in events {
			let result = apply_event_unknown(
				event,
				crate::behavior::functions::ThisValue::UseParent,
				&mut arguments,
				environment,
				&mut crate::context::calling::Target::new_default(),
				&mut checking_data.types,
			);
		}

	}
}

#[must_use]
pub(crate) fn evaluate_iterations(
	iterations: usize,
	events: &Vec<Event>,
	initial: TypeArguments,
	structure_arguments: Option<StructureGenericArguments>,
	environment: &mut Environment,
	types: &mut TypeStore,
) -> Option<EventResult> {
	let mut arguments = FunctionTypeArguments {
		structure_arguments,
		local_arguments: initial,
		closure_id: Default::default(),
	};
	'main_iterations: for _ in 0..iterations {
		'inner_loop: for event in events.clone().into_iter() {
			let mut errors = Vec::new();
			let result = apply_event(
				event,
				crate::behavior::functions::ThisValue::UseParent,
				&mut arguments,
				environment,
				&mut crate::context::calling::Target::new_default(),
				types,
				// Shouldn't matter
				&mut errors,
			);

			if !errors.is_empty() {
				unreachable!()
			}

			if let Some(result) = result {
				match result {
					EventResult::Continue { label } => {
						if let Some(label) = label {
							crate::utils::notify!("TODO labels");
						}
						break 'inner_loop;
					}
					EventResult::Break { label } => {
						if let Some(label) = label {
							crate::utils::notify!("TODO labels");
						}
						break 'main_iterations;
					}
					e @ EventResult::Return(..) => return Some(e),
					EventResult::Throw => {
						break 'main_iterations;
					}
				}
			}
		}
	}

	None
}

struct Values {
	pub variables: HashMap<VariableId, TypeId>,
	pub properties: HashMap<
		TypeId,
		Vec<(
			crate::context::facts::Publicity,
			crate::types::properties::PropertyKey<'static>,
			crate::PropertyValue,
		)>,
	>,
}

/// Calculates the maximum amount of times the loop iterates
///
/// TODO doesn't handle breaks etc
/// TODO look at properties as well
fn calculate_maximum_iterations(
	condition: TypeId,
	loop_variables: &Option<HashMap<VariableId, (TypeId, TypeId)>>,
	parent_environment: &Environment,
	types: &TypeStore,
	loop_facts: Values,
) -> Option<usize> {
	let condition_ty = types.get_type_by_id(condition);

	crate::utils::notify!("condition is {:?}", condition_ty);

	// TODO some other cases
	// - and for less than equal
	if let Type::Constructor(Constructor::CanonicalRelationOperator {
		lhs,
		operator: CanonicalEqualityAndInequality::LessThan,
		rhs,
	}) = condition_ty
	{
		// TODO sort by constant. Assumed here that dependent is on the LHS

		let lhs_ty = types.get_type_by_id(*lhs);
		if let Type::RootPolyType(PolyNature::FreeVariable { reference, based_on }) = lhs_ty {
			let less_than_ty = types.get_type_by_id(*rhs);
			if let Type::Constant(Constant::Number(roof)) = less_than_ty {
				if let RootReference::Variable(v) = reference {
					let value_after_running_expressions_in_loop = types.get_type_by_id(
						if let Some((_start, end)) =
							loop_variables.as_ref().and_then(|vs| vs.get(&v).copied())
						{
							end
						} else {
							*loop_facts.variables.get(&v).unwrap()
						},
					);

					crate::utils::notify!(
						"incremented is {:?}",
						value_after_running_expressions_in_loop
					);

					// Looking at incrementor
					if let Type::Constructor(Constructor::BinaryOperator {
						lhs: assignment,
						operator,
						rhs,
					}) = value_after_running_expressions_in_loop
					{
						debug_assert!(lhs == assignment, "incrementor not the same as condition?");
						if let Type::Constant(Constant::Number(increment)) =
							types.get_type_by_id(*rhs)
						{
							// Some(*n)
							let start = if let Some((start, _end)) =
								loop_variables.as_ref().and_then(|vs| vs.get(&v).copied())
							{
								start
							} else {
								// let id = if let RootReference::Variable(id) = reference {
								// 	id
								// } else {
								// 	unreachable!("this")
								// };
								*parent_environment.facts.variable_current_value.get(v).unwrap()
							};

							if let crate::Type::Constant(Constant::Number(start)) =
								types.get_type_by_id(start)
							{
								let end = *roof;
								let iterations = (end - start) / increment;
								crate::utils::notify!(
									"Evaluating iteration: start={}, end={}, increment={}, iterations={}",
									start,
									end,
									increment,
									iterations
								);
								return Some(iterations.ceil() as usize);
							}
						}
					}
				}
			} else {
				crate::utils::notify!("{:?} has no max", less_than_ty);
			}
		} else {
			crate::utils::notify!("LHS {:?} is not free variable ", lhs_ty);
		}
	}
	None
}
