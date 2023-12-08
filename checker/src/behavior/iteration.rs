use std::collections::{HashMap, HashSet};

use crate::{
	behavior::{operations::CanonicalEqualityAndInequality, variables::VariableOrImport},
	events::{apply_event, Event, RootReference},
	types::{poly_types::FunctionTypeArguments, Constructor, PolyNature, TypeStore},
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

pub fn evaluate_iteration<'a, T: crate::ReadFromFS, A: crate::ASTImplementation>(
	behavior: IterationBehavior<'a, A>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
	loop_body: impl FnOnce(&mut Environment, &mut CheckingData<T, A>),
) {
	let (condition, facts, loop_variables) = match behavior {
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

					environment.facts.events.push(Event::Conditionally {
						condition,
						events_if_truthy: Box::new([Event::Break { position: None }]),
						else_events: Default::default(),
						position: None,
					});

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

					environment.facts.events.push(Event::Conditionally {
						condition,
						events_if_truthy: Box::new([Event::Break { position: None }]),
						else_events: Default::default(),
						position: None,
					});

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

								environment.facts.events.push(Event::Conditionally {
									condition,
									events_if_truthy: Box::new([Event::Break { position: None }]),
									else_events: Default::default(),
									position: None,
								});

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

	let Facts { variable_current_value, current_properties, events, .. } = facts.unwrap().0;

	let result = calculate_iterations(
		condition,
		&loop_variables,
		environment,
		&checking_data.types,
		Values { variables: variable_current_value, properties: current_properties },
	);

	crate::utils::notify!("events in iteration = {:?}", events);

	if let Some((start, increment, end)) = result {
		let iterations = (end - start) / increment;
		crate::utils::notify!(
			"Evaluating iteration: start={}, end={}, increment={}, iterations={}",
			start,
			end,
			increment,
			iterations
		);

		// TODO temp fix
		if let Some(loop_variables) = loop_variables {
			for (var, (start, _)) in loop_variables {
				environment.facts.variable_current_value.insert(var, start);
			}
		}

		let mut arguments = FunctionTypeArguments {
			structure_arguments: Default::default(),
			local_arguments: map_vec::Map::new(),
			closure_id: Default::default(),
		};

		// Think this is okay?
		for _ in 0..(iterations.into_inner() as usize) {
			for event in events.clone().into_iter() {
				apply_event(
					event,
					crate::behavior::functions::ThisValue::UseParent,
					&mut arguments,
					environment,
					&mut crate::context::calling::Target::new_default(),
					&mut checking_data.types,
					// Shouldn't matter
					&mut Vec::new(),
				);
			}
		}
	} else {
		let mut arguments = FunctionTypeArguments {
			structure_arguments: Default::default(),
			local_arguments: map_vec::Map::new(),
			closure_id: Default::default(),
		};

		todo!("apply events with special general target (same as recursion cases)");

		// TODO generate loop event

		// for event in events.clone().into_iter() {
		// 	// TODO temp
		// 	apply_event(
		// 		event,
		// 		crate::behavior::functions::ThisValue::UseParent,
		// 		&mut arguments,
		// 		environment,
		// 		&mut crate::context::calling::Target::new_default(),
		// 		&mut checking_data.types,
		// 	);
		// }
	}
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

/// TODO doesn't handle breaks etc
/// TODO look at properties as well
fn calculate_iterations(
	condition: TypeId,
	loop_variables: &Option<HashMap<VariableId, (TypeId, TypeId)>>,
	parent_environment: &Environment,
	types: &TypeStore,
	loop_facts: Values,
) -> Option<(ordered_float::NotNan<f64>, ordered_float::NotNan<f64>, ordered_float::NotNan<f64>)> {
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

		if let Type::RootPolyType(PolyNature::FreeVariable { reference, based_on }) =
			types.get_type_by_id(*lhs)
		{
			if let Type::Constant(Constant::Number(lhs_less_than)) = types.get_type_by_id(*rhs) {
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
						if let Type::Constant(Constant::Number(incrementor)) =
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
								return Some((*start, *incrementor, *lhs_less_than));
							}
						}
					}
				}
			}
		}
	}
	None
}
