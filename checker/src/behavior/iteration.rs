use std::collections::HashMap;

use crate::{
	behavior::operations::CanonicalEqualityAndInequality,
	context::{calling::Target, environment::Label, get_value_of_variable, CallCheckingBehavior},
	events::{
		application::ErrorsAndInfo, apply_event, Event, EventResult, InitialVariables,
		RootReference,
	},
	types::{
		poly_types::{generic_type_arguments::TypeArgumentStore, FunctionTypeArguments},
		printing::print_type,
		substitute, Constructor, ObjectNature, PolyNature, TypeStore,
	},
	CheckingData, Constant, Environment, Facts, Scope, Type, TypeId, VariableId,
};

#[derive(Clone, Copy)]
pub enum IterationBehavior<'a, A: crate::ASTImplementation> {
	While(&'a A::MultipleExpression<'a>),
	DoWhile(&'a A::MultipleExpression<'a>),
	For {
		initialiser: &'a Option<A::ForStatementInitiliser<'a>>,
		condition: &'a Option<A::MultipleExpression<'a>>,
		afterthought: &'a Option<A::MultipleExpression<'a>>,
	},
	ForIn {
		lhs: &'a A::VariableField<'a>,
		rhs: &'a A::MultipleExpression<'a>,
	},
	ForOf {
		lhs: &'a A::VariableField<'a>,
		rhs: &'a A::Expression<'a>,
	},
}

#[allow(clippy::needless_pass_by_value)]
pub fn synthesise_iteration<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	behavior: IterationBehavior<A>,
	label: Label,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
	loop_body: impl FnOnce(&mut Environment, &mut CheckingData<T, A>),
) {
	match behavior {
		IterationBehavior::While(condition) => {
			let (condition, result, ..) = environment.new_lexical_environment_fold_into_parent(
				Scope::Looping { label },
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
						else_events: Box::new([Event::Break { position: None, carry: 0 }]),
						position: None,
					};
					environment.facts.events.push(break_event);

					loop_body(environment, checking_data);

					condition
				},
			);

			let (Facts { variable_current_value, current_properties, events, .. }, _closes_over) =
				result.unwrap();

			let loop_facts = Values {
				variable_values: variable_current_value,
				_properties_values: current_properties,
			};

			let fixed_iterations = calculate_result_of_loop(
				condition,
				None,
				environment,
				&checking_data.types,
				&loop_facts,
			);

			if let Some(_value) = run_iteration_block(
				IterationKind::Condition { under: fixed_iterations.ok(), postfix_condition: false },
				events,
				InitialVariablesInput::Compute,
				&mut FunctionTypeArguments::new(),
				environment,
				&mut crate::context::calling::Target::new_default(),
				// TODO shouldn't be needed
				&mut Default::default(),
				&mut checking_data.types,
			) {
				todo!()
			}
		}
		IterationBehavior::DoWhile(condition) => {
			// let is_do_while = matches!(behavior, IterationBehavior::DoWhile(..));

			// Same as above but condition is evaluated at end. Don't know whether events should be evaluated once...?
			let (condition, result, ..) = environment.new_lexical_environment_fold_into_parent(
				Scope::Looping { label },
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
						else_events: Box::new([Event::Break { position: None, carry: 0 }]),
						position: None,
					};
					environment.facts.events.push(break_event);

					condition
				},
			);

			let (Facts { variable_current_value, current_properties, events, .. }, _closes_over) =
				result.unwrap();

			let loop_facts = Values {
				variable_values: variable_current_value,
				_properties_values: current_properties,
			};

			let fixed_iterations = calculate_result_of_loop(
				condition,
				None,
				environment,
				&checking_data.types,
				&loop_facts,
			);

			if let Some(_value) = run_iteration_block(
				IterationKind::Condition { under: fixed_iterations.ok(), postfix_condition: true },
				events,
				InitialVariablesInput::Compute,
				&mut FunctionTypeArguments::new(),
				environment,
				&mut crate::context::calling::Target::new_default(),
				// TODO shouldn't be needed
				&mut Default::default(),
				&mut checking_data.types,
			) {
				todo!()
			}
		}
		IterationBehavior::For { initialiser, condition, afterthought } => {
			// 99% of the time need to do this, so doing here anyway
			let ((condition, result, dependent_variables), ..) = environment
				.new_lexical_environment_fold_into_parent(
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
								Scope::Looping { label },
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
											carry: 0,
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

						(condition, events, dependent_variables)
					},
				);

			let (Facts { variable_current_value, current_properties, events, .. }, _closes_over) =
				result.unwrap();

			let loop_facts = Values {
				variable_values: variable_current_value,
				_properties_values: current_properties,
			};

			let fixed_iterations = calculate_result_of_loop(
				condition,
				Some(&dependent_variables),
				environment,
				&checking_data.types,
				&loop_facts,
			);

			for (var, (start, _)) in dependent_variables {
				environment.facts.variable_current_value.insert(var, start);
			}

			if let Some(_value) = run_iteration_block(
				IterationKind::Condition { under: fixed_iterations.ok(), postfix_condition: false },
				events,
				InitialVariablesInput::Compute,
				&mut FunctionTypeArguments::new(),
				environment,
				&mut crate::context::calling::Target::new_default(),
				// TODO shouldn't be needed
				&mut Default::default(),
				&mut checking_data.types,
			) {
				todo!()
			}
		}
		IterationBehavior::ForIn { lhs: _, rhs } => {
			// TODO for of Object.keys ???
			let on = A::synthesise_multiple_expression(
				rhs,
				TypeId::ANY_TYPE,
				environment,
				checking_data,
			);

			let ((), result, ..) = environment.new_lexical_environment_fold_into_parent(
				Scope::Looping { label },
				checking_data,
				|environment, checking_data| {
					loop_body(environment, checking_data);
				},
			);

			let events = result.unwrap().0.events;

			if let Some(_value) = run_iteration_block(
				IterationKind::Properties(on),
				events,
				InitialVariablesInput::Compute,
				&mut FunctionTypeArguments::new(),
				environment,
				&mut crate::context::calling::Target::new_default(),
				// TODO shouldn't be needed
				&mut Default::default(),
				&mut checking_data.types,
			) {
				todo!()
			}
		}
		IterationBehavior::ForOf { lhs: _, rhs: _ } => todo!(),
	}
}

pub enum InitialVariablesInput {
	Calculated(InitialVariables),
	Compute,
}

#[derive(Debug, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum IterationKind {
	Condition {
		/// `Some` if under certain conditions it can evaluate the loop
		under: Option<LoopStructure>,
		/// `true` for do-while loops
		postfix_condition: bool,
	},
	Properties(TypeId),
	Iterator(TypeId),
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_iteration_block(
	condition: IterationKind,
	events: Vec<Event>,
	initial: InitialVariablesInput,
	type_arguments: &mut FunctionTypeArguments,
	environment: &mut Environment,
	target: &mut Target,
	errors: &mut ErrorsAndInfo,
	types: &mut TypeStore,
) -> Option<EventResult> {
	/// TODO via config
	const MAX_ITERATIONS: usize = 1000;

	match condition {
		IterationKind::Condition { postfix_condition, under } => {
			// let mut buf = String::new();
			// crate::types::printing::debug_effects(
			// 	&mut buf,
			// 	&events,
			// 	&checking_data.types,
			// 	&environment.as_general_context(),
			// 	true,
			// );
			// crate::utils::notify!("events in iteration = {}", buf);

			let non_exorbitant_amount_of_iterations = under
				.and_then(|under| under.calculate_iterations(types).ok())
				.and_then(|iterations| (iterations < MAX_ITERATIONS).then_some(iterations));

			if let Some(mut iterations) = non_exorbitant_amount_of_iterations {
				// These bodies always run at least once. TODO is there a better way?
				if postfix_condition {
					iterations += 1;
				}

				crate::utils::notify!(
					"Evaluating a constant amount of iterations {:?}",
					iterations
				);

				if let InitialVariablesInput::Calculated(initial) = initial {
					for (variable_id, initial_value) in &initial {
						target
							.get_latest_facts(environment)
							.variable_current_value
							.insert(*variable_id, *initial_value);
					}
				} else {
					crate::utils::notify!("Here ??");
				}

				evaluate_iterations(
					iterations,
					&events,
					type_arguments,
					environment,
					target,
					errors,
					types,
				)
			} else {
				let initial = match initial {
					InitialVariablesInput::Calculated(initial) => {
						for (id, value) in &initial {
							target
								.get_latest_facts(environment)
								.variable_current_value
								.insert(*id, *value);
						}

						initial
					}
					InitialVariablesInput::Compute => {
						// TODO maybe treat the same way as closures
						let mut initial = map_vec::Map::new();

						for event in &events {
							// TODO also nested events right?
							if let Event::ReadsReference {
								reference: RootReference::Variable(variable_id),
								reflects_dependency: Some(free_variable_id),
								position: _,
							} = event
							{
								let value_before_iterations = get_value_of_variable(
									environment.facts_chain(),
									*variable_id,
									None::<&crate::types::poly_types::FunctionTypeArguments>,
								)
								.unwrap();

								crate::utils::notify!(
									"setting '{}' to have initial type {}",
									environment.get_variable_name(*variable_id),
									print_type(
										value_before_iterations,
										types,
										&environment.as_general_context(),
										true
									)
								);

								initial.insert(*variable_id, value_before_iterations);

								environment
									.facts
									.variable_current_value
									.insert(*variable_id, *free_variable_id);
							}
						}
						initial
					}
				};

				crate::utils::notify!("Saving events");

				target.get_latest_facts(environment).events.push(Event::Iterate {
					kind: IterationKind::Condition { under, postfix_condition },
					initial,
					iterate_over: events.into_boxed_slice(),
				});

				// TODO can skip if at the end of a function
				// for event in events {
				// 	let result = apply_event_unknown(
				// 		event,
				// 		crate::behavior::functions::ThisValue::UseParent,
				// 		&mut arguments,
				// 		environment,
				// 		&mut crate::context::calling::Target::new_default(),
				// 		&mut checking_data.types,
				// 	);
				// }
				None
			}
		}
		IterationKind::Properties(on) => {
			if let Type::Object(ObjectNature::RealDeal) = types.get_type_by_id(on) {
				for (_publicity, property, _value) in environment.get_properties_on_type(on) {
					// TODO enumerable
					crate::utils::notify!("Property: {:?}", property);
				}
				None
			} else {
				todo!("dependent in")
			}
		}
		IterationKind::Iterator(_) => todo!(),
	}
}

#[must_use]
fn evaluate_iterations(
	iterations: usize,
	events: &[Event],
	arguments: &mut FunctionTypeArguments,
	environment: &mut Environment,
	target: &mut Target,
	errors: &mut ErrorsAndInfo,
	types: &mut TypeStore,
) -> Option<EventResult> {
	// TODO temp fix
	if !errors.errors.is_empty() {
		return None;
	}

	'main_iterations: for _ in 0..iterations {
		'inner_loop: for event in events {
			let result = apply_event(
				event.clone(),
				crate::behavior::functions::ThisValue::UseParent,
				arguments,
				environment,
				// TODO new nested target
				target,
				types,
				// Shouldn't matter
				errors,
			);

			if !errors.errors.is_empty() {
				unreachable!("errors when calling loop")
			}

			if let Some(result) = result {
				match result {
					EventResult::Continue { carry: 0 } => {
						break 'inner_loop;
					}
					EventResult::Break { carry: 0 } => {
						break 'main_iterations;
					}
					EventResult::Continue { carry } => {
						return Some(EventResult::Continue { carry: carry - 1 })
					}
					EventResult::Break { carry } => {
						return Some(EventResult::Break { carry: carry - 1 })
					}
					e @ (EventResult::Return(..) | EventResult::Throw) => return Some(e),
				}
			}
		}
	}

	None
}

/// Denotes values at the end of a loop
/// TODO Cow
struct Values {
	pub variable_values: HashMap<VariableId, TypeId>,
	pub _properties_values: HashMap<
		TypeId,
		Vec<(
			crate::context::facts::Publicity,
			crate::types::properties::PropertyKey<'static>,
			crate::PropertyValue,
		)>,
	>,
}

/// Not quite a "Hoare triple"
#[derive(Debug, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub struct LoopStructure {
	pub start: TypeId,
	pub increment_by: TypeId,
	pub roof: TypeId,
}

impl LoopStructure {
	pub(crate) fn specialise<T: TypeArgumentStore>(
		self,
		arguments: &mut T,
		// TODO temp
		environment: &mut Environment,
		types: &mut TypeStore,
	) -> Self {
		Self {
			start: substitute(self.start, arguments, environment, types),
			increment_by: substitute(self.increment_by, arguments, environment, types),
			roof: substitute(self.roof, arguments, environment, types),
		}
	}

	pub fn calculate_iterations(self, types: &TypeStore) -> Result<usize, Self> {
		let values = (
			types.get_type_by_id(self.start),
			types.get_type_by_id(self.increment_by),
			types.get_type_by_id(self.roof),
		);

		if let (
			Type::Constant(Constant::Number(start)),
			Type::Constant(Constant::Number(increments_by)),
			Type::Constant(Constant::Number(roof)),
		) = values
		{
			let iterations = (roof - start) / increments_by;
			// crate::utils::notify!(
			// 	"Evaluating iteration: start={}, end={}, increment={}, iterations={}",
			// 	start,
			// 	end,
			// 	increment,
			// 	iterations
			// );
			Ok(iterations.ceil() as usize)
		} else {
			crate::utils::notify!("Iterations was {:?}", values);
			Err(self)
		}
	}
}

fn calculate_result_of_loop(
	condition: TypeId,
	loop_variables: Option<&HashMap<VariableId, (TypeId, TypeId)>>,
	parent_environment: &Environment,
	types: &TypeStore,
	inside_loop: &Values,
) -> Result<LoopStructure, ()> {
	let condition_ty = types.get_type_by_id(condition);

	crate::utils::notify!("condition is {:?}", condition_ty);

	// TODO some other cases
	// - and for less than equal
	if let Type::Constructor(Constructor::CanonicalRelationOperator {
		lhs: less_than_reference_type_id,
		operator: CanonicalEqualityAndInequality::LessThan,
		rhs: roof,
	}) = condition_ty
	{
		// TODO sort by constant. Assumed here that dependent is on the LHS

		let reference_ty = types.get_type_by_id(*less_than_reference_type_id);
		// TODO what about properties etc
		if let Type::RootPolyType(PolyNature::FreeVariable {
			reference: less_than_reference,
			based_on: _,
		}) = reference_ty
		{
			if let RootReference::Variable(possible_changing_variable_id) = less_than_reference {
				let roof_ty = types.get_type_by_id(*roof);
				// TODO temp
				let roof: TypeId = if let Type::RootPolyType(PolyNature::FreeVariable {
					reference: RootReference::Variable(roof_id),
					based_on: _,
				}) = roof_ty
				{
					let changed = if let Some((_start, _end)) =
						loop_variables.as_ref().and_then(|vs| vs.get(roof_id).copied())
					{
						crate::utils::notify!("Found loop variables");
						false
					} else if let Some(inside) = inside_loop.variable_values.get(roof_id) {
						crate::utils::notify!(
							"Found loop here {:?}",
							types.get_type_by_id(*inside)
						);
						false
					} else {
						crate::utils::notify!("Here, roof not changed");
						true
					};

					if changed {
						if let Some((_start, end)) =
							loop_variables.as_ref().and_then(|vs| vs.get(roof_id).copied())
						{
							end
						} else {
							*parent_environment.facts.variable_current_value.get(roof_id).unwrap()
						}
					} else {
						crate::utils::notify!("Roof changed in loop");
						return Err(());
					}
				} else if let Type::Constant(_) = roof_ty {
					*roof
				} else {
					return Err(());
				};

				let value_after_running_expressions_in_loop = types.get_type_by_id(
					if let Some((_start, end)) = loop_variables
						.as_ref()
						.and_then(|vs| vs.get(possible_changing_variable_id).copied())
					{
						end
					} else {
						*inside_loop.variable_values.get(possible_changing_variable_id).unwrap()
					},
				);

				// crate::utils::notify!(
				// 	"incremented is {:?}",
				// 	value_after_running_expressions_in_loop
				// );

				// Looking at incrementor
				if let Type::Constructor(Constructor::BinaryOperator {
					lhs: assignment,
					operator: _,
					rhs: increments_by,
				}) = value_after_running_expressions_in_loop
				{
					debug_assert!(
						assignment == less_than_reference_type_id,
						"incrementor not the same as condition?"
					);

					let start = if let Some((start, _end)) = loop_variables
						.as_ref()
						.and_then(|vs| vs.get(possible_changing_variable_id).copied())
					{
						start
					} else {
						// let id = if let RootReference::Variable(id) = reference {
						// 	id
						// } else {
						// 	unreachable!("this")
						// };
						*parent_environment
							.facts
							.variable_current_value
							.get(possible_changing_variable_id)
							.unwrap()
					};

					return Ok(LoopStructure { start, roof, increment_by: *increments_by });
				}
			} else {
				crate::utils::notify!("{:?} has no max", roof);
			}
		} else {
			crate::utils::notify!("LHS {:?} is not free variable ", reference_ty);
		}
	}
	Err(())
}
