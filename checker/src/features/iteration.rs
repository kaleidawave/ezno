use std::collections::HashMap;

use crate::{
	context::{
		environment::Label, get_value_of_variable, information::get_properties_on_type,
		invocation::InvocationContext, CallCheckingBehavior,
	},
	events::{
		application::ErrorsAndInfo, apply_event, Event, FinalEvent, InitialVariables, RootReference,
	},
	features::operations::CanonicalEqualityAndInequality,
	types::{
		poly_types::{generic_type_arguments::TypeArgumentStore, FunctionTypeArguments},
		substitute, Constructor, ObjectNature, PolyNature, TypeStore,
	},
	CheckingData, Constant, Environment, LocalInformation, Scope, Type, TypeId, VariableId,
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
				Scope::Iteration { label },
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
						true_events: Default::default(),
						else_events: Box::new([
							FinalEvent::Break { position: None, carry: 0 }.into()
						]),
						position: None,
					};
					environment.info.events.push(break_event);

					loop_body(environment, checking_data);

					// crate::utils::notify!("Loop does {:#?}", environment.info.events);

					condition
				},
			);

			let (
				LocalInformation { variable_current_value, current_properties, events, .. },
				_closes_over,
			) = result.unwrap();

			let loop_info = Values {
				variable_values: variable_current_value,
				_properties_values: current_properties,
			};

			let fixed_iterations = calculate_result_of_loop(
				condition,
				None,
				environment,
				&checking_data.types,
				&loop_info,
			);

			let mut errors_and_info = ErrorsAndInfo::default();

			if let Some(early_return) = run_iteration_block(
				IterationKind::Condition { under: fixed_iterations.ok(), postfix_condition: false },
				events,
				InitialVariablesInput::Compute,
				&mut FunctionTypeArguments::new_arguments_for_use_in_loop(),
				environment,
				&mut InvocationContext::new_empty(),
				&mut errors_and_info,
				&mut checking_data.types,
			) {
				crate::utils::notify!("Loop returned {:?}", early_return);
				environment.info.events.push(Event::FinalEvent(early_return));
			}

			// TODO for other blocks
			for warning in errors_and_info.warnings {
				checking_data.diagnostics_container.add_info(
					crate::diagnostics::Diagnostic::Position {
						reason: warning.0,
						// TODO temp
						position: source_map::Nullable::NULL,
						kind: crate::diagnostics::DiagnosticKind::Info,
					},
				);
			}
		}
		IterationBehavior::DoWhile(condition) => {
			// let is_do_while = matches!(behavior, IterationBehavior::DoWhile(..));

			// Same as above but condition is evaluated at end. Don't know whether events should be evaluated once...?
			let (condition, result, ..) = environment.new_lexical_environment_fold_into_parent(
				Scope::Iteration { label },
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
						true_events: Default::default(),
						else_events: Box::new([
							FinalEvent::Break { position: None, carry: 0 }.into()
						]),
						position: None,
					};
					environment.info.events.push(break_event);

					condition
				},
			);

			let (
				LocalInformation { variable_current_value, current_properties, events, .. },
				_closes_over,
			) = result.unwrap();

			let loop_info = Values {
				variable_values: variable_current_value,
				_properties_values: current_properties,
			};

			let fixed_iterations = calculate_result_of_loop(
				condition,
				None,
				environment,
				&checking_data.types,
				&loop_info,
			);

			if let Some(early_return) = run_iteration_block(
				IterationKind::Condition { under: fixed_iterations.ok(), postfix_condition: true },
				events,
				InitialVariablesInput::Compute,
				&mut FunctionTypeArguments::new_arguments_for_use_in_loop(),
				environment,
				&mut InvocationContext::new_empty(),
				// TODO shouldn't be needed
				&mut Default::default(),
				&mut checking_data.types,
			) {
				todo!("{early_return:?}")
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
											.info
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
								Scope::Iteration { label },
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
										true_events: Default::default(),
										else_events: Box::new([FinalEvent::Break {
											position: None,
											carry: 0,
										}
										.into()]),
										position: None,
									};
									environment.info.events.push(break_event);

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
													.info
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

			let (
				LocalInformation { variable_current_value, current_properties, events, .. },
				_closes_over,
			) = result.unwrap();

			let loop_info = Values {
				variable_values: variable_current_value,
				_properties_values: current_properties,
			};

			let fixed_iterations = calculate_result_of_loop(
				condition,
				Some(&dependent_variables),
				environment,
				&checking_data.types,
				&loop_info,
			);

			for (var, (start, _)) in dependent_variables {
				environment.info.variable_current_value.insert(var, start);
			}

			if let Some(early_return) = run_iteration_block(
				IterationKind::Condition { under: fixed_iterations.ok(), postfix_condition: false },
				events,
				InitialVariablesInput::Compute,
				&mut FunctionTypeArguments::new_arguments_for_use_in_loop(),
				environment,
				&mut InvocationContext::new_empty(),
				// TODO shouldn't be needed
				&mut Default::default(),
				&mut checking_data.types,
			) {
				todo!("{early_return:?}")
			}
		}
		IterationBehavior::ForIn { lhs, rhs } => {
			let on = A::synthesise_multiple_expression(
				rhs,
				TypeId::ANY_TYPE,
				environment,
				checking_data,
			);

			let variable =
				checking_data.types.register_type(Type::RootPolyType(PolyNature::Parameter {
					// TODO can do `"prop1" | "prop2" | "prop3" | ...`
					fixed_to: TypeId::STRING_TYPE,
				}));

			let ((), result, ..) = environment.new_lexical_environment_fold_into_parent(
				Scope::Iteration { label },
				checking_data,
				|environment, checking_data| {
					A::declare_and_assign_to_fields(lhs, environment, checking_data, variable);
					loop_body(environment, checking_data);
				},
			);

			let events = result.unwrap().0.events;

			if let Some(early_return) = run_iteration_block(
				IterationKind::Properties { on, variable },
				events,
				InitialVariablesInput::Compute,
				&mut FunctionTypeArguments::new_arguments_for_use_in_loop(),
				environment,
				&mut InvocationContext::new_empty(),
				// TODO shouldn't be needed
				&mut Default::default(),
				&mut checking_data.types,
			) {
				todo!("{early_return:?}")
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
	Properties {
		on: TypeId,
		/// To substitute
		variable: TypeId,
	},
	Iterator {
		on: TypeId,
		/// To substitute
		variable: TypeId,
	},
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_iteration_block(
	condition: IterationKind,
	events: Vec<Event>,
	initial: InitialVariablesInput,
	type_arguments: &mut FunctionTypeArguments,
	top_environment: &mut Environment,
	invocation_context: &mut InvocationContext,
	errors: &mut ErrorsAndInfo,
	types: &mut TypeStore,
) -> Option<FinalEvent> {
	/// TODO via config and per line
	const MAX_ITERATIONS: usize = 100;

	match condition {
		IterationKind::Condition { postfix_condition, under } => {
			// let mut buf = String::new();
			// crate::types::printing::debug_effects(
			// 	&mut buf,
			// 	&events,
			// 	&checking_data.types,
			// 	&top_environment.as_general_context(),
			// 	true,
			// );
			// crate::utils::notify!("events in iteration = {}", buf);

			// TODO if depends on something maybe limit to 10?
			let non_exorbitant_amount_of_iterations = under
				.and_then(|under| under.calculate_iterations(types).ok())
				.and_then(|iterations| (iterations < MAX_ITERATIONS).then_some(iterations));

			if let Some(mut iterations) = non_exorbitant_amount_of_iterations {
				// These bodies always run at least once. TODO is there a better way?
				if postfix_condition {
					iterations += 1;
				}

				// crate::utils::notify!(
				// 	"Evaluating a constant amount of iterations {:?}",
				// 	iterations
				// );

				if let InitialVariablesInput::Calculated(initial) = initial {
					for (variable_id, initial_value) in &initial {
						invocation_context
							.get_latest_info(top_environment)
							.variable_current_value
							.insert(*variable_id, *initial_value);
					}
				}

				for _ in 0..iterations {
					let result = evaluate_single_loop_iteration(
						&events,
						type_arguments,
						top_environment,
						invocation_context,
						errors,
						types,
					);

					if let Some(result) = result {
						match result {
							FinalEvent::Continue { carry: 0, position: _ } => {}
							FinalEvent::Break { carry: 0, position: _ } => {
								break;
							}
							FinalEvent::Continue { carry, position } => {
								return Some(FinalEvent::Continue { carry: carry - 1, position })
							}
							FinalEvent::Break { carry, position } => {
								return Some(FinalEvent::Break { carry: carry - 1, position })
							}
							e @ (FinalEvent::Return { .. } | FinalEvent::Throw { .. }) => {
								return Some(e)
							}
						}
					}
				}

				None
			} else {
				evaluate_unknown_iteration_for_loop(
					initial,
					condition,
					invocation_context,
					top_environment,
					events,
					types,
				)
			}
		}
		IterationKind::Properties { on, variable } => {
			if let Type::Object(ObjectNature::RealDeal) = types.get_type_by_id(on) {
				for (_publicity, property, _value) in get_properties_on_type(on, top_environment) {
					crate::utils::notify!("Property: {:?}", property);

					let property_key_as_type = match property {
						crate::types::properties::PropertyKey::String(str) => {
							types.new_constant_type(Constant::String(str.to_string()))
						}
						crate::types::properties::PropertyKey::Type(ty) => ty,
					};

					type_arguments.set_id_from_event_application(variable, property_key_as_type);

					// TODO enumerable
					let result = evaluate_single_loop_iteration(
						&events,
						type_arguments,
						top_environment,
						invocation_context,
						errors,
						types,
					);

					if let Some(result) = result {
						match result {
							FinalEvent::Continue { carry: 0, position: _ } => {}
							FinalEvent::Break { carry: 0, position: _ } => {
								break;
							}
							FinalEvent::Continue { carry, position } => {
								return Some(FinalEvent::Continue { carry: carry - 1, position })
							}
							FinalEvent::Break { carry, position } => {
								return Some(FinalEvent::Break { carry: carry - 1, position })
							}
							e @ (FinalEvent::Return { .. } | FinalEvent::Throw { .. }) => {
								return Some(e)
							}
						}
					}
				}
				None
			} else {
				evaluate_unknown_iteration_for_loop(
					initial,
					condition,
					invocation_context,
					top_environment,
					events,
					types,
				)
			}
		}
		IterationKind::Iterator { .. } => todo!(),
	}
}

fn evaluate_unknown_iteration_for_loop(
	initial: InitialVariablesInput,
	kind: IterationKind,
	invocation_context: &mut InvocationContext,
	top_environment: &mut crate::context::Context<crate::context::Syntax<'_>>,
	events: Vec<Event>,
	types: &mut TypeStore,
) -> Option<FinalEvent> {
	let initial = match initial {
		InitialVariablesInput::Calculated(initial) => {
			for (id, value) in &initial {
				invocation_context
					.get_latest_info(top_environment)
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
						top_environment,
						*variable_id,
						None::<&crate::types::poly_types::FunctionTypeArguments>,
					)
					.unwrap();

					crate::utils::notify!(
						"setting '{}' to have initial type {}",
						top_environment.get_variable_name(*variable_id),
						crate::types::printing::print_type(
							value_before_iterations,
							types,
							top_environment,
							true
						)
					);

					initial.insert(*variable_id, value_before_iterations);

					top_environment
						.info
						.variable_current_value
						.insert(*variable_id, *free_variable_id);
				}
			}
			initial
		}
	};

	invocation_context.get_latest_info(top_environment).events.push(Event::Iterate {
		kind,
		initial,
		iterate_over: events.into_boxed_slice(),
	});

	// TODO can skip if at the end of a function
	// for event in events {
	// 	let result = apply_event_unknown(
	// 		event,
	// 		crate::behavior::functions::ThisValue::UseParent,
	// 		&mut arguments,
	// 		top_environment,
	// 		&mut crate::context::calling::InvocationContext::new_default(),
	// 		&mut checking_data.types,
	// 	);
	// }
	None
}

fn evaluate_single_loop_iteration(
	events: &[Event],
	arguments: &mut FunctionTypeArguments,
	top_environment: &mut Environment,
	invocation_context: &mut InvocationContext,
	errors: &mut ErrorsAndInfo,
	types: &mut TypeStore,
) -> Option<FinalEvent> {
	let final_event = invocation_context.new_loop_iteration(|invocation_context| {
		for event in events {
			let result = apply_event(
				event.clone(),
				crate::features::functions::ThisValue::UseParent,
				arguments,
				top_environment,
				invocation_context,
				types,
				errors,
			);
			if result.is_some() {
				return result;
			}
		}
		None
	});

	if !errors.errors.is_empty() {
		// unreachable!("errors when calling loop")
		crate::utils::notify!("errors when calling loop");
	}

	final_event
}

/// Denotes values at the end of a loop
/// TODO Cow
struct Values {
	pub variable_values: HashMap<VariableId, TypeId>,
	pub _properties_values: HashMap<
		TypeId,
		Vec<(
			crate::context::information::Publicity,
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
		top_environment: &mut Environment,
		types: &mut TypeStore,
	) -> Self {
		Self {
			start: substitute(self.start, arguments, top_environment, types),
			increment_by: substitute(self.increment_by, arguments, top_environment, types),
			roof: substitute(self.roof, arguments, top_environment, types),
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
			// crate::utils::notify!("Iterations was {:?}", values);
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
							*parent_environment.info.variable_current_value.get(roof_id).unwrap()
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
							.info
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
