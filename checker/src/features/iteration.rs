//! This contains logic for synthesising iteration structures (`for`, `while`, `do {} ... while`, etc)
//! and running them (sometimes)

use std::collections::HashMap;

use source_map::{BaseSpan, Nullable, SpanWithSource};

use crate::{
	context::{
		environment::Label, invocation::InvocationContext, CallCheckingBehavior,
		ClosedOverReferencesInScope, Environment, LocalInformation, Scope,
	},
	events::{
		application::ApplicationInput, apply_events, ApplicationResult, Event, FinalEvent,
		RootReference,
	},
	features::{functions::ClosedOverVariables, operations::CanonicalEqualityAndInequality},
	types::{
		calling::{CallingContext, CallingDiagnostics},
		properties::get_properties_on_single_type,
		substitute, Constructor, ObjectNature, PolyNature, SubstitutionArguments, TypeStore,
	},
	CheckingData, Constant, Type, TypeId, VariableId,
};

/// The type of iteration to synthesis
#[derive(Clone, Copy)]
pub enum IterationBehavior<'a, A: crate::ASTImplementation> {
	While(&'a A::MultipleExpression<'a>),
	/// Same as above but run the body first
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
	position: SpanWithSource,
) {
	let application_input = ApplicationInput {
		this_value: crate::types::calling::ThisValue::UseParent,
		call_site: position,
		max_inline: checking_data.options.max_inline_count,
	};

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

					let values = super::narrowing::narrow_based_on_expression_into_vec(
						condition,
						false,
						environment,
						&mut checking_data.types,
					);

					crate::utilities::notify!("{:?}", values);

					environment.info.narrowed_values = values;

					// TODO not always needed
					add_loop_described_break_event(
						condition,
						position,
						&mut environment.info.events,
					);

					// TODO narrowing
					loop_body(environment, checking_data);

					condition
				},
			);

			let (
				LocalInformation { variable_current_value, current_properties, events, .. },
				closes_over,
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

			let mut diagnostics = CallingDiagnostics::default();

			run_iteration_block(
				IterationKind::Condition { under: fixed_iterations.ok(), postfix_condition: false },
				&events,
				&application_input,
				RunBehavior::References(closes_over),
				&mut SubstitutionArguments::new_arguments_for_use_in_loop(),
				environment,
				&mut InvocationContext::new_empty(),
				&mut diagnostics,
				&mut checking_data.types,
			);

			diagnostics
				.append_to(CallingContext::Iteration, &mut checking_data.diagnostics_container);

			// if let ApplicationResult::Interrupt(early_return) = run_iteration_block {
			// 	crate::utilities::notify!("Loop returned {:?}", early_return);
			// 	environment.info.events.push(Event::FinalEvent(early_return));
			// }
		}
		IterationBehavior::DoWhile(condition) => {
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
					add_loop_described_break_event(
						condition,
						position,
						&mut environment.info.events,
					);

					condition
				},
			);

			let (
				LocalInformation { variable_current_value, current_properties, events, .. },
				closes_over,
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

			let mut diagnostics = CallingDiagnostics::default();

			run_iteration_block(
				IterationKind::Condition { under: fixed_iterations.ok(), postfix_condition: true },
				&events,
				&application_input,
				RunBehavior::References(closes_over),
				&mut SubstitutionArguments::new_arguments_for_use_in_loop(),
				environment,
				&mut InvocationContext::new_empty(),
				&mut diagnostics,
				&mut checking_data.types,
			);

			diagnostics
				.append_to(CallingContext::Iteration, &mut checking_data.diagnostics_container);

			// if let ApplicationResult::Interrupt(early_return) = run_iteration_block {
			// 	todo!("{early_return:?}")
			// }
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

									let values =
										super::narrowing::narrow_based_on_expression_into_vec(
											condition,
											false,
											environment,
											&mut checking_data.types,
										);

									crate::utilities::notify!(
										"Narrowed values in loop {:?}",
										values
									);
									environment.info.narrowed_values = values;

									// TODO not always needed
									add_loop_described_break_event(
										condition,
										position,
										&mut environment.info.events,
									);

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

						let values = super::narrowing::narrow_based_on_expression_into_vec(
							condition,
							false,
							environment,
							&mut checking_data.types,
						);

						environment.info.narrowed_values = values;

						(condition, events, dependent_variables)
					},
				);

			let (
				LocalInformation { variable_current_value, current_properties, events, .. },
				closes_over,
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

			let mut diagnostics = CallingDiagnostics::default();

			run_iteration_block(
				IterationKind::Condition { under: fixed_iterations.ok(), postfix_condition: false },
				&events,
				&application_input,
				RunBehavior::References(closes_over),
				&mut SubstitutionArguments::new_arguments_for_use_in_loop(),
				environment,
				&mut InvocationContext::new_empty(),
				&mut diagnostics,
				&mut checking_data.types,
			);

			diagnostics
				.append_to(CallingContext::Iteration, &mut checking_data.diagnostics_container);
			// if let ApplicationResult::Interrupt(early_return) = run_iteration_block {
			// 	todo!("{early_return:?}")
			// }
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
					let arguments = crate::VariableRegisterArguments {
						// TODO based on LHS
						constant: true,
						space: None,
						initial_value: Some(variable),
						allow_reregistration: false,
					};
					A::declare_and_assign_to_fields(lhs, environment, checking_data, arguments);
					loop_body(environment, checking_data);
				},
			);

			let (LocalInformation { events, .. }, closes_over) = result.unwrap();

			let mut diagnostics = CallingDiagnostics::default();

			run_iteration_block(
				IterationKind::Properties { on, variable },
				&events,
				&application_input,
				RunBehavior::References(closes_over),
				&mut SubstitutionArguments::new_arguments_for_use_in_loop(),
				environment,
				&mut InvocationContext::new_empty(),
				&mut diagnostics,
				&mut checking_data.types,
			);

			diagnostics
				.append_to(CallingContext::Iteration, &mut checking_data.diagnostics_container);

			// if let ApplicationResult::Interrupt(early_return) = run_iteration_block {
			// 	todo!("{early_return:?}")
			// }
		}
		IterationBehavior::ForOf { lhs: _, rhs } => {
			let _position = A::expression_position(rhs).with_source(environment.get_source());
			let _rhs = A::synthesise_expression(rhs, TypeId::ANY_TYPE, environment, checking_data);

			// let _ = IteratorHelper::from_type(rhs, environment, checking_data, position);
			todo!()
		}
	}
}

/// Technically the `max_iterations` should make this obsolete
fn add_loop_described_break_event(
	condition: TypeId,
	position: SpanWithSource,
	events: &mut Vec<Event>,
) {
	let break_event =
		Event::Conditionally { condition, truthy_events: 0, otherwise_events: 1, position };
	events.push(break_event);
	events.push(FinalEvent::Break { position, carry: 0 }.into());
	events.push(Event::EndOfControlFlow(1));
}

pub enum InitialVariablesInput {
	Calculated,
	Compute(ClosedOverReferencesInScope),
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

pub enum RunBehavior {
	/// From events
	Run,
	/// For initial synthesis
	References(ClosedOverReferencesInScope),
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_iteration_block(
	condition: IterationKind,
	events: &[Event],
	input: &ApplicationInput,
	initial: RunBehavior,
	type_arguments: &mut SubstitutionArguments,
	top_environment: &mut Environment,
	invocation_context: &mut InvocationContext,
	errors: &mut CallingDiagnostics,
	types: &mut TypeStore,
) -> Option<ApplicationResult> {
	// {
	// 	let mut s = String::new();
	// 	debug_effects(&mut s, events, types, top_environment, 0, true);
	// 	crate::utilities::notify!("Applying:\n{}", s);
	// }

	match condition {
		IterationKind::Condition { postfix_condition, under } => {
			// {
			// 	let mut buf = String::new();
			// 	crate::types::printing::debug_effects(
			// 		&mut buf,
			// 		&events,
			// 		types,
			// 		top_environment,
			// 		true,
			// 	);
			// 	crate::utilities::notify!("events in iteration = {}", buf);
			// 	// crate::utilities::notify!("Iteration events: {:#?}", events);
			// }

			crate::utilities::notify!("under={:?}", under);

			let non_exorbitant_amount_of_iterations = under
				.and_then(|under| under.calculate_iterations(types).ok())
				.and_then(|iterations| {
					let events_to_be_applied = iterations * events.len();

					crate::utilities::notify!(
						"applying {:?} events (max {:?})",
						events_to_be_applied,
						input.max_inline
					);
					(events_to_be_applied < input.max_inline as usize).then_some(iterations)
				});

			if let Some(mut iterations) = non_exorbitant_amount_of_iterations {
				// These bodies always run at least once. TODO is there a better way?
				if postfix_condition {
					iterations += 1;
				}

				crate::utilities::notify!("Running {} times", iterations);

				run_iteration_loop(
					invocation_context,
					iterations,
					events,
					input,
					type_arguments,
					top_environment,
					types,
					errors,
					|_, _| {},
				)
			} else {
				evaluate_unknown_iteration_for_loop(
					events,
					initial,
					condition,
					type_arguments,
					invocation_context,
					top_environment,
					types,
				);
				None
			}
		}
		IterationKind::Properties { on, variable } => {
			// Or exact ...?. TODO split ors
			if let Type::Object(ObjectNature::RealDeal) = types.get_type_by_id(on) {
				let properties = get_properties_on_single_type(
					on,
					types,
					top_environment,
					true,
					TypeId::ANY_TYPE,
				);
				let mut n = 0;
				run_iteration_loop(
					invocation_context,
					properties.len(),
					events,
					input,
					type_arguments,
					top_environment,
					types,
					errors,
					|type_arguments, types| {
						let (_publicity, property, _value) = &properties[n];
						let property_key_as_type = property.into_type(types);

						type_arguments.set_during_application(variable, property_key_as_type);

						n += 1;
					},
				)
			} else {
				evaluate_unknown_iteration_for_loop(
					events,
					initial,
					condition,
					type_arguments,
					invocation_context,
					top_environment,
					types,
				);
				None
			}
		}
		IterationKind::Iterator { .. } => {
			// if let Some(mut iterations) = non_exorbitant_amount_of_iterations {
			// 	tod
			// } else {
			evaluate_unknown_iteration_for_loop(
				events,
				initial,
				condition,
				type_arguments,
				invocation_context,
				top_environment,
				types,
			);
			None
			// }
		}
	}
}

#[allow(clippy::too_many_arguments)]
fn run_iteration_loop(
	invocation_context: &mut InvocationContext,
	iterations: usize,
	events: &[Event],
	input: &ApplicationInput,
	type_arguments: &mut SubstitutionArguments,
	top_environment: &mut Environment,
	types: &mut TypeStore,
	errors: &mut CallingDiagnostics,
	// For `for in` (TODO for of)
	mut each_iteration: impl for<'a> FnMut(&'a mut SubstitutionArguments, &mut TypeStore),
) -> Option<ApplicationResult> {
	invocation_context.new_loop_iteration(|invocation_context| {
		crate::utilities::notify!("running inline events: {:#?}", events);
		for _ in 0..iterations {
			each_iteration(type_arguments, types);
			let result = apply_events(
				events,
				input,
				type_arguments,
				top_environment,
				invocation_context,
				types,
				errors,
			);

			if let Some(result) = result {
				crate::utilities::notify!("{:?}", result);
				match result {
					ApplicationResult::Continue { carry: 0, position: _ } => {
						continue;
					}
					ApplicationResult::Break { carry: 0, position: _ } => {
						break;
					}
					ApplicationResult::Continue { carry, position } => {
						return Some(ApplicationResult::Continue { carry: carry - 1, position });
					}
					ApplicationResult::Break { carry, position } => {
						return Some(ApplicationResult::Break { carry: carry - 1, position });
					}
					result => {
						return Some(result);
					}
				}
			}
		}

		None
	})
}

fn evaluate_unknown_iteration_for_loop(
	events: &[Event],
	initial: RunBehavior,
	kind: IterationKind,
	type_arguments: &mut SubstitutionArguments,
	invocation_context: &mut InvocationContext,
	top_environment: &mut Environment,
	types: &mut TypeStore,
) {
	let initial = match initial {
		RunBehavior::Run => ClosedOverVariables(Default::default()),
		RunBehavior::References(v) => {
			crate::features::create_closed_over_references(&v, top_environment, types)
		}
	};

	let mut calling_diagnostics = CallingDiagnostics::default();

	// Make rest of scope aware of changes under the loop
	// TODO can skip if at the end of a function
	let _res = invocation_context.new_unknown_target(|invocation_context| {
		// TODO
		let max_inline = 10;

		apply_events(
			events,
			&ApplicationInput {
				this_value: crate::types::calling::ThisValue::UseParent,
				call_site: BaseSpan::NULL,
				max_inline,
			},
			type_arguments,
			top_environment,
			invocation_context,
			types,
			&mut calling_diagnostics,
		)
	});

	// add event
	{
		{
			// let _in_definition = top_environment.parents_iter().any(|env| {
			// 	matches!(
			// 		env,
			// 		GeneralContext::Syntax(crate::context::Context {
			// 			context_type: Syntax { scope: Scope::DefinitionModule { .. }, .. },
			// 			..
			// 		})
			// 	)
			// });

			// if !in_definition {
			// 	crate::utilities::notify!("adding iteration events: {:#?}", events);
			// }
		}

		let get_latest_info = invocation_context.get_latest_info(top_environment);
		get_latest_info.events.push(Event::Iterate {
			kind,
			initial,
			iterate_over: events.len() as u32,
		});

		get_latest_info.events.extend(events.iter().cloned());
		get_latest_info.events.push(Event::EndOfControlFlow(events.len() as u32));
	}
}

/// Denotes values at the end of a loop
/// TODO Cow
struct Values {
	pub variable_values: HashMap<VariableId, TypeId>,
	pub _properties_values: HashMap<
		TypeId,
		Vec<(
			crate::types::properties::Publicity,
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
	pub(crate) fn specialise(
		self,
		arguments: &SubstitutionArguments,
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
		self.calculate_iterations_f64(types).map(|result| result as usize)
	}

	pub fn known_to_never_exist(self, types: &TypeStore) -> bool {
		self.calculate_iterations_f64(types).is_ok_and(f64::is_infinite)
	}

	fn calculate_iterations_f64(self, types: &TypeStore) -> Result<f64, Self> {
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
			// crate::utilities::notify!(
			// 	"Evaluating iteration: start={}, end={}, increment={}, iterations={}",
			// 	start,
			// 	end,
			// 	increment,
			// 	iterations
			// );
			Ok(iterations.ceil())
		} else {
			// crate::utilities::notify!("Iterations was {:?}", values);
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

	crate::utilities::notify!("condition is {:?}", condition_ty);

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
		crate::utilities::notify!("{:?}", reference_ty);

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
						crate::utilities::notify!("Found loop variables");
						false
					} else if let Some(inside) = inside_loop.variable_values.get(roof_id) {
						crate::utilities::notify!(
							"Found loop here {:?}",
							types.get_type_by_id(*inside)
						);
						false
					} else {
						crate::utilities::notify!("Here, roof not changed");
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
						crate::utilities::notify!("Roof changed in loop");
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
						inside_loop
							.variable_values
							.get(possible_changing_variable_id)
							.or_else(|| {
								// TODO wip, value doesn't change?
								parent_environment
									.info
									.variable_current_value
									.get(possible_changing_variable_id)
							})
							.copied()
							.unwrap_or(TypeId::ERROR_TYPE)
					},
				);

				// crate::utilities::notify!(
				// 	"incremented is {:?}",
				// 	value_after_running_expressions_in_loop
				// );

				// Looking at incrementor
				if let Type::Constructor(Constructor::BinaryOperator {
					lhs: assignment,
					operator: _,
					rhs: increments_by,
					result: _,
				}) = value_after_running_expressions_in_loop
				{
					let assignment = crate::types::helpers::get_origin(*assignment, types);
					debug_assert!(
						assignment == *less_than_reference_type_id,
						"incrementor free variable type not the same as condition free variable type?"
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
				crate::utilities::notify!("{:?} has no max", roof);
			}
		} else {
			crate::utilities::notify!("LHS {:?} is not free variable ", reference_ty);
		}
	}
	Err(())
}
