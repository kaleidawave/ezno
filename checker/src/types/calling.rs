use source_map::{BaseSpan, Nullable, SpanWithSource};

use crate::{
	context::{
		information::InformationChain, invocation::CheckThings, CallCheckingBehavior, Environment,
		Logical, MissingOrToCalculate, PossibleLogical,
	},
	diagnostics::{TypeCheckError, TypeStringRepresentation, TDZ},
	events::{application::ErrorsAndInfo, apply_events, ApplicationResult, Event, RootReference},
	features::{
		constant_functions::{
			call_constant_function, CallSiteTypeArguments, ConstantFunctionError, ConstantOutput,
		},
		functions::{FunctionBehavior, ThisValue},
		objects::{ObjectBuilder, SpecialObjects},
	},
	subtyping::{
		type_is_subtype, type_is_subtype_with_generics, State, SubTypeResult, SubTypingMode,
		SubTypingOptions,
	},
	types::{
		functions::SynthesisedArgument, generics::substitution::SubstitutionArguments,
		get_structure_arguments_based_on_object_constraint, substitute, FunctionEffect,
		FunctionType, GenericChainLink, ObjectNature, PartiallyAppliedGenerics, Type,
	},
	FunctionId, GenericTypeParameters, ReadFromFS, SpecialExpressions, TypeId,
};

use super::{
	generics::{contributions::Contributions, generic_type_arguments::GenericArguments},
	get_constraint, is_type_constant,
	properties::PropertyKey,
	Constructor, GenericChain, PolyNature, TypeRestrictions, TypeStore,
};

pub struct CallingInput {
	/// Also depicts what happens with `this`
	pub called_with_new: CalledWithNew,
	pub call_site_type_arguments: Option<Vec<(TypeId, SpanWithSource)>>,
	pub call_site: SpanWithSource,
}

pub struct UnsynthesisedArgument<'a, A: crate::ASTImplementation> {
	pub spread: bool,
	pub expression: &'a A::Expression<'a>,
}

/// Intermediate type for calling a function
///
/// Generic arguments handled with `Logical::Implies`
#[derive(Debug)]
struct FunctionLike {
	pub(crate) function: FunctionId,
	/// For generic calls
	pub(crate) from: Option<TypeId>,
	/// From, maybe ignored if [`CalledWithNew`] overrides
	pub(crate) this_value: ThisValue,
}

/// TODO *result* name bad
pub struct CallingOutput {
	pub called: Option<FunctionId>,
	/// TODO should this be
	pub result: Option<ApplicationResult>,
	/// TODO is [`InfoDiagnostic`] the best type here?
	pub warnings: Vec<InfoDiagnostic>,
	pub special: Option<SpecialExpressions>,
	// pub special: Option<SpecialExpressions>,
	pub result_was_const_computation: bool,
}

pub struct BadCallOutput {
	/// This might be from specialisation
	pub returned_type: TypeId,
	pub errors: Vec<FunctionCallingError>,
}

/// Also synthesise arguments in terms of expected types
pub fn call_type_handle_errors<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	ty: TypeId,
	arguments: &[UnsynthesisedArgument<A>],
	input: CallingInput,
	environment: &mut Environment,
	checking_data: &mut crate::CheckingData<T, A>,
	_expected: TypeId,
) -> (TypeId, Option<SpecialExpressions>) {
	let call_site = input.call_site;

	// input.this_value
	let callable = get_logical_callable_from_type(ty, None, None, &checking_data.types);

	match callable {
		Ok(callable) => {
			// crate::utilities::notify!("{:?}", callable);

			// Fix as `.map` doesn't get this passed down
			let parent_arguments = if let Logical::Pure(FunctionLike {
				this_value: ThisValue::Passed(this_passed),
				..
			}) = callable
			{
				if let Type::Object(..) = checking_data.types.get_type_by_id(this_passed) {
					if let Some(arguments) = get_structure_arguments_based_on_object_constraint(
						this_passed,
						environment,
						&checking_data.types,
					) {
						crate::utilities::notify!("Here :)");
						Some(arguments.clone())
					} else {
						let prototype = environment
							.get_chain_of_info()
							.find_map(|facts| facts.prototypes.get(&this_passed))
							.copied();

						if prototype.is_some_and(|prototype| {
							checking_data.types.lookup_generic_map.contains_key(&prototype)
						}) {
							crate::utilities::notify!("Registering lookup for calling");

							Some(GenericArguments::LookUp { on: this_passed })
						} else {
							None
						}
					}
				} else {
					None
				}
			} else {
				None
			};

			let (arguments, type_argument_restrictions) =
				synthesise_argument_expressions_wrt_parameters(
					&callable,
					arguments,
					(input.call_site_type_arguments, parent_arguments.as_ref()),
					environment,
					checking_data,
				);

			let mut check_things = CheckThings { debug_types: checking_data.options.debug_types };
			let result = call_logical(
				callable,
				input.called_with_new,
				input.call_site,
				type_argument_restrictions,
				parent_arguments,
				arguments,
				environment,
				&mut checking_data.types,
				&mut check_things,
			);

			match result {
				Ok(CallingOutput {
					result,
					warnings,
					called: _,
					special,
					result_was_const_computation: _,
				}) => {
					for warning in warnings {
						checking_data.diagnostics_container.add_info(
							crate::diagnostics::Diagnostic::Position {
								reason: warning.0,
								position: call_site,
								kind: crate::diagnostics::DiagnosticKind::Info,
							},
						);
					}

					// if thrown_type != TypeId::NEVER_TYPE {
					// 	todo!()
					// 	// environment.context_type.state.append_termination(FinalEvent::Throw {
					// 	// 	thrown: thrown_type,
					// 	// 	position: call_site,
					// 	// });
					// }

					let returned_type = application_result_to_return_type(
						result,
						environment,
						&mut checking_data.types,
					);

					(returned_type, special)
				}
				Err(error) => {
					for error in error.errors {
						checking_data
							.diagnostics_container
							.add_error(TypeCheckError::FunctionCallingError(error));
					}
					(error.returned_type, None)
				}
			}
		}
		Err(MissingOrToCalculate::Error) => (TypeId::ERROR_TYPE, None),
		Err(MissingOrToCalculate::Infer { on: _ }) => {
			todo!("function calling inference")
		}
		Err(MissingOrToCalculate::Proxy(..)) => {
			todo!("calling proxy")
		}
		Err(MissingOrToCalculate::Missing) => {
			checking_data.diagnostics_container.add_error(TypeCheckError::FunctionCallingError(
				FunctionCallingError::NotCallable {
					calling: TypeStringRepresentation::from_type_id(
						ty,
						environment,
						&checking_data.types,
						checking_data.options.debug_types,
					),
					call_site,
				},
			));
			(TypeId::ERROR_TYPE, None)
		}
	}
}

/// TODO also return partial result
pub fn application_result_to_return_type(
	result: Option<ApplicationResult>,
	environment: &mut Environment,
	types: &mut TypeStore,
) -> TypeId {
	if let Some(result) = result {
		match result {
			// TODO
			ApplicationResult::Return { returned, position: _ } => returned,
			ApplicationResult::Throw { thrown, position } => {
				environment.throw_value(thrown, position);
				TypeId::NEVER_TYPE
			}
			ApplicationResult::Yield {} => todo!("Create generator object"),
			ApplicationResult::Or { on, truthy_result, otherwise_result } => {
				let truthy_result =
					application_result_to_return_type(Some(*truthy_result), environment, types);
				let otherwise_result =
					application_result_to_return_type(Some(*otherwise_result), environment, types);
				types.new_conditional_type(on, truthy_result, otherwise_result)
			}
			ApplicationResult::Continue { .. } | ApplicationResult::Break { .. } => {
				unreachable!("returning conditional or break (carry failed)")
			}
		}
	} else {
		TypeId::UNDEFINED_TYPE
	}
}

/// In events
pub(crate) fn call_type<E: CallCheckingBehavior>(
	on: TypeId,
	arguments: Vec<SynthesisedArgument>,
	input: &CallingInput,
	top_environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Result<CallingOutput, BadCallOutput> {
	// input.this_value,
	let callable = get_logical_callable_from_type(on, None, None, types).ok();
	try_call_logical(callable, input, arguments, None, top_environment, types, behavior, on)
}

/// This could possibly be done in one step
fn get_logical_callable_from_type(
	ty: TypeId,
	on: Option<ThisValue>,
	from: Option<TypeId>,
	types: &TypeStore,
) -> PossibleLogical<FunctionLike> {
	if ty == TypeId::ERROR_TYPE {
		return Err(MissingOrToCalculate::Error);
	}
	if ty == TypeId::ANY_TYPE {
		return Err(MissingOrToCalculate::Infer { on: from.unwrap() });
	}

	let le_ty = types.get_type_by_id(ty);

	// crate::utilities::notify!("ty1={:?} ({:?})", le_ty, ty);

	match le_ty {
		Type::Class { .. } | Type::Interface { .. } | Type::Constant(_) | Type::Object(_) => {
			Err(MissingOrToCalculate::Missing)
		}
		Type::And(_, _) => todo!(),
		Type::Or(left, right) => {
			let left = get_logical_callable_from_type(*left, on, from, types);
			let right = get_logical_callable_from_type(*right, on, from, types);
			Ok(Logical::Or {
				condition: TypeId::BOOLEAN_TYPE,
				left: Box::new(left),
				right: Box::new(right),
			})
		}
		Type::AliasTo { to, name: _, parameters } => {
			if parameters.is_some() {
				todo!()
			}
			get_logical_callable_from_type(*to, on, from, types)
		}
		Type::FunctionReference(f) => {
			let function = FunctionLike {
				// TODO
				function: *f,
				from,
				this_value: on.unwrap_or(ThisValue::UseParent),
			};
			Ok(Logical::Pure(function))
		}
		Type::SpecialObject(SpecialObjects::Function(f, t)) => {
			let this_value = on.unwrap_or(*t);
			let from = Some(from.unwrap_or(ty));
			Ok(Logical::Pure(FunctionLike { from, function: *f, this_value }))
		}
		Type::SpecialObject(SpecialObjects::ClassConstructor { constructor, .. }) => {
			Ok(Logical::Pure(FunctionLike {
				from,
				function: *constructor,
				this_value: ThisValue::UseParent,
			}))
		}
		Type::SpecialObject(so) => match so {
			crate::features::objects::SpecialObjects::Proxy { .. } => todo!(),
			_ => Err(MissingOrToCalculate::Missing),
		},
		Type::PartiallyAppliedGenerics(generic) => {
			get_logical_callable_from_type(generic.on, on, from, types).map(|res| {
				crate::utilities::notify!("Calling found {:?}", generic.arguments);
				Logical::Implies { on: Box::new(res), antecedent: generic.arguments.clone() }
			})
		}
		Type::Constructor(Constructor::Property { on, under: _, result, bind_this }) => {
			crate::utilities::notify!("Passing {:?}", on);

			let this_value = if *bind_this { ThisValue::Passed(*on) } else { ThisValue::UseParent };
			let result =
				get_logical_callable_from_type(*result, Some(this_value), Some(ty), types)?;

			let and_then = get_constraint(*on, types).and_then(|c| {
				if let Type::PartiallyAppliedGenerics(generic) = types.get_type_by_id(c) {
					Some(generic.arguments.clone())
				} else {
					None
				}
			});

			if let Some(antecedent) = and_then {
				Ok(Logical::Implies { on: Box::new(result), antecedent })
			} else {
				Ok(result)
			}
		}
		Type::RootPolyType(_) | Type::Constructor(_) => {
			let constraint = get_constraint(ty, types).unwrap();
			crate::utilities::notify!("Calling constructor / root poly type! {:?}", constraint);
			get_logical_callable_from_type(constraint, on, Some(ty), types)
		}
	}
}

#[allow(clippy::too_many_arguments)]
fn try_call_logical<E: CallCheckingBehavior>(
	callable: Option<Logical<FunctionLike>>,
	input: &CallingInput,
	arguments: Vec<SynthesisedArgument>,
	type_argument_restrictions: Option<TypeRestrictions>,
	top_environment: &mut Environment,
	types: &mut TypeStore,
	behavior: &mut E,
	on: TypeId,
) -> Result<CallingOutput, BadCallOutput> {
	if let Some(logical) = callable {
		call_logical(
			logical,
			input.called_with_new,
			input.call_site,
			type_argument_restrictions,
			None,
			arguments,
			top_environment,
			types,
			behavior,
		)
	} else {
		Err(BadCallOutput {
			returned_type: TypeId::ERROR_TYPE,
			errors: vec![FunctionCallingError::NotCallable {
				calling: crate::diagnostics::TypeStringRepresentation::from_type_id(
					on,
					top_environment,
					types,
					behavior.debug_types(),
				),
				call_site: input.call_site,
			}],
		})
	}
}

#[allow(clippy::too_many_arguments)]
fn call_logical<E: CallCheckingBehavior>(
	logical: Logical<FunctionLike>,
	called_with_new: CalledWithNew,
	call_site: SpanWithSource,
	explicit_type_arguments: Option<CallSiteTypeArguments>,
	structure_generics: Option<GenericArguments>,
	arguments: Vec<SynthesisedArgument>,
	top_environment: &mut Environment,
	types: &mut TypeStore,
	behavior: &mut E,
) -> Result<CallingOutput, BadCallOutput> {
	match logical {
		Logical::Pure(function) => {
			if let Some(function_type) = types.functions.get(&function.function) {
				let function_type = function_type.clone();

				let mut result = function_type.call(
					called_with_new,
					function.this_value,
					call_site,
					&arguments,
					explicit_type_arguments,
					structure_generics,
					top_environment,
					behavior,
					types,
					true,
				)?;

				// crate::utilities::notify!("result {:?} {:?}", result.called, result.returned_type);

				// is poly
				let is_poly_or_failed_const_call = matches!(
					function_type.effect,
					FunctionEffect::Unknown | FunctionEffect::InputOutput { .. }
				) || (matches!(function_type.effect, FunctionEffect::Constant { .. } if !result.result_was_const_computation));

				if is_poly_or_failed_const_call && function.from.is_some() {
					let on = function.from.unwrap();

					// if function_type.effect
					// This should be okay, constant or IO functions don't mutate their arguments...?
					if !matches!(
						&function_type.effect,
						FunctionEffect::Constant { .. } | FunctionEffect::InputOutput { .. }
					) {
						find_possible_mutations(&arguments, types, top_environment);
					}

					// match function_type.effect {
					// 	FunctionEffect::SideEffects { .. } => unreachable!(),
					// 	FunctionEffect::Constant { may_throw, .. }
					// 	| FunctionEffect::InputOutput { may_throw, .. } => {
					// 		if let Some(thrown_type) = may_throw {
					// 			// TODO not top
					// 			top_environment
					// 				.context_type
					// 				.state
					// 				.append_termination(ApplicationResult::may_throw(thrown_type));
					// 		}
					// 	}
					// 	FunctionEffect::Unknown => {
					// 		top_environment
					// 			.context_type
					// 			.state
					// 			.append_termination(ApplicationResult::may_throw(TypeId::ANY_TYPE));
					// 	}
					// }

					let with = arguments.clone().into_boxed_slice();
					let result_as_type = application_result_to_return_type(
						result.result.take(),
						top_environment,
						types,
					);
					let reflects_dependency = if is_type_constant(result_as_type, types) {
						None
					} else {
						let id = types.register_type(Type::Constructor(Constructor::Image {
							on,
							// TODO...
							with: with.clone(),
							result: result_as_type,
						}));

						Some(id)
					};

					result.result = Some(ApplicationResult::Return {
						returned: reflects_dependency.unwrap_or(result_as_type),
						position: call_site,
					});

					let possibly_thrown = if let FunctionEffect::InputOutput { may_throw, .. }
					| FunctionEffect::Constant { may_throw, .. } = &function_type.effect
					{
						crate::utilities::notify!("Here");
						*may_throw
					} else {
						None
					};

					behavior.get_latest_info(top_environment).events.push(Event::CallsType {
						on,
						with,
						timing: crate::events::CallingTiming::Synchronous,
						called_with_new,
						reflects_dependency,
						position: call_site,
						possibly_thrown,
					});
				}

				Ok(result)
			} else {
				panic!()
			}
		}
		Logical::Or { condition: based_on, left, right } => {
			if let (Ok(_left), Ok(_right)) = (*left, *right) {
				// let (truthy_result, otherwise_result) = behavior.evaluate_conditionally(
				// 	top_environment,
				// 	types,
				// 	based_on,
				// 	(left, right),
				// 	(explicit_type_arguments, structure_generics, arguments),
				// 	|top_environment, types, target, func, data| {
				// 		let (explicit_type_arguments, structure_generics, arguments) = data;
				// 		let result = call_logical(
				// 			func,
				// 			called_with_new,
				// 			call_site,
				// 			explicit_type_arguments.clone(),
				// 			structure_generics.clone(),
				// 			arguments.clone(),
				// 			top_environment,
				// 			types,
				// 			target,
				// 		);

				// 		result.map(|result| {
				// 			application_result_to_return_type(
				// 				result.result,
				// 				top_environment,
				// 				types,
				// 			)
				// 		})
				// 	},
				// );
				// let truthy_result = truthy_result?;
				// let otherwise_result = otherwise_result?;
				// // truthy_result.warnings.append(&mut otherwise_result.warnings);
				// let _result =
				// 	types.new_conditional_type(based_on, truthy_result, otherwise_result);

				todo!();
			// TODO combine information (merge)
			// Ok(CallingOutput { called: None, result: Soe, warnings: (), special: (), result_was_const_computation: () })
			} else {
				// TODO inference and errors
				let err = FunctionCallingError::NotCallable {
					calling: TypeStringRepresentation::from_type_id(
						based_on,
						top_environment,
						types,
						false,
					),
					call_site,
				};
				Err(BadCallOutput { returned_type: TypeId::ERROR_TYPE, errors: vec![err] })
			}
		}
		Logical::Implies { on, antecedent } => call_logical(
			*on,
			called_with_new,
			call_site,
			explicit_type_arguments,
			Some(antecedent),
			// this_value,
			arguments,
			top_environment,
			types,
			behavior,
		),
	}
}

fn find_possible_mutations(
	arguments: &Vec<SynthesisedArgument>,
	types: &mut TypeStore,
	top_environment: &mut Environment,
) {
	for argument in arguments {
		// TODO need to do in a function
		// All properties
		// Functions free variables etc
		// TODO if spread
		match types.get_type_by_id(argument.value) {
			Type::Interface { .. }
			| Type::Class { .. }
			| Type::AliasTo { .. }
			| Type::And(_, _)
			| Type::Object(ObjectNature::AnonymousTypeAnnotation)
			| Type::FunctionReference(_)
			| Type::PartiallyAppliedGenerics(_)
			| Type::Or(_, _) => {
				crate::utilities::notify!("Unreachable");
			}
			Type::Constant(_) => {}
			Type::RootPolyType(_) | Type::Constructor(_) => {
				// All dependent anyway
				crate::utilities::notify!("TODO if any properties set etc");
			}
			Type::SpecialObject(SpecialObjects::Function(_, _)) => {
				crate::utilities::notify!("TODO record that function could be called");
			}
			Type::Object(ObjectNature::RealDeal) => {
				top_environment.possibly_mutated_objects.insert(argument.value);
				crate::utilities::notify!("TODO record methods could be called here as well");
			}
			Type::SpecialObject(_) => {
				crate::utilities::notify!("TODO record stuff if mutable");
			}
		}
	}
}

/// Errors from trying to call a function
pub enum FunctionCallingError {
	InvalidArgumentType {
		parameter_type: TypeStringRepresentation,
		argument_type: TypeStringRepresentation,
		argument_position: SpanWithSource,
		parameter_position: SpanWithSource,
		restriction: Option<(SpanWithSource, TypeStringRepresentation)>,
	},
	MissingArgument {
		parameter_position: SpanWithSource,
		call_site: SpanWithSource,
	},
	ExcessArguments {
		count: usize,
		position: SpanWithSource,
	},
	ExcessTypeArguments {
		count: usize,
	},
	NotCallable {
		calling: TypeStringRepresentation,
		call_site: SpanWithSource,
	},
	ReferenceRestrictionDoesNotMatch {
		reference: RootReference,
		requirement: TypeStringRepresentation,
		found: TypeStringRepresentation,
	},
	CyclicRecursion(FunctionId, SpanWithSource),
	NoLogicForIdentifier(String, SpanWithSource),
	NeedsToBeCalledWithNewKeyword(SpanWithSource),
	TDZ {
		error: TDZ,
		/// Should be set
		call_site: SpanWithSource,
	},
	SetPropertyConstraint {
		property_type: TypeStringRepresentation,
		value_type: TypeStringRepresentation,
		assignment_position: SpanWithSource,
		/// Should be set
		call_site: SpanWithSource,
	},
	/// TODO WIP
	UnconditionalThrow {
		value: TypeStringRepresentation,
		/// Should be set
		call_site: SpanWithSource,
	},
	MismatchedThis {
		expected: TypeStringRepresentation,
		found: TypeStringRepresentation,
		call_site: SpanWithSource,
	},
	CannotCatch {
		catch: TypeStringRepresentation,
		thrown: TypeStringRepresentation,
		thrown_position: SpanWithSource,
	},
}

pub struct InfoDiagnostic(pub String);

#[derive(Debug, Default, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum CalledWithNew {
	/// With `new X(...)`;
	New {
		// [See new.target](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target), which contains a reference to what called new. This does not == this_type
		on: TypeId,
	},
	/// With `super(...)`
	Super { this_type: TypeId },
	#[default]
	None,
}

impl FunctionType {
	/// Calls the function and returns warnings and errors
	#[allow(clippy::too_many_arguments)]
	pub(crate) fn call<E: CallCheckingBehavior>(
		&self,
		called_with_new: CalledWithNew,
		this_value: ThisValue,
		call_site: SpanWithSource,
		arguments: &[SynthesisedArgument],
		call_site_type_arguments: Option<CallSiteTypeArguments>,
		parent_arguments: Option<GenericArguments>,
		environment: &mut Environment,
		behavior: &mut E,
		types: &mut crate::TypeStore,
		// This fixes recursive case of call_function
		call_constant: bool,
	) -> Result<CallingOutput, BadCallOutput> {
		// TODO check that parameters vary
		if behavior.in_recursive_cycle(self.id) {
			crate::utilities::notify!("Encountered recursion");
			crate::utilities::notify!("TODO should be BadCallOutput");
			let returned = types.new_error_type(self.return_type);
			return Ok(CallingOutput {
				called: Some(self.id),
				result: Some(ApplicationResult::Return { returned, position: call_site }),
				warnings: Vec::new(),
				// TODO ?
				special: None,
				result_was_const_computation: false,
			});
			// let reason = FunctionCallingError::Recursed(self.id, call_site);
			// return Err(vec![reason])
		}

		if environment.is_always_run() {
			types.called_functions.insert(self.id);
		}

		if let (FunctionEffect::Constant { identifier: ref const_fn_ident, .. }, true) =
			(&self.effect, call_constant)
		{
			let has_dependent_argument =
				arguments.iter().any(|arg| types.get_type_by_id(arg.value).is_dependent());

			// || matches!(this_value, ThisValue::Passed(ty) if types.get_type_by_id(ty).is_dependent());

			let call_anyway = matches!(
				const_fn_ident.as_str(),
				"debug_type"
					| "debug_type_rust" | "print_type"
					| "print_and_debug_type"
					| "debug_effects" | "debug_effects_rust"
					| "satisfies" | "is_dependent"
					| "bind" | "create_proxy"
			);

			// TODO just for debugging. These have their constant things called every time AND queue an event
			let is_independent = const_fn_ident.ends_with("independent");

			// Most of the time don't call using constant function if an argument is dependent.
			// But sometimes do for the cases above
			if call_anyway || is_independent || !has_dependent_argument {
				// TODO event
				let result = call_constant_function(
					const_fn_ident,
					// TODO
					this_value,
					call_site_type_arguments.as_ref(),
					arguments,
					types,
					environment,
					call_site,
				);

				match result {
					Ok(ConstantOutput::Value(value)) => {
						// Very special
						let special = if const_fn_ident == "compile_type_to_object" {
							Some(SpecialExpressions::CompileOut)
						} else {
							None
						};
						return Ok(CallingOutput {
							result: Some(ApplicationResult::Return {
								returned: value,
								position: call_site,
							}),
							warnings: Default::default(),
							called: None,
							result_was_const_computation: !is_independent,
							special,
						});
					}
					Ok(ConstantOutput::Diagnostic(diagnostic)) => {
						// crate::utilities::notify!("Here, constant output");
						return Ok(CallingOutput {
							result: None,
							warnings: vec![InfoDiagnostic(diagnostic)],
							called: None,
							result_was_const_computation: !is_independent,
							// TODO!!
							special: Some(SpecialExpressions::Marker),
						});
					}
					Err(ConstantFunctionError::NoLogicForIdentifier(name)) => {
						let item = FunctionCallingError::NoLogicForIdentifier(name, call_site);
						return Err(BadCallOutput {
							returned_type: types.new_error_type(self.return_type),
							errors: vec![item],
						});
					}
					Err(ConstantFunctionError::BadCall) => {
						crate::utilities::notify!(
							"Constant function calling failed, non constant params"
						);
					}
				}
			} else if has_dependent_argument {
				// TODO with cloned!!
				let call = self.call(
					called_with_new,
					this_value,
					call_site,
					arguments,
					call_site_type_arguments,
					parent_arguments,
					environment,
					behavior,
					types,
					// Very important!
					false,
				);

				if let Err(ref _err) = call {
					crate::utilities::notify!("Calling function with dependent argument failed");
				}

				let result = call?.result;

				return Ok(CallingOutput {
					result,
					warnings: Default::default(),
					called: None,
					special: None,
					result_was_const_computation: false,
					// TODO
				});
			}
		}

		let mut errors = ErrorsAndInfo::default();

		let mut type_arguments = SubstitutionArguments {
			parent: None,
			arguments: crate::Map::default(),
			closures: if let Some(GenericArguments::Closure(ref cs)) = parent_arguments {
				cs.clone()
			} else {
				Default::default()
			},
		};

		self.set_this_for_behavior(
			called_with_new,
			this_value,
			&mut type_arguments.arguments,
			environment,
			types,
			&mut errors,
			call_site,
			behavior,
		);

		self.assign_arguments_to_parameters::<E>(
			arguments,
			&mut type_arguments,
			call_site_type_arguments.clone(),
			parent_arguments.as_ref(),
			environment,
			types,
			&mut errors,
			call_site,
			behavior,
		);

		if E::CHECK_PARAMETERS {
			// TODO check free variables from inference
		}

		if !errors.errors.is_empty() {
			return Err(BadCallOutput {
				returned_type: types.new_error_type(self.return_type),
				errors: errors.errors,
			});
		}

		let result = if let FunctionEffect::SideEffects {
			events,
			closed_over_variables,
			free_variables: _,
		} = &self.effect
		{
			behavior.new_function_context(self.id, |target| {
				// crate::utilities::notify!("events: {:?}", events);
				{
					let substitution: &mut SubstitutionArguments = &mut type_arguments;
					let errors: &mut ErrorsAndInfo = &mut errors;
					if !closed_over_variables.0.is_empty() {
						let closure_id = types.new_closure_id();
						substitution.closures.push(closure_id);

						crate::utilities::notify!("Setting closure variables");

						// Set closed over values
						// TODO `this`
						for (variable, value) in closed_over_variables.0.iter() {
							let value = substitute(*value, substitution, environment, types);
							environment
								.info
								.closure_current_values
								.insert((closure_id, RootReference::Variable(*variable)), value);

							crate::utilities::notify!(
								"in {:?} set {:?} to {:?}",
								closure_id,
								variable,
								value
							);
						}
					}

					let current_errors = errors.errors.len();

					// Apply events here
					let result = apply_events(
						events,
						this_value,
						substitution,
						environment,
						target,
						types,
						errors,
						call_site,
					);

					// Adjust call sites. (because they aren't currently passed down)
					for d in &mut errors.errors[current_errors..] {
						if let FunctionCallingError::TDZ { call_site: ref mut c, .. }
						| FunctionCallingError::SetPropertyConstraint {
							call_site: ref mut c,
							..
						}
						| FunctionCallingError::UnconditionalThrow {
							call_site: ref mut c,
							..
						} = d
						{
							*c = call_site;
						}
					}

					result
				}
			})
		} else {
			if let Some(_parent_arguments) = parent_arguments {
				crate::utilities::notify!("TODO");
			}
			if let Some(ref call_site_type_arguments) = call_site_type_arguments {
				for (k, (v, _)) in call_site_type_arguments.iter() {
					type_arguments.arguments.insert(*k, *v);
				}
			}

			crate::utilities::notify!(
				"Substituting return type (no return) {:?}",
				&type_arguments.arguments
			);

			let returned = substitute(self.return_type, &type_arguments, environment, types);
			Some(ApplicationResult::Return { returned, position: call_site })
		};

		if !errors.errors.is_empty() {
			crate::utilities::notify!("Got {} application errors", errors.errors.len());
			return Err(BadCallOutput {
				returned_type: types.new_error_type(self.return_type),
				errors: errors.errors,
			});
		}

		// TODO what does super return?
		if let CalledWithNew::New { .. } = called_with_new {
			// TODO ridiculous early return primitive rule
			match self.behavior {
				FunctionBehavior::ArrowFunction { is_async: _ } => todo!(),
				FunctionBehavior::Method { .. } => todo!(),
				FunctionBehavior::Constructor { prototype: _, this_object_type }
				| FunctionBehavior::Function {
					is_async: _,
					is_generator: _,
					free_this_id: this_object_type,
					..
				} => {
					let new_instance_type =
						type_arguments.arguments.get(&this_object_type).expect("no this argument?");

					// TODO if return type is primitive (or replace primitives with new_instance_type)
					return Ok(CallingOutput {
						result: Some(ApplicationResult::Return {
							returned: *new_instance_type,
							position: call_site,
						}),
						warnings: errors.warnings,
						called: Some(self.id),
						special: None,
						result_was_const_computation: false,
					});
				}
			}
		}

		Ok(CallingOutput {
			result,
			warnings: errors.warnings,
			// unconditional_exception,
			called: Some(self.id),
			special: None,
			result_was_const_computation: false,
		})
	}

	/// TODO set not using `type_arguments`
	#[allow(clippy::too_many_arguments)]
	fn set_this_for_behavior<E: CallCheckingBehavior>(
		&self,
		called_with_new: CalledWithNew,
		this_value: ThisValue,
		type_arguments: &mut crate::Map<TypeId, TypeId>,
		environment: &mut Environment,
		types: &mut TypeStore,
		errors: &mut ErrorsAndInfo,
		call_site: SpanWithSource,
		behavior: &E,
	) {
		match self.behavior {
			FunctionBehavior::ArrowFunction { .. } => {}
			FunctionBehavior::Method { free_this_id, .. } => {
				// TODO
				let value_of_this = if let Some(value) = this_value.get_passed() {
					value
				} else {
					crate::utilities::notify!(
						"method has no 'this' passed :?. Passing `undefined` here"
					);
					TypeId::UNDEFINED_TYPE
				};

				let base_type = get_constraint(free_this_id, types).unwrap_or(free_this_id);

				let mut state = State {
					already_checked: Default::default(),
					contributions: None,
					mode: SubTypingMode::default(),
					object_constraints: None,
					others: Default::default(),
				};

				let type_is_subtype =
					type_is_subtype(base_type, value_of_this, &mut state, environment, types);

				if let SubTypeResult::IsNotSubType(_reason) = type_is_subtype {
					errors.errors.push(FunctionCallingError::MismatchedThis {
						expected: TypeStringRepresentation::from_type_id(
							free_this_id,
							environment,
							types,
							behavior.debug_types(),
						),
						found: TypeStringRepresentation::from_type_id(
							value_of_this,
							environment,
							types,
							behavior.debug_types(),
						),
						call_site,
					});
				}

				crate::utilities::notify!(
					"free this id {:?} & value of this {:?}",
					free_this_id,
					value_of_this
				);

				type_arguments.insert(free_this_id, value_of_this);
			}
			FunctionBehavior::Function {
				is_async: _,
				is_generator: _,
				free_this_id,
				prototype,
			} => {
				match called_with_new {
					CalledWithNew::New { on: _ } => {
						crate::utilities::notify!("TODO set prototype");
						// if let Some(prototype) = non_super_prototype {
						// let this_ty = environment.create_this(prototype, types);

						let this_constraint = get_constraint(free_this_id, types);
						// Check `this` has all prototype information
						if let Some(this_constraint) = this_constraint {
							let mut state = State {
								already_checked: Default::default(),
								mode: SubTypingMode::default(),
								contributions: None,
								object_constraints: None,
								others: SubTypingOptions::default(),
							};

							let result = type_is_subtype(
								this_constraint,
								prototype,
								&mut state,
								environment,
								types,
							);

							if let SubTypeResult::IsNotSubType(_reason) = result {
								errors.errors.push(FunctionCallingError::MismatchedThis {
									expected: TypeStringRepresentation::from_type_id(
										this_constraint,
										environment,
										types,
										behavior.debug_types(),
									),
									found: TypeStringRepresentation::from_type_id(
										prototype,
										environment,
										types,
										behavior.debug_types(),
									),
									call_site,
								});
							}
						}

						// TODO think so
						let is_under_dyn = true;
						let value_of_this = environment.info.new_object(
							Some(prototype),
							types,
							call_site,
							is_under_dyn,
						);

						type_arguments.insert(free_this_id, value_of_this);
					}
					CalledWithNew::Super { this_type } => {
						type_arguments.insert(free_this_id, this_type);
					}
					CalledWithNew::None => {
						// TODO
						let value_of_this = this_value.get(environment, types, call_site);

						type_arguments.insert(free_this_id, value_of_this);
					}
				}
			}
			FunctionBehavior::Constructor { prototype, this_object_type } => {
				crate::utilities::notify!("Here {:?}", called_with_new);
				match called_with_new {
					CalledWithNew::None => {
						errors
							.errors
							.push(FunctionCallingError::NeedsToBeCalledWithNewKeyword(call_site));
					}
					CalledWithNew::New { on: _ } => {
						// TODO think so
						let is_under_dyn = true;
						crate::utilities::notify!(
							"Creating new class object with prototype={:?}",
							prototype
						);
						let value_of_this = environment.info.new_object(
							Some(prototype),
							types,
							call_site,
							is_under_dyn,
						);

						crate::utilities::notify!("Here with new");
						type_arguments.insert(this_object_type, value_of_this);
					}
					CalledWithNew::Super { this_type } => {
						crate::utilities::notify!(
							"Setting this_object {:?} to {:?} in super call",
							this_object_type,
							this_type
						);
						type_arguments.insert(this_object_type, this_type);
					}
				}
			}
		}

		{
			let new_target_value = match called_with_new {
				CalledWithNew::New { on } => on,
				CalledWithNew::Super { .. } => {
					crate::utilities::notify!("Get this type for super new.target");
					TypeId::ERROR_TYPE
					// let ty = this_value.0;
					// let on = crate::types::printing::print_type(
					// 	ty,
					// 	types,
					// 	environment,
					// 	true,
					// );
					// crate::utilities::notify!("This argument {}", on);
					// ty
				}
				// In spec, not `new` -> `new.target === undefined`
				CalledWithNew::None => TypeId::UNDEFINED_TYPE,
			};

			type_arguments.insert(TypeId::NEW_TARGET_ARG, new_target_value);
		}
	}

	#[allow(clippy::too_many_arguments)]
	fn assign_arguments_to_parameters<E: CallCheckingBehavior>(
		&self,
		arguments: &[SynthesisedArgument],
		type_arguments: &mut SubstitutionArguments<'static>,
		call_site_type_arguments: Option<CallSiteTypeArguments>,
		parent: Option<&GenericArguments>,
		environment: &mut Environment,
		types: &mut TypeStore,
		errors: &mut ErrorsAndInfo,
		call_site: SpanWithSource,
		behavior: &E,
	) {
		for (parameter_idx, parameter) in self.parameters.parameters.iter().enumerate() {
			// TODO temp

			// This handles if the argument is missing but allowing elided arguments
			let argument = arguments.get(parameter_idx);

			if let Some(SynthesisedArgument { spread, value, position }) = argument {
				if *spread {
					crate::utilities::notify!("TODO spread arguments");
				}

				if E::CHECK_PARAMETERS {
					let result = check_parameter_type(
						parameter.ty,
						call_site_type_arguments.as_ref(),
						parent,
						*value,
						type_arguments,
						environment,
						types,
					);

					if let SubTypeResult::IsNotSubType(_reasons) = result {
						let type_arguments = Some(GenericChainLink::FunctionRoot {
							parent_link: parent,
							call_site_type_arguments: call_site_type_arguments.as_ref(),
							type_arguments: &type_arguments.arguments,
						});

						// crate::utilities::notify!("Type arguments are {:?}", type_arguments);

						errors.errors.push(FunctionCallingError::InvalidArgumentType {
							parameter_type: TypeStringRepresentation::from_type_id_with_generics(
								parameter.ty,
								type_arguments,
								environment,
								types,
								behavior.debug_types(),
							),
							argument_type: TypeStringRepresentation::from_type_id_with_generics(
								*value,
								type_arguments,
								environment,
								types,
								behavior.debug_types(),
							),
							parameter_position: parameter.position,
							argument_position: *position,
							restriction: None,
						});
					}
				} else {
					// Already checked so can set
					type_arguments.arguments.insert(parameter.ty, *value);
				}
			} else if parameter.is_optional {
				type_arguments.arguments.insert(parameter.ty, TypeId::UNDEFINED_TYPE);
			} else {
				errors.errors.push(FunctionCallingError::MissingArgument {
					parameter_position: parameter.position,
					call_site,
				});
			}
		}

		// Spread parameters here
		if self.parameters.parameters.len() < arguments.len() {
			if let Some(ref rest_parameter) = self.parameters.rest_parameter {
				// TODO reuse synthesise_array literal logic (especially for spread items)
				let mut basis = ObjectBuilder::new(
					Some(TypeId::ARRAY_TYPE),
					types,
					rest_parameter.position,
					&mut environment.info,
				);

				let mut count = 0;

				for argument in arguments.iter().skip(self.parameters.parameters.len()) {
					if E::CHECK_PARAMETERS {
						let result = check_parameter_type(
							rest_parameter.item_type,
							call_site_type_arguments.as_ref(),
							parent,
							argument.value,
							type_arguments,
							environment,
							types,
						);

						// TODO different diagnostic?
						if let SubTypeResult::IsNotSubType(_reasons) = result {
							let type_arguments = Some(GenericChainLink::FunctionRoot {
								parent_link: parent,
								call_site_type_arguments: call_site_type_arguments.as_ref(),
								type_arguments: &type_arguments.arguments,
							});

							errors.errors.push(FunctionCallingError::InvalidArgumentType {
								parameter_type:
									TypeStringRepresentation::from_type_id_with_generics(
										rest_parameter.ty,
										type_arguments,
										environment,
										types,
										behavior.debug_types(),
									),
								argument_type: TypeStringRepresentation::from_type_id_with_generics(
									argument.value,
									type_arguments,
									environment,
									types,
									behavior.debug_types(),
								),
								parameter_position: rest_parameter.position,
								argument_position: argument.position,
								restriction: None,
							});
						}
					}

					{
						let key = PropertyKey::from_usize(count);

						basis.append(
							environment,
							crate::types::properties::Publicity::Public,
							key,
							crate::types::properties::PropertyValue::Value(argument.value),
							argument.position,
						);
					}

					count += 1;
				}

				{
					let length = types.new_constant_type(crate::Constant::Number(
						(count as f64).try_into().unwrap(),
					));

					basis.append(
						environment,
						crate::types::properties::Publicity::Public,
						PropertyKey::String("length".into()),
						crate::types::properties::PropertyValue::Value(length),
						rest_parameter.position,
					);
				}

				let rest_parameter_array_type = basis.build_object();
				type_arguments.arguments.insert(rest_parameter.ty, rest_parameter_array_type);
			} else {
				// TODO types.options.allow_extra_arguments
				let mut left_over = arguments.iter().skip(self.parameters.parameters.len());
				let first = left_over.next().unwrap();
				let mut count = 1;
				let mut end = None;
				while let arg @ Some(_) = left_over.next() {
					count += 1;
					end = arg;
				}
				// Creating position for excess arguments
				let position = if let Some(end) = end {
					first
						.position
						.without_source()
						.union(end.position.without_source())
						.with_source(end.position.source)
				} else {
					first.position
				};

				if E::CHECK_PARAMETERS {
					errors.errors.push(FunctionCallingError::ExcessArguments { count, position });
				}
			}
		}

		// Set restrictions as arguments IF not set already. So can
		if let Some(_call_site_type_arguments) = call_site_type_arguments {
			crate::utilities::notify!("TODO type_arguments.covariants");
			// for (on, (arg, _)) in call_site_type_arguments {
			// 	if type_arguments.arguments.get(&on).is_none() {
			// 		type_arguments.arguments.insert(on, arg)
			// 	}
			// }
		}
	}
}

fn check_parameter_type(
	parameter_ty: TypeId,
	call_site_type_arguments: Option<&CallSiteTypeArguments>,
	parent: Option<&GenericArguments>,
	argument_ty: TypeId,
	type_arguments: &mut SubstitutionArguments,
	environment: &mut Environment,
	types: &mut TypeStore,
) -> SubTypeResult {
	// crate::utilities::notify!("Argument is {:?}, parent generics {:?}", value, parent);

	// TODO properties
	let contributions = Contributions {
		parent,
		call_site_type_arguments,
		staging_covariant: Default::default(),
		staging_contravariant: Default::default(),
	};

	// TODO WIP
	let mut state = State {
		already_checked: Vec::new(),
		mode: Default::default(),
		contributions: Some(contributions),
		others: SubTypingOptions { allow_errors: true },
		object_constraints: None,
	};

	let result = type_is_subtype_with_generics(
		(parameter_ty, GenericChain::None),
		(argument_ty, GenericChain::None),
		&mut state,
		environment,
		types,
	);

	if result.is_subtype() {
		// mut parameter_constraint_request,
		let Contributions { staging_covariant, staging_contravariant, .. } =
			state.contributions.unwrap();

		for (on, (value, _)) in staging_covariant.iter() {
			crate::utilities::notify!("TODO pick highest covariant?");
			if let Some(value) = type_arguments.arguments.get_mut(on) {
				*value = types.new_or_type(*value, *value);
			} else {
				type_arguments.arguments.insert(*on, *value);
			}
		}

		for (idx, (on, (value, depth))) in staging_contravariant.iter().enumerate() {
			// This is TSC behavior
			// If there exists another argument that is in a deeper than the current don't add it
			// TODO only if explicit generic
			let is_weaker =
				staging_contravariant.iter().enumerate().any(|(idx2, (on2, (_, other_depth)))| {
					(on == on2) && idx != idx2 && depth < other_depth
				});

			if is_weaker {
				crate::utilities::notify!("Here skipping as weaker");
				continue;
			}

			// TODO maybe this should have entry api
			let value = value.clone().into_type(types);
			if let Some(existing) = type_arguments.arguments.get_mut(on) {
				crate::utilities::notify!("Here");
				*existing = types.new_or_type(*existing, value);
			} else {
				type_arguments.arguments.insert(*on, value);
			}
		}
	}

	// environment.context_type.requests.append(&mut parameter_constraint_request);

	result
}

#[allow(clippy::type_complexity)]
fn synthesise_argument_expressions_wrt_parameters<T: ReadFromFS, A: crate::ASTImplementation>(
	callable: &Logical<FunctionLike>,
	arguments: &[UnsynthesisedArgument<A>],
	(call_site_type_arguments, parent_arguments): (
		Option<Vec<(TypeId, SpanWithSource)>>,
		Option<&GenericArguments>,
	),
	environment: &mut Environment,
	checking_data: &mut crate::CheckingData<T, A>,
) -> (Vec<SynthesisedArgument>, Option<TypeRestrictions>) {
	fn synthesise_call_site_type_argument_hints(
		type_parameters: &GenericTypeParameters,
		call_site_type_arguments: Vec<(TypeId, SpanWithSource)>,
		types: &crate::types::TypeStore,
		environment: &mut Environment,
	) -> TypeRestrictions {
		crate::utilities::notify!("call_site_type_arguments {:?}", call_site_type_arguments);

		type_parameters
			.0
			.iter()
			.zip(call_site_type_arguments)
			.map(|(param, (ty, position))| {
				if let Type::RootPolyType(PolyNature::FunctionGeneric { eager_fixed, .. }) =
					types.get_type_by_id(param.type_id)
				{
					let mut state = State {
						already_checked: Default::default(),
						mode: SubTypingMode::default(),
						contributions: None,
						others: Default::default(),
						object_constraints: None,
					};
					let type_is_subtype =
						type_is_subtype(*eager_fixed, ty, &mut state, environment, types);

					if let SubTypeResult::IsNotSubType(_) = type_is_subtype {
						todo!("generic argument does not match restriction")
					}
				} else {
					todo!();
					// crate::utilities::notify!("Generic parameter with no aliasing restriction, I think this fine on internals");
				};

				(param.type_id, (ty, position))
			})
			.collect()
	}

	match callable {
		Logical::Pure(function) => {
			let function = checking_data.types.get_function_from_id(function.function);

			let type_arguments_restrictions =
				match (&function.type_parameters, call_site_type_arguments) {
					(None, Some(call_site_type_arguments)) => {
						checking_data.diagnostics_container.add_error(
							TypeCheckError::FunctionCallingError(
								FunctionCallingError::ExcessTypeArguments {
									count: call_site_type_arguments.len(),
								},
							),
						);

						None
					}
					(Some(ref type_parameters), Some(call_site_type_arguments)) => {
						if call_site_type_arguments.len() > type_parameters.0.len() {
							checking_data.diagnostics_container.add_error(
								TypeCheckError::FunctionCallingError(
									FunctionCallingError::ExcessTypeArguments {
										count: call_site_type_arguments.len(),
									},
								),
							);
							None
						} else {
							Some(synthesise_call_site_type_argument_hints(
								type_parameters,
								call_site_type_arguments,
								&checking_data.types,
								environment,
							))
						}
					}

					_ => None,
				};

			let parameters = function.parameters.clone();

			let type_arguments = if type_arguments_restrictions.is_some()
				|| parent_arguments.is_some()
			{
				let mut arguments = type_arguments_restrictions.clone().unwrap_or_default();
				match parent_arguments {
					Some(GenericArguments::LookUp { on }) => {
						// TODO copied from somewhere
						let prototype = environment
							.get_chain_of_info()
							.find_map(|env| env.prototypes.get(on))
							.unwrap();

						crate::utilities::notify!(
							"Here calculating lookup argument {:?}",
							prototype
						);
						let map =
							checking_data.types.lookup_generic_map.get(prototype).unwrap().clone();

						for (under, lookup) in map.iter() {
							let entries = lookup.calculate_lookup(environment, *on);
							let mut iter = entries.into_iter();
							let mut ty = iter.next().unwrap();
							for other in iter {
								ty = checking_data.types.new_or_type(ty, other);
							}

							arguments.insert(*under, (ty, BaseSpan::NULL));
						}
					}
					Some(GenericArguments::ExplicitRestrictions(ers)) => {
						arguments.extend(ers.iter().map(|(k, v)| (*k, *v)));
					}
					Some(GenericArguments::Closure(..)) | None => {}
				}

				Some(arguments)
			} else {
				None
			};

			let arguments = arguments
				.iter()
				.enumerate()
				.map(|(idx, argument)| {
					let expected_type = parameters.get_parameter_type_at_index(idx).map_or(
						TypeId::ANY_TYPE,
						|(parameter_type, _)| {
							// crate::utilities::notify!("(pairing explicit generics for) Generic parameter = {:?}", parameter_type);

							let ty = checking_data.types.get_type_by_id(parameter_type);
							let parameter_type = if let Type::RootPolyType(
								PolyNature::Parameter { fixed_to },
							) = ty
							{
								*fixed_to
							} else {
								// crate::utilities::notify!(
								// 	"Parameter is not `PolyNature::Parameter`? {:?}",
								// 	ty
								// );
								parameter_type
							};

							if let Some(arguments) = type_arguments.clone() {
								// Unfortunately this seems the best way to pass down to expected type
								checking_data.types.register_type(Type::PartiallyAppliedGenerics(
									PartiallyAppliedGenerics {
										on: parameter_type,
										arguments: GenericArguments::ExplicitRestrictions(
											arguments,
										),
									},
								))
							} else {
								parameter_type
							}
						},
					);

					let value = A::synthesise_expression(
						argument.expression,
						expected_type,
						environment,
						checking_data,
					);

					let position = A::expression_position(argument.expression)
						.with_source(environment.get_source());

					SynthesisedArgument { spread: argument.spread, position, value }
				})
				.collect();

			(arguments, type_arguments_restrictions)
		}
		Logical::Implies { on, antecedent } => synthesise_argument_expressions_wrt_parameters(
			on,
			arguments,
			// TODO chain
			(call_site_type_arguments, Some(antecedent)),
			environment,
			checking_data,
		),
		Logical::Or { .. } => (
			arguments
				.iter()
				.map(|argument| SynthesisedArgument {
					spread: argument.spread,
					position: A::expression_position(argument.expression)
						.with_source(environment.get_source()),
					value: A::synthesise_expression(
						argument.expression,
						// TODO union
						TypeId::ANY_TYPE,
						environment,
						checking_data,
					),
				})
				.collect(),
			None,
		),
	}
}
