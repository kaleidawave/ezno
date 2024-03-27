use source_map::{SourceId, SpanWithSource};
use std::vec;

use crate::{
	context::{
		information::InformationChain,
		invocation::{CheckThings, InvocationContext},
		CallCheckingBehavior, Environment, Logical, Missing, PossibleLogical,
	},
	diagnostics::{TypeCheckError, TypeStringRepresentation, TDZ},
	events::{application::ErrorsAndInfo, apply_event, ApplicationResult, Event, RootReference},
	features::{
		constant_functions::{
			call_constant_function, CallSiteTypeArguments, ConstantFunctionError, ConstantOutput,
		},
		functions::{ClosedOverVariables, FunctionBehavior, ThisValue},
		objects::SpecialObjects,
	},
	subtyping::{type_is_subtype, type_is_subtype_with_generics, BasicEquality, SubTypeResult},
	types::{
		functions::SynthesisedArgument, substitute, FunctionEffect, FunctionType, GenericChainLink,
		ObjectNature, StructureGenerics, Type,
	},
	FunctionId, GenericTypeParameters, ReadFromFS, SpecialExpressions, TypeId,
};

use super::{
	get_constraint, is_type_constant,
	poly_types::{
		contributions::Contributions, generic_type_arguments::StructureGenericArguments,
		FunctionTypeArguments,
	},
	properties::PropertyKey,
	Constructor, GenericChain, PolyNature, TypeArguments, TypeRestrictions, TypeStore,
};

pub struct CallingInput {
	pub called_with_new: CalledWithNew,
	/// Exclusively from `.call()` special function
	pub this_value: Option<ThisValue>,
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
	pub(crate) this_value: ThisValue,
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

	let callable = get_logical_callable_from_type(ty, input.this_value, None, &checking_data.types);

	match callable {
		Ok(callable) => {
			// TODO temp fix as `.map` doesn't get this passed down
			let structure_generics = if let Logical::Pure(FunctionLike {
				this_value: ThisValue::Passed(this_passed),
				..
			}) = callable
			{
				if let Type::Object(..) = checking_data.types.get_type_by_id(this_passed) {
					if let Some(object_constraint) = environment
						.get_chain_of_info()
						.find_map(|c| c.object_constraints.get(&this_passed).copied())
					{
						if let Type::Constructor(Constructor::StructureGenerics(
							StructureGenerics { arguments, .. },
						)) = checking_data.types.get_type_by_id(object_constraint)
						{
							crate::utils::notify!("Here");
							Some(arguments.clone())
						} else {
							None
						}
					} else {
						let prototype = environment
							.get_chain_of_info()
							.find_map(|facts| facts.prototypes.get(&this_passed))
							.copied();

						if prototype.is_some_and(|prototype| {
							checking_data.types.lookup_generic_map.contains_key(&prototype)
						}) {
							crate::utils::notify!("Registering lookup for calling");

							Some(StructureGenericArguments::LookUp { on: this_passed })
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

			let (arguments, type_argument_restrictions) = synthesise_arguments_for_parameter(
				&callable,
				arguments,
				input.call_site_type_arguments,
				structure_generics.as_ref(),
				environment,
				checking_data,
			);

			let mut check_things = CheckThings { debug_types: checking_data.options.debug_types };
			let result = call_logical(
				callable,
				input.called_with_new,
				input.call_site,
				type_argument_restrictions,
				None,
				arguments,
				environment,
				&mut checking_data.types,
				&mut check_things,
			);

			match result {
				Ok(FunctionCallResult {
					returned_type,
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

					(returned_type, special)
				}
				Err(errors) => {
					for error in errors {
						checking_data
							.diagnostics_container
							.add_error(TypeCheckError::FunctionCallingError(error));
					}
					(TypeId::ERROR_TYPE, None)
				}
			}
		}
		Err(Missing::Error) => (TypeId::ERROR_TYPE, None),
		Err(Missing::Infer { on: _ }) => {
			todo!("function calling inference")
		}
		Err(Missing::None) => {
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

/// In events
pub(crate) fn call_type<E: CallCheckingBehavior>(
	on: TypeId,
	arguments: Vec<SynthesisedArgument>,
	input: &CallingInput,
	top_environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
	let callable = get_logical_callable_from_type(on, input.this_value, None, types).ok();
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
		return Err(Missing::Error);
	}
	if ty == TypeId::ANY_TYPE {
		return Err(Missing::Infer { on: from.unwrap() });
	}

	let le_ty = types.get_type_by_id(ty);

	// crate::utils::notify!("ty1={:?} ({:?})", le_ty, ty);

	match le_ty {
		Type::Class { .. } | Type::Interface { .. } | Type::Constant(_) | Type::Object(_) => {
			Err(Missing::None)
		}
		Type::And(_, _) => todo!(),
		Type::Or(left, right) => {
			let left = get_logical_callable_from_type(*left, on, from, types);
			let right = get_logical_callable_from_type(*right, on, from, types);
			Ok(Logical::Or {
				based_on: TypeId::BOOLEAN_TYPE,
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
			// Always from
			Ok(Logical::Pure(FunctionLike {
				from: Some(from.unwrap_or(ty)),
				function: *f,
				this_value: on.unwrap_or(*t),
			}))
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
			_ => Err(Missing::None),
		},
		Type::Constructor(Constructor::StructureGenerics(generic)) => {
			get_logical_callable_from_type(generic.on, on, from, types).map(|res| {
				crate::utils::notify!("Calling found {:?}", generic.arguments);
				Logical::Implies { on: Box::new(res), antecedent: generic.arguments.clone() }
			})
		}
		Type::Constructor(Constructor::Property { on, under: _, result, bind_this }) => {
			crate::utils::notify!("Passing {:?}", on);

			let this_value = if *bind_this { ThisValue::Passed(*on) } else { ThisValue::UseParent };
			let result =
				get_logical_callable_from_type(*result, Some(this_value), Some(ty), types)?;

			if let Some(antecedent) = get_constraint(*on, types).and_then(|c| {
				if let Type::Constructor(Constructor::StructureGenerics(generic)) =
					types.get_type_by_id(c)
				{
					Some(generic.arguments.clone())
				} else {
					None
				}
			}) {
				Ok(Logical::Implies { on: Box::new(result), antecedent })
			} else {
				Ok(result)
			}
		}
		Type::RootPolyType(_) | Type::Constructor(_) => {
			let constraint = get_constraint(ty, types).unwrap();
			crate::utils::notify!("Calling constructor / root poly type! {:?}", constraint);
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
) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
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
		Err(vec![FunctionCallingError::NotCallable {
			calling: crate::diagnostics::TypeStringRepresentation::from_type_id(
				on,
				top_environment,
				types,
				behavior.debug_types(),
			),
			call_site: input.call_site,
		}])
	}
}

#[allow(clippy::too_many_arguments)]
fn call_logical<E: CallCheckingBehavior>(
	logical: Logical<FunctionLike>,
	called_with_new: CalledWithNew,
	call_site: SpanWithSource,
	explicit_type_arguments: Option<CallSiteTypeArguments>,
	structure_generics: Option<StructureGenericArguments>,
	arguments: Vec<SynthesisedArgument>,
	top_environment: &mut Environment,
	types: &mut TypeStore,
	behavior: &mut E,
) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
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

				// is poly
				if matches!(
					function_type.effect,
					FunctionEffect::Unknown | FunctionEffect::InputOutput(..)
				) || (matches!(function_type.effect, FunctionEffect::Constant(..))
					&& !result.result_was_const_computation)
				{
					let on = function.from.unwrap();

					// if function_type.effect
					// This should be okay, constant or IO functions don't mutate their arguments...?
					if !matches!(
						&function_type.effect,
						FunctionEffect::Constant(..) | FunctionEffect::InputOutput(..)
					) {
						find_possible_mutations(&arguments, types, top_environment);
					}

					let with = arguments.clone().into_boxed_slice();
					let reflects_dependency = if is_type_constant(result.returned_type, types) {
						None
					} else {
						let id = types.register_type(Type::Constructor(Constructor::Image {
							on,
							// TODO...
							with: with.clone(),
							result: result.returned_type,
						}));

						result.returned_type = id;

						Some(id)
					};

					behavior.get_latest_info(top_environment).events.push(Event::CallsType {
						on,
						with,
						timing: crate::events::CallingTiming::Synchronous,
						called_with_new,
						reflects_dependency,
						position: call_site,
					});
				}

				Ok(result)
			} else {
				panic!()
			}
		}
		Logical::Or { .. } => {
			todo!("environment conditional?");
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
			| Type::Or(_, _) => {
				crate::utils::notify!("Unreachable");
			}
			Type::Constant(_) => {}
			Type::RootPolyType(_) | Type::Constructor(_) => {
				// All dependent anyway
				crate::utils::notify!("TODO if any properties set etc");
			}
			Type::SpecialObject(SpecialObjects::Function(_, _)) => {
				crate::utils::notify!("TODO record that function could be called");
			}
			Type::Object(ObjectNature::RealDeal) => {
				top_environment.possibly_mutated_objects.insert(argument.value);
				crate::utils::notify!("TODO record methods could be called here as well");
			}
			Type::SpecialObject(_) => {
				crate::utils::notify!("TODO record stuff if mutable");
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
		call_site: Option<SpanWithSource>,
	},
	SetPropertyConstraint {
		property_type: TypeStringRepresentation,
		value_type: TypeStringRepresentation,
		assignment_position: SpanWithSource,
		/// Should be set
		call_site: Option<SpanWithSource>,
	},
	/// TODO WIP
	UnconditionalThrow {
		value: TypeStringRepresentation,
		/// Should be set
		call_site: Option<SpanWithSource>,
	},
}

pub struct InfoDiagnostic(pub String);

/// TODO *result* name bad
pub struct FunctionCallResult {
	pub called: Option<FunctionId>,
	pub returned_type: TypeId,
	// TODO
	pub warnings: Vec<InfoDiagnostic>,
	pub special: Option<SpecialExpressions>,
	pub result_was_const_computation: bool,
	// /// For exception always thrown from a nested call
	// pub unconditional_exception: Option<TypeId>,
}

#[derive(Debug, Default, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum CalledWithNew {
	New {
		// [See new.target](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target), which contains a reference to what called new. This does not == this_type
		on: TypeId,
	},
	SpecialSuperCall {
		this_type: TypeId,
	},
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
		structure_generics: Option<StructureGenericArguments>,
		environment: &mut Environment,
		behavior: &mut E,
		types: &mut crate::TypeStore,
		// This fixes recursive case of call_function
		call_constant: bool,
	) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
		// TODO check that parameters vary
		if behavior.in_recursive_cycle(self.id) {
			crate::utils::notify!("Encountered recursion");
			return Ok(FunctionCallResult {
				called: Some(self.id),
				returned_type: TypeId::ERROR_TYPE,
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

		if let (FunctionEffect::Constant(ref const_fn_ident), true) = (&self.effect, call_constant)
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
				);

				match result {
					Ok(ConstantOutput::Value(value)) => {
						// Very special
						let special = if const_fn_ident == "compile_type_to_object" {
							Some(SpecialExpressions::CompileOut)
						} else {
							None
						};
						return Ok(FunctionCallResult {
							returned_type: value,
							warnings: Default::default(),
							called: None,
							result_was_const_computation: !is_independent,
							special,
						});
					}
					Ok(ConstantOutput::Diagnostic(diagnostic)) => {
						// crate::utils::notify!("Here, constant output");
						return Ok(FunctionCallResult {
							returned_type: TypeId::UNDEFINED_TYPE,
							warnings: vec![InfoDiagnostic(diagnostic)],
							called: None,
							result_was_const_computation: !is_independent,
							// TODO!!
							special: Some(SpecialExpressions::Marker),
						});
					}
					Err(ConstantFunctionError::NoLogicForIdentifier(name)) => {
						let item = FunctionCallingError::NoLogicForIdentifier(name, call_site);
						return Err(vec![item]);
					}
					Err(ConstantFunctionError::BadCall) => {
						crate::utils::notify!(
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
					structure_generics,
					environment,
					behavior,
					types,
					// Very important!
					false,
				);

				if let Err(ref _err) = call {
					crate::utils::notify!("Calling function with dependent argument failed");
				}

				let result = call?.returned_type;

				return Ok(FunctionCallResult {
					returned_type: result,
					warnings: Default::default(),
					called: None,
					special: None,
					result_was_const_computation: false,
				});
			}
		}

		let mut errors = ErrorsAndInfo::default();

		// Contravariant
		let mut type_arguments = map_vec::Map::new();

		self.set_this_for_behavior(
			called_with_new,
			this_value,
			&mut type_arguments,
			environment,
			types,
			&mut errors,
			call_site,
		);

		let local_arguments = self.assign_arguments_to_parameters::<E>(
			arguments,
			type_arguments,
			call_site_type_arguments,
			structure_generics.as_ref(),
			environment,
			types,
			&mut errors,
			call_site,
			behavior,
		);

		let mut type_arguments = FunctionTypeArguments {
			local_arguments,
			closure_ids: if let Some(StructureGenericArguments::Closure(cs)) = structure_generics {
				cs
			} else {
				Default::default()
			},
			call_site,
		};

		if E::CHECK_PARAMETERS {
			// TODO check free variables from inference
		}

		if !errors.errors.is_empty() {
			return Err(errors.errors);
		}

		let returned_type = if let FunctionEffect::SideEffects {
			events,
			closed_over_variables,
			free_variables: _,
		} = &self.effect
		{
			let returned_from_evaluation = behavior.new_function_context(self.id, |target| {
				Self::evaluate_function_side_effects(
					events.clone(),
					closed_over_variables.clone(),
					types,
					&mut type_arguments,
					&mut errors,
					this_value,
					environment,
					target,
					call_site,
				)
			});

			returned_from_evaluation.returned_type(types)
		} else {
			type_arguments.local_arguments.remove(&TypeId::NEW_TARGET_ARG);

			crate::utils::notify!("Substituting return type (no return) {:?}", type_arguments);
			let base = substitute(self.return_type, &mut type_arguments, environment, types);

			// Don't need to wrap in open as
			base

			// TODO not always
			// types.new_open_type(base)
		};

		if !errors.errors.is_empty() {
			crate::utils::notify!("Got {} application errors", errors.errors.len());
			return Err(errors.errors);
		}

		if let CalledWithNew::New { .. } = called_with_new {
			// TODO ridiculous early return primitive rule
			match self.behavior {
				FunctionBehavior::ArrowFunction { is_async: _ } => todo!(),
				FunctionBehavior::Method { .. } => todo!(),
				FunctionBehavior::Function { is_async: _, is_generator: _, free_this_id } => {
					let new_instance_type = type_arguments
						.local_arguments
						.remove(&free_this_id)
						.expect("no this argument?");

					return Ok(FunctionCallResult {
						returned_type: new_instance_type,
						warnings: errors.warnings,
						called: Some(self.id),
						special: None,
						result_was_const_computation: false,
					});
				}
				FunctionBehavior::Constructor { non_super_prototype: _, this_object_type } => {
					crate::utils::notify!("Registered this {:?}", this_object_type);
					let new_instance_type = type_arguments
						.local_arguments
						.remove(&this_object_type)
						.expect("no this argument?");

					return Ok(FunctionCallResult {
						returned_type: new_instance_type,
						warnings: errors.warnings,
						called: Some(self.id),
						special: None,
						result_was_const_computation: false,
					});
				}
			}
		}

		Ok(FunctionCallResult {
			returned_type,
			warnings: errors.warnings,
			// unconditional_exception,
			called: Some(self.id),
			special: None,
			result_was_const_computation: false,
		})
	}

	#[allow(clippy::too_many_arguments)]
	fn evaluate_function_side_effects(
		events: Vec<Event>,
		closed_over_variables: ClosedOverVariables,
		types: &mut TypeStore,
		type_arguments: &mut FunctionTypeArguments,
		errors: &mut ErrorsAndInfo,
		this_value: ThisValue,
		environment: &mut Environment,
		target: &mut InvocationContext,
		call_site: SpanWithSource,
	) -> ApplicationResult {
		let this_closure_id = if closed_over_variables.0.is_empty() {
			None
		} else {
			let closure_id = types.new_closure_id();
			type_arguments.closure_ids.push(closure_id);
			Some(closure_id)
		};

		// TODO
		let mut return_result = ApplicationResult::Completed;

		// Apply events here
		let mut events_iterator = events.into_iter();
		while let Some(event) = events_iterator.next() {
			let current_errors = errors.errors.len();
			let result = apply_event(
				event,
				&mut events_iterator,
				this_value,
				type_arguments,
				environment,
				target,
				types,
				errors,
			);

			// Adjust call sites. (because they aren't currently passed down)
			for d in &mut errors.errors[current_errors..] {
				if let FunctionCallingError::TDZ { call_site: ref mut c, .. }
				| FunctionCallingError::SetPropertyConstraint {
					call_site: ref mut c, ..
				}
				| FunctionCallingError::UnconditionalThrow { call_site: ref mut c, .. } = d
				{
					*c = Some(call_site);
				}
			}

			if result.is_it_so_over() {
				return_result = result;
				break;
			}
		}

		// TODO conditional
		if let ApplicationResult::Interrupt(crate::events::FinalEvent::Throw { thrown, position }) =
			&return_result
		{
			environment.throw_value(*thrown, *position);
		}

		if let Some(closure_id) = this_closure_id {
			crate::utils::notify!("Setting closure variables");

			// Set closed over values
			// TODO `this`
			closed_over_variables.0.iter().for_each(|(variable, value)| {
				let value = substitute(*value, type_arguments, environment, types);
				environment
					.info
					.closure_current_values
					.insert((closure_id, RootReference::Variable(*variable)), value);

				crate::utils::notify!("in {:?} set {:?} to {:?}", closure_id, variable, value);
			});
		}

		return_result
	}

	#[allow(clippy::too_many_arguments)]
	fn set_this_for_behavior(
		&self,
		called_with_new: CalledWithNew,
		this_value: ThisValue,
		type_arguments: &mut map_vec::Map<TypeId, TypeId>,
		environment: &mut Environment,
		types: &mut TypeStore,
		errors: &mut ErrorsAndInfo,
		call_site: source_map::BaseSpan<SourceId>,
	) {
		match self.behavior {
			FunctionBehavior::ArrowFunction { .. } => {}
			FunctionBehavior::Method { free_this_id, .. } => {
				// TODO
				let value_of_this = if let Some(value) = this_value.get_passed() {
					value
				} else {
					crate::utils::notify!(
						"method has no 'this' passed :?. Passing `undefined` here"
					);
					TypeId::UNDEFINED_TYPE
				};

				crate::utils::notify!(
					"free this id {:?} & value of this {:?}",
					free_this_id,
					value_of_this
				);

				type_arguments.insert(free_this_id, value_of_this);
			}
			FunctionBehavior::Function { is_async: _, is_generator: _, free_this_id } => {
				match called_with_new {
					CalledWithNew::New { on: _ } => {
						crate::utils::notify!("TODO set prototype");
						// if let Some(prototype) = non_super_prototype {
						// let this_ty = environment.create_this(prototype, types);
						let value_of_this =
							types.register_type(Type::Object(crate::types::ObjectNature::RealDeal));

						type_arguments.insert(free_this_id, value_of_this);
					}
					CalledWithNew::SpecialSuperCall { this_type } => {
						type_arguments.insert(free_this_id, this_type);
					}
					CalledWithNew::None => {
						// TODO
						let value_of_this = this_value.get(environment, types, call_site);

						type_arguments.insert(free_this_id, value_of_this);
					}
				}
			}
			FunctionBehavior::Constructor { non_super_prototype: _, this_object_type } => {
				crate::utils::notify!("Here {:?}", called_with_new);
				match called_with_new {
					CalledWithNew::None => {
						errors
							.errors
							.push(FunctionCallingError::NeedsToBeCalledWithNewKeyword(call_site));
					}
					CalledWithNew::New { on: _ } => {
						// TODO is this okay?
					}
					CalledWithNew::SpecialSuperCall { this_type } => {
						crate::utils::notify!("Result is {:?}", this_object_type);
						type_arguments.insert(this_object_type, this_type);
					}
				}
			}
		}

		{
			let new_target_value = match called_with_new {
				CalledWithNew::New { on } => on,
				CalledWithNew::SpecialSuperCall { .. } => {
					crate::utils::notify!("Get this type for super new.target");
					TypeId::ERROR_TYPE
					// let ty = this_value.0;
					// let on = crate::types::printing::print_type(
					// 	ty,
					// 	types,
					// 	environment,
					// 	true,
					// );
					// crate::utils::notify!("This argument {}", on);
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
		mut type_arguments: TypeArguments,
		call_site_type_arguments: Option<CallSiteTypeArguments>,
		parent: Option<&StructureGenericArguments>,
		environment: &mut Environment,
		types: &mut TypeStore,
		errors: &mut ErrorsAndInfo,
		call_site: source_map::BaseSpan<SourceId>,
		behavior: &E,
	) -> TypeArguments {
		for (parameter_idx, parameter) in self.parameters.parameters.iter().enumerate() {
			// TODO temp

			// This handles if the argument is missing but allowing elided arguments
			let argument = arguments.get(parameter_idx);

			if let Some(SynthesisedArgument { spread, value, position }) = argument {
				if *spread {
					crate::utils::notify!("TODO spread arguments");
				}

				if E::CHECK_PARAMETERS {
					let result = check_parameter_type(
						parameter.ty,
						call_site_type_arguments.as_ref(),
						parent,
						*value,
						&mut type_arguments,
						environment,
						types,
					);

					if let SubTypeResult::IsNotSubType(_reasons) = result {
						let type_arguments = Some(GenericChainLink::FunctionRoot {
							parent,
							call_site_type_arguments: call_site_type_arguments.as_ref(),
							type_arguments: &type_arguments,
						});

						// crate::utils::notify!("Type arguments are {:?}", type_arguments);

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
					type_arguments.insert(parameter.ty, *value);
				}
			} else if parameter.is_optional {
				type_arguments.insert(parameter.ty, TypeId::UNDEFINED_TYPE);
			} else {
				errors.errors.push(FunctionCallingError::MissingArgument {
					parameter_position: parameter.position,
					call_site,
				});
			}
		}
		if self.parameters.parameters.len() < arguments.len() {
			if let Some(ref rest_parameter) = self.parameters.rest_parameter {
				// TODO use object builder
				let rest_parameter_array_type =
					environment.info.new_object(Some(TypeId::ARRAY_TYPE), types, true, false);

				for (idx, argument) in
					arguments.iter().enumerate().skip(self.parameters.parameters.len())
				{
					if E::CHECK_PARAMETERS {
						let result = check_parameter_type(
							rest_parameter.item_type,
							call_site_type_arguments.as_ref(),
							parent,
							argument.value,
							&mut type_arguments,
							environment,
							types,
						);

						// TODO different diagnostic?
						if let SubTypeResult::IsNotSubType(_reasons) = result {
							let type_arguments = Some(GenericChainLink::FunctionRoot {
								parent,
								call_site_type_arguments: call_site_type_arguments.as_ref(),
								type_arguments: &type_arguments,
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

					environment.info.register_property(
						rest_parameter_array_type,
						crate::context::information::Publicity::Public,
						PropertyKey::from_usize(idx),
						crate::PropertyValue::Value(argument.value),
						true,
						Some(argument.position),
					);
				}

				// TODO only if no error
				type_arguments.insert(rest_parameter.ty, rest_parameter_array_type);
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
		if let Some(call_site_type_arguments) = call_site_type_arguments {
			for (on, (arg, _)) in call_site_type_arguments {
				type_arguments.entry(on).or_insert(arg);
			}
		}

		type_arguments
	}
}

fn check_parameter_type(
	parameter_ty: TypeId,
	call_site_type_arguments: Option<&CallSiteTypeArguments>,
	parent_generics: Option<&StructureGenericArguments>,
	value: TypeId,
	type_arguments: &mut TypeArguments,
	environment: &mut Environment,
	types: &mut TypeStore,
) -> SubTypeResult {
	// TODO properties
	let mut contributions = Contributions {
		call_site_type_arguments,
		staging_covariant: map_vec::Map::new(),
		staging_contravariant: map_vec::Map::new(),
		parent: parent_generics,
		existing_covariant: type_arguments,
	};

	crate::utils::notify!("Value is {:?}", value);

	// TODO WIP
	let result = type_is_subtype_with_generics(
		parameter_ty,
		GenericChain::None,
		value,
		GenericChain::None,
		// Some(GenericChain::new_from_ref(super::StructureGenericArgumentsRef {
		// 	type_restrictions: &restrictions,
		// 	properties: &properties,
		// })),
		&mut contributions,
		environment,
		types,
		crate::subtyping::SubTypingMode::Contravariant { depth: 0 },
		&mut Default::default(),
	);

	let Contributions { staging_covariant, staging_contravariant, .. } = contributions;

	for (_res, _pos) in staging_contravariant {
		crate::utils::notify!("TODO merge? pick highest?");
	}

	// TODO WIP
	for (on, implementations) in staging_covariant {
		let mut into_iter = implementations.into_iter();
		let mut most_accurate = into_iter.next().unwrap();
		for next in into_iter {
			if next.1 > most_accurate.1 {
				most_accurate = next;
			}
		}

		match type_arguments.entry(on) {
			map_vec::map::Entry::Occupied(mut existing) => {
				let new = types.new_or_type(*existing.get(), most_accurate.0);
				existing.insert(new);
			}
			map_vec::map::Entry::Vacant(vacant) => {
				vacant.insert(most_accurate.0);
			}
		}
	}

	result
}

fn synthesise_arguments_for_parameter<T: ReadFromFS, A: crate::ASTImplementation>(
	callable: &Logical<FunctionLike>,
	arguments: &[UnsynthesisedArgument<A>],
	call_site_type_arguments: Option<Vec<(TypeId, SpanWithSource)>>,
	parent_arguments: Option<&StructureGenericArguments>,
	environment: &mut Environment,
	checking_data: &mut crate::CheckingData<T, A>,
) -> (Vec<SynthesisedArgument>, Option<TypeRestrictions>) {
	fn synthesise_call_site_type_argument_hints(
		type_parameters: &GenericTypeParameters,
		call_site_type_arguments: Vec<(TypeId, SpanWithSource)>,
		types: &crate::types::TypeStore,
		environment: &mut Environment,
	) -> TypeRestrictions {
		crate::utils::notify!("call_site_type_arguments {:?}", call_site_type_arguments);

		type_parameters
			.0
			.iter()
			.zip(call_site_type_arguments)
			.map(|(param, (ty, position))| {
				if let Type::RootPolyType(PolyNature::FunctionGeneric { eager_fixed, .. }) =
					types.get_type_by_id(param.id)
				{
					let mut basic_subtyping = BasicEquality {
						add_property_restrictions: false,
						position,
						// This shouldn't be needed in this scenario
						object_constraints: Default::default(),
						allow_errors: true,
					};

					let type_is_subtype =
						type_is_subtype(*eager_fixed, ty, &mut basic_subtyping, environment, types);

					match type_is_subtype {
						SubTypeResult::IsSubType => {}
						SubTypeResult::IsNotSubType(_) => {
							todo!("generic argument does not match restriction")
						}
					}
				} else {
					todo!();
					// crate::utils::notify!("Generic parameter with no aliasing restriction, I think this fine on internals");
				};

				(param.id, (ty, position))
			})
			.collect()
	}

	match callable {
		Logical::Pure(function) => {
			let function = checking_data.types.get_function_from_id(function.function);

			let type_arguments_restrictions =
				if let (Some(ref type_parameters), Some(call_site_type_arguments)) =
					(&function.type_parameters, call_site_type_arguments)
				{
					Some(synthesise_call_site_type_argument_hints(
						type_parameters,
						call_site_type_arguments,
						&checking_data.types,
						environment,
					))
				} else {
					None
				};

			let parameters = function.parameters.clone();

			let arguments = arguments
				.iter()
				.enumerate()
				.map(|(idx, argument)| {
					let expected_type = parameters.get_parameter_type_at_index(idx).map_or(
						TypeId::ANY_TYPE,
						|(parameter_type, _)| {
							crate::utils::notify!("Here {:?}", parameter_type);

							let parameter_type =
								if let Type::RootPolyType(PolyNature::Parameter { fixed_to }) =
									checking_data.types.get_type_by_id(parameter_type)
								{
									*fixed_to
								} else {
									crate::utils::notify!("Not parameter type");
									parameter_type
								};

							if type_arguments_restrictions.is_some() || parent_arguments.is_some() {
								let arguments =
									match parent_arguments {
										Some(arguments) => {
											let mut restrictions = arguments.clone();
											if let Some(type_arguments_restrictions) =
												type_arguments_restrictions.clone()
											{
												if let StructureGenericArguments::ExplicitRestrictions(ref mut s) = restrictions {
												s
													.extend(type_arguments_restrictions);
												} else {
													todo!("cannot extend closure or lookup")
												}
											}
											restrictions
										}
										None => StructureGenericArguments::ExplicitRestrictions(
											type_arguments_restrictions.clone().unwrap(),
										),
									};

								crate::utils::notify!("{:?} with {:?}", parameter_type, arguments);

								checking_data.types.register_type(Type::Constructor(
									Constructor::StructureGenerics(StructureGenerics {
										on: parameter_type,
										arguments,
									}),
								))
							} else {
								crate::utils::notify!("No generics");
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
		Logical::Implies { on, antecedent } => synthesise_arguments_for_parameter(
			on,
			arguments,
			call_site_type_arguments,
			Some(antecedent),
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
