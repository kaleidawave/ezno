use source_map::{SourceId, SpanWithSource};
use std::vec;

use crate::{
	context::{invocation::CheckThings, CallCheckingBehavior, Environment, Logical},
	diagnostics::{TypeCheckError, TypeStringRepresentation, TDZ},
	events::{application::ErrorsAndInfo, apply_event, Event, FinalEvent, RootReference},
	features::{
		constant_functions::{call_constant_function, ConstantFunctionError, ConstantOutput},
		functions::{FunctionBehavior, ThisValue},
	},
	subtyping::{type_is_subtype, BasicEquality, SubTypeResult},
	types::{functions::SynthesisedArgument, substitute, ObjectNature},
	types::{FunctionType, Type},
	FunctionId, SpecialExpressions, TypeId,
};

use super::{
	get_constraint, is_type_constant,
	poly_types::{
		generic_type_arguments::StructureGenericArguments, FunctionTypeArguments, SeedingContext,
	},
	properties::PropertyKey,
	Constructor, PolyNature, TypeStore,
};

pub struct CallingInput {
	pub called_with_new: CalledWithNew,
	pub this_value: ThisValue,
	pub call_site_type_arguments: Option<Vec<(TypeId, SpanWithSource)>>,
	pub call_site: SpanWithSource,
}

pub struct CallingInputWithoutThis {
	pub called_with_new: CalledWithNew,
	pub call_site_type_arguments: Option<Vec<(TypeId, SpanWithSource)>>,
	pub call_site: SpanWithSource,
}

pub struct UnsynthesisedArgument<'a, A: crate::ASTImplementation> {
	pub spread: bool,
	pub expression: &'a A::Expression<'a>,
}

pub fn call_type_handle_errors<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	ty: TypeId,
	arguments: Vec<UnsynthesisedArgument<A>>,
	input: CallingInput,
	environment: &mut Environment,
	checking_data: &mut crate::CheckingData<T, A>,
) -> (TypeId, Option<SpecialExpressions>) {
	// TODO maybe could do lower down...?
	let base_function = get_constraint(ty, &checking_data.types).unwrap_or(ty);

	// TODO fails for ors
	// TODO not great, gets parameters here, then
	let arguments = if let Type::Function(f, _) | Type::FunctionReference(f) =
		checking_data.types.get_type_by_id(base_function)
	{
		// TODO clone here bad
		let parameters = checking_data.types.get_function_from_id(*f).parameters.clone();
		arguments
			.into_iter()
			.enumerate()
			.map(|(idx, argument)| {
				let expected = parameters.parameters.get(idx).map_or(TypeId::ANY_TYPE, |p| {
					get_constraint(p.ty, &checking_data.types).unwrap_or(p.ty)
				});

				SynthesisedArgument {
					spread: argument.spread,
					position: A::expression_position(argument.expression)
						.with_source(environment.get_source()),
					value: A::synthesise_expression(
						argument.expression,
						expected,
						environment,
						checking_data,
					),
				}
			})
			.collect()
	} else {
		arguments
			.into_iter()
			.map(|argument| SynthesisedArgument {
				spread: argument.spread,
				position: A::expression_position(argument.expression)
					.with_source(environment.get_source()),
				value: A::synthesise_expression(
					argument.expression,
					TypeId::ANY_TYPE,
					environment,
					checking_data,
				),
			})
			.collect()
	};

	let call_site = input.call_site;

	let result =
		call_type(ty, arguments, input, environment, &mut CheckThings, &mut checking_data.types);

	match result {
		Ok(FunctionCallResult {
			returned_type,
			warnings,
			called: _,
			special,
			found_dependent_argument: _,
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

/// TODO this and aliases kindof broken
pub fn call_type<E: CallCheckingBehavior>(
	on: TypeId,
	arguments: Vec<SynthesisedArgument>,
	CallingInput { called_with_new, this_value, call_site_type_arguments, call_site }: CallingInput,
	top_environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
	let never_or_error_type =
		matches!(on, TypeId::ERROR_TYPE | TypeId::NEVER_TYPE).then_some(on).or_else(|| {
			arguments.iter().find_map(|a| {
				matches!(a.value, TypeId::ERROR_TYPE | TypeId::NEVER_TYPE).then_some(a.value)
			})
		});

	if let Some(never_or_error_type) = never_or_error_type {
		crate::utils::notify!(
			"Exiting earlier because of Never or Error being called or as argument"
		);
		return Ok(FunctionCallResult {
			called: None,
			returned_type: never_or_error_type,
			warnings: Vec::new(),
			special: None,
			found_dependent_argument: false,
		});
	}

	if let Some(constraint) = get_constraint(on, types) {
		create_generic_function_call(
			constraint,
			CallingInput { called_with_new, this_value, call_site_type_arguments, call_site },
			arguments,
			on,
			top_environment,
			behavior,
			types,
		)
	} else {
		let callable = get_logical_callable_from_type(on, types);

		if let Some(logical) = callable {
			let structure_generics = None;
			call_logical(
				logical,
				types,
				CallingInputWithoutThis { called_with_new, call_site_type_arguments, call_site },
				structure_generics,
				arguments,
				top_environment,
				behavior,
			)
		} else {
			Err(vec![FunctionCallingError::NotCallable {
				calling: crate::diagnostics::TypeStringRepresentation::from_type_id(
					on,
					&top_environment.as_general_context(),
					types,
					false,
				),
				call_site,
			}])
		}
	}
}

fn call_logical<E: CallCheckingBehavior>(
	logical: Logical<(FunctionId, ThisValue)>,
	types: &mut TypeStore,
	CallingInputWithoutThis { called_with_new, call_site_type_arguments, call_site }: CallingInputWithoutThis,
	structure_generics: Option<StructureGenericArguments>,
	arguments: Vec<SynthesisedArgument>,
	environment: &mut Environment,
	behavior: &mut E,
) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
	match logical {
		Logical::Pure((func, this_value)) => {
			if let Some(function_type) = types.functions.get(&func) {
				function_type.clone().call(
					CallingInput {
						called_with_new,
						this_value,
						call_site_type_arguments,
						call_site,
					},
					structure_generics,
					&arguments,
					environment,
					behavior,
					types,
					true,
				)
			} else {
				todo!("recursive function type")
			}
		}
		Logical::Or { left: _, right: _ } => todo!(),
		Logical::Implies { on, antecedent } => call_logical(
			*on,
			types,
			CallingInputWithoutThis { called_with_new, call_site_type_arguments, call_site },
			Some(antecedent),
			arguments,
			environment,
			behavior,
		),
	}
}

fn get_logical_callable_from_type(
	on: TypeId,
	types: &TypeStore,
) -> Option<Logical<(FunctionId, ThisValue)>> {
	match types.get_type_by_id(on) {
		Type::And(_, _) => todo!(),
		Type::Or(left, right) => {
			if let (Some(left), Some(right)) = (
				get_logical_callable_from_type(*left, types),
				get_logical_callable_from_type(*right, types),
			) {
				Some(Logical::Or { left: Box::new(left), right: Box::new(right) })
			} else {
				None
			}
		}
		Type::Constructor(Constructor::StructureGenerics(generic)) => {
			get_logical_callable_from_type(generic.on, types).map(|res| Logical::Implies {
				on: Box::new(res),
				antecedent: generic.arguments.clone(),
			})
		}
		Type::RootPolyType(_) | Type::Constructor(_) => todo!(),

		Type::AliasTo { to, name: _, parameters } => {
			if parameters.is_some() {
				todo!()
			}
			get_logical_callable_from_type(*to, types)
		}
		Type::Interface { .. } | Type::Constant(_) | Type::Object(_) => None,
		Type::Function(f, t) => Some(Logical::Pure((*f, *t))),
		// TODO not sure about `on`... also #98
		Type::FunctionReference(f) => Some(Logical::Pure((*f, ThisValue::Passed(on)))),
		Type::SpecialObject(_) => todo!(),
	}
}

fn create_generic_function_call<E: CallCheckingBehavior>(
	constraint: TypeId,
	CallingInput { called_with_new, this_value, call_site_type_arguments, call_site }: CallingInput,
	arguments: Vec<SynthesisedArgument>,
	on: TypeId,
	top_environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
	// crate::utils::notify!("On {:?}", types.get_type_by_id(constraint));

	let result = call_type(
		constraint,
		// TODO clone
		arguments.clone(),
		CallingInput { called_with_new, this_value, call_site_type_arguments, call_site },
		top_environment,
		behavior,
		types,
	)?;

	// TODO temp position, this should be added in `call_type`. TODO if open poly...? or pure
	let can_skip = if let Type::FunctionReference(f) = types.get_type_by_id(constraint) {
		let func = types.functions.get(f).unwrap();
		// TODO or some events that don't modify etc
		func.constant_function.is_some()
	} else {
		false
	};

	if !can_skip {
		for argument in &arguments {
			// TODO need to do in a function
			// All properties
			// Functions free variables etc
			// TODO if spread
			match types.get_type_by_id(argument.value) {
				Type::Interface { .. }
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
				Type::Function(_, _) => {
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

	let with = arguments.into_boxed_slice();

	// Skip constant returned reflects types
	if is_type_constant(result.returned_type, types) {
		crate::utils::notify!("Adding calls type here");

		behavior.get_latest_facts(top_environment).events.push(Event::CallsType {
			on,
			with,
			timing: crate::events::CallingTiming::Synchronous,
			called_with_new, // Don't care about output.
			reflects_dependency: None,
			position: call_site,
		});

		Ok(result)
	} else {
		// TODO work this out
		let is_open_poly = false;

		let reflects_dependency = if is_open_poly {
			None
		} else {
			// TODO does this need a recursive implementation?
			let returned_type_space = if TypeId::VOID_TYPE == result.returned_type {
				TypeId::ANY_TYPE
			} else {
				result.returned_type
			};

			if result.found_dependent_argument {
				Some(returned_type_space)
			} else {
				let constructor = Constructor::Image {
					// TODO on or to
					on,
					with: with.clone(),
					result: returned_type_space,
				};

				let constructor_return = types.register_type(Type::Constructor(constructor));

				Some(constructor_return)
			}
		};

		// Event already added if dependent argument
		if !result.found_dependent_argument {
			behavior.get_latest_facts(top_environment).events.push(Event::CallsType {
				on,
				with,
				timing: crate::events::CallingTiming::Synchronous,
				called_with_new,
				reflects_dependency,
				position: call_site,
			});
		}

		// TODO should wrap result in open poly
		Ok(FunctionCallResult {
			called: result.called,
			returned_type: reflects_dependency.unwrap_or(result.returned_type),
			warnings: result.warnings,
			special: None,
			found_dependent_argument: result.found_dependent_argument,
		})
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
}

pub struct InfoDiagnostic(pub String);

/// TODO *result* name bad
pub struct FunctionCallResult {
	pub called: Option<FunctionId>,
	pub returned_type: TypeId,
	// TODO
	pub warnings: Vec<InfoDiagnostic>,
	pub special: Option<SpecialExpressions>,
	pub found_dependent_argument: bool,
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
	/// Calls the function
	///
	/// Returns warnings and errors
	// Move references in a wrapping struct can be hard due to lifetimes
	#[allow(clippy::too_many_arguments)]
	pub(crate) fn call<E: CallCheckingBehavior>(
		&self,
		CallingInput { called_with_new, this_value, call_site_type_arguments, call_site }: CallingInput,
		parent_type_arguments: Option<StructureGenericArguments>,
		arguments: &[SynthesisedArgument],
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
				found_dependent_argument: false,
			});
			// let reason = FunctionCallingError::Recursed(self.id, call_site);
			// return Err(vec![reason])
		}

		if !environment.is_possibly_uncalled() {
			types.called_functions.insert(self.id);
		}

		if let (Some(const_fn_ident), true) = (self.constant_function.as_deref(), call_constant) {
			let has_dependent_argument =
				arguments.iter().any(|arg| types.get_type_by_id(arg.value).is_dependent())
					|| matches!(this_value, ThisValue::Passed(ty) if types.get_type_by_id(ty).is_dependent());

			let call_anyway = matches!(
				const_fn_ident,
				"debug_type"
					| "debug_type_rust" | "print_type"
					| "debug_effects" | "debug_effects_rust"
					| "satisfies" | "is_dependent"
					| "bind" | "create_proxy"
			);

			// Most of the time don't call using constant function if an argument is dependent.
			// But sometimes do for the cases above
			if call_anyway || !has_dependent_argument {
				// TODO event
				let result = call_constant_function(
					const_fn_ident,
					this_value,
					&call_site_type_arguments,
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
							found_dependent_argument: false,
							special,
						});
					}
					Ok(ConstantOutput::Diagnostic(diagnostic)) => {
						// crate::utils::notify!("Here, constant output");
						return Ok(FunctionCallResult {
							returned_type: TypeId::UNDEFINED_TYPE,
							warnings: vec![InfoDiagnostic(diagnostic)],
							called: None,
							found_dependent_argument: false,
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
				let with = arguments.to_vec().into_boxed_slice();
				// TODO with cloned!!
				let calling_input = CallingInput {
					called_with_new,
					this_value,
					call_site_type_arguments,
					call_site,
				};
				let call = self.call(
					calling_input,
					parent_type_arguments,
					arguments,
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

				// TODO pass down
				let on = types.register_type(Type::Function(self.id, this_value));
				let new_type =
					Type::Constructor(Constructor::Image { on, with: with.clone(), result });

				let ty = types.register_type(new_type);

				behavior.get_latest_facts(environment).events.push(Event::CallsType {
					on,
					with: arguments.to_vec().into_boxed_slice(),
					reflects_dependency: Some(ty),
					timing: crate::events::CallingTiming::Synchronous,
					called_with_new,
					position: call_site,
				});

				return Ok(FunctionCallResult {
					returned_type: ty,
					warnings: Default::default(),
					called: None,
					special: None,
					found_dependent_argument: true,
				});
			}
		}

		let mut errors = ErrorsAndInfo::default();

		// Type arguments of the function
		let local_type_argument_as_restrictions: map_vec::Map<
			TypeId,
			Vec<(TypeId, SpanWithSource)>,
		> = if let (Some(call_site_type_arguments), true) =
			(call_site_type_arguments, E::CHECK_PARAMETERS)
		{
			self.synthesise_call_site_type_arguments(call_site_type_arguments, types, environment)
		} else {
			map_vec::Map::new()
		};

		let mut seeding_context = SeedingContext {
			type_arguments: map_vec::Map::new(),
			type_restrictions: local_type_argument_as_restrictions,
			locally_held_functions: map_vec::Map::new(),
			argument_position_and_parameter_idx: (source_map::Nullable::NULL, 0),
		};

		match self.behavior {
			FunctionBehavior::ArrowFunction { is_async: _ } => {}
			FunctionBehavior::Method { free_this_id, .. } => {
				let value_of_this = if let Some(value) = this_value.get_passed() {
					value
				} else {
					crate::utils::notify!("method has no 'this' passed :?");
					TypeId::UNDEFINED_TYPE
				};

				crate::utils::notify!("ft id {:?} & vot {:?}", free_this_id, value_of_this);

				// TODO checking
				seeding_context
					.type_arguments
					.insert(free_this_id, vec![(value_of_this, source_map::Nullable::NULL, 0)]);
			}
			FunctionBehavior::Function { is_async: _, is_generator: _, free_this_id } => {
				match called_with_new {
					CalledWithNew::New { on: _ } => {
						crate::utils::notify!("TODO set prototype");
						// if let Some(prototype) = non_super_prototype {
						// let this_ty = environment.create_this(prototype, types);
						let value_of_this =
							types.register_type(Type::Object(crate::types::ObjectNature::RealDeal));

						seeding_context.type_arguments.insert(
							// TODO
							free_this_id,
							vec![(value_of_this, source_map::Nullable::NULL, 0)],
						);
					}
					CalledWithNew::SpecialSuperCall { this_type: _ } => todo!(),
					CalledWithNew::None => {
						// TODO
						let value_of_this = this_value.get(environment, types, &call_site);

						seeding_context.type_arguments.insert(
							free_this_id,
							vec![(value_of_this, source_map::Nullable::NULL, 0)],
						);
					}
				}
			}
			FunctionBehavior::Constructor { non_super_prototype: _, this_object_type: _ } => {
				if let CalledWithNew::None = called_with_new {
					errors
						.errors
						.push(FunctionCallingError::NeedsToBeCalledWithNewKeyword(call_site));
				}
			}
		}

		{
			let import_new_argument = match called_with_new {
				CalledWithNew::New { on } => on,
				CalledWithNew::SpecialSuperCall { .. } => {
					todo!()
					// let ty = this_value.0;
					// let on = crate::types::printing::print_type(
					// 	ty,
					// 	types,
					// 	&environment.as_general_context(),
					// 	true,
					// );
					// crate::utils::notify!("This argument {}", on);
					// ty
				}
				// In spec == undefined
				CalledWithNew::None => TypeId::UNDEFINED_TYPE,
			};

			// TODO on type arguments, not seeding context
			seeding_context.type_arguments.insert(
				TypeId::NEW_TARGET_ARG,
				vec![(import_new_argument, source_map::Nullable::NULL, 0)],
			);
		}

		let seeding_context = self.synthesise_arguments::<E>(
			arguments,
			seeding_context,
			environment,
			types,
			&mut errors,
			&call_site,
		);

		let SeedingContext {
			type_arguments: found,
			type_restrictions,
			locally_held_functions: _,
			argument_position_and_parameter_idx: _,
		} = seeding_context;

		// take the found and inject back into what it resolved
		let mut result_type_arguments = map_vec::Map::new();

		for (item, values) in found {
			// TODO only first ??
			let (value, argument_position, param) =
				values.into_iter().next().expect("no type argument ...?");

			if let Some(restrictions_for_item) = type_restrictions.get(&item) {
				for (restriction, restriction_position) in restrictions_for_item {
					let mut behavior = BasicEquality {
						add_property_restrictions: false,
						position: *restriction_position,
					};

					let result =
						type_is_subtype(*restriction, value, &mut behavior, environment, types);

					if let SubTypeResult::IsNotSubType(_err) = result {
						let argument_type = TypeStringRepresentation::from_type_id(
							value,
							&environment.as_general_context(),
							types,
							false,
						);
						let restriction_type = TypeStringRepresentation::from_type_id(
							*restriction,
							&environment.as_general_context(),
							types,
							false,
						);

						let restriction = Some((*restriction_position, restriction_type));
						let synthesised_parameter = &self.parameters.parameters[param];
						let parameter_type = TypeStringRepresentation::from_type_id(
							synthesised_parameter.ty,
							&environment.as_general_context(),
							types,
							false,
						);

						errors.errors.push(FunctionCallingError::InvalidArgumentType {
							argument_type,
							argument_position,
							parameter_type,
							parameter_position: synthesised_parameter.position,
							restriction,
						});
					}
				}
			}

			// TODO position is just the first
			result_type_arguments.insert(item, (value, argument_position));
		}

		let mut type_arguments = FunctionTypeArguments {
			structure_arguments: parent_type_arguments,
			local_arguments: result_type_arguments,
			closure_id: None,
		};

		if E::CHECK_PARAMETERS {
			// TODO check free variables from inference
		}

		if !errors.errors.is_empty() {
			return Err(errors.errors);
		}

		// Evaluate effects directly into environment
		let early_return = behavior.new_function_context(self.id, |target| {
			type_arguments.closure_id = if self.closed_over_variables.is_empty() {
				None
			} else {
				let closure_id = types.new_closure_id();
				Some(closure_id)
			};

			let mut return_result = None;

			// Apply events here
			for event in self.effects.clone() {
				let current_errors = errors.errors.len();
				let result = apply_event(
					event,
					this_value,
					&mut type_arguments,
					environment,
					target,
					types,
					&mut errors,
				);

				// Adjust call sites. (because they aren't currently passed down)
				for d in &mut errors.errors[current_errors..] {
					if let FunctionCallingError::TDZ { call_site: ref mut c, .. } = d {
						*c = Some(call_site);
					} else if let FunctionCallingError::SetPropertyConstraint {
						call_site: ref mut c,
						..
					} = d
					{
						*c = Some(call_site);
					}
				}

				if let value @ Some(_) = result {
					return_result = value;
					break;
				}
			}

			if let Some(closure_id) = type_arguments.closure_id {
				crate::utils::notify!("Setting closure variables");

				// Set closed over values
				self.closed_over_variables.iter().for_each(|(reference, value)| {
					let value = substitute(*value, &mut type_arguments, environment, types);
					environment
						.facts
						.closure_current_values
						.insert((closure_id, reference.clone()), value);

					crate::utils::notify!("in {:?} set {:?} to {:?}", closure_id, reference, value);
				});
			}

			return_result
		});

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
						.expect("no this argument7")
						.0;

					return Ok(FunctionCallResult {
						returned_type: new_instance_type,
						warnings: errors.warnings,
						called: Some(self.id),
						special: None,
						found_dependent_argument: false,
					});
				}
				FunctionBehavior::Constructor { non_super_prototype: _, this_object_type } => {
					crate::utils::notify!("Registered this {:?}", this_object_type);
					let new_instance_type = type_arguments
						.local_arguments
						.remove(&this_object_type)
						.expect("no this argument7")
						.0;

					return Ok(FunctionCallResult {
						returned_type: new_instance_type,
						warnings: errors.warnings,
						called: Some(self.id),
						special: None,
						found_dependent_argument: false,
					});
				}
			}
		}

		let returned_type = if let Some(early_return) = early_return {
			match early_return {
				FinalEvent::Break { .. } | FinalEvent::Continue { .. } => {
					unreachable!("function ended on continue / break")
				}
				FinalEvent::Throw { thrown: value, position } => {
					behavior.get_latest_facts(environment).throw_value_in_facts(value, position);
					TypeId::NEVER_TYPE
				}
				FinalEvent::Return { returned, returned_position: _ } => {
					// set events should cover property specialisation here:
					returned
				}
			}
		} else {
			crate::utils::notify!("Substituting return type (no return)");
			substitute(self.return_type, &mut type_arguments, environment, types)
		};

		Ok(FunctionCallResult {
			returned_type,
			warnings: errors.warnings,
			called: Some(self.id),
			special: None,
			found_dependent_argument: false,
		})
	}

	fn synthesise_arguments<E: CallCheckingBehavior>(
		&self,
		arguments: &[SynthesisedArgument],
		mut seeding_context: SeedingContext,
		environment: &mut Environment,
		types: &mut TypeStore,
		errors: &mut ErrorsAndInfo,
		call_site: &source_map::BaseSpan<SourceId>,
	) -> SeedingContext {
		for (parameter_idx, parameter) in self.parameters.parameters.iter().enumerate() {
			// TODO temp

			// This handles if the argument is missing but allowing elided arguments
			let argument = arguments.get(parameter_idx);

			if let Some(SynthesisedArgument { spread, value, position }) = argument {
				if *spread {
					crate::utils::notify!("TODO spread arguments");
				}

				// TODO bad
				seeding_context.argument_position_and_parameter_idx = (*position, parameter_idx);

				// crate::utils::notify!(
				// 	"param {}, arg {}",
				// 	types.debug_type(parameter.ty),
				// 	types.debug_type(*argument_type)
				// );

				if E::CHECK_PARAMETERS {
					// crate::utils::notify!("Param {:?} :> {:?}", parameter.ty, *argument_type);
					let result = type_is_subtype(
						parameter.ty,
						*value,
						&mut seeding_context,
						environment,
						types,
					);

					if let SubTypeResult::IsNotSubType(_reasons) = result {
						errors.errors.push(FunctionCallingError::InvalidArgumentType {
							parameter_type: TypeStringRepresentation::from_type_id(
								parameter.ty,
								&environment.as_general_context(),
								types,
								false,
							),
							argument_type: TypeStringRepresentation::from_type_id(
								*value,
								&environment.as_general_context(),
								types,
								false,
							),
							parameter_position: parameter.position,
							argument_position: *position,
							restriction: None,
						});
					}
				} else {
					// Already checked so can set
					seeding_context.set_id(parameter.ty, (*value, *position, parameter_idx), false);
				}
			} else if parameter.optional {
				seeding_context.set_id(
					parameter.ty,
					(TypeId::UNDEFINED_TYPE, source_map::Nullable::NULL, parameter_idx),
					false,
				);
			} else {
				errors.errors.push(FunctionCallingError::MissingArgument {
					parameter_position: parameter.position,
					call_site: *call_site,
				});
			}

			// if let SubTypeResult::IsNotSubType(_reasons) = result {
			// 	if auto_inserted_arg {
			// 		todo!("different error");
			// 	} else {
			// 		errors.errors.push(FunctionCallingError::InvalidArgumentType {
			// 			parameter_index: idx,
			// 			argument_type: argument_type.into_owned(),
			// 			parameter_type: parameter_type.clone(),
			// 			parameter_pos: parameter.2.clone().unwrap(),
			// 		});
			// 	}
			// }
			// } else {
			// 	errors.errors.push(FunctionCallingError::MissingArgument {
			// 		parameter_pos: parameter.2.clone().unwrap(),
			// 	});
			// }
		}
		if self.parameters.parameters.len() < arguments.len() {
			if let Some(ref rest_parameter) = self.parameters.rest_parameter {
				// TODO use object builder
				let rest_parameter_array_type =
					environment.facts.new_object(Some(TypeId::ARRAY_TYPE), types, true, false);

				for (idx, argument) in
					arguments.iter().enumerate().skip(self.parameters.parameters.len())
				{
					if E::CHECK_PARAMETERS {
						let result = type_is_subtype(
							rest_parameter.item_type,
							argument.value,
							&mut seeding_context,
							environment,
							types,
						);

						if let SubTypeResult::IsNotSubType(_reasons) = result {
							errors.errors.push(FunctionCallingError::InvalidArgumentType {
								parameter_type: TypeStringRepresentation::from_type_id(
									rest_parameter.item_type,
									&environment.as_general_context(),
									types,
									false,
								),
								argument_type: TypeStringRepresentation::from_type_id(
									argument.value,
									&environment.as_general_context(),
									types,
									false,
								),
								argument_position: argument.position,
								parameter_position: rest_parameter.position,
								restriction: None,
							});
						}
					}

					environment.facts.register_property(
						rest_parameter_array_type,
						crate::context::facts::Publicity::Public,
						PropertyKey::from_usize(idx),
						crate::PropertyValue::Value(argument.value),
						true,
						Some(argument.position),
					);
				}

				// TODO what does the 0 mean here?
				seeding_context.set_id(
					rest_parameter.ty,
					(rest_parameter_array_type, source_map::Nullable::NULL, 0),
					false,
				);
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
				errors.errors.push(FunctionCallingError::ExcessArguments { count, position });
			}
		}
		seeding_context
	}

	fn synthesise_call_site_type_arguments(
		&self,
		call_site_type_arguments: Vec<(TypeId, SpanWithSource)>,
		types: &crate::types::TypeStore,
		environment: &mut Environment,
	) -> map_vec::Map<TypeId, Vec<(TypeId, SpanWithSource)>> {
		if let Some(ref typed_parameters) = self.type_parameters {
			typed_parameters
				.0
				.iter()
				.zip(call_site_type_arguments)
				.map(|(param, (ty, position))| {
					if let Type::RootPolyType(PolyNature::Generic { eager_fixed, .. }) =
						types.get_type_by_id(param.id)
					{
						let mut basic_subtyping =
							BasicEquality { add_property_restrictions: false, position };

						let type_is_subtype = type_is_subtype(
							*eager_fixed,
							ty,
							&mut basic_subtyping,
							environment,
							types,
						);

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

					(param.id, vec![(ty, position)])
				})
				.collect()
		} else {
			crate::utils::notify!("Call site arguments on function without typed parameters...");
			map_vec::Map::new()
		}
	}
}
