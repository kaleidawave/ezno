use source_map::{BaseSpan, Nullable, SpanWithSource};

use crate::{
	context::{invocation::CheckThings, CallCheckingBehavior, Environment, InformationChain},
	diagnostics::{
		InfoDiagnostic, TypeCheckError, TypeCheckWarning, TypeStringRepresentation,
		VariableUsedInTDZ,
	},
	events::{
		application::ApplicationInput, apply_events, ApplicationResult, Event, RootReference,
	},
	features::{
		constant_functions::{
			call_constant_function, CallSiteTypeArguments, ConstantFunctionError, ConstantOutput,
		},
		objects::{ObjectBuilder, SpecialObject},
	},
	subtyping::{
		type_is_subtype, type_is_subtype_with_generics, State, SubTypeResult, SubTypingMode,
		SubTypingOptions,
	},
	types::{
		functions::{FunctionBehavior, FunctionEffect, FunctionType},
		generics::substitution::SubstitutionArguments,
		get_structure_arguments_based_on_object_constraint,
		logical::{Invalid, Logical, LogicalOrValid, NeedsCalculation, PossibleLogical},
		properties::AccessMode,
		substitute, GenericChainLink, ObjectNature, PartiallyAppliedGenerics, Type,
	},
	FunctionId, GenericTypeParameters, ReadFromFS, SpecialExpressions, TypeId,
};

use super::{
	generics::{contributions::Contributions, generic_type_arguments::GenericArguments},
	get_constraint,
	properties::PropertyKey,
	Constructor, GenericChain, PolyNature, TypeRestrictions, TypeStore,
};

/// Other information to do with calling
#[derive(Clone, Copy)]
pub struct CallingInput {
	/// Also depicts what happens with `this`
	pub called_with_new: CalledWithNew,
	pub call_site: SpanWithSource,

	/// An option to invocation
	pub max_inline: u16,
}

#[derive(Clone, Copy, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub enum ThisValue {
	Passed(TypeId),
	/// Or pick from [`Constructor::Property`]
	#[default]
	UseParent,
}

impl ThisValue {
	pub(crate) fn get(
		self,
		environment: &mut Environment,
		types: &TypeStore,
		position: SpanWithSource,
	) -> TypeId {
		match self {
			ThisValue::Passed(value) => value,
			ThisValue::UseParent => environment.get_value_of_this(types, position),
		}
	}

	pub(crate) fn get_passed(self) -> Option<TypeId> {
		match self {
			ThisValue::Passed(value) => Some(value),
			ThisValue::UseParent => None,
		}
	}
}

/// For diagnostics
#[derive(Copy, Clone, Default)]
pub enum CallingContext {
	#[default]
	Regular,
	Getter,
	Setter,
	JSX,
	TemplateLiteral,
	Super,
	Iteration,
}

#[derive(Default)]
pub struct CallingDiagnostics {
	pub errors: Vec<FunctionCallingError>,
	pub warnings: Vec<crate::diagnostics::TypeCheckWarning>,
	pub info: Vec<crate::diagnostics::InfoDiagnostic>,
}

impl CallingDiagnostics {
	pub(crate) fn append_to(
		self,
		context: CallingContext,
		diagnostics_container: &mut crate::DiagnosticsContainer,
	) {
		self.errors.into_iter().for_each(|error| {
			let error = match context {
				CallingContext::Regular => TypeCheckError::FunctionCallingError(error),
				CallingContext::JSX => TypeCheckError::JSXCallingError(error),
				CallingContext::TemplateLiteral => {
					TypeCheckError::TemplateLiteralCallingError(error)
				}
				CallingContext::Getter => TypeCheckError::GetterCallingError(error),
				CallingContext::Setter => TypeCheckError::SetterCallingError(error),
				CallingContext::Super => TypeCheckError::SuperCallError(error),
				// TODO maybe separate error (even thoguh this should not occur)
				#[allow(clippy::match_same_arms)]
				CallingContext::Iteration => TypeCheckError::FunctionCallingError(error),
			};
			diagnostics_container.add_error(error);
		});
		self.warnings.into_iter().for_each(|warning| diagnostics_container.add_warning(warning));
		self.info.into_iter().for_each(|crate::diagnostics::InfoDiagnostic(message, position)| {
			diagnostics_container.add_info(crate::diagnostics::Diagnostic::Position {
				reason: message,
				position,
				kind: crate::diagnostics::DiagnosticKind::Info,
			});
		});
	}
}

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct SynthesisedArgument {
	pub(crate) spread: bool,
	pub(crate) value: TypeId,
	pub(crate) position: SpanWithSource,
}

impl SynthesisedArgument {
	pub fn non_spread_type(&self) -> Result<TypeId, ()> {
		if self.spread {
			Err(())
		} else {
			Ok(self.value)
		}
	}
}

/// Intermediate step for inference
pub struct UnsynthesisedArgument<'a, A: crate::ASTImplementation> {
	pub spread: bool,
	pub expression: &'a A::Expression<'a>,
}

/// Intermediate type for calling a function
///
/// Generic arguments handled with `Logical::Implies`
#[derive(Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub struct FunctionLike {
	pub(crate) function: FunctionId,
	/// For generic calls
	pub(crate) from: Option<TypeId>,
	/// From, maybe ignored if [`CalledWithNew`] overrides
	pub(crate) this_value: ThisValue,
}

pub struct CallingOutput {
	pub called: Option<FunctionId>,
	/// TODO should this be
	pub result: Option<ApplicationResult>,
	pub special: Option<SpecialExpressions>,
	// pub special: Option<SpecialExpressions>,
	pub result_was_const_computation: bool,
}

pub struct BadCallOutput {
	/// This might be from specialisation
	pub returned_type: TypeId,
}

/// Also synthesise arguments in terms of expected types
pub fn call_type_handle_errors<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	ty: TypeId,
	call_site_type_arguments: Option<Vec<(TypeId, SpanWithSource)>>,
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
		Ok(LogicalOrValid::Logical(callable)) => {
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
					(call_site_type_arguments, parent_arguments.as_ref()),
					environment,
					checking_data,
				);

			let mut check_things = CheckThings { debug_types: checking_data.options.debug_types };
			let mut diagnostics = CallingDiagnostics::default();
			let result = call_logical(
				callable,
				(arguments, type_argument_restrictions, parent_arguments),
				input,
				environment,
				&mut checking_data.types,
				(&mut check_things, &mut diagnostics),
			);

			diagnostics
				.append_to(CallingContext::Regular, &mut checking_data.diagnostics_container);

			match result {
				Ok(CallingOutput {
					result,
					called: _,
					special,
					result_was_const_computation: _,
				}) => {
					let returned_type = application_result_to_return_type(
						result,
						environment,
						&mut checking_data.types,
					);

					(returned_type, special)
				}
				Err(error) => (error.returned_type, None),
			}
		}
		Ok(LogicalOrValid::NeedsCalculation(l)) => match l {
			NeedsCalculation::Infer { on } => {
				if on == TypeId::ERROR_TYPE {
					(TypeId::UNIMPLEMENTED_ERROR_TYPE, None)
				} else {
					let mut new_arguments = Vec::<SynthesisedArgument>::new();
					let mut inferred_parameters =
						super::functions::SynthesisedParameters::default();

					for (idx, argument) in arguments.iter().enumerate() {
						let value = A::synthesise_expression(
							argument.expression,
							TypeId::ANY_TYPE,
							environment,
							checking_data,
						);

						let position = A::expression_position(argument.expression)
							.with_source(environment.get_source());

						new_arguments.push(SynthesisedArgument {
							spread: argument.spread,
							position,
							value,
						});
						inferred_parameters.parameters.push(
							super::functions::SynthesisedParameter {
								name: char::from(0x61u8 + idx as u8).to_string(),
								is_optional: false,
								ty: value,
								position,
							},
						);
					}

					let image_ty =
						checking_data.types.register_type(Type::Constructor(Constructor::Image {
							// TODO
							on,
							with: new_arguments.into_boxed_slice(),
							result: TypeId::ANY_TO_INFER_TYPE,
						}));

					let new = checking_data.types.new_function_type_annotation(
						None,
						inferred_parameters,
						image_ty,
						&call_site,
					);

					environment.constraint_inference_requests.insert(on, new);

					(image_ty, None)
				}
			}
			NeedsCalculation::Proxy(..) => {
				crate::utilities::notify!("TODO calling proxy");
				(TypeId::UNIMPLEMENTED_ERROR_TYPE, None)
			}
		},
		Err(Invalid(ty)) => {
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
				environment.throw_value(thrown, position, types);
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

#[derive(Debug, Copy, Clone, binary_serialize_derive::BinarySerializable)]
pub enum Callable {
	/// TODO `ThisValue` not always needed
	Fixed(FunctionId, ThisValue),
	Type(TypeId),
}

impl Callable {
	pub(crate) fn from_type(ty: TypeId, types: &TypeStore) -> Self {
		if let Type::SpecialObject(SpecialObject::Function(func_id, _)) = types.get_type_by_id(ty) {
			Callable::Fixed(*func_id, ThisValue::UseParent)
		} else {
			crate::utilities::notify!("Here!!");
			Callable::Type(ty)
		}
	}

	pub(crate) fn new_from_function(function: FunctionType, types: &mut TypeStore) -> Self {
		let id = function.id;
		types.functions.insert(id, function);
		Callable::Fixed(id, ThisValue::UseParent)
	}

	pub(crate) fn into_type(self, types: &mut TypeStore) -> TypeId {
		match self {
			Callable::Fixed(id, this_value) => {
				types.register_type(Type::SpecialObject(SpecialObject::Function(id, this_value)))
			}
			Callable::Type(ty) => ty,
		}
	}

	pub(crate) fn get_return_type(self, types: &TypeStore) -> TypeId {
		match self {
			Callable::Fixed(id, _this_value) => types.get_function_from_id(id).return_type,
			Callable::Type(ty) => {
				if let Type::SpecialObject(SpecialObject::Function(id, _)) =
					types.get_type_by_id(ty)
				{
					types.get_function_from_id(*id).return_type
				} else {
					crate::utilities::notify!("Cannot get return type");
					TypeId::UNIMPLEMENTED_ERROR_TYPE
				}
			}
		}
	}

	pub(crate) fn get_first_argument(self, types: &TypeStore) -> TypeId {
		match self {
			Callable::Fixed(id, _this_value) => types
				.get_function_from_id(id)
				.parameters
				.get_parameter_type_at_index(0)
				.map_or(TypeId::ERROR_TYPE, |(ty, _)| ty),
			Callable::Type(ty) => {
				if let Type::SpecialObject(SpecialObject::Function(id, _)) =
					types.get_type_by_id(ty)
				{
					types
						.get_function_from_id(*id)
						.parameters
						.get_parameter_type_at_index(0)
						.map_or(TypeId::ERROR_TYPE, |(ty, _)| ty)
				} else {
					crate::utilities::notify!("Cannot get first arugment");
					TypeId::ERROR_TYPE
				}
			}
		}
	}

	pub(crate) fn call<B: CallCheckingBehavior>(
		self,
		arguments: Vec<SynthesisedArgument>,
		input: CallingInput,
		top_environment: &mut Environment,
		(behavior, diagnostics): (&mut B, &mut CallingDiagnostics),
		types: &mut TypeStore,
	) -> Result<CallingOutput, BadCallOutput> {
		match self {
			Callable::Fixed(on, this_value) => {
				let function_like = FunctionLike { function: on, from: None, this_value };

				call_logical(
					Logical::Pure(function_like),
					(arguments, None, None),
					input,
					top_environment,
					types,
					(behavior, diagnostics),
				)
			}
			Callable::Type(on) => {
				let callable = get_logical_callable_from_type(on, None, None, types);
				if let Ok(LogicalOrValid::Logical(callable)) = callable {
					call_logical(
						callable,
						(arguments, None, None),
						input,
						top_environment,
						types,
						(behavior, diagnostics),
					)
				} else {
					crate::utilities::notify!("callable={:?}", callable);

					diagnostics.errors.push(FunctionCallingError::NotCallable {
						calling: crate::diagnostics::TypeStringRepresentation::from_type_id(
							on,
							top_environment,
							types,
							behavior.debug_types(),
						),
						call_site: input.call_site,
					});

					Err(BadCallOutput { returned_type: TypeId::ERROR_TYPE })
				}
			}
		}
	}
}

/// This could possibly be done in one step
fn get_logical_callable_from_type(
	ty: TypeId,
	on: Option<ThisValue>,
	from: Option<TypeId>,
	types: &TypeStore,
) -> PossibleLogical<FunctionLike> {
	// if ty == TypeId::ERROR_TYPE {
	// 	return Err(MissingOrToCalculate::Error);
	// }
	if ty == TypeId::ANY_TYPE {
		crate::utilities::notify!("Calling ANY");
		return Ok(NeedsCalculation::Infer { on: from.unwrap() }.into());
	}

	let le_ty = types.get_type_by_id(ty);

	match le_ty {
		Type::Class { .. } | Type::Interface { .. } | Type::Constant(_) | Type::Object(_) => {
			Err(Invalid(ty))
		}
		Type::And(_, _) => todo!(),
		Type::Or(left, right) => {
			let left = get_logical_callable_from_type(*left, on, from, types)?;
			let right = get_logical_callable_from_type(*right, on, from, types)?;
			Ok(Logical::Or {
				condition: TypeId::OPEN_BOOLEAN_TYPE,
				left: Box::new(left),
				right: Box::new(right),
			}
			.into())
		}
		Type::Narrowed { narrowed_to, from } => {
			get_logical_callable_from_type(*narrowed_to, on, Some(*from), types)
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
			Ok(Logical::Pure(function).into())
		}
		Type::SpecialObject(SpecialObject::Function(f, t)) => {
			let this_value = on.unwrap_or(*t);
			let from = Some(from.unwrap_or(ty));
			Ok(Logical::Pure(FunctionLike { from, function: *f, this_value }).into())
		}
		Type::SpecialObject(so) => match so {
			crate::features::objects::SpecialObject::Proxy { .. } => todo!(),
			_ => Err(Invalid(ty)),
		},
		Type::PartiallyAppliedGenerics(generic) => {
			let result = get_logical_callable_from_type(generic.on, on, from, types)?;
			if let LogicalOrValid::Logical(result) = result {
				Ok(Logical::Implies { on: Box::new(result), antecedent: generic.arguments.clone() }
					.into())
			} else {
				crate::utilities::notify!("Should not be here");
				Ok(result)
			}
		}
		Type::Constructor(Constructor::Property { on, under: _, result, mode }) => {
			crate::utilities::notify!("Passing {:?}", on);

			let this_value = if let AccessMode::DoNotBindThis = mode {
				ThisValue::UseParent
			} else {
				ThisValue::Passed(*on)
			};
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
				if let LogicalOrValid::Logical(result) = result {
					Ok(Logical::Implies { on: Box::new(result), antecedent }.into())
				} else {
					crate::utilities::notify!("TODO not antecedent {:?}", antecedent);
					Ok(result)
				}
			} else {
				Ok(result)
			}
		}
		Type::RootPolyType(_) | Type::Constructor(_) => {
			let constraint = get_constraint(ty, types).unwrap();
			crate::utilities::notify!(
				"Getting callable constructor / root poly type! {:?}",
				constraint
			);
			get_logical_callable_from_type(constraint, on, Some(ty), types)
		}
	}
}

#[allow(clippy::too_many_arguments)]
fn call_logical<B: CallCheckingBehavior>(
	logical: Logical<FunctionLike>,
	(arguments, explicit_type_arguments, structure_generics): (
		Vec<SynthesisedArgument>,
		Option<CallSiteTypeArguments>,
		Option<GenericArguments>,
	),
	input: CallingInput,
	top_environment: &mut Environment,
	types: &mut TypeStore,
	(behavior, diagnostics): (&mut B, &mut CallingDiagnostics),
) -> Result<CallingOutput, BadCallOutput> {
	match logical {
		Logical::Pure(function) => {
			if let Some(function_type) = types.functions.get(&function.function) {
				// TODO clone
				let function_type = function_type.clone();

				if let FunctionEffect::Constant { identifier: ref const_fn_ident, .. } =
					&function_type.effect
				{
					let has_dependent_argument =
						arguments.iter().any(|arg| types.get_type_by_id(arg.value).is_dependent());

					// || matches!(this_value, ThisValue::Passed(ty) if types.get_type_by_id(ty).is_dependent());

					crate::utilities::notify!(
						"has_dependent_argument={:?}, const_fn_ident={:?}",
						has_dependent_argument,
						const_fn_ident
					);

					// TODO just for debugging. These have their constant things called every time AND queue an event
					let is_independent_function = const_fn_ident.ends_with("independent");

					let call_anyway = const_fn_ident.starts_with("debug")
						|| const_fn_ident.starts_with("print")
						|| is_independent_function
						|| matches!(
							const_fn_ident.as_str(),
							"satisfies" | "is_dependent" | "bind" | "proxy:constructor"
						);

					// {
					// 	let arguments = arguments
					// 		.iter()
					// 		.map(|a| types.get_type_by_id(a.value))
					// 		.collect::<Vec<_>>();

					// 	crate::utilities::notify!(
					// 		"E::CHECK_PARAMETERS={:?}, args={:?}, ident={:?}",
					// 		E::CHECK_PARAMETERS,
					// 		arguments,
					// 		const_fn_ident
					// 	);
					// }

					// For debugging only
					if is_independent_function && B::CHECK_PARAMETERS {
						let function_id = function.function;
						let value = Event::CallsType {
							on: Callable::Fixed(function.function, function.this_value),
							with: arguments.clone().into_boxed_slice(),
							timing: crate::events::CallingTiming::Synchronous,
							called_with_new: input.called_with_new,
							reflects_dependency: None,
							call_site: input.call_site,
							possibly_thrown: None,
						};
						behavior.get_latest_info(top_environment).events.push(value);
						return Ok(CallingOutput {
							called: Some(function_id),
							result: None,
							special: Some(SpecialExpressions::Marker),
							result_was_const_computation: true,
						});
					}

					// Most of the time don't call using constant function if an argument is dependent.
					// But sometimes do for the cases above
					if call_anyway || !has_dependent_argument {
						// TODO event
						let result = call_constant_function(
							const_fn_ident,
							// TODO
							function.this_value,
							explicit_type_arguments.as_ref(),
							&arguments,
							types,
							top_environment,
							input.call_site,
							diagnostics,
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
										position: input.call_site,
									}),
									called: None,
									result_was_const_computation: !is_independent_function,
									special,
								});
							}
							Ok(ConstantOutput::Diagnostic(diagnostic)) => {
								diagnostics.info.push(InfoDiagnostic(diagnostic, input.call_site));
								return Ok(CallingOutput {
									result: None,
									called: None,
									result_was_const_computation: !is_independent_function,
									// WIP!!
									special: Some(SpecialExpressions::Marker),
								});
							}
							Err(ConstantFunctionError::NoLogicForIdentifier(name)) => {
								let err = FunctionCallingError::NoLogicForIdentifier(
									name,
									input.call_site,
								);
								diagnostics.errors.push(err);
								return Err(BadCallOutput {
									returned_type: types.new_error_type(function_type.return_type),
								});
							}
							Err(ConstantFunctionError::FunctionCallingError(err)) => {
								diagnostics.errors.push(err);
								return Err(BadCallOutput {
									returned_type: types.new_error_type(function_type.return_type),
								});
							}
							Err(ConstantFunctionError::CannotComputeConstant) => {
								crate::utilities::notify!(
									"Constant function calling failed, non constant params"
								);
							}
						}
					} else if !has_dependent_argument {
						// TODO with cloned!!
						let call = function_type.call(
							(
								function.this_value,
								&arguments,
								explicit_type_arguments,
								structure_generics,
							),
							input,
							top_environment,
							(behavior, diagnostics),
							types,
						);

						return match call {
							Ok(call) => Ok(CallingOutput {
								result: call.result,
								called: None,
								special: None,
								result_was_const_computation: false,
								// TODO
							}),
							Err(err) => {
								crate::utilities::notify!(
									"Calling function with dependent argument failed"
								);
								Err(err)
							}
						};
					}
				}

				// Temp fix for structure generics, doesn't work in access
				let structure_generics = if let (false, Some(on)) =
					(structure_generics.is_some(), function.this_value.get_passed())
				{
					if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
						on: _,
						arguments,
					}) = types.get_type_by_id(get_constraint(on, types).unwrap_or(on))
					{
						Some(arguments.clone())
					} else {
						None
					}
				} else {
					structure_generics
				};

				let mut result = function_type.call(
					(function.this_value, &arguments, explicit_type_arguments, structure_generics),
					input,
					top_environment,
					(behavior, diagnostics),
					types,
				)?;

				// crate::utilities::notify!("result {:?} {:?}", result.called, result.returned_type);

				// is poly
				let is_poly_or_failed_const_call = matches!(
					function_type.effect,
					FunctionEffect::Unknown | FunctionEffect::InputOutput { .. }
				) || (matches!(function_type.effect, FunctionEffect::Constant { .. } if !result.result_was_const_computation));

				if is_poly_or_failed_const_call && function.from.is_some() {
					// if function_type.effect
					// This should be okay, constant or IO functions don't mutate their arguments...?
					// if !matches!(
					// 	&function_type.effect,
					// 	FunctionEffect::Constant { .. } | FunctionEffect::InputOutput { .. }
					// ) {
					// 	for a
					// 	mark_possible_mutation(&arguments, types, top_environment);
					// }

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
					let reflects_dependency = if types.get_type_by_id(result_as_type).is_constant()
					{
						None
					} else {
						let id = types.register_type(Type::Constructor(Constructor::Image {
							// TODO
							on: function.from.expect("function `on`"),
							// TODO...
							with: with.clone(),
							result: result_as_type,
						}));

						Some(id)
					};

					result.result = Some(ApplicationResult::Return {
						returned: reflects_dependency.unwrap_or(result_as_type),
						position: input.call_site,
					});

					let possibly_thrown = if let FunctionEffect::InputOutput { may_throw, .. }
					| FunctionEffect::Constant { may_throw, .. } = &function_type.effect
					{
						// crate::utilities::notify!("Constant / input output: may_throw");
						*may_throw
					} else {
						None
					};

					let on = match function.from {
						Some(ty) => Callable::Type(ty),
						None => Callable::Fixed(function.function, function.this_value),
					};
					behavior.get_latest_info(top_environment).events.push(Event::CallsType {
						on,
						with,
						timing: crate::events::CallingTiming::Synchronous,
						called_with_new: input.called_with_new,
						reflects_dependency,
						call_site: input.call_site,
						possibly_thrown,
					});
				}

				Ok(result)
			} else {
				panic!("no function")
			}
		}
		Logical::Or { .. } => {
			crate::utilities::notify!("Calling OR");
			Err(BadCallOutput { returned_type: TypeId::UNIMPLEMENTED_ERROR_TYPE })
			// todo!("{:?}", (condition, left, right));
			// if let (Ok(_left), Ok(_right)) = (*left, *right) {
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

			// TODO combine information (merge)
			// Ok(CallingOutput { called: None, result: Soe, warnings: (), special: (), result_was_const_computation: () })
			// } else {
			// 	// TODO inference and errors
			// 	let err = FunctionCallingError::NotCallable {
			// 		calling: TypeStringRepresentation::from_type_id(
			// 			based_on,
			// 			top_environment,
			// 			types,
			// 			false,
			// 		),
			// 		call_site: input.call_site,
			// 	};
			// 	Err(BadCallOutput { returned_type: TypeId::ERROR_TYPE, errors: vec![err] })
			// }
		}
		Logical::Implies { on, antecedent } => {
			// crate::utilities::notify!("antecedent={:?}", antecedent);
			call_logical(
				*on,
				(arguments, explicit_type_arguments, Some(antecedent)),
				input,
				top_environment,
				types,
				(behavior, diagnostics),
			)
		}
		Logical::BasedOnKey { .. } => {
			crate::utilities::notify!("Calling based on key?");
			Err(BadCallOutput { returned_type: TypeId::UNIMPLEMENTED_ERROR_TYPE })
		}
	}
}

fn mark_possible_mutation(
	argument: &SynthesisedArgument,
	parameter_type: TypeId,
	types: &mut TypeStore,
	top_environment: &mut Environment,
) {
	match types.get_type_by_id(argument.value) {
		Type::Interface { .. }
		| Type::Class { .. }
		| Type::AliasTo { .. }
		| Type::And(_, _)
		| Type::Object(ObjectNature::AnonymousTypeAnnotation(_))
		| Type::FunctionReference(_)
		| Type::PartiallyAppliedGenerics(_)
		| Type::Or(_, _) => {
			crate::utilities::notify!("Unreachable");
		}
		Type::Constant(_) => {}
		Type::Narrowed { .. } | Type::RootPolyType(_) | Type::Constructor(_) => {
			// All dependent anyway
			crate::utilities::notify!("TODO if any properties set etc");
		}
		Type::SpecialObject(SpecialObject::Function(_, _)) => {
			crate::utilities::notify!("TODO record that function could be called");
		}
		Type::Object(ObjectNature::RealDeal) => {
			top_environment.possibly_mutated_objects.insert(argument.value, parameter_type);
			crate::utilities::notify!("TODO record methods could be called here as well");
		}
		Type::SpecialObject(_) => {
			crate::utilities::notify!("TODO record stuff if mutable");
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
		expected_count: usize,
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
	VariableUsedInTDZ {
		error: VariableUsedInTDZ,
		/// Should be set by parent
		call_site: SpanWithSource,
	},
	InvalidRegExp(crate::diagnostics::InvalidRegExp),
	/// For #18
	SetPropertyConstraint {
		property_type: TypeStringRepresentation,
		value_type: TypeStringRepresentation,
		assignment_position: SpanWithSource,
		/// Should be set by parent
		call_site: SpanWithSource,
	},
	/// For #18
	DeleteConstraint {
		constraint: TypeStringRepresentation,
		delete_position: SpanWithSource,
		/// Should be set by parent
		call_site: SpanWithSource,
	},
	NotConfigurable {
		property: crate::diagnostics::PropertyKeyRepresentation,
		/// Should be set by parent
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

#[derive(Debug, Default, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum CalledWithNew {
	/// With `new X(...)`;
	New {
		// [See new.target](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target), which contains a reference to what called new. This does not == this_type
		on: TypeId,
	},
	GetterOrSetter {
		this_type: TypeId,
	},
	/// With `super(...)`
	Super {
		this_type: TypeId,
	},
	#[default]
	None,
}

impl FunctionType {
	/// Calls the function and returns warnings and errors
	pub(crate) fn call<B: CallCheckingBehavior>(
		&self,
		(this_value, arguments, call_site_type_arguments, parent_arguments): (
			ThisValue,
			&[SynthesisedArgument],
			Option<CallSiteTypeArguments>,
			Option<GenericArguments>,
		),
		input: CallingInput,
		environment: &mut Environment,
		(behavior, diagnostics): (&mut B, &mut CallingDiagnostics),
		types: &mut crate::TypeStore,
	) -> Result<CallingOutput, BadCallOutput> {
		// TODO check that parameters vary
		if behavior.in_recursive_cycle(self.id) {
			crate::utilities::notify!("Encountered recursion");
			crate::utilities::notify!("TODO should be BadCallOutput");
			let returned = types.new_error_type(self.return_type);
			return Ok(CallingOutput {
				called: Some(self.id),
				result: Some(ApplicationResult::Return { returned, position: input.call_site }),
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

		let errors = CallingDiagnostics::default();

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
			input.called_with_new,
			this_value,
			&mut type_arguments.arguments,
			environment,
			types,
			input.call_site,
			(behavior, diagnostics),
		);

		self.assign_arguments_to_parameters::<B>(
			arguments,
			&mut type_arguments,
			(call_site_type_arguments.clone(), parent_arguments.as_ref()),
			environment,
			types,
			input.call_site,
			matches!(self.effect, FunctionEffect::SideEffects { .. }),
			(behavior, diagnostics),
		);

		if !diagnostics.errors.is_empty() {
			return Err(BadCallOutput {
				// TODO what about `new`
				returned_type: types.new_error_type(self.return_type),
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

					let current_errors = diagnostics.errors.len();
					let current_warnings = diagnostics.warnings.len();

					// Apply events here
					let result = apply_events(
						events,
						&ApplicationInput {
							this_value,
							call_site: input.call_site,
							max_inline: input.max_inline,
						},
						substitution,
						environment,
						target,
						types,
						diagnostics,
					);

					// Adjust call sites. (because they aren't currently passed down)
					for d in &mut diagnostics.errors[current_errors..] {
						if let FunctionCallingError::VariableUsedInTDZ {
							call_site: ref mut c,
							..
						}
						| FunctionCallingError::SetPropertyConstraint {
							call_site: ref mut c,
							..
						} = d
						{
							*c = input.call_site;
						}
					}
					for d in &mut diagnostics.warnings[current_warnings..] {
						if let TypeCheckWarning::ConditionalExceptionInvoked {
							call_site: ref mut c,
							..
						} = d
						{
							*c = input.call_site;
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

			// crate::utilities::notify!(
			// 	"Substituting return type (no return) type_arguments={:?}",
			// 	type_arguments
			// 		.arguments
			// 		.iter()
			// 		.map(|(k, v)| (types.get_type_by_id(*k), types.get_type_by_id(*v)))
			// 		.collect::<Vec<_>>()
			// );

			let returned = substitute(self.return_type, &type_arguments, environment, types);
			Some(ApplicationResult::Return { returned, position: input.call_site })
		};

		if !diagnostics.errors.is_empty() {
			crate::utilities::notify!("Got {} application errors", errors.errors.len());
			return Err(BadCallOutput {
				// TODO what about `new`
				returned_type: types.new_error_type(self.return_type),
			});
		}

		// TODO what does super return?
		let result = if let CalledWithNew::New { .. } = input.called_with_new {
			// TODO ridiculous early return primitive rule
			let returned = match self.behavior {
				FunctionBehavior::ArrowFunction { .. } | FunctionBehavior::Method { .. } => {
					TypeId::ERROR_TYPE
				}
				FunctionBehavior::Constructor { prototype: _, this_object_type, name: _ } => {
					*type_arguments.arguments.get(&this_object_type).expect("no this argument?")
				}
				FunctionBehavior::Function { is_async: _, is_generator: _, this_id, .. } => {
					if let Type::Constructor(Constructor::ConditionalResult {
						truthy_result, ..
					}) = types.get_type_by_id(this_id)
					{
						*type_arguments.arguments.get(truthy_result).expect("no this argument?")
					} else {
						unreachable!()
					}
				}
			};
			// TODO if return type is primitive (or replace primitives with new_instance_type)
			Some(ApplicationResult::Return { returned, position: input.call_site })
		} else {
			result
		};

		Ok(CallingOutput {
			result,
			called: Some(self.id),
			special: None,
			result_was_const_computation: false,
		})
	}

	/// TODO set not using `type_arguments`
	#[allow(clippy::too_many_arguments)]
	fn set_this_for_behavior<B: CallCheckingBehavior>(
		&self,
		called_with_new: CalledWithNew,
		this_value: ThisValue,
		type_arguments: &mut crate::Map<TypeId, TypeId>,
		environment: &mut Environment,
		types: &mut TypeStore,
		call_site: SpanWithSource,
		(behavior, diagnostics): (&mut B, &mut CallingDiagnostics),
	) {
		match self.behavior {
			FunctionBehavior::ArrowFunction { .. } => {}
			FunctionBehavior::Method { free_this_id, .. } => {
				// TODO
				let value_of_this =
					if let CalledWithNew::GetterOrSetter { this_type } = called_with_new {
						this_type
					} else if let Some(value) = this_value.get_passed() {
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
					constraint_inference_requests: None,
					others: Default::default(),
				};

				let type_is_subtype =
					type_is_subtype(base_type, value_of_this, &mut state, environment, types);

				if let SubTypeResult::IsNotSubType(_reason) = type_is_subtype {
					diagnostics.errors.push(FunctionCallingError::MismatchedThis {
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
				this_id,
				prototype,
				name: _,
			} => {
				match called_with_new {
					CalledWithNew::New { on: _ } => {
						// This condition is by creation
						// TODO THIS IS FRAGILE AND UNCLEAR
						let (this_id, constraint) =
							if let Type::Constructor(Constructor::ConditionalResult {
								truthy_result: this_id,
								otherwise_result: constraint,
								..
							}) = types.get_type_by_id(this_id)
							{
								(*this_id, *constraint)
							} else {
								unreachable!()
							};

						// {
						// 	let debug = true;
						// 	crate::utilities::notify!(
						// 		"Checking against this constraint {}",
						// 		crate::types::printing::print_type(
						// 			this_id,
						// 			types,
						// 			environment,
						// 			debug
						// 		)
						// 	);
						// 	crate::utilities::notify!(
						// 		"Other {}",
						// 		crate::types::printing::print_type(
						// 			constraint,
						// 			types,
						// 			environment,
						// 			debug
						// 		)
						// 	);
						// }

						// Check `this` constraint
						{
							let this_constraint = get_constraint(constraint, types);

							// Check `this` has all prototype information
							if let Some(this_constraint) = this_constraint {
								let mut state = State {
									already_checked: Default::default(),
									mode: SubTypingMode::default(),
									contributions: None,
									object_constraints: None,
									others: SubTypingOptions::default(),
									constraint_inference_requests: None,
								};

								let result = type_is_subtype(
									this_constraint,
									prototype,
									&mut state,
									environment,
									types,
								);

								// crate::utilities::notify!("result={:?}", result);

								if let SubTypeResult::IsNotSubType(_reason) = result {
									diagnostics.errors.push(FunctionCallingError::MismatchedThis {
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
						}

						// Set argument
						{
							// TODO think so
							let is_under_dyn = true;
							let value_of_this = environment.info.new_object(
								Some(prototype),
								types,
								call_site,
								is_under_dyn,
							);

							type_arguments.insert(this_id, value_of_this);

							// crate::utilities::notify!("Calling new on regular function");
							// crate::utilities::notify!("Set {:?} top {:?}", free_this_id, value_of_this);
						}
					}
					CalledWithNew::GetterOrSetter { this_type }
					| CalledWithNew::Super { this_type } => {
						crate::utilities::notify!("Super / getter / setter on regular function?");

						type_arguments.insert(this_id, this_type);
					}
					CalledWithNew::None => {
						// TODO
						let value_of_this = this_value.get(environment, types, call_site);

						type_arguments.insert(this_id, value_of_this);
					}
				}
			}
			FunctionBehavior::Constructor { prototype, this_object_type, name: _ } => {
				crate::utilities::notify!("Here {:?}", called_with_new);
				match called_with_new {
					CalledWithNew::GetterOrSetter { .. } | CalledWithNew::None => {
						diagnostics
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
					TypeId::UNIMPLEMENTED_ERROR_TYPE
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
				CalledWithNew::GetterOrSetter { .. } | CalledWithNew::None => {
					TypeId::UNDEFINED_TYPE
				}
			};

			type_arguments.insert(TypeId::NEW_TARGET_ARG, new_target_value);
		}
	}

	#[allow(clippy::too_many_arguments)]
	fn assign_arguments_to_parameters<B: CallCheckingBehavior>(
		&self,
		arguments: &[SynthesisedArgument],
		type_arguments: &mut SubstitutionArguments<'static>,
		(call_site_type_arguments, parent_arguments): (
			Option<CallSiteTypeArguments>,
			Option<&GenericArguments>,
		),
		environment: &mut Environment,
		types: &mut TypeStore,
		call_site: SpanWithSource,
		evaluating_effects: bool,
		(behavior, diagnostics): (&mut B, &mut CallingDiagnostics),
	) {
		for (parameter_idx, parameter) in self.parameters.parameters.iter().enumerate() {
			// TODO temp

			// This handles if the argument is missing but allowing elided arguments
			let argument = arguments.get(parameter_idx);

			if let Some(argument) = argument {
				if argument.spread {
					crate::utilities::notify!("TODO spread arguments");
				}

				if B::CHECK_PARAMETERS {
					let result = check_parameter_type(
						parameter.ty,
						call_site_type_arguments.as_ref(),
						parent_arguments,
						argument,
						type_arguments,
						environment,
						types,
					);

					if let SubTypeResult::IsNotSubType(_reasons) = result {
						let type_arguments = Some(GenericChainLink::FunctionRoot {
							parent_arguments,
							call_site_type_arguments: call_site_type_arguments.as_ref(),
							type_arguments: &type_arguments.arguments,
						});

						crate::utilities::notify!("Type arguments are {:?}", type_arguments);

						diagnostics.errors.push(FunctionCallingError::InvalidArgumentType {
							parameter_type: TypeStringRepresentation::from_type_id_with_generics(
								parameter.ty,
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
							parameter_position: parameter.position,
							argument_position: argument.position,
							restriction: None,
						});
					}
				} else {
					// Already checked so can set
					type_arguments.arguments.insert(parameter.ty, argument.value);
				}
			} else if parameter.is_optional {
				type_arguments.arguments.insert(parameter.ty, TypeId::UNDEFINED_TYPE);
			} else {
				diagnostics.errors.push(FunctionCallingError::MissingArgument {
					parameter_position: parameter.position,
					call_site,
				});
			}
		}

		// Spread parameters here
		if self.parameters.parameters.len() < arguments.len() {
			if let Some(ref rest_parameter) = self.parameters.rest_parameter {
				// TODO reuse synthesise_array literal logic (especially for spread items)
				let mut basis = evaluating_effects.then(|| {
					ObjectBuilder::new(
						Some(TypeId::ARRAY_TYPE),
						types,
						rest_parameter.position,
						&mut environment.info,
					)
				});

				let mut count = 0;

				for argument in arguments.iter().skip(self.parameters.parameters.len()) {
					if B::CHECK_PARAMETERS {
						let result = check_parameter_type(
							rest_parameter.item_type,
							call_site_type_arguments.as_ref(),
							parent_arguments,
							argument,
							type_arguments,
							environment,
							types,
						);

						// TODO different diagnostic?
						if let SubTypeResult::IsNotSubType(_reasons) = result {
							let type_arguments = Some(GenericChainLink::FunctionRoot {
								parent_arguments,
								call_site_type_arguments: call_site_type_arguments.as_ref(),
								type_arguments: &type_arguments.arguments,
							});

							diagnostics.errors.push(FunctionCallingError::InvalidArgumentType {
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

					// Add to spread array
					if let Some(basis) = basis.as_mut() {
						let key = PropertyKey::from_usize(count);
						basis.append(
							crate::types::properties::Publicity::Public,
							key,
							crate::types::properties::PropertyValue::Value(argument.value),
							argument.position,
							&mut environment.info,
						);
					}

					count += 1;
				}

				// Set length of spread array
				if let Some(mut basis) = basis {
					let length = types.new_constant_type(crate::Constant::Number(
						(count as f64).try_into().unwrap(),
					));

					basis.append(
						crate::types::properties::Publicity::Public,
						PropertyKey::String("length".into()),
						crate::types::properties::PropertyValue::Value(length),
						rest_parameter.position,
						&mut environment.info,
					);

					let rest_parameter_array_type = basis.build_object();
					type_arguments.arguments.insert(rest_parameter.ty, rest_parameter_array_type);
				}
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

				if B::CHECK_PARAMETERS {
					diagnostics
						.errors
						.push(FunctionCallingError::ExcessArguments { count, position });
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
	parameter_type: TypeId,
	call_site_type_arguments: Option<&CallSiteTypeArguments>,
	parent: Option<&GenericArguments>,
	argument: &SynthesisedArgument,
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
		// TODO some
		constraint_inference_requests: Some(Vec::new()),
	};

	let result = type_is_subtype_with_generics(
		(parameter_type, GenericChain::None),
		(argument.value, GenericChain::None),
		&mut state,
		environment,
		types,
	);

	// TODO WIP abstract. Also need to merge somethings
	for (key, value) in state.constraint_inference_requests.unwrap() {
		environment.constraint_inference_requests.insert(key, value);
	}

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

		// TODO
		let no_events = false;
		if no_events {
			mark_possible_mutation(argument, parameter_type, types, environment);
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
				if let Type::RootPolyType(PolyNature::FunctionGeneric { extends, .. }) =
					types.get_type_by_id(param.type_id)
				{
					let mut state = State {
						already_checked: Default::default(),
						mode: SubTypingMode::default(),
						contributions: None,
						others: Default::default(),
						object_constraints: None,
						constraint_inference_requests: None,
					};
					let type_is_subtype =
						type_is_subtype(*extends, ty, &mut state, environment, types);

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
						let first_excess_type_parameter = &call_site_type_arguments[0];
						checking_data.diagnostics_container.add_error(
							TypeCheckError::FunctionCallingError(
								FunctionCallingError::ExcessTypeArguments {
									expected_count: 0,
									count: call_site_type_arguments.len(),
									position: first_excess_type_parameter.1,
								},
							),
						);

						None
					}
					(Some(ref function_type_parameters), Some(call_site_type_arguments)) => {
						let expected_parameters_length = function_type_parameters.0.len();
						let provided_parameters_length = call_site_type_arguments.len();
						if provided_parameters_length > expected_parameters_length {
							let first_excess_type_parameter =
								&call_site_type_arguments[expected_parameters_length];
							let last_excess_type_parameter = call_site_type_arguments
								.last()
								.unwrap_or(first_excess_type_parameter);

							let error_position = first_excess_type_parameter
								.1
								.without_source()
								.union(last_excess_type_parameter.1.without_source())
								.with_source(first_excess_type_parameter.1.source);

							checking_data.diagnostics_container.add_error(
								TypeCheckError::FunctionCallingError(
									FunctionCallingError::ExcessTypeArguments {
										expected_count: expected_parameters_length,
										count: provided_parameters_length,
										position: error_position,
									},
								),
							);
						}
						Some(synthesise_call_site_type_argument_hints(
							function_type_parameters,
							call_site_type_arguments,
							&checking_data.types,
							environment,
						))
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
							"Here calculating lookup argument prototype={:?} {:?}",
							prototype,
							on
						);
						let map =
							checking_data.types.lookup_generic_map.get(prototype).unwrap().clone();

						for (under, lookup) in map.iter() {
							let entries =
								lookup.calculate_lookup(environment, &checking_data.types, *on);
							let mut iter = entries.into_iter();
							let mut ty = iter.next().unwrap_or(TypeId::NEVER_TYPE);
							for other in iter {
								// crate::utilities::notify!("here {:?}", checking_data.types.get_type_by_id(other));
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
								crate::utilities::notify!(
									"Parameter is not `PolyNature::Parameter`? {:?}",
									ty
								);
								parameter_type
							};

							if let Some(arguments) = type_arguments.clone() {
								if let Some((ty, _)) = arguments.get(&parameter_type) {
									*ty
								} else {
									// Unfortunately this seems the best way to pass down to expected type
									crate::utilities::notify!("Here");
									checking_data.types.register_type(
										Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
											on: parameter_type,
											arguments: GenericArguments::ExplicitRestrictions(
												arguments,
											),
										}),
									)
								}
							} else {
								parameter_type
							}
						},
					);

					// {
					// 	let debug = true;
					// 	crate::utilities::notify!(
					// 		"Here!, expected = {}",
					// 		crate::types::printing::print_type(
					// 			expected_type,
					// 			&checking_data.types,
					// 			environment,
					// 			debug
					// 		)
					// 	);
					// }

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
		Logical::BasedOnKey { .. } => todo!(),
	}
}
