use source_map::{SourceId, Span, SpanWithSource};
use std::{collections::HashMap, iter};

use crate::{
	behavior::{
		constant_functions::{call_constant_function, ConstantResult},
		functions::ThisValue,
	},
	context::{calling::CheckThings, get_value_of_variable, CallCheckingBehavior, Environment},
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	events::{apply_event, Event, RootReference},
	subtyping::{type_is_subtype, BasicEquality, NonEqualityReason, SubTypeResult},
	types::{
		functions::SynthesisedArgument, poly_types::generic_type_arguments::TypeArgumentStore,
		specialize,
	},
	types::{FunctionType, Type},
	FunctionId, TypeId,
};

use super::{
	poly_types::{
		generic_type_arguments::{FunctionTypeArgument, StructureGenericArguments},
		FunctionTypeArguments, SeedingContext,
	},
	Constructor, FunctionKind, PolyNature, StructureGenerics, TypeStore,
};

pub fn call_type_handle_errors<T: crate::FSResolver>(
	ty: TypeId,
	// Overwritten by .call, else look at binding
	called_with_new: CalledWithNew,
	this_value: ThisValue,
	call_site_type_arguments: Option<Vec<(SpanWithSource, TypeId)>>,
	arguments: Vec<SynthesisedArgument>,
	call_site: SpanWithSource,
	environment: &mut Environment,
	checking_data: &mut crate::CheckingData<T>,
) -> TypeId {
	let result = call_type(
		ty,
		called_with_new,
		this_value,
		call_site_type_arguments,
		arguments,
		call_site.clone(),
		environment,
		&mut CheckThings,
		&mut checking_data.types,
	);
	match result {
		Ok(FunctionCallResult { returned_type, warnings, called }) => {
			for warning in warnings {
				checking_data.diagnostics_container.add_info(
					crate::diagnostics::Diagnostic::Position {
						reason: warning.0,
						position: call_site.clone(),
						kind: crate::diagnostics::DiagnosticKind::Info,
					},
				)
			}

			if let Some(called) = called {
				checking_data.type_mappings.called_functions.insert(called);
			}
			returned_type
		}
		Err(errors) => {
			for error in errors {
				checking_data
					.diagnostics_container
					.add_error(TypeCheckError::FunctionCallingError(error))
			}
			TypeId::ERROR_TYPE
		}
	}
}

/// TODO this and aliases kindof broken
pub(crate) fn call_type<'a, E: CallCheckingBehavior>(
	on: TypeId,
	called_with_new: CalledWithNew,
	// Overwritten by .call, else look at binding
	this_value: ThisValue,
	call_site_type_arguments: Option<Vec<(SpanWithSource, TypeId)>>,
	arguments: Vec<SynthesisedArgument>,
	call_site: SpanWithSource,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
	if on == TypeId::ERROR_TYPE
		|| arguments.iter().any(|arg| match arg {
			SynthesisedArgument::NonSpread { ty, .. } => *ty == TypeId::ERROR_TYPE,
		}) {
		return Ok(FunctionCallResult {
			called: None,
			returned_type: TypeId::ERROR_TYPE,
			warnings: Vec::new(),
		});
	}

	let ty = types.get_type_by_id(on);
	if let Type::FunctionReference(func, this_value) | Type::Function(func, this_value) = ty {
		// TODO clone ...
		let function_type = types.functions.get(func).unwrap().clone();
		function_type.call(
			called_with_new,
			*this_value,
			call_site_type_arguments,
			None,
			&arguments,
			call_site,
			environment,
			behavior,
			types,
			true,
		)
	} else if let Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
		on,
		arguments: structure_arguments,
	})) = ty
	{
		// TODO class with this_value
		if let Type::FunctionReference(func, this_value) | Type::Function(func, this_value) =
			types.get_type_by_id(*on)
		{
			// TODO clone ...
			let function_type = types.functions.get(func).unwrap().clone();
			function_type.call(
				called_with_new,
				*this_value,
				call_site_type_arguments,
				Some(structure_arguments.clone()),
				&arguments,
				call_site,
				environment,
				behavior,
				types,
				true,
			)
		} else {
			todo!()
		}
	} else if let Some(constraint) = environment.get_poly_base(on, types) {
		create_generic_function_call(
			constraint,
			called_with_new,
			this_value,
			call_site_type_arguments,
			arguments,
			call_site,
			on,
			environment,
			behavior,
			types,
		)
	} else {
		return Err(vec![FunctionCallingError::NotCallable {
			calling: crate::diagnostics::TypeStringRepresentation::from_type_id(
				on,
				&environment.into_general_context(),
				types,
				false,
			),
			call_site,
		}]);
	}
}

fn create_generic_function_call<'a, E: CallCheckingBehavior>(
	constraint: TypeId,
	called_with_new: CalledWithNew,
	this_value: ThisValue,
	call_site_type_arguments: Option<Vec<(SpanWithSource, TypeId)>>,
	arguments: Vec<SynthesisedArgument>,
	call_site: SpanWithSource,
	on: TypeId,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
	// match constraint {
	// 	PolyBase::Fixed { to, is_open_poly } => {
	let result = call_type(
		constraint,
		called_with_new,
		this_value,
		call_site_type_arguments,
		// TODO clone
		arguments.clone(),
		call_site,
		environment,
		behavior,
		types,
	)?;

	let with = arguments.into_boxed_slice();

	// TODO
	let is_open_poly = false;
	let reflects_dependency = if !is_open_poly {
		// TODO check trivial result
		let constructor = Constructor::FunctionResult {
			// TODO on or to
			on,
			with: with.clone(),
			// TODO unwrap
			result: result.returned_type,
		};
		let constructor_return = types.register_type(Type::Constructor(constructor));

		Some(constructor_return)
	} else {
		None
	};

	// TODO nearest fact
	environment.facts.events.push(Event::CallsType {
		on,
		with,
		timing: crate::events::CallingTiming::Synchronous,
		called_with_new,
		reflects_dependency,
	});

	// TODO should wrap result in open poly
	Ok(FunctionCallResult {
		called: result.called,
		returned_type: reflects_dependency.unwrap_or(result.returned_type),
		warnings: result.warnings,
	})
	// }
	// PolyBase::Dynamic { to, boundary } => {
	// if to == TypeId::ANY_TYPE {
	// 	let parameters = arguments
	// 		.iter()
	// 		.cloned()
	// 		.enumerate()
	// 		.map(|(idx, argument)| match argument {
	// 			synthesisedArgument::NonSpread { ty, position } => {
	// 				SynthesisedParameter {
	// 					name: format!("i{}", idx),
	// 					ty,
	// 					// TODO
	// 					position,
	// 					// TODO
	// 					missing_value: None,
	// 				}
	// 			}
	// 		})
	// 		.collect();

	// 	// Inferred function type

	// 	let function_type = FunctionType {
	// 		// TODO explain
	// 		type_parameters: None,
	// 		parameters: SynthesisedParameters {
	// 			parameters,
	// 			// TODO I think this is okay
	// 			rest_parameter: Default::default(),
	// 		},
	// 		return_type: TypeId::ANY_TYPE,
	// 		// This is where it would be good for a smaller type reference based function type
	// 		effects: Default::default(),
	// 		closed_over_references: Default::default(),
	// 		// TODO
	// 		kind: crate::types::FunctionKind::Arrow,
	// 		constant_id: None,
	// 		id: FunctionId::NULL,
	// 	};

	// 	let new_constraint = types.register_type(Type::Function(
	// 		function_type,
	// 		crate::types::FunctionNature::BehindPoly {
	// 			function_id_if_open_poly: None,
	// 			this_type: None,
	// 		},
	// 	));
	// 	environment.attempt_to_modify_base(on, boundary, new_constraint);
	// 	todo!()
	// } else {
	// }
	// todo!();
	// 	}
	// }
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
	Recursed(FunctionId, SpanWithSource),
}

pub struct InfoDiagnostic(pub String);

/// TODO *result* name bad
pub struct FunctionCallResult {
	pub called: Option<FunctionId>,
	pub returned_type: TypeId,
	// TODO
	pub warnings: Vec<InfoDiagnostic>,
}

#[derive(Debug, Default, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum CalledWithNew {
	New {
		import_new: TypeId,
	},
	SpecialSuperCall {
		on: TypeId,
	},
	#[default]
	None,
}

impl FunctionType {
	/// Calls the function
	///
	/// Returns warnings and errors
	pub(crate) fn call<'a, E: CallCheckingBehavior>(
		&self,
		called_with_new: CalledWithNew,
		this_value: ThisValue,
		call_site_type_arguments: Option<Vec<(SpanWithSource, TypeId)>>,
		parent_type_arguments: Option<StructureGenericArguments>,
		arguments: &[SynthesisedArgument],
		call_site: SpanWithSource,
		environment: &mut Environment,
		behavior: &mut E,
		types: &mut crate::TypeStore,
		// This fixes recursion
		call_constant: bool,
	) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
		// TODO check that parameters vary
		if behavior.in_recursive_cycle(self.id) {
			crate::utils::notify!("Encountered recursion");
			return Ok(FunctionCallResult {
				called: Some(self.id),
				returned_type: TypeId::ERROR_TYPE,
				warnings: Vec::new(),
			});
			// let reason = FunctionCallingError::Recursed(self.id, call_site);
			// return Err(vec![reason])
		}

		if let (Some(const_fn_ident), true) = (self.constant_id.as_deref(), call_constant) {
			let has_dependent_argument = arguments.iter().any(|arg| {
				types
					.get_type_by_id(arg.into_type().expect("dependent spread types"))
					.is_dependent()
			});

			// TODO temp, need a better solution
			let call_anyway = matches!(
				const_fn_ident,
				"debug_type"
					| "print_type" | "debug_effects"
					| "satisfies" | "is_dependent"
					| "call" | "bind"
			);

			if !call_anyway && has_dependent_argument {
				let with = arguments.to_vec().into_boxed_slice();
				// TODO with cloned!!
				let result = self
					.call(
						called_with_new,
						this_value,
						call_site_type_arguments,
						parent_type_arguments,
						arguments,
						call_site,
						environment,
						behavior,
						types,
						// Very important!
						false,
					)?
					.returned_type;

				// TODO pass down
				let on = types.register_type(Type::Function(self.id, this_value));
				let new_type = Type::Constructor(Constructor::FunctionResult {
					on,
					with: with.clone(),
					result,
				});

				let ty = types.register_type(new_type);

				behavior.get_top_level_facts(environment).events.push(Event::CallsType {
					on,
					with: arguments.to_vec().into_boxed_slice(),
					reflects_dependency: Some(ty),
					timing: crate::events::CallingTiming::Synchronous,
					called_with_new,
				});

				return Ok(FunctionCallResult {
					returned_type: ty,
					warnings: Default::default(),
					called: None,
				});
			} else {
				// TODO event
				let result = call_constant_function(
					const_fn_ident,
					this_value,
					&call_site_type_arguments,
					arguments,
					types,
					environment,
				);

				if let Ok(returned) = result {
					match returned {
						ConstantResult::Value(returned_type) => {
							return Ok(FunctionCallResult {
								returned_type,
								warnings: Default::default(),
								called: None,
							});
						}
						ConstantResult::Diagnostic(diagnostic) => {
							return Ok(FunctionCallResult {
								returned_type: TypeId::UNDEFINED_TYPE,
								warnings: vec![InfoDiagnostic(diagnostic)],
								called: None,
							});
						}
					}
				} else {
					crate::utils::notify!("Constant function calling failed, not constant params");
				}
			}
		}

		let (mut errors, mut warnings) = (Vec::new(), Vec::<()>::new());

		// Type arguments of the function
		let local_arguments: map_vec::Map<TypeId, FunctionTypeArgument> =
			if let (Some(call_site_type_arguments), true) =
				(call_site_type_arguments, E::CHECK_PARAMETERS)
			{
				self.synthesise_call_site_type_arguments(
					call_site_type_arguments,
					types,
					environment,
				)
			} else {
				map_vec::Map::new()
			};

		let mut type_arguments = FunctionTypeArguments {
			structure_arguments: parent_type_arguments, //.cloned(),
			local_arguments,
			closure_id: None,
		};

		match self.kind {
			FunctionKind::Arrow => {}
			// match get_set {
			// crate::GetSetGeneratorOrNone::Generator => todo!(),
			// crate::GetSetGeneratorOrNone::Get
			// | crate::GetSetGeneratorOrNone::Set
			// | crate::GetSetGeneratorOrNone::None => {
			// 	if !matches!(called_with_new, CalledWithNew::None) {
			// 		todo!("Error");
			// 	}
			// }
			// },
			// class_prototype, class_constructor
			FunctionKind::ClassConstructor {} => {
				todo!()
				// match called_with_new {
				// 	CalledWithNew::New { .. } => {}
				// 	CalledWithNew::SpecialSuperCall { on } => {
				// 		type_arguments.set_this(on);
				// 		this_argument = Some(on);
				// 	}
				// 	CalledWithNew::None => {
				// 		todo!("Error")
				// 	}
				// }
			}
			FunctionKind::Function { function_prototype } => {
				if let (CalledWithNew::None { .. }, ThisValue::Passed(arg)) =
					(called_with_new, this_value)
				{
					type_arguments.set_this(arg);
				}
			}
			FunctionKind::Method => {}
		}

		let arg = match called_with_new {
			CalledWithNew::New { import_new } => import_new,
			CalledWithNew::SpecialSuperCall { .. } => {
				let ty = this_value.unwrap();
				let on = crate::types::printing::print_type(
					ty,
					types,
					&environment.into_general_context(),
					true,
				);
				crate::utils::notify!("This argument {}", on);
				ty
			}
			CalledWithNew::None => TypeId::UNDEFINED_TYPE,
		};

		type_arguments.local_arguments.insert(
			TypeId::NEW_TARGET_ARG,
			FunctionTypeArgument { value: Some(arg), restriction: None },
		);

		let SeedingContext { mut type_arguments, mut locally_held_functions } = {
			let mut seeding_context =
				SeedingContext { type_arguments, locally_held_functions: HashMap::new() };

			for (idx, parameter) in self.parameters.parameters.iter().enumerate() {
				// This handles if the argument is missing but allowing elided arguments
				let argument = arguments.get(idx);

				let argument_type_and_pos = argument.map(|argument| {
					if let SynthesisedArgument::NonSpread { ty, position: pos } = argument {
						(ty, pos)
					} else {
						todo!()
					}
				});

				// let (auto_inserted_arg, argument_type) =
				// 	if argument_type.is_none() && checking_data.settings.allow_elided_arguments {
				// 		(true, Some(Cow::Owned(crate::types::Term::Undefined.into())))
				// 	} else {
				// 		(false, argument_type.map(Cow::Borrowed))
				// 	};

				if let Some((argument_type, argument_position)) = argument_type_and_pos {
					// crate::utils::notify!(
					// 	"param {}, arg {}",
					// 	types.debug_type(parameter.ty),
					// 	types.debug_type(*argument_type)
					// );

					if E::CHECK_PARAMETERS {
						// crate::utils::notify!("Param {:?} :> {:?}", parameter.ty, *argument_type);
						let result = type_is_subtype(
							parameter.ty,
							*argument_type,
							None,
							&mut seeding_context,
							environment,
							types,
						);
						// crate::utils::notify!("Aftermath {:?}", seeding_context.type_arguments);
						if let SubTypeResult::IsNotSubType(reasons) = result {
							let restriction =
								if let NonEqualityReason::GenericRestrictionMismatch {
									restriction,
									reason,
									pos,
								} = reasons
								{
									Some((
										pos,
										TypeStringRepresentation::from_type_id(
											restriction,
											&environment.into_general_context(),
											types,
											false,
										),
									))
								} else {
									None
								};

							errors.push(FunctionCallingError::InvalidArgumentType {
								parameter_type: TypeStringRepresentation::from_type_id(
									parameter.ty,
									&environment.into_general_context(),
									types,
									false,
								),
								argument_type: TypeStringRepresentation::from_type_id(
									*argument_type,
									&environment.into_general_context(),
									types,
									false,
								),
								parameter_position: parameter.position.clone(),
								argument_position: argument_position.clone(),
								restriction,
							})
						}
					} else {
						// Already checked so can set. TODO destructuring etc
						seeding_context.type_arguments.set_id(parameter.ty, *argument_type, types);
					}
				} else if let Some(value) = parameter.missing_value {
					// TODO evaluate effects
					seeding_context.type_arguments.set_id(parameter.ty, value, types)
				} else {
					// TODO group
					errors.push(FunctionCallingError::MissingArgument {
						parameter_position: parameter.position.clone(),
						call_site: call_site.clone(),
					});
				}

				// if let SubTypeResult::IsNotSubType(_reasons) = result {
				// 	if auto_inserted_arg {
				// 		todo!("different error");
				// 	} else {
				// 		errors.push(FunctionCallingError::InvalidArgumentType {
				// 			parameter_index: idx,
				// 			argument_type: argument_type.into_owned(),
				// 			parameter_type: parameter_type.clone(),
				// 			parameter_pos: parameter.2.clone().unwrap(),
				// 		});
				// 	}
				// }
				// } else {
				// 	errors.push(FunctionCallingError::MissingArgument {
				// 		parameter_pos: parameter.2.clone().unwrap(),
				// 	});
				// }
			}

			// Rest parameters:
			if self.parameters.parameters.len() < arguments.len() {
				if let Some(ref rest_parameter) = self.parameters.rest_parameter {
					for (idx, argument) in
						arguments.iter().enumerate().skip(self.parameters.parameters.len())
					{
						let (argument_type, argument_pos) =
							if let SynthesisedArgument::NonSpread { ty, position: pos } = argument {
								(ty, pos)
							} else {
								todo!()
							};

						let item_type = if let Type::Constructor(Constructor::StructureGenerics(
							StructureGenerics { on, arguments },
						)) = types.get_type_by_id(rest_parameter.item_type)
						{
							assert_eq!(*on, TypeId::ARRAY_TYPE);
							arguments.get_argument(TypeId::T_TYPE).unwrap()
						} else {
							unreachable!()
						};

						if E::CHECK_PARAMETERS {
							let result = type_is_subtype(
								item_type,
								*argument_type,
								None,
								&mut seeding_context,
								environment,
								types,
							);

							if let SubTypeResult::IsNotSubType(reasons) = result {
								let restriction =
									if let NonEqualityReason::GenericRestrictionMismatch {
										restriction,
										reason,
										pos,
									} = reasons
									{
										Some((
											pos,
											TypeStringRepresentation::from_type_id(
												restriction,
												&environment.into_general_context(),
												types,
												false,
											),
										))
									} else {
										None
									};

								errors.push(FunctionCallingError::InvalidArgumentType {
									parameter_type: TypeStringRepresentation::from_type_id(
										rest_parameter.item_type,
										&environment.into_general_context(),
										types,
										false,
									),
									argument_type: TypeStringRepresentation::from_type_id(
										*argument_type,
										&environment.into_general_context(),
										types,
										false,
									),
									argument_position: argument_pos.clone(),
									parameter_position: rest_parameter.position.clone(),
									restriction,
								})
							}
						} else {
							todo!("specialize")
						}
					}
				} else {
					// TODO types.settings.allow_extra_arguments
					let mut left_over = arguments.iter().skip(self.parameters.parameters.len());
					let first = left_over.next().unwrap();
					let mut count = 1;
					let mut end = None;
					while let arg @ Some(_) = left_over.next() {
						count += 1;
						end = arg;
					}
					let position = if let Some(end) = end {
						first
							.get_position()
							.without_source()
							.union(&end.get_position().without_source())
							.with_source(end.get_position().source)
					} else {
						first.get_position()
					};
					errors.push(FunctionCallingError::ExcessArguments { count, position });
				}
			}

			// TODO settings.allow_missing_arguments, warning or error...?

			seeding_context
		};

		if E::CHECK_PARAMETERS {
			for (reference, restriction) in self.used_parent_references.clone().into_iter() {
				match reference {
					RootReference::Variable(ref variable) => {
						let current_value = get_value_of_variable(
							environment.facts_chain(),
							*variable,
							Some(&type_arguments),
						)
						.expect("reference not assigned");

						let mut basic_subtyping = BasicEquality {
							add_property_restrictions: false,
							position: SpanWithSource { start: 0, end: 0, source: SourceId::NULL },
						};
						if let SubTypeResult::IsNotSubType(reasons) = type_is_subtype(
							restriction,
							current_value,
							None,
							// TODO temp position
							&mut basic_subtyping,
							environment,
							types,
						) {
							errors.push(FunctionCallingError::ReferenceRestrictionDoesNotMatch {
								reference,
								requirement: TypeStringRepresentation::from_type_id(
									restriction,
									&environment.into_general_context(),
									types,
									false,
								),
								found: TypeStringRepresentation::from_type_id(
									current_value,
									&environment.into_general_context(),
									types,
									false,
								),
							});
						}
					}
					RootReference::This if matches!(called_with_new, CalledWithNew::None) => {}
					RootReference::This => {
						let value_of_this = this_value.get(environment, types);

						let mut basic_subtyping = BasicEquality {
							add_property_restrictions: false,
							position: SpanWithSource { start: 0, end: 0, source: SourceId::NULL },
						};
						if let SubTypeResult::IsNotSubType(reasons) = type_is_subtype(
							restriction,
							value_of_this,
							None,
							// TODO temp position
							&mut basic_subtyping,
							environment,
							types,
						) {
							errors.push(FunctionCallingError::ReferenceRestrictionDoesNotMatch {
								reference,
								requirement: TypeStringRepresentation::from_type_id(
									restriction,
									&environment.into_general_context(),
									types,
									false,
								),
								found: TypeStringRepresentation::from_type_id(
									value_of_this,
									&environment.into_general_context(),
									types,
									false,
								),
							});
						}
					}
				}
			}
		}

		if !errors.is_empty() {
			return Err(errors);
		}

		// Evaluate effects directly into environment
		// let mut chain = Vec::new();
		// chain: Annex::new(&mut chain),
		let mut early_return = behavior.new_function_target(self.id, |target| {
			let closure_id = if !self.closed_over_variables.is_empty() {
				let closure_id = types.new_closure_id();
				crate::utils::notify!("Here {:?} {:?}", closure_id, self.effects);
				type_arguments.closure_id = Some(closure_id);
				Some(closure_id)
			} else {
				None
			};

			let mut return_result = None;

			for event in self.effects.clone().into_iter() {
				let result =
					apply_event(event, this_value, &mut type_arguments, environment, target, types);

				if let value @ Some(_) = result {
					return_result = value;
					break;
				}
			}

			if let Some(closure_id) = closure_id {
				// Set closed over values
				for (reference, value) in self.closed_over_variables.iter() {
					let value = specialize(*value, &mut type_arguments, environment, types);
					environment
						.facts
						.closure_current_values
						.insert((closure_id, reference.clone()), value);

					crate::utils::notify!("in {:?} set {:?} to {:?}", closure_id, reference, value);
				}
			}

			return_result
		});

		if let Some(returned_type) = early_return {
			if let CalledWithNew::New { .. } = called_with_new {
				// TODO skip returns if new, need a warning or something.
				crate::utils::notify!("Returned despite being called with new...");
				// todo!("warning")
			}

			return Ok(FunctionCallResult {
				returned_type,
				warnings: Default::default(),
				called: Some(self.id),
			});
		}

		// set events should cover property specialization here:
		// let returned_type = if let CalledWithNew::New { ..} = called_with_new {
		// 	// TODO specialize under the primitive conditional rules
		// 	let new_instance_type = type_arguments
		// 		.local_arguments
		// 		.get_mut(&TypeId::THIS_ARG)
		// 		.unwrap()
		// 		.value
		// 		.take()
		// 		.unwrap();
		// 	new_instance_type
		// } else {
		// };

		let returned_type = specialize(self.return_type, &mut type_arguments, environment, types);

		Ok(FunctionCallResult {
			returned_type,
			warnings: Default::default(),
			called: Some(self.id),
		})
	}

	fn synthesise_call_site_type_arguments(
		&self,
		call_site_type_arguments: Vec<(SpanWithSource, TypeId)>,
		types: &mut crate::types::TypeStore,
		environment: &mut Environment,
	) -> map_vec::Map<TypeId, FunctionTypeArgument> {
		if let Some(ref typed_parameters) = self.type_parameters {
			typed_parameters
				.0
				.iter()
				.zip(call_site_type_arguments)
				.map(|(param, (pos, ty))| {
					if let Type::RootPolyType(PolyNature::Generic { eager_fixed, .. }) =
						types.get_type_by_id(param.id)
					{
						let mut basic_subtyping = BasicEquality {
							add_property_restrictions: false,
							position: pos.clone(),
						};
						let type_is_subtype = type_is_subtype(
							*eager_fixed,
							ty,
							None,
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

					(param.id, FunctionTypeArgument { value: None, restriction: Some((pos, ty)) })
				})
				.collect()
		} else {
			crate::utils::notify!("Call site arguments on function without typed parameters...");
			map_vec::Map::new()
		}
	}
}
