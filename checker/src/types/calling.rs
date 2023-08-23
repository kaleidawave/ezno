use source_map::Span;

use crate::{
	context::{Environment, PolyBase},
	diagnostics::TypeCheckError,
	events::{CalledWithNew, Event, FunctionCallResult, FunctionCallingError},
	types::functions::SynthesizedArgument,
	types::functions::{SynthesizedParameter, SynthesizedParameters},
	types::{FunctionType, Type},
	FunctionId, TypeId,
};

use super::{Constructor, FunctionNature, TypeStore};

pub fn call_type_handle_errors<T: crate::FSResolver>(
	ty: TypeId,
	arguments: Vec<SynthesizedArgument>,
	// Overwritten by .call, else look at binding
	this_argument: Option<TypeId>,
	call_site_type_arguments: Option<Vec<(Span, TypeId)>>,
	environment: &mut Environment,
	checking_data: &mut crate::CheckingData<T>,
	called_with_new: CalledWithNew,
	call_site: Span,
) -> TypeId {
	let result = call_type(
		ty,
		arguments,
		this_argument,
		call_site_type_arguments,
		environment,
		&mut checking_data.types,
		called_with_new,
	);
	match result {
		Ok(FunctionCallResult { returned_type, warnings, called }) => {
			for warning in warnings {
				if let crate::events::InfoDiagnostic(info) = warning {
					checking_data.diagnostics_container.add_info(
						crate::diagnostics::Diagnostic::Position {
							reason: info,
							position: call_site.clone(),
							kind: crate::diagnostics::DiagnosticKind::Info,
						},
					)
				}
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
pub fn call_type(
	on: TypeId,
	arguments: Vec<SynthesizedArgument>,
	// Overwritten by .call, else look at binding
	this_argument: Option<TypeId>,
	call_site_type_arguments: Option<Vec<(Span, TypeId)>>,
	environment: &mut Environment,
	types: &mut TypeStore,
	called_with_new: CalledWithNew,
) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
	if on == TypeId::ERROR_TYPE {
		Ok(FunctionCallResult { returned_type: on, called: None, warnings: Default::default() })
	} else if let Type::Function(function_type, variant) = types.get_type_by_id(on) {
		// TODO as Rc to avoid expensive clone
		let function_type = function_type.clone();

		let this_argument =
			if let FunctionNature::Source(this_arg) = variant { this_arg.clone() } else { None };

		// TODO should be done after call to check that arguments are correct
		if let Some(const_fn_ident) = function_type.constant_id.as_deref() {
			let this_argument = this_argument.or(this_argument);
			let has_dependent_argument = arguments.iter().any(|arg| {
				types
					.get_type_by_id(arg.into_type().expect("dependent spread types"))
					.is_dependent()
			});

			if has_dependent_argument {
				let with = arguments.to_vec().into_boxed_slice();
				// TODO with cloned!!
				let result = function_type
					.clone()
					.call(
						&arguments,
						this_argument,
						call_site_type_arguments,
						// TODO
						&None,
						types,
						environment,
						called_with_new,
					)?
					.returned_type;

				let new_type = Type::Constructor(Constructor::FunctionResult {
					on,
					with: with.clone(),
					result: super::PolyPointer::Fixed(result),
				});

				let ty = types.register_type(new_type);

				environment.context_type.events.push(Event::CallsType {
					on,
					with: arguments.clone().into_boxed_slice(),
					reflects_dependency: Some(ty),
					timing: crate::events::CallingTiming::Synchronous,
					called_with_new,
				});

				return Ok(FunctionCallResult {
					returned_type: ty,
					warnings: Default::default(),
					called: None,
				});
			}
		}

		function_type.call(
			&arguments,
			this_argument,
			call_site_type_arguments,
			// TODO
			&None,
			types,
			environment,
			called_with_new,
		)
	} else if let Some(constraint) = environment.get_poly_base(on, &types) {
		match constraint {
			PolyBase::Fixed { to, is_open_poly } => {
				let result = call_type(
					to,
					// TODO clone
					arguments.clone(),
					this_argument,
					call_site_type_arguments,
					environment,
					types,
					called_with_new,
				)?;

				let with = arguments.into_boxed_slice();

				let reflects_dependency = if !is_open_poly {
					// TODO check trivial result
					let constructor_return =
						types.register_type(Type::Constructor(Constructor::FunctionResult {
							// TODO on or to
							on,
							with: with.clone(),
							// TODO unwrap
							result: super::PolyPointer::Fixed(result.returned_type),
						}));

					Some(constructor_return)
				} else {
					None
				};

				environment.context_type.events.push(Event::CallsType {
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
			}
			PolyBase::Dynamic { to, boundary } => {
				if to == TypeId::ANY_TYPE {
					let parameters = arguments
						.iter()
						.cloned()
						.enumerate()
						.map(|(idx, argument)| match argument {
							SynthesizedArgument::NonSpread { ty, position } => {
								SynthesizedParameter {
									name: format!("i{}", idx),
									ty,
									// TODO
									position,
									// TODO
									missing_value: None,
								}
							}
						})
						.collect();

					// Inferred function type

					let function_type = FunctionType {
						// TODO explain
						type_parameters: None,
						parameters: SynthesizedParameters {
							parameters,
							// TODO I think this is okay
							rest_parameter: Default::default(),
						},
						return_type: TypeId::ANY_TYPE,
						// This is where it would be good for a smaller type reference based function type
						effects: Default::default(),
						closed_over_references: Default::default(),
						// TODO
						kind: crate::types::FunctionKind::Arrow {
							get_set: crate::GetSetGeneratorOrNone::None,
						},
						constant_id: None,
						id: FunctionId::NULL,
					};

					let new_constraint = types.register_type(Type::Function(
						function_type,
						crate::types::FunctionNature::BehindPoly {
							function_id_if_open_poly: None,
							this_type: None,
						},
					));
					environment.attempt_to_modify_base(on, boundary, new_constraint);
					todo!()
				} else {
					todo!();
				}
			}
		}
	} else {
		panic!("Trying to call type")
	}
}
