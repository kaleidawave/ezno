use source_map::Span;

use crate::{
	context::{Environment, PolyBase},
	errors::TypeCheckError,
	events::{CalledWithNew, Event, FunctionCallResult},
	structures::{
		functions::{FunctionType, SynthesizedArgument},
		parameters::{SynthesizedParameter, SynthesizedParameters},
	},
	types::Type,
	TypeId,
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
		Ok(FunctionCallResult { returned_type, warnings }) => returned_type,
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
) -> Result<FunctionCallResult, Vec<crate::structures::functions::FunctionCallingError>> {
	if on == TypeId::ERROR_TYPE {
		Ok(FunctionCallResult { returned_type: on, warnings: Default::default() })
	} else if let Type::Function(function_type, variant) = types.get_type_by_id(on) {
		// TODO as Rc to avoid expensive clone
		let function_type = function_type.clone();
		let arg =
			if let FunctionNature::Source(_, this_arg) = variant { this_arg.clone() } else { None };

		// TODO should be done after call to check that arguments are correct
		if let Some(const_fn_ident) = function_type.constant_id.as_deref() {
			let this_argument = this_argument.or(arg);
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

				crate::utils::notify!("{:?}", types.debug_type(result));

				let ty = types.register_type(new_type);

				environment.context_type.events.push(Event::CallsType {
					on,
					with: arguments.clone().into_boxed_slice(),
					reflects_dependency: Some(ty),
					timing: crate::events::CallingTiming::Synchronous,
					called_with_new,
				});

				return Ok(FunctionCallResult { returned_type: ty, warnings: Default::default() });
			} else {
				// TODO event
				let returned_type = crate::behavior::constant_functions::call_constant_function(
					// TODO temp
					&const_fn_ident.to_owned(),
					this_argument,
					&arguments,
					types,
				);

				if let Ok(returned_type) = returned_type {
					return Ok(FunctionCallResult { returned_type, warnings: Default::default() });
				} else {
					crate::utils::notify!("Constant function calling failed, not constant pararms");
				}
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
				let call_result = call_type(
					to,
					// TODO clone
					arguments.clone(),
					this_argument,
					call_site_type_arguments,
					environment,
					types,
					called_with_new,
				);
				return match call_result {
					Ok(mut result) => {
						// TODO "get_constant_type" little temp
						let with = arguments.into_boxed_slice();
						todo!()
						// let constructor_ty =
						// 	if environment.get_constant_type(result.returned_type).is_none() {
						// 		let constructor_return = environment.new_type(Type::Constructor(
						// 			Constructor::FunctionResult { on: ty, with: with.clone() },
						// 		));

						// 		result.returned_type = constructor_return;

						// 		Some(constructor_return)
						// 	} else {
						// 		None
						// 	};

						// environment.context_type.events.push(Event::CallsType {
						// 	on: to,
						// 	with,
						// 	return_type_matches: constructor_ty,
						// 	timing: crate::events::CallingTiming::Synchronous,
						// 	called_with_new,
						// });

						// Ok(result)
					}
					Err(err) => Err(err),
				};
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
							optional_parameters: Default::default(),
							// TODO I think this is okay
							rest_parameter: Default::default(),
						},
						return_type: TypeId::ANY_TYPE,
						// This is where it would be good for a smaller type reference based function type
						effects: Default::default(),
						closed_over_references: Default::default(),
						// TODO
						kind: crate::structures::functions::FunctionKind::Arrow {
							get_set: crate::GetSetGeneratorOrNone::None,
						},
						constant_id: None,
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
		panic!("Trying to call type {}", environment.debug_type(on, &types))
	}
}
