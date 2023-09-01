use source_map::Span;

use crate::{
	context::{calling::CheckThings, CallCheckingBehavior, Environment, PolyBase},
	diagnostics::TypeCheckError,
	events::{CalledWithNew, Event, FunctionCallResult, FunctionCallingError},
	types::functions::SynthesizedArgument,
	types::{FunctionType, Type},
	TypeId,
};

use super::{Constructor, FunctionNature, TypeStore};

pub fn call_type_handle_errors<T: crate::FSResolver>(
	ty: TypeId,
	// Overwritten by .call, else look at binding
	called_with_new: CalledWithNew,
	this_argument: Option<TypeId>,
	call_site_type_arguments: Option<Vec<(Span, TypeId)>>,
	arguments: Vec<SynthesizedArgument>,
	call_site: Span,
	environment: &mut Environment,
	checking_data: &mut crate::CheckingData<T>,
) -> TypeId {
	let result = call_type(
		ty,
		called_with_new,
		this_argument,
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
pub(crate) fn call_type(
	on: TypeId,
	called_with_new: CalledWithNew,
	// Overwritten by .call, else look at binding
	this_argument: Option<TypeId>,
	call_site_type_arguments: Option<Vec<(Span, TypeId)>>,
	arguments: Vec<SynthesizedArgument>,
	call_site: Span,
	environment: &mut Environment,
	behavior: &mut impl CallCheckingBehavior,
	types: &mut TypeStore,
) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
	if on == TypeId::ERROR_TYPE
		|| arguments.iter().any(|arg| match arg {
			SynthesizedArgument::NonSpread { ty, .. } => *ty == TypeId::ERROR_TYPE,
		}) {
		return Ok(FunctionCallResult {
			called: None,
			returned_type: TypeId::ERROR_TYPE,
			warnings: Vec::new(),
		});
	}

	if let Type::Function(function_type, variant) = types.get_type_by_id(on) {
		// TODO as Rc to avoid expensive clone
		call_using_function_type(
			function_type.clone(),
			variant.clone(),
			&arguments,
			called_with_new,
			call_site_type_arguments,
			call_site,
			on,
			environment,
			behavior,
			types,
		)
	} else if let Some(constraint) = environment.get_poly_base(on, &types) {
		create_generic_function_call(
			constraint,
			called_with_new,
			this_argument,
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

fn create_generic_function_call(
	constraint: PolyBase,
	called_with_new: CalledWithNew,
	this_argument: Option<TypeId>,
	call_site_type_arguments: Option<Vec<(Span, TypeId)>>,
	arguments: Vec<SynthesizedArgument>,
	call_site: Span,
	on: TypeId,
	environment: &mut Environment,
	behavior: &mut impl CallCheckingBehavior,
	types: &mut TypeStore,
) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
	match constraint {
		PolyBase::Fixed { to, is_open_poly } => {
			let result = call_type(
				to,
				called_with_new,
				this_argument,
				call_site_type_arguments,
				// TODO clone
				arguments.clone(),
				call_site,
				environment,
				behavior,
				types,
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
		}
		PolyBase::Dynamic { to, boundary } => {
			// if to == TypeId::ANY_TYPE {
			// 	let parameters = arguments
			// 		.iter()
			// 		.cloned()
			// 		.enumerate()
			// 		.map(|(idx, argument)| match argument {
			// 			SynthesizedArgument::NonSpread { ty, position } => {
			// 				SynthesizedParameter {
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
			// 		parameters: SynthesizedParameters {
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
			todo!();
		}
	}
}

fn call_using_function_type(
	function_type: FunctionType,
	variant: FunctionNature,
	arguments: &Vec<SynthesizedArgument>,
	called_with_new: CalledWithNew,
	call_site_type_arguments: Option<Vec<(Span, TypeId)>>,
	call_site: Span,
	on: TypeId,
	environment: &mut Environment,
	behavior: &mut impl CallCheckingBehavior,
	types: &mut TypeStore,
) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
	let this_argument =
		if let FunctionNature::Source(this_arg) = variant { this_arg.clone() } else { None };

	// TODO should be done after call to check that arguments are correct
	if let Some(const_fn_ident) = function_type.constant_id.as_deref() {
		let this_argument = this_argument.or(this_argument);
		let has_dependent_argument = arguments.iter().any(|arg| {
			types.get_type_by_id(arg.into_type().expect("dependent spread types")).is_dependent()
		});

		// TODO temp, need a better solution
		let call_anyway = matches!(
			const_fn_ident,
			"debug_type" | "print_type" | "is_dependent" | "call" | "bind"
		);

		if !call_anyway && has_dependent_argument {
			let with = arguments.to_vec().into_boxed_slice();
			// TODO with cloned!!
			let result = function_type
				.clone()
				.call(
					called_with_new,
					this_argument,
					call_site_type_arguments,
					// TODO
					&None,
					arguments,
					call_site,
					environment,
					behavior,
					types,
				)?
				.returned_type;

			let new_type = Type::Constructor(Constructor::FunctionResult {
				on,
				with: with.clone(),
				result: super::PolyPointer::Fixed(result),
			});

			let ty = types.register_type(new_type);

			behavior.get_top_level_facts(environment).events.push(Event::CallsType {
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
		called_with_new,
		this_argument,
		call_site_type_arguments,
		// TODO
		&None,
		arguments,
		call_site,
		environment,
		behavior,
		types,
	)
}
