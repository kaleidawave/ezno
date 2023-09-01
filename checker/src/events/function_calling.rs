//! Contains logic for function calling + constant calling operations

use std::collections::HashMap;

use source_map::{SourceId, Span};

use crate::{
	context::{calling::Target, get_value_of_variable, CallCheckingBehavior},
	diagnostics::TypeStringRepresentation,
	types::functions::SynthesizedArgument,
	types::{
		poly_types::{
			generics::generic_type_arguments::{
				CurriedFunctionTypeArguments, FunctionTypeArgument,
			},
			*,
		},
		specialize,
		subtyping::{type_is_subtype, BasicEquality, NonEqualityReason, SubTypeResult},
		Constructor, FunctionKind, FunctionType, PolyNature, PolyPointer, Type, TypeId,
	},
	Environment, FunctionId,
};

use map_vec::Map as SmallMap;

use super::{apply_event, EarlyReturn, RootReference};

/// Errors from trying to call a function
pub enum FunctionCallingError {
	InvalidArgumentType {
		parameter_type: TypeStringRepresentation,
		argument_type: TypeStringRepresentation,
		argument_position: Span,
		parameter_position: Span,
		restriction: Option<(Span, TypeStringRepresentation)>,
	},
	MissingArgument {
		parameter_position: Span,
		call_site: Span,
	},
	ExcessArguments {
		count: usize,
		position: Span,
	},
	NotCallable {
		calling: TypeStringRepresentation,
		call_site: Span,
	},
	ReferenceRestrictionDoesNotMatch {
		reference: RootReference,
		requirement: TypeStringRepresentation,
		found: TypeStringRepresentation,
	},
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
		mut this_argument: Option<TypeId>,
		call_site_type_arguments: Option<Vec<(Span, TypeId)>>,
		parent_type_arguments: &Option<CurriedFunctionTypeArguments>,
		arguments: &[SynthesizedArgument],
		call_site: Span,
		environment: &mut Environment,
		behavior: &E,
		types: &mut crate::TypeStore,
	) -> Result<FunctionCallResult, Vec<FunctionCallingError>> {
		let (mut errors, mut warnings) = (Vec::new(), Vec::<()>::new());

		// Type arguments of the function
		let local_arguments: map_vec::Map<TypeId, FunctionTypeArgument> =
			if let (Some(call_site_type_arguments), true) =
				(call_site_type_arguments, E::CHECK_TYPES)
			{
				self.synthesize_call_site_type_arguments(
					call_site_type_arguments,
					types,
					environment,
				)
			} else {
				SmallMap::new()
			};

		let mut type_arguments =
			TypeArguments { structure_arguments: parent_type_arguments.clone(), local_arguments };

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
				if let (CalledWithNew::None { .. }, Some(arg)) = (called_with_new, &this_argument) {
					type_arguments.set_this(*arg);
				}
			}
			FunctionKind::Method => {}
		}

		let arg = match called_with_new {
			CalledWithNew::New { import_new } => import_new,
			CalledWithNew::SpecialSuperCall { .. } => {
				let ty = this_argument.unwrap();
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
					if let SynthesizedArgument::NonSpread { ty, position: pos } = argument {
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

					if E::CHECK_TYPES {
						// crate::utils::notify!("Param {:?} :> {:?}", parameter.ty, *argument_type);
						let result = type_is_subtype(
							parameter.ty,
							*argument_type,
							None,
							&mut seeding_context,
							environment,
							&types,
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
										crate::diagnostics::TypeStringRepresentation::from_type_id(
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
								parameter_type:
									crate::diagnostics::TypeStringRepresentation::from_type_id(
										parameter.ty,
										&environment.into_general_context(),
										types,
										false,
									),
								argument_type:
									crate::diagnostics::TypeStringRepresentation::from_type_id(
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
						seeding_context.type_arguments.set_id(parameter.ty, *argument_type, &types);
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
							if let SynthesizedArgument::NonSpread { ty, position: pos } = argument {
								(ty, pos)
							} else {
								todo!()
							};

						let item_type =
							if let Type::Constructor(Constructor::StructureGenerics { on, with }) =
								types.get_type_by_id(rest_parameter.item_type)
							{
								assert_eq!(*on, TypeId::ARRAY_TYPE);
								with.get(&TypeId::T_TYPE).unwrap().clone()
							} else {
								unreachable!()
							};

						if E::CHECK_TYPES {
							let result = type_is_subtype(
								item_type,
								*argument_type,
								None,
								&mut seeding_context,
								environment,
								&types,
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
										crate::diagnostics::TypeStringRepresentation::from_type_id(
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
									parameter_type:
										crate::diagnostics::TypeStringRepresentation::from_type_id(
											rest_parameter.item_type,
											&environment.into_general_context(),
											types,
											false,
										),
									argument_type:
										crate::diagnostics::TypeStringRepresentation::from_type_id(
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
						first.get_position().union(&end.get_position())
					} else {
						first.get_position()
					};
					errors.push(FunctionCallingError::ExcessArguments { count, position });
				}
			}

			// TODO settings.allow_missing_arguments, warning or error...?

			seeding_context
		};

		if let Some(ref constant_id) = self.constant_id {
			// TODO event
			let result = crate::behavior::constant_functions::call_constant_function(
				constant_id,
				this_argument,
				&arguments,
				types,
				environment,
			);

			if let Ok(returned) = result {
				match returned {
					crate::behavior::constant_functions::ConstantResult::Value(returned_type) => {
						return Ok(FunctionCallResult {
							returned_type,
							warnings: Default::default(),
							called: None,
						});
					}
					crate::behavior::constant_functions::ConstantResult::Diagnostic(diagnostic) => {
						return Ok(FunctionCallResult {
							returned_type: TypeId::UNDEFINED_TYPE,
							warnings: vec![crate::events::InfoDiagnostic(diagnostic)],
							called: None,
						});
					}
				}
			} else {
				crate::utils::notify!("Constant function calling failed, not constant params");
			}
		}

		if E::CHECK_TYPES {
			for (reference, restriction) in self.closed_over_references.clone().into_iter() {
				match reference {
					RootReference::VariableId(ref variable) => {
						let current_value =
							get_value_of_variable(environment.facts_chain(), variable.clone())
								.expect("closed over reference not assigned");

						let mut basic_subtyping = BasicEquality {
							add_property_restrictions: false,
							position: Span { start: 0, end: 0, source: SourceId::NULL },
						};
						if let SubTypeResult::IsNotSubType(reasons) = type_is_subtype(
							restriction,
							current_value,
							None,
							// TODO temp position
							&mut basic_subtyping,
							environment,
							&types,
						) {
							errors.push(FunctionCallingError::ReferenceRestrictionDoesNotMatch {
								reference,
								requirement:
									crate::diagnostics::TypeStringRepresentation::from_type_id(
										restriction,
										&environment.into_general_context(),
										&types,
										false,
									),
								found: crate::diagnostics::TypeStringRepresentation::from_type_id(
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
						let value_of_this =
							this_argument.unwrap_or_else(|| environment.get_value_of_this(types));

						let mut basic_subtyping = BasicEquality {
							add_property_restrictions: false,
							position: Span { start: 0, end: 0, source: SourceId::NULL },
						};
						if let SubTypeResult::IsNotSubType(reasons) = type_is_subtype(
							restriction,
							value_of_this,
							None,
							// TODO temp position
							&mut basic_subtyping,
							environment,
							&types,
						) {
							errors.push(FunctionCallingError::ReferenceRestrictionDoesNotMatch {
								reference,
								requirement:
									crate::diagnostics::TypeStringRepresentation::from_type_id(
										restriction,
										&environment.into_general_context(),
										types,
										false,
									),
								found: crate::diagnostics::TypeStringRepresentation::from_type_id(
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
		let mut target = Target { facts: None };
		for event in self.effects.clone().into_iter() {
			// TODO extract
			// let mut target = environment.new_function_target();
			// TODO
			let apply_event = apply_event(
				event,
				this_argument,
				&mut type_arguments,
				environment,
				&mut target,
				types,
			);

			if let EarlyReturn::Some(returned_type) = apply_event {
				if let CalledWithNew::New { .. } = called_with_new {
					// TODO skip returns if new, need a warning or something.
					crate::utils::notify!("Returned despite being called with new...");
					// todo!("warning")
				}

				return Ok(FunctionCallResult {
					returned_type,
					warnings: Default::default(),
					called: Some(self.id.clone()),
				});
			}
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

		let returned_type = specialize(
			self.return_type,
			&mut type_arguments,
			&environment.into_general_context(),
			types,
		);

		Ok(FunctionCallResult {
			returned_type,
			warnings: Default::default(),
			called: Some(self.id.clone()),
		})
	}

	fn synthesize_call_site_type_arguments(
		&self,
		call_site_type_arguments: Vec<(Span, TypeId)>,
		types: &mut crate::types::TypeStore,
		environment: &mut Environment,
	) -> SmallMap<TypeId, FunctionTypeArgument> {
		if let Some(ref typed_parameters) = self.type_parameters {
			typed_parameters
				.0
				.iter()
				.zip(call_site_type_arguments.into_iter())
				.map(|(param, (pos, ty))| {
					if let Type::RootPolyType(PolyNature::Generic {
						eager_fixed: PolyPointer::Fixed(gen_ty),
						..
					}) = types.get_type_by_id(param.id)
					{
						let mut basic_subtyping = BasicEquality {
							add_property_restrictions: false,
							position: pos.clone(),
						};
						let type_is_subtype = type_is_subtype(
							*gen_ty,
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
			SmallMap::new()
		}
	}
}
