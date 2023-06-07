use crate::{
	context::{get_env, Environment, Logical, PolyBase, SetPropertyError},
	events::Event,
	TypeId,
};

use super::{Constructor, Type, TypeStore};

pub enum PropertyResult {
	Direct(TypeId),
	Getter(TypeId),
	Generic(TypeId),
}

impl From<PropertyResult> for TypeId {
	fn from(result: PropertyResult) -> Self {
		match result {
			PropertyResult::Direct(type_id)
			| PropertyResult::Getter(type_id)
			| PropertyResult::Generic(type_id) => type_id,
		}
	}
}

/// Also evaluates getter and binds `this`
///
/// *be aware this creates a new type every time, bc of this binding. could cache this bound
/// types at some point*
pub(crate) fn get_property(
	environment: &mut Environment,
	on: TypeId,
	under: TypeId,
	types: &mut TypeStore,
	with: Option<TypeId>,
) -> Option<PropertyResult> {
	if on == TypeId::ERROR_TYPE || under == TypeId::ERROR_TYPE {
		return Some(PropertyResult::Direct(TypeId::ERROR_TYPE));
	}

	enum GetResult {
		AccessIntroducesDependence(TypeId),
		/// These always return the same value
		FromAObject(TypeId),
	}

	let value: GetResult =
		if let Some(constraint) = environment.get_poly_base(on, types) {
			match constraint {
				PolyBase::Fixed { to, is_open_poly } => {
					crate::utils::notify!(
						"Get property found fixed constraint {}, is_open_poly={:?}",
						environment.debug_type(on, types),
						is_open_poly
					);

					let fact = environment.get_property_unbound(to, under, types)?;

					match fact {
						Logical::Pure(og) => {
							match types.get_type_by_id(og) {
								Type::Function(func, _) => {
									// TODO only want to do sometimes, or even never as it can be pulled using the poly chain
									let with_this = types.register_type(Type::Function(
										func.clone(),
										crate::types::FunctionNature::BehindPoly {
											// TODO
											function_id_if_open_poly: None,
											this_type: Some(on),
										},
									));
									crate::utils::notify!("Temp setting this on poly");

									GetResult::AccessIntroducesDependence(with_this)
								}
								Type::And(_, _) => todo!(),
								Type::Or(_, _) => todo!(),
								Type::RootPolyType(_) => todo!(),
								Type::Constructor(_) => todo!(),
								Type::AliasTo { .. } | Type::NamedRooted { .. } => {
									if is_open_poly {
										crate::utils::notify!("TODO evaluate getter...");
									} else {
										crate::utils::notify!("TODO don't evaluate getter");
									}

									// TODO don't have to recreate if don't have any events.
									let constructor_result = types.register_type(
										Type::Constructor(Constructor::Property { on, under }),
									);

									GetResult::AccessIntroducesDependence(constructor_result)
								}
								Type::Constant(_) => GetResult::FromAObject(og),
								Type::Object(..) => todo!(),
							}
						}
						Logical::Or(_) => todo!(),
						Logical::Implies(_, _) => todo!(),
					}
				}
				PolyBase::Dynamic { to, boundary } => {
					crate::utils::notify!(
						"Getting property {:?} which has a dynamic constraint {:?}",
						on,
						to
					);
					// If the property on the dynamic constraint is None
					if environment.get_property_unbound(to, under, types).is_none() {
						let on = if to == TypeId::ANY_TYPE {
							let new_constraint = types.register_type(Type::Object(
								crate::types::ObjectNature::ModifiableConstraint,
							));
							crate::utils::notify!("Here!!!");
							environment.attempt_to_modify_base(on, boundary, new_constraint);
							new_constraint
						} else if matches!(
							types.get_type_by_id(to),
							Type::AliasTo { to: TypeId::OBJECT_TYPE, .. }
						) {
							to
						} else {
							todo!("new and")
						};

						environment
							.properties
							.entry(on)
							.or_default()
							.push((under, with.unwrap_or(TypeId::ANY_TYPE)));
					} else {
						crate::utils::notify!("Found existing property on dynamic constraint");
					}

					let constructor_result =
						types.register_type(Type::Constructor(Constructor::Property { on, under }));

					// environment
					// 	.context_type
					// 	.get_inferrable_constraints_mut()
					// 	.unwrap()
					// 	.insert(constructor_result);

					GetResult::AccessIntroducesDependence(constructor_result)
				}
			}
		} else if let Some(_) = environment.get_poly_base(under, types) {
			todo!()
		} else {
			let property = environment.get_property_unbound(on, under, types)?;

			let property = if let Logical::Pure(og) = property {
				og
			} else {
				unreachable!("TODO multiple properties");
			};

			let ty = types.get_type_by_id(property);
			let value = match ty {
				Type::Function(ty, nature) => {
					// TODO getter
					// TODO if arrow function
					match nature {
						super::FunctionNature::BehindPoly { .. } => todo!(),
						super::FunctionNature::Source(id, get_set, this) => {
							if get_set.is_some() {
								todo!()
							}
							if this.is_some() {
								panic!()
							}
							types.register_type(Type::Function(
								ty.clone(),
								super::FunctionNature::Source(id.clone(), None, Some(on)),
							))
						}
						super::FunctionNature::Constructor(_) => todo!(),
					}
				}
				Type::Constant(..) => property,
				Type::NamedRooted { .. }
				| Type::And(_, _)
				| Type::Or(_, _)
				| Type::Constructor(Constructor::StructureGenerics { .. }) => {
					unreachable!("property was {:?} {:?}", property, ty)
				}
				Type::Constructor(constructor) => {
					unreachable!("Interesting property was {:?}", constructor);
				}
				Type::Object(..) | Type::RootPolyType { .. } => {
					property
					// if let super::PolyNature::Open = nature {
					// } else {
					// 	crate::utils::notify!("Warning property has ty of nature: {:?}", nature);
					// 	property
					// }
				}
				Type::AliasTo { to, name, parameters } => {
					todo!()
					// if environment.is_getter(property) {
					// 	// TODO catch unwrap as error:
					// 	let result = call_type(
					// 		property,
					// 		vec![],
					// 		Some(on),
					// 		None,
					// 		environment,
					// 		checking_data,
					// 		CalledWithNew::None,
					// 	)
					// 	.unwrap()
					// 	.returned_type;

					// 	return Some(PropertyResult::Getter(result));
					// } else {
					// 	// Bind this is this is function
					// 	let is_function = *to == TypeId::FUNCTION_TYPE
					// 		|| matches!(environment.get_type_by_id(*to), Type::AliasTo { to, .. } if *to == TypeId::FUNCTION_TYPE);

					// 	if is_function {
					// 		let alias = environment.new_type(Type::AliasTo {
					// 			to: property,
					// 			name: None,
					// 			parameters: None,
					// 		});
					// 		environment.this_bindings.insert(alias, on);
					// 		alias
					// 	} else {
					// 		property
					// 	}
					// }
				}
			};

			GetResult::FromAObject(value)
		};

	let reflects_dependency = match value {
		GetResult::AccessIntroducesDependence(s) => Some(s),
		GetResult::FromAObject(_) => None,
	};

	environment.context_type.events.push(Event::Getter { on, under, reflects_dependency });

	let (GetResult::AccessIntroducesDependence(value) | GetResult::FromAObject(value)) = value else { unreachable!() };

	// Carry the frozen part
	if let Some(frozen) = environment.is_frozen(on) {
		environment.frozen.insert(value, frozen);
	}

	// TODO generic
	Some(PropertyResult::Direct(value))
}

/// Aka a assignment to a property, **INCLUDING initialization of a new one**
///
/// Evaluates setters
pub(crate) fn set_property(
	environment: &mut Environment,
	on: TypeId,
	under: TypeId,
	new: TypeId,
	types: &mut TypeStore,
) -> Result<Option<TypeId>, SetPropertyError> {
	let pair = (on, under);

	if environment.parents_iter().any(|env| get_env!(env.writable.get(&pair)).is_some()) {
		return Err(SetPropertyError::NotWriteable);
	}

	let property_constraint = {
		let constraint = environment
			.parents_iter()
			.find_map(|env| get_env!(env.object_constraints.get(&on)).cloned());

		match constraint {
			Some(constraint) => {
				let result = environment.get_property_unbound(constraint, under, types);
				if result.is_none() {
					// TODO does not exist
					return Err(SetPropertyError::DoesNotMeetConstraint(todo!()));
				}
				result
			}
			None => None,
		}
	};

	if let Some(constraint) = property_constraint {
		todo!();
		// TODO maybe perform update anyway...?

		// let mut basic_subtyping = crate::types::BasicEquality {
		// 	add_property_restrictions: true,
		// 	position: parser::Span { start: 0, end: 0, source_id: parser::SourceId::NULL },
		// };
		// if let SubTypeResult::IsNotSubType(sub_type_error) = type_is_subtype(
		// 	constraint,
		// 	new,
		// 	None,
		// 	// TODO undecided here
		// 	// TODO position here
		// 	&mut basic_subtyping,
		// 	environment,
		// 	&checking_data.types,
		// ) {
		// 	return Err(SetPropertyError::DoesNotMeetConstraint(sub_type_error));
		// }
	}

	let current_property = environment.get_property_unbound(on, under, types);

	if let Some(fact) = current_property {
		match fact {
			Logical::Pure(og) => match types.get_type_by_id(og) {
				Type::Function(..) => todo!("look at setter"),
				Type::Object(..) | Type::Constant(_) => {
					environment.properties.entry(on).or_default().push((under, new));
					environment.context_type.events.push(Event::Setter {
						on,
						new,
						under,
						reflects_dependency: None,
						initialization: true,
					});
				}
				Type::And(_, _) | Type::Or(_, _) => todo!(),
				Type::RootPolyType(_) => todo!(),
				Type::Constructor(_) => todo!(),
				Type::NamedRooted { name, parameters } => todo!(),
				Type::AliasTo { to, name, parameters } => {
					todo!();
					// if environment.is_setter(og) {
					// 	// TODO catch unwrap as error:
					// 	let arg = SynthesizedArgument::NonSpread {
					// 		ty: new,
					// 		position: parser::Span {
					// 			start: 0,
					// 			end: 0,
					// 			source_id: parser::SourceId::NULL,
					// 		},
					// 	};
					// 	return Ok(Some(
					// 		call_type(
					// 			og,
					// 			vec![arg],
					// 			Some(on),
					// 			None,
					// 			environment,
					// 			checking_data,
					// 			CalledWithNew::default(),
					// 		)
					// 		.unwrap()
					// 		.returned_type,
					// 	));
					// } else {
					// 	environment.context_type.events.push(Event::Setter {
					// 		on,
					// 		new,
					// 		under,
					// 		reflects_dependency: None,
					// 		initialization: false,
					// 	});
					// 	environment.proofs.new_property(on, under, new, false);
					// }
				}
			},
			Logical::Or(_) => todo!(),
			Logical::Implies(_, _) => todo!(),
		}
	} else {
		// TODO abstract
		environment.properties.entry(on).or_default().push((under, new));
		environment.context_type.events.push(Event::Setter {
			on,
			new,
			under,
			reflects_dependency: None,
			initialization: true,
		});
	}
	Ok(None)
}
