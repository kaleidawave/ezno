use crate::{
	behavior::functions::ThisValue,
	context::{CallCheckingBehavior, Logical, SetPropertyError},
	events::Event,
	subtyping::{type_is_subtype, SubTypeResult},
	types::{specialize, FunctionType},
	Environment, TypeId,
};

use source_map::{SourceId, Span, SpanWithSource};

use super::{calling::CalledWithNew, Constructor, Type, TypeStore};

pub enum PropertyKind {
	Direct,
	Getter,
	/// TODO not sure
	Generic,
}

/// TODO type predicate based
///
/// TODO getter, setting need a closure id
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum Property {
	Value(TypeId),
	Getter(Box<FunctionType>),
	Setter(Box<FunctionType>),
	GetterAndSetter(Box<FunctionType>, Box<FunctionType>),
}

impl Property {
	pub(crate) fn as_get_type(&self) -> TypeId {
		match self {
			Property::Value(value) => *value,
			Property::Getter(func) => func.return_type,
			Property::Setter(_) => todo!(),
			Property::GetterAndSetter(_, _) => todo!(),
		}
	}
}

/// Also evaluates getter and binds `this`
///
/// *be aware this creates a new type every time, bc of this binding. could cache this bound
/// types at some point*
pub(crate) fn get_property<'a, E: CallCheckingBehavior>(
	on: TypeId,
	under: TypeId,
	with: Option<TypeId>,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Option<(PropertyKind, TypeId)> {
	if on == TypeId::ERROR_TYPE || under == TypeId::ERROR_TYPE {
		return Some((PropertyKind::Direct, TypeId::ERROR_TYPE));
	}

	enum GetResult {
		AccessIntroducesDependence(TypeId),
		/// These always return the same value
		FromAObject(TypeId),
	}

	let value: GetResult = if let Some(constraint) = environment.get_poly_base(on, types) {
		GetResult::AccessIntroducesDependence(getter_on_type(
			constraint,
			under,
			on,
			with,
			environment,
			behavior,
			types,
		)?)
	} else if let Some(_) = environment.get_poly_base(under, types) {
		todo!()
	} else {
		// TODO
		return get_from_an_object(on, under, environment, behavior, types);
	};

	let reflects_dependency = match value {
		GetResult::AccessIntroducesDependence(s) => Some(s),
		GetResult::FromAObject(_) => None,
	};

	behavior.get_top_level_facts(environment).events.push(Event::Getter {
		on,
		under,
		reflects_dependency,
	});

	let (GetResult::AccessIntroducesDependence(value) | GetResult::FromAObject(value)) = value
	else {
		unreachable!()
	};

	// Carry the frozen part
	// if let Some(frozen) = environment.is_frozen(on) {
	// 	environment.facts.frozen.insert(value, frozen);
	// }

	// TODO generic
	Some((PropertyKind::Direct, value))
}

fn get_from_an_object<'a, E: CallCheckingBehavior>(
	on: TypeId,
	under: TypeId,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Option<(PropertyKind, TypeId)> {
	return property_on_logical(
		environment.get_property_unbound(on, under, types)?,
		types,
		on,
		environment,
		behavior,
	);

	fn property_on_logical<E: CallCheckingBehavior>(
		logical: Logical<Property>,
		types: &mut TypeStore,
		on: TypeId,
		environment: &mut Environment,
		behavior: &mut E,
	) -> Option<(PropertyKind, TypeId)> {
		match logical {
			Logical::Pure(property) => {
				match property {
					Property::Value(value) => {
						let ty = types.get_type_by_id(value);
						match ty {
							// TODO function :: bind_this
							Type::Function(func, state) => {
								let func = types.register_type(Type::Function(
									func.clone(),
									crate::behavior::functions::ThisValue::Passed(on),
								));
								Some((PropertyKind::Direct, func))
							}
							Type::FunctionReference(func, this_argument) => {
								crate::utils::notify!("TODO temp reference function business");
								let func = types.register_type(Type::FunctionReference(
									func.clone(),
									crate::behavior::functions::ThisValue::Passed(on),
								));
								Some((PropertyKind::Direct, func))
							}
							Type::Class(..) => todo!(),
							Type::Object(..) | Type::RootPolyType { .. } | Type::Constant(..) => {
								Some((PropertyKind::Direct, value))
							}
							Type::NamedRooted { .. } | Type::And(_, _) | Type::Or(_, _) => {
								crate::utils::notify!(
								    "property was {:?} {:?}, which should be NOT be able to be returned from a function",
								    property, ty
							    );
								let value = types.register_type(Type::RootPolyType(
									crate::types::PolyNature::Open(value),
								));
								Some((PropertyKind::Direct, value))
							}
							Type::Constructor(Constructor::StructureGenerics { .. }) => {
								// TODO curry type arguments
								Some((PropertyKind::Direct, value))
							}
							Type::Constructor(constructor) => {
								unreachable!("Interesting property on {:?}", constructor);
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
						}
					}
					Property::GetterAndSetter(getter, _) | Property::Getter(getter) => {
						let state = crate::behavior::functions::ThisValue::Passed(on);
						let call = getter.call(
							CalledWithNew::None,
							state,
							None,
							// TODO
							None,
							&[],
							SpanWithSource::NULL_SPAN,
							environment,
							behavior,
							types,
							true,
						);
						return match call {
							Ok(res) => Some((PropertyKind::Getter, res.returned_type)),
							Err(_) => {
								todo!()
							}
						};
					}
					Property::Setter(_) => todo!(),
				}
			}
			Logical::Or(_) => todo!(),
			Logical::Implies { on: log_on, mut antecedent } => {
				let (kind, ty) = property_on_logical(*log_on, types, on, environment, behavior)?;
				let ty = specialize(ty, &mut antecedent, environment, types);
				Some((kind, ty))
			}
		}
	}
}

fn getter_on_type<'a, E: CallCheckingBehavior>(
	constraint: TypeId,
	under: TypeId,
	on: TypeId,
	with: Option<TypeId>,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Option<TypeId> {
	// match constraint {
	// 	PolyBase::Fixed { to, is_open_poly } => {
	// crate::utils::notify!(
	// 	"Get property found fixed constraint {}, is_open_poly={:?}",
	// 	environment.debug_type(on, types),
	// 	is_open_poly
	// );

	let fact = environment.get_property_unbound(constraint, under, types)?;

	match fact {
		Logical::Pure(og) => {
			match og {
				Property::Value(og) => {
					match types.get_type_by_id(og) {
						Type::FunctionReference(func, _) => {
							// TODO only want to do sometimes, or even never as it can be pulled using the poly chain
							let with_this = types
								.register_type(Type::Function(func.clone(), ThisValue::Passed(og)));
							Some(with_this)
						}
						Type::Class(..) | Type::Function(..) => todo!(),
						Type::And(_, _)
						| Type::Object(..)
						| Type::RootPolyType(_)
						| Type::Constructor(_)
						| Type::Or(_, _)
						| Type::AliasTo { .. }
						| Type::NamedRooted { .. } => {
							// TODO this isn't necessary sometimes
							let constructor_result =
								types.register_type(Type::Constructor(Constructor::Property {
									on,
									under,
								}));

							Some(constructor_result)
						}
						Type::Constant(_) => Some(og),
					}
				}
				Property::GetterAndSetter(_getter, _) | Property::Getter(_getter) => {
					// if is_open_poly {
					// 	crate::utils::notify!("TODO evaluate getter...");
					// } else {
					// 	crate::utils::notify!("TODO don't evaluate getter");
					// }
					todo!()
				}
				Property::Setter(_) => todo!("error"),
			}
		}
		Logical::Or(_) => todo!(),
		Logical::Implies { .. } => todo!(),
	}
	// }
	// PolyBase::Dynamic { to, boundary } => {
	// 	todo!("this is likely changing")
	// crate::utils::notify!(
	// 	"Getting property {:?} which has a dynamic constraint {:?}",
	// 	on,
	// 	to
	// );
	// // If the property on the dynamic constraint is None
	// if environment.get_property_unbound(to, under, types).is_none() {
	// 	let on = if to == TypeId::ANY_TYPE {
	// 		let new_constraint = types.register_type(Type::Object(
	// 			crate::types::ObjectNature::ModifiableConstraint,
	// 		));
	// 		crate::utils::notify!("Here!!!");
	// 		environment.attempt_to_modify_base(on, boundary, new_constraint);
	// 		new_constraint
	// 	} else if matches!(
	// 		types.get_type_by_id(to),
	// 		Type::AliasTo { to: TypeId::OBJECT_TYPE, .. }
	// 	) {
	// 		to
	// 	} else {
	// 		todo!("new and")
	// 	};

	// 	environment
	// 		.facts
	// 		.current_properties
	// 		.entry(on)
	// 		.or_default()
	// 		.push((under, Property::Value(with.unwrap_or(TypeId::ANY_TYPE))));
	// } else {
	// 	crate::utils::notify!("Found existing property on dynamic constraint");
	// }

	// let constructor_result =
	// 	types.register_type(Type::Constructor(Constructor::Property { on, under }));

	// // environment
	// // 	.context_type
	// // 	.get_inferrable_constraints_mut()
	// // 	.unwrap()
	// // 	.insert(constructor_result);

	// Some(GetResult::AccessIntroducesDependence(constructor_result))
	// }
	// }
}

/// Aka a assignment to a property, **INCLUDING initialization of a new one**
///
/// Evaluates setters
pub(crate) fn set_property<'a, E: CallCheckingBehavior>(
	on: TypeId,
	under: TypeId,
	new: Property,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Result<Option<TypeId>, SetPropertyError> {
	// TODO
	// if environment.is_not_writeable(on, under) {
	// 	return Err(SetPropertyError::NotWriteable);
	// }

	// if E::CHECK_PARAMETERS {
	let property_constraint = {
		let constraint = environment.get_object_constraint(on);

		match constraint {
			Some(constraint) => {
				let result = environment.get_property_unbound(constraint, under, types);
				if result.is_none() {
					// TODO does not exist
					return Err(SetPropertyError::DoesNotMeetConstraint(
						new.as_get_type(),
						todo!("no property"),
					));
				}
				result
			}
			None => None,
		}
	};

	if let Some(constraint) = property_constraint {
		let mut basic_subtyping = crate::types::subtyping::BasicEquality {
			add_property_restrictions: true,
			position: source_map::SpanWithSource { start: 0, end: 0, source: SourceId::NULL },
		};
		let base_type = constraint.prop_to_type();
		if let SubTypeResult::IsNotSubType(sub_type_error) = type_is_subtype(
			base_type,
			new.as_get_type(),
			None,
			// TODO undecided here
			// TODO position here
			&mut basic_subtyping,
			environment,
			types,
		) {
			// TODO don't short circuit
			return Err(SetPropertyError::DoesNotMeetConstraint(base_type, sub_type_error));
		}
	}
	// }

	// crate::utils::notify!(
	// 	"setting {:?} {:?} {:?}",
	// 	crate::types::printing::print_type(types, on, &environment.into_general_context(), true),
	// 	crate::types::printing::print_type(types, under, &environment.into_general_context(), true),
	// 	crate::types::printing::print_type(
	// 		types,
	// 		new.as_get_type(),
	// 		&environment.into_general_context(),
	// 		true
	// 	)
	// );

	let current_property = environment.get_property_unbound(on, under, types);

	let new = Property::Value(new.as_get_type());

	if let Some(fact) = current_property {
		match fact {
			Logical::Pure(og) => match og {
				Property::Value(og) => {
					let facts = behavior.get_top_level_facts(environment);
					facts.current_properties.entry(on).or_default().push((under, new.clone()));
					facts.events.push(Event::Setter {
						on,
						new,
						under,
						// TODO
						reflects_dependency: None,
						initialization: false,
					});
				}
				Property::Getter(_) => todo!(),
				Property::GetterAndSetter(_, _setter) | Property::Setter(_setter) => todo!(),
			},
			Logical::Or(_) => todo!(),
			Logical::Implies { .. } => todo!(),
		}
	} else {
		// TODO abstract
		behavior.get_top_level_facts(environment).register_property(on, under, new, false);
	}
	Ok(None)
}
