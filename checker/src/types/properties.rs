use crate::{
	context::{CallCheckingBehavior, Logical, PolyBase, SetPropertyError},
	events::Event,
	subtyping::{type_is_subtype, SubTypeResult},
	types::FunctionType,
	Environment, TypeId,
};

use source_map::{SourceId, Span};

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

/// TODO type predicate based
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
) -> Option<PropertyResult> {
	if on == TypeId::ERROR_TYPE || under == TypeId::ERROR_TYPE {
		return Some(PropertyResult::Direct(TypeId::ERROR_TYPE));
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

	let (GetResult::AccessIntroducesDependence(value) | GetResult::FromAObject(value)) = value else { unreachable!() };

	// Carry the frozen part
	// if let Some(frozen) = environment.is_frozen(on) {
	// 	environment.facts.frozen.insert(value, frozen);
	// }

	// TODO generic
	Some(PropertyResult::Direct(value))
}

fn get_from_an_object<'a, E: CallCheckingBehavior>(
	on: TypeId,
	under: TypeId,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Option<PropertyResult> {
	match environment.get_property_unbound(on, under, types)? {
		Logical::Pure(property) => {
			match property {
				Property::Value(value) => {
					let ty = types.get_type_by_id(value);
					match ty {
						Type::Function(func, nature) => match nature {
							super::FunctionNature::BehindPoly { .. } => todo!(),
							super::FunctionNature::Source(this) => {
								if this.is_some() {
									panic!()
								}
								todo!()
								// Some(GetResult::AccessIntroducesDependence(types.register_type(Type::Function(
								// 	func.clone(),
								// 	super::FunctionNature::Source(Some(on)),
								// ))
							}
							super::FunctionNature::Constructor => todo!(),
							super::FunctionNature::Reference => {
								crate::utils::notify!("TODO temp reference function business");
								let func = types.register_type(Type::Function(
									func.clone(),
									super::FunctionNature::Source(Some(on)),
								));
								Some(PropertyResult::Direct(func))
							}
						},
						Type::Object(..) | Type::RootPolyType { .. } | Type::Constant(..) => {
							Some(PropertyResult::Direct(value))
						}
						Type::NamedRooted { .. }
						| Type::And(_, _)
						| Type::Or(_, _)
						| Type::Constructor(Constructor::StructureGenerics { .. }) => {
							crate::utils::notify!(
								"property was {:?} {:?}, which should be NOT be able to be returned from a function",
								property, ty
							);
							let value = types.register_type(Type::RootPolyType(
								crate::types::PolyNature::Open(value),
							));
							Some(PropertyResult::Direct(value))
						}
						Type::Constructor(constructor) => {
							unreachable!("Interesting property was {:?}", constructor);
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
				Property::GetterAndSetter(func, _) | Property::Getter(func) => {
					let call = func.call(
						crate::events::CalledWithNew::None,
						Some(on),
						None,
						&None,
						&[],
						Span::NULL_SPAN,
						environment,
						behavior,
						types,
					);
					return match call {
						Ok(res) => Some(PropertyResult::Getter(res.returned_type)),
						Err(_) => {
							todo!()
						}
					};
				}
				Property::Setter(_) => todo!(),
			}
		}
		Logical::Or(_) => todo!(),
		Logical::Implies(_, _) => todo!(),
	}
}

fn getter_on_type<'a, E: CallCheckingBehavior>(
	constraint: PolyBase,
	under: TypeId,
	on: TypeId,
	with: Option<TypeId>,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Option<TypeId> {
	match constraint {
		PolyBase::Fixed { to, is_open_poly } => {
			// crate::utils::notify!(
			// 	"Get property found fixed constraint {}, is_open_poly={:?}",
			// 	environment.debug_type(on, types),
			// 	is_open_poly
			// );

			let fact = environment.get_property_unbound(to, under, types)?;

			match fact {
				Logical::Pure(og) => {
					match og {
						Property::Value(og) => {
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

									Some(with_this)
								}
								Type::And(_, _)
								| Type::Object(..)
								| Type::RootPolyType(_)
								| Type::Constructor(_)
								| Type::Or(_, _)
								| Type::AliasTo { .. }
								| Type::NamedRooted { .. } => {
									// TODO this isn't necessary sometimes
									let constructor_result = types.register_type(
										Type::Constructor(Constructor::Property { on, under }),
									);

									Some(constructor_result)
								}
								Type::Constant(_) => Some(og),
							}
						}
						Property::GetterAndSetter(_getter, _) | Property::Getter(_getter) => {
							if is_open_poly {
								crate::utils::notify!("TODO evaluate getter...");
							} else {
								crate::utils::notify!("TODO don't evaluate getter");
							}
							todo!()
						}
						Property::Setter(_) => todo!("error"),
					}
				}
				Logical::Or(_) => todo!(),
				Logical::Implies(_, _) => todo!(),
			}
		}
		PolyBase::Dynamic { to, boundary } => {
			todo!("this is likely changing")
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
		}
	}
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

	if E::CHECK_PARAMETERS {
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
				position: Span { start: 0, end: 0, source: SourceId::NULL },
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
	}

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
			Logical::Implies(_, _) => todo!(),
		}
	} else {
		// TODO abstract
		behavior.get_top_level_facts(environment).register_property(on, under, new, false);
	}
	Ok(None)
}
