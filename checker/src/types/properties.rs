use std::borrow::Cow;

use crate::{
	behavior::functions::ThisValue,
	context::{facts::Publicity, CallCheckingBehavior, Logical, SetPropertyError},
	events::Event,
	subtyping::{type_is_subtype, SubTypeResult},
	types::{
		calling::CallingInput, printing::print_type, substitute, FunctionType, StructureGenerics,
	},
	Constant, Environment, TypeId,
};

use ordered_float::NotNan;
use source_map::{SourceId, Span, SpanWithSource};

use super::{calling::CalledWithNew, Constructor, Type, TypeStore};

pub enum PropertyKind {
	Direct,
	Getter,
	/// TODO not sure
	Generic,
}

/// TODO symbol
/// Implements basic definition equality, not type equality
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PropertyKey<'a> {
	String(Cow<'a, str>),
	// Special
	Type(TypeId),
}

impl<'a> PropertyKey<'a> {
	#[must_use]
	pub fn into_owned(&self) -> PropertyKey<'static> {
		match self {
			PropertyKey::String(s) => PropertyKey::String(Cow::Owned(s.to_string())),
			PropertyKey::Type(s) => PropertyKey::Type(*s),
		}
	}

	pub(crate) fn from_type(ty: TypeId, types: &TypeStore) -> PropertyKey<'static> {
		if let Type::Constant(c) = types.get_type_by_id(ty) {
			match c {
				Constant::Number(n) => {
					// if n.fractional ??
					PropertyKey::from_usize(n.into_inner() as usize)
				}
				Constant::String(s) => PropertyKey::String(Cow::Owned(s.to_owned())),
				Constant::Boolean(_) => todo!(),
				Constant::Regexp(_) => todo!(),
				Constant::Symbol { key } => todo!(),
				Constant::Undefined => todo!(),
				Constant::Null => todo!(),
				Constant::NaN => todo!(),
			}
		} else {
			PropertyKey::Type(ty)
		}
	}
}

impl crate::serialization::BinarySerializable for PropertyKey<'static> {
	fn serialize(self, buf: &mut Vec<u8>) {
		todo!()
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		todo!()
	}
}

static NUMBERS: &str = "0123456789";

impl<'a> PropertyKey<'a> {
	/// For array indexes
	#[must_use]
	pub fn from_usize(a: usize) -> Self {
		if a < 10 {
			Self::String(Cow::Borrowed(&NUMBERS[a..=a]))
		} else {
			Self::String(Cow::Owned(a.to_string()))
		}
	}
}

/// TODO type predicate based
///
/// TODO getter, setting need a closure id
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum PropertyValue {
	Value(TypeId),
	Getter(Box<FunctionType>),
	Setter(Box<FunctionType>),
	Deleted,
}

impl PropertyValue {
	/// TODO wip
	#[must_use]
	pub fn as_get_type(&self) -> TypeId {
		match self {
			PropertyValue::Value(value) => *value,
			PropertyValue::Getter(getter) => getter.return_type,
			// TODO not sure about these two
			PropertyValue::Setter(_) => TypeId::UNDEFINED_TYPE,
			PropertyValue::Deleted => TypeId::NEVER_TYPE,
		}
	}

	#[must_use]
	pub fn as_set_type(&self) -> TypeId {
		match self {
			PropertyValue::Value(value) => *value,
			PropertyValue::Setter(setter) => setter.return_type,
			// TODO not sure about these two
			PropertyValue::Getter(_) => TypeId::UNDEFINED_TYPE,
			PropertyValue::Deleted => TypeId::NEVER_TYPE,
		}
	}
}

/// Also evaluates getter and binds `this`
///
/// *be aware this creates a new type every time, bc of this binding. could cache this bound
/// types at some point*
// https://github.com/kaleidawave/ezno/pull/88
#[allow(clippy::too_many_arguments)]
pub(crate) fn get_property<E: CallCheckingBehavior>(
	on: TypeId,
	publicity: Publicity,
	under: PropertyKey,
	with: Option<TypeId>,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
	position: SpanWithSource,
) -> Option<(PropertyKind, TypeId)> {
	enum GetResult {
		AccessIntroducesDependence(TypeId),
		/// These always return the same value
		FromAObject(TypeId),
	}

	// || under == TypeId::ERROR_TYPE
	if on == TypeId::ERROR_TYPE {
		return Some((PropertyKind::Direct, TypeId::ERROR_TYPE));
	}

	let value: GetResult = if let Some(constraint) = environment.get_poly_base(on, types) {
		GetResult::AccessIntroducesDependence(evaluate_get_on_poly(
			constraint,
			on,
			publicity,
			under.clone(),
			with,
			environment,
			behavior,
			types,
		)?)
	} else {
		// if environment.get_poly_base(under, types).is_some() {
		// 	todo!()
		// }
		// TODO
		return get_from_an_object(on, publicity, under, environment, behavior, types);
	};

	let reflects_dependency = match value {
		GetResult::AccessIntroducesDependence(s) => Some(s),
		GetResult::FromAObject(_) => None,
	};

	behavior.get_top_level_facts(environment).events.push(Event::Getter {
		on,
		under: under.into_owned(),
		reflects_dependency,
		publicity,
		position,
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

fn get_from_an_object<E: CallCheckingBehavior>(
	on: TypeId,
	publicity: Publicity,
	under: PropertyKey,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
) -> Option<(PropertyKind, TypeId)> {
	/// Generates closure arguments, values of this and more. Runs getters
	fn resolve_property_on_logical<E: CallCheckingBehavior>(
		logical: Logical<PropertyValue>,
		types: &mut TypeStore,
		on: TypeId,
		environment: &mut Environment,
		behavior: &mut E,
	) -> Option<(PropertyKind, TypeId)> {
		match logical {
			Logical::Pure(property) => {
				match property {
					PropertyValue::Value(value) => {
						let ty = types.get_type_by_id(value);
						match ty {
							// TODO function :: bind_this
							Type::Function(func, state) => {
								let func = types
									.register_type(Type::Function(*func, ThisValue::Passed(on)));
								Some((PropertyKind::Direct, func))
							}
							Type::FunctionReference(func, this_argument) => {
								crate::utils::notify!("TODO temp reference function business");
								let func = types.register_type(Type::FunctionReference(
									*func,
									ThisValue::Passed(on),
								));
								Some((PropertyKind::Direct, func))
							}
							Type::SpecialObject(..)
							| Type::Object(..)
							| Type::RootPolyType { .. }
							| Type::Constant(..) => Some((PropertyKind::Direct, value)),
							Type::Interface { .. } | Type::And(_, _) | Type::Or(_, _) => {
								crate::utils::notify!(
								    "property was {:?} {:?}, which should be NOT be able to be returned from a function",
								    property, ty
							    );
								let value = types.register_type(Type::RootPolyType(
									crate::types::PolyNature::Open(value),
								));
								Some((PropertyKind::Direct, value))
							}
							Type::Constructor(Constructor::StructureGenerics(
								StructureGenerics { on: sg_on, arguments },
							)) => {
								// TODO not great... need less overhead
								if let Type::Function(f, p) = types.get_type_by_id(*sg_on) {
									let arguments = arguments.clone();
									let f = types
										.register_type(Type::Function(*f, ThisValue::Passed(on)));
									let ty = types.register_type(Type::Constructor(
										Constructor::StructureGenerics(StructureGenerics {
											on: f,
											arguments,
										}),
									));
									Some((PropertyKind::Direct, ty))
								} else {
									Some((PropertyKind::Direct, value))
								}
							}
							Type::Constructor(constructor) => {
								unreachable!("Interesting property on {:?}", constructor);
							}
							Type::AliasTo { to, name, parameters } => {
								todo!()
							}
						}
					}
					PropertyValue::Getter(getter) => {
						let state = ThisValue::Passed(on);
						let call = getter.call(
							CallingInput {
								called_with_new: CalledWithNew::None,
								this_value: state,
								call_site_type_arguments: None,
								call_site: SpanWithSource::NULL_SPAN,
							},
							// TODO
							None,
							&[],
							environment,
							behavior,
							types,
							true,
						);
						match call {
							Ok(res) => Some((PropertyKind::Getter, res.returned_type)),
							Err(_) => {
								todo!()
							}
						}
					}
					PropertyValue::Setter(_) => todo!(),
					PropertyValue::Deleted => None,
				}
			}
			Logical::Or { .. } => todo!(),
			Logical::Implies { on: log_on, mut antecedent } => {
				let (kind, ty) =
					resolve_property_on_logical(*log_on, types, on, environment, behavior)?;
				let ty = substitute(ty, &mut antecedent, environment, types);
				Some((kind, ty))
			}
		}
	}

	let result = environment.get_property_unbound(on, publicity, under, types)?;

	resolve_property_on_logical(result, types, on, environment, behavior)
}

// https://github.com/kaleidawave/ezno/pull/88
#[allow(clippy::too_many_arguments)]
fn evaluate_get_on_poly<E: CallCheckingBehavior>(
	constraint: TypeId,
	on: TypeId,
	publicity: Publicity,
	under: PropertyKey,
	with: Option<TypeId>,
	environment: &mut Environment,
	behavior: &E,
	types: &mut TypeStore,
) -> Option<TypeId> {
	// match constraint {
	// 	PolyBase::Fixed { to, is_open_poly } => {
	// crate::utils::notify!(
	// 	"Get property found fixed constraint {}, is_open_poly={:?}",
	// 	environment.debug_type(on, types),
	// 	is_open_poly
	// );

	fn get_property_from_logical(
		fact: Logical<PropertyValue>,
		types: &mut TypeStore,
		on: TypeId,
		under: PropertyKey,
	) -> Option<TypeId> {
		match fact {
			Logical::Pure(og) => {
				match og {
					PropertyValue::Value(og) => {
						match types.get_type_by_id(og) {
							Type::FunctionReference(func, _) => {
								// TODO only want to do sometimes, or even never as it can be pulled using the poly chain
								let depends_on_this =
									types.get_function_from_id(*func).behavior.can_be_bound();

								if depends_on_this {
									let with_this = types.register_type(Type::Function(
										*func,
										ThisValue::Passed(on),
									));
									Some(with_this)
								} else {
									Some(og)
								}
							}
							Type::Function(..) => todo!(),
							Type::And(_, _)
							| Type::RootPolyType(_)
							| Type::Constructor(_)
							| Type::Or(_, _)
							| Type::AliasTo { .. }
							| Type::Interface { .. } => {
								// TODO this isn't necessary sometimes
								let constructor_result =
									types.register_type(Type::Constructor(Constructor::Property {
										on,
										under: under.into_owned(),
										result: og,
									}));

								Some(constructor_result)
							}
							Type::Constant(_) | Type::Object(..) | Type::SpecialObject(..) => {
								Some(og)
							}
						}
					}
					PropertyValue::Getter(getter) => {
						// if is_open_poly {
						// 	crate::utils::notify!("TODO evaluate getter...");
						// } else {
						// 	crate::utils::notify!("TODO don't evaluate getter");
						// }
						// TODO : getter.return_type
						Some(types.register_type(Type::Constructor(Constructor::Property {
							on,
							under: under.into_owned(),
							result: getter.return_type,
						})))
					}
					PropertyValue::Setter(_) => todo!("error"),
					// Very important
					PropertyValue::Deleted => None,
				}
			}
			Logical::Or { left, right } => {
				let left = get_property_from_logical(*left, types, on, under.clone());
				let right = get_property_from_logical(*right, types, on, under);

				if let (Some(lhs), Some(rhs)) = (left, right) {
					crate::utils::notify!("TODO how does conditionality work");
					Some(types.new_or_type(lhs, rhs))
				} else {
					crate::utils::notify!("TODO emit some diagnostic about missing");
					None
				}
			}
			Logical::Implies { on: a, antecedent } => {
				todo!()
				// TODO pass down argument instead ...
				// let a = get_property_from_logical(*a, types, on, under.clone())?;
				// Some(substitute(*a, &mut antecedent.type_arguments, environment, types))
			}
		}
	}

	let fact = environment.get_property_unbound(constraint, publicity, under.clone(), types)?;

	get_property_from_logical(fact, types, on, under)
}

/// Aka a assignment to a property, **INCLUDING initialization of a new one**
///
/// Evaluates setters

// https://github.com/kaleidawave/ezno/pull/88
#[allow(clippy::too_many_arguments)]
pub(crate) fn set_property<E: CallCheckingBehavior>(
	on: TypeId,
	publicity: Publicity,
	under: &PropertyKey,
	new: &PropertyValue,
	environment: &mut Environment,
	behavior: &mut E,
	types: &TypeStore,
	setter_position: Option<SpanWithSource>,
) -> Result<Option<TypeId>, SetPropertyError> {
	// TODO
	// if environment.is_not_writeable(on, under) {
	// 	return Err(SetPropertyError::NotWriteable);
	// }

	// if E::CHECK_PARAMETERS {
	let object_constraint = environment.get_object_constraint(on);

	for constraint in object_constraint {
		let property_constraint =
			environment.get_property_unbound(constraint, publicity, under.clone(), types);

		// crate::utils::notify!(
		// 	"Re-assignment constraint {}, prop={} {:?}",
		// 	print_type(constraint, types, &environment.as_general_context(), true),
		// 	print_type(under, types, &environment.as_general_context(), true),
		// 	property_constraint
		// );

		if let Some(property) = property_constraint {
			let mut basic_subtyping = crate::types::subtyping::BasicEquality {
				// This is important for free variables, sometimes ?
				add_property_restrictions: true,
				// TODO position here
				position: source_map::SpanWithSource::NULL_SPAN,
			};
			let property = property.prop_to_type();
			crate::utils::notify!(
				"{}",
				print_type(property, types, &environment.as_general_context(), true)
			);
			if let SubTypeResult::IsNotSubType(sub_type_error) = type_is_subtype(
				property,
				new.as_get_type(),
				&mut basic_subtyping,
				environment,
				types,
			) {
				// TODO don't short circuit
				return Err(SetPropertyError::DoesNotMeetConstraint(property, sub_type_error));
			}
		} else {
			// TODO does not exist warning
			// return Err(SetPropertyError::DoesNotMeetConstraint(
			// 	new.as_get_type(),
			// 	todo!("no property"),
			// ));
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

	let current_property = environment.get_property_unbound(on, publicity, under.clone(), types);

	let new = PropertyValue::Value(new.as_get_type());

	if let Some(fact) = current_property {
		match fact {
			Logical::Pure(og) => match og {
				PropertyValue::Deleted | PropertyValue::Value(..) => {
					let facts = behavior.get_top_level_facts(environment);
					facts.current_properties.entry(on).or_default().push((
						publicity,
						under.into_owned(),
						new.clone(),
					));
					facts.events.push(Event::Setter {
						on,
						new,
						under: under.into_owned(),
						publicity,
						// TODO
						reflects_dependency: None,
						initialization: false,
						position: setter_position,
					});
				}
				PropertyValue::Getter(_) => todo!(),
				PropertyValue::Setter(_setter) => {
					todo!()
				}
			},
			Logical::Or { .. } => todo!(),
			Logical::Implies { .. } => todo!(),
		}
	} else {
		// TODO abstract
		// TODO only if dependent?
		let register_setter_event = true;
		behavior.get_top_level_facts(environment).register_property(
			on,
			publicity,
			under.into_owned(),
			new,
			register_setter_event,
			setter_position,
		);
	}
	Ok(None)
}
