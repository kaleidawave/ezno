use std::borrow::Cow;

use crate::{
	behavior::functions::ThisValue,
	context::{facts::Publicity, CallCheckingBehavior, Logical, SetPropertyError},
	diagnostics::TypeStringRepresentation,
	events::Event,
	subtyping::{type_is_subtype, type_is_subtype_of_property, SubTypeResult},
	types::{
		calling::CallingInput, get_constraint, printing::print_type, substitute, FunctionType,
		ObjectNature, StructureGenerics,
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

	pub(crate) fn as_number(&self) -> Option<usize> {
		match self {
			PropertyKey::String(s) => s.parse::<usize>().ok(),
			// TODO
			PropertyKey::Type(_) => None,
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

	let value: GetResult = if let Some(constraint) = get_constraint(on, types) {
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

	// TODO explain what happens around non constant strings
	if let Type::Constant(Constant::String(s)) = types.get_type_by_id(on) {
		if let Some(n) = under.as_number() {
			return match s.chars().nth(n) {
				Some(s) => Some((
					PropertyKind::Direct,
					types.new_constant_type(Constant::String(s.to_string())),
				)),
				None => None,
			};
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
	fn resolve_logical_with_poly(
		fact: Logical<PropertyValue>,
		on: TypeId,
		under: PropertyKey,
		environment: &mut Environment,
		types: &mut TypeStore,
	) -> Option<TypeId> {
		match fact {
			Logical::Pure(og) => {
				match og {
					PropertyValue::Value(value) => {
						match types.get_type_by_id(value) {
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
									Some(value)
								}
							}
							Type::Function(..) => todo!(),
							Type::And(_, _)
							| Type::RootPolyType(_)
							| Type::Constructor(_)
							| Type::Or(_, _)
							| Type::AliasTo { .. }
							| Type::Object(ObjectNature::AnonymousTypeAnnotation)
							| Type::Interface { .. } => {
								let constructor_result =
									types.register_type(Type::Constructor(Constructor::Property {
										on,
										under: under.into_owned(),
										result: value,
									}));

								Some(constructor_result)
							}
							Type::Constant(_)
							| Type::Object(ObjectNature::RealDeal)
							| Type::SpecialObject(..) => Some(value),
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
				let left = resolve_logical_with_poly(*left, on, under.clone(), environment, types);
				let right = resolve_logical_with_poly(*right, on, under, environment, types);

				if let (Some(lhs), Some(rhs)) = (left, right) {
					crate::utils::notify!("TODO how does conditionality work");
					Some(types.new_or_type(lhs, rhs))
				} else {
					crate::utils::notify!("TODO emit some diagnostic about missing");
					None
				}
			}
			Logical::Implies { on: implies_on, antecedent } => {
				// TODO maybe pass down arguments ...
				let general_property =
					resolve_logical_with_poly(*implies_on, on, under.clone(), environment, types)?;

				let specialised_property_value = substitute(
					general_property,
					&mut antecedent.type_arguments.clone(),
					environment,
					types,
				);

				crate::utils::notify!(
					"general {:?} specialised_property_value {:?}",
					general_property,
					specialised_property_value
				);
				Some(specialised_property_value)
			}
		}
	}

	let fact = environment.get_property_unbound(constraint, publicity, under.clone(), types)?;

	// crate::utils::notify!("unbound is is {:?}", fact);

	resolve_logical_with_poly(fact, on, under, environment, types)
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

		// crate::utils::notify!("Property constraint .is_some() {:?}", property_constraint.is_some());

		// crate::utils::notify!(
		// 	"Re-assignment constraint {}, prop={} {:?}",
		// 	print_type(constraint, types, &environment.as_general_context(), true),
		// 	print_type(under, types, &environment.as_general_context(), true),
		// 	property_constraint
		// );

		if let Some(property_constraint) = property_constraint {
			let mut basic_subtyping = crate::types::subtyping::BasicEquality {
				// This is important for free variables, sometimes ?
				add_property_restrictions: true,
				// TODO position here
				position: source_map::SpanWithSource::NULL_SPAN,
			};

			match new {
				PropertyValue::Value(value) => {
					let result = type_is_subtype_of_property(
						&property_constraint,
						None,
						*value,
						&mut basic_subtyping,
						environment,
						types,
					);
					if let SubTypeResult::IsNotSubType(reason) = result {
						return Err(SetPropertyError::DoesNotMeetConstraint {
							property_constraint: TypeStringRepresentation::from_property_constraint(
								property_constraint,
								None,
								&environment.as_general_context(),
								types,
								false,
							),
							reason,
						});
					}
				}
				PropertyValue::Getter(_) => todo!(),
				PropertyValue::Setter(_) => todo!(),
				PropertyValue::Deleted => todo!(),
			}
		} else {
			// TODO does not exist warning
			// return Err(SetPropertyError::DoesNotMeetConstraint(
			// 	new.as_get_type(),
			// 	todo!("no property"),
			// ));
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
			Logical::Implies { on: implies_on, antecedent } => {
				crate::utils::notify!("Check that `implies_on` could be a setter here");
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
					initialization: false,
					position: setter_position,
				});
			}
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
