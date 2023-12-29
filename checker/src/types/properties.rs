use std::borrow::Cow;

use crate::{
	behavior::functions::ThisValue,
	context::{facts::Publicity, CallCheckingBehavior, Logical, SetPropertyError},
	diagnostics::TypeStringRepresentation,
	events::Event,
	subtyping::{type_is_subtype_of_property, SubTypeResult},
	types::{
		calling::CallingInput, get_constraint,
		poly_types::generic_type_arguments::StructureGenericArguments, substitute, FunctionType,
		ObjectNature, StructureGenerics,
	},
	Constant, Environment, TypeId,
};

use source_map::{SourceId, SpanWithSource};

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
				Constant::Symbol { key: _ } => todo!(),
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
	fn serialize(self, _buf: &mut Vec<u8>) {
		todo!()
	}

	fn deserialize<I: Iterator<Item = u8>>(_iter: &mut I, _source: SourceId) -> Self {
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
	top_environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
	position: SpanWithSource,
) -> Option<(PropertyKind, TypeId)> {
	if on == TypeId::ERROR_TYPE
		|| matches!(under, PropertyKey::Type(under) if under == TypeId::ERROR_TYPE)
	{
		return Some((PropertyKind::Direct, TypeId::ERROR_TYPE));
	}

	if let Some(constraint) = get_constraint(on, types) {
		evaluate_get_on_poly(
			constraint,
			on,
			publicity,
			under.clone(),
			with,
			top_environment,
			behavior,
			types,
			position,
		)
	} else if top_environment.possibly_mutated_objects.contains(&on) {
		let items = top_environment.get_object_constraints(on);
		let constraint = if items.len() == 1 {
			items.into_iter().next().unwrap()
		} else if items.is_empty() {
			todo!("inference")
		} else {
			todo!("build and type")
		};
		evaluate_get_on_poly(
			constraint,
			on,
			publicity,
			under.clone(),
			with,
			top_environment,
			behavior,
			types,
			position,
		)
	} else {
		// if environment.get_poly_base(under, types).is_some() {
		// 	todo!()
		// }
		// TODO
		get_from_an_object(on, publicity, under, top_environment, behavior, types)
	}
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
							Type::Function(func, _state) => {
								let func = types
									.register_type(Type::Function(*func, ThisValue::Passed(on)));
								Some((PropertyKind::Direct, func))
							}
							Type::FunctionReference(func, _this_argument) => {
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
								if let Type::Function(f, _p) = types.get_type_by_id(*sg_on) {
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
							Type::AliasTo { to: _, name: _, parameters: _ } => {
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

				crate::utils::notify!("Specialising implies");
				let ty = substitute(ty, &mut antecedent, environment, types);
				Some((kind, ty))
			}
		}
	}

	// TODO explain what happens around non constant strings
	if let Type::Constant(Constant::String(s)) = types.get_type_by_id(on) {
		if let Some(n) = under.as_number() {
			return s.chars().nth(n).map(|s| {
				(PropertyKind::Direct, types.new_constant_type(Constant::String(s.to_string())))
			});
		}
	}

	let result = environment.get_property_unbound(on, publicity, under, types)?;

	resolve_property_on_logical(result, types, on, environment, behavior)
}

#[allow(clippy::too_many_arguments)]
#[allow(clippy::needless_pass_by_value)]
fn evaluate_get_on_poly<E: CallCheckingBehavior>(
	constraint: TypeId,
	on: TypeId,
	publicity: Publicity,
	under: PropertyKey,
	_with: Option<TypeId>,
	top_environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
	position: SpanWithSource,
) -> Option<(PropertyKind, TypeId)> {
	fn resolve_logical_with_poly(
		fact: Logical<PropertyValue>,
		on: TypeId,
		under: PropertyKey,
		// TODO generic chain
		arguments: Option<&StructureGenericArguments>,
		environment: &mut Environment,
		types: &mut TypeStore,
	) -> Option<TypeId> {
		match fact {
			Logical::Pure(og) => {
				match og {
					PropertyValue::Value(value) => match types.get_type_by_id(value) {
						t @ (Type::And(_, _)
						| Type::Or(_, _)
						| Type::RootPolyType(_)
						| Type::Constructor(_)) => {
							let result = if let Some(arguments) = arguments {
								substitute(
									value,
									&mut arguments.type_arguments.clone(),
									environment,
									types,
								)
							} else {
								crate::utils::notify!("Here, getting property on {:?}", t);
								value
							};
							let constructor_result =
								types.register_type(Type::Constructor(Constructor::Property {
									on,
									under: under.into_owned(),
									result,
									// TODO #98
									bind_this: true,
								}));

							Some(constructor_result)
						}
						Type::Function(..) => unreachable!(),
						Type::FunctionReference(..)
						| Type::AliasTo { .. }
						| Type::Object(ObjectNature::AnonymousTypeAnnotation)
						| Type::Interface { .. } => {
							let constructor_result =
								types.register_type(Type::Constructor(Constructor::Property {
									on,
									under: under.into_owned(),
									result: value,
									// TODO #98
									bind_this: true,
								}));

							Some(constructor_result)
						}
						Type::Constant(_)
						| Type::Object(ObjectNature::RealDeal)
						| Type::SpecialObject(..) => Some(value),
					},
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
							bind_this: false,
						})))
					}
					PropertyValue::Setter(_) => todo!("error"),
					// Very important
					PropertyValue::Deleted => None,
				}
			}
			Logical::Or { left, right } => {
				let left = resolve_logical_with_poly(
					*left,
					on,
					under.clone(),
					arguments,
					environment,
					types,
				);
				let right =
					resolve_logical_with_poly(*right, on, under, arguments, environment, types);

				if let (Some(lhs), Some(rhs)) = (left, right) {
					crate::utils::notify!("TODO how does conditionality work");
					Some(types.new_or_type(lhs, rhs))
				} else {
					crate::utils::notify!("TODO emit some diagnostic about missing");
					None
				}
			}
			Logical::Implies { on: implies_on, antecedent } => {
				if arguments.is_some() {
					todo!("generics chain")
				}

				resolve_logical_with_poly(
					*implies_on,
					on,
					under.clone(),
					Some(&antecedent),
					environment,
					types,
				)
			}
		}
	}

	let fact = top_environment.get_property_unbound(constraint, publicity, under.clone(), types)?;

	// crate::utils::notify!("unbound is is {:?}", fact);

	let value = resolve_logical_with_poly(fact, on, under.clone(), None, top_environment, types)?;

	behavior.get_latest_facts(top_environment).events.push(Event::Getter {
		on,
		under: under.into_owned(),
		reflects_dependency: Some(value),
		publicity,
		position,
	});

	Some((PropertyKind::Direct, value))
}

/// Aka a assignment to a property, **INCLUDING initialization of a new one**
///
/// Evaluates setters
#[allow(clippy::too_many_arguments)] // https://github.com/kaleidawave/ezno/pull/88
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
	let object_constraint = environment.get_object_constraints(on);

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

	// crate::utils::notify!("(2) Made it here assigning to {:?}", types.get_type_by_id(on));

	let new = PropertyValue::Value(new.as_get_type());

	if let Some(fact) = current_property {
		match fact {
			Logical::Pure(og) => match og {
				PropertyValue::Deleted | PropertyValue::Value(..) => {
					let facts = behavior.get_latest_facts(environment);
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
			Logical::Implies { on: _implies_on, antecedent: _ } => {
				crate::utils::notify!("Check that `implies_on` could be a setter here");
				let facts = behavior.get_latest_facts(environment);
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
		behavior.get_latest_facts(environment).register_property(
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
