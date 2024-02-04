use std::borrow::Cow;

use crate::{
	context::{
		information::{get_property_unbound, Publicity},
		CallCheckingBehavior, Logical, SetPropertyError,
	},
	diagnostics::TypeStringRepresentation,
	events::Event,
	features::functions::ThisValue,
	subtyping::{type_is_subtype_of_property, SubTypeResult},
	types::{
		get_constraint,
		poly_types::generic_type_arguments::{ExplicitTypeArguments, StructureGenericArguments},
		substitute, FunctionType, GenericChain, ObjectNature, StructureGenerics,
	},
	Constant, Environment, TypeId,
};

use source_map::SpanWithSource;

use super::{calling::CalledWithNew, Constructor, Type, TypeStore};

pub enum PropertyKind {
	Direct,
	Getter,
	/// TODO unsure
	Generic,
}

/// TODO symbol
/// Implements basic definition equality, not type equality
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PropertyKey<'a> {
	String(Cow<'a, str>),
	Type(TypeId),
	// SomeThingLike(TypeId),
}

impl crate::BinarySerializable for PropertyKey<'static> {
	fn serialize(self, buf: &mut Vec<u8>) {
		match self {
			PropertyKey::String(s) => {
				buf.push(0);
				crate::BinarySerializable::serialize(s.into_owned(), buf);
			}
			PropertyKey::Type(t) => {
				buf.push(1);
				crate::BinarySerializable::serialize(t, buf);
			}
		}
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: source_map::SourceId) -> Self {
		match iter.next().unwrap() {
			0 => Self::String(Cow::Owned(crate::BinarySerializable::deserialize(iter, source))),
			1 => Self::Type(crate::BinarySerializable::deserialize(iter, source)),
			_ => panic!("bad code"),
		}
	}
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

	pub(crate) fn as_number(&self, types: &TypeStore) -> Option<usize> {
		match self {
			PropertyKey::String(s) => s.parse::<usize>().ok(),
			PropertyKey::Type(t) => {
				if let Type::Constant(Constant::Number(n)) = types.get_type_by_id(*t) {
					if n.trunc() == **n {
						Some(**n as usize)
					} else {
						None
					}
				} else {
					None
				}
			}
		}
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
			// TODO unsure about these two
			PropertyValue::Setter(_) => TypeId::UNDEFINED_TYPE,
			PropertyValue::Deleted => TypeId::NEVER_TYPE,
		}
	}

	#[must_use]
	pub fn as_set_type(&self) -> TypeId {
		match self {
			PropertyValue::Value(value) => *value,
			PropertyValue::Setter(setter) => setter.return_type,
			// TODO unsure about these two
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

	if get_constraint(on, types).is_some() {
		// // TODO temp fix for assigning to a poly type. What about unions etc
		// if let Some(value) = top_environment.info_chain().find_map(|f| {
		// 	f.current_properties.get(&on).and_then(|props| {
		// 		props.iter().find_map(|(_publicity, key, value)| (key == &under).then_some(value))
		// 	})
		// }) {
		// 	return Some((PropertyKind::Direct, value.as_get_type()));
		// }

		evaluate_get_on_poly(
			// constraint,
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
		let constraint = if let Some(item) = top_environment.get_object_constraint(on) {
			item
		} else {
			todo!("inference")
		};
		// TODO ...
		evaluate_get_on_poly(
			constraint,
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
		on: TypeId,
		generics: Option<GenericChain>,
		environment: &mut Environment,
		types: &mut TypeStore,
		behavior: &mut E,
	) -> Option<(PropertyKind, TypeId)> {
		match logical {
			Logical::Error => Some((PropertyKind::Direct, TypeId::ERROR_TYPE)),
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
							Type::FunctionReference(func) => {
								// used for `"hi".toUpperCase()`;
								// TODO a little bit weird how it goes from FunctionReference -> Function... but should be okay.
								let func = types
									.register_type(Type::Function(*func, ThisValue::Passed(on)));

								let ty = if let Some(chain) = generics {
									assert!(chain.parent.is_none());
									types.register_type(Type::Constructor(
										Constructor::StructureGenerics(StructureGenerics {
											on: func,
											arguments: chain.value.into_owned(),
										}),
									))
								} else {
									func
								};

								Some((PropertyKind::Direct, ty))
							}
							// TODO if uses generics
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
							// For closures
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
						let call = getter.call(
							CalledWithNew::None,
							ThisValue::Passed(on),
							source_map::Nullable::NULL,
							&[],
							None,
							// TODO structure generics
							None,
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
			Logical::Implies { on: log_on, antecedent } => resolve_property_on_logical(
				*log_on,
				on,
				Some(GenericChain::append(generics.as_ref(), &antecedent)),
				environment,
				types,
				behavior,
			),
		}
	}

	// TODO explain what happens around non constant strings
	if let Type::Constant(Constant::String(s)) = types.get_type_by_id(on) {
		if let Some(n) = under.as_number(&types) {
			return s.chars().nth(n).map(|s| {
				(PropertyKind::Direct, types.new_constant_type(Constant::String(s.to_string())))
			});
		}
	}

	let result = get_property_unbound(on, publicity, under, types, environment)?;

	crate::utils::notify!("Found {:?}", result);

	resolve_property_on_logical(result, on, None, environment, types, behavior)
}

#[allow(clippy::too_many_arguments)]
#[allow(clippy::needless_pass_by_value)]
fn evaluate_get_on_poly<E: CallCheckingBehavior>(
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
			Logical::Error => Some(TypeId::ERROR_TYPE),
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
									&mut ExplicitTypeArguments(
										&mut arguments.type_restrictions.clone(),
									),
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

	let fact = get_property_unbound(on, publicity, under.clone(), types, top_environment)?;

	// crate::utils::notify!("unbound is is {:?}", fact);

	let value = resolve_logical_with_poly(fact, on, under.clone(), None, top_environment, types)?;

	behavior.get_latest_info(top_environment).events.push(Event::Getter {
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
	new: PropertyValue,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
	setter_position: Option<SpanWithSource>,
) -> Result<Option<TypeId>, SetPropertyError> {
	// TODO
	// if environment.is_not_writeable(on, under) {
	// 	return Err(SetPropertyError::NotWriteable);
	// }

	// if E::CHECK_PARAMETERS {
	if let Some(constraint) = environment.get_object_constraint(on) {
		let property_constraint =
			get_property_unbound(constraint, publicity, under.clone(), types, environment);

		// crate::utils::notify!("Property constraint .is_some() {:?}", property_constraint.is_some());

		// crate::utils::notify!(
		// 	"Re-assignment constraint {}, prop={} {:?}",
		// 	print_type(constraint, types, environment, true),
		// 	print_type(under, types, environment, true),
		// 	property_constraint
		// );

		if let Some(property_constraint) = property_constraint {
			let mut basic_subtyping = crate::types::subtyping::BasicEquality {
				// This is important for free variables, sometimes ?
				add_property_restrictions: true,
				// TODO position here
				position: source_map::Nullable::NULL,
				object_constraints: Default::default(),
			};

			match new {
				PropertyValue::Value(value) => {
					let result = type_is_subtype_of_property(
						&property_constraint,
						None,
						value,
						&mut basic_subtyping,
						environment,
						types,
					);
					if let SubTypeResult::IsNotSubType(reason) = result {
						return Err(SetPropertyError::DoesNotMeetConstraint {
							property_constraint: TypeStringRepresentation::from_property_constraint(
								property_constraint,
								None,
								environment,
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

			environment.add_object_constraints(basic_subtyping.object_constraints, types);
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
	// 	crate::types::printing::print_type(types, on, environment, true),
	// 	crate::types::printing::print_type(types, under, environment, true),
	// 	crate::types::printing::print_type(types, new.as_get_type(), environment, true),
	// );

	let current_property = get_property_unbound(on, publicity, under.clone(), types, environment);

	// crate::utils::notify!("(2) Made it here assigning to {:?}", types.get_type_by_id(on));

	// Cascade if it is a union (unsure tho)
	if let Type::Constructor(Constructor::ConditionalResult {
		truthy_result,
		else_result,
		condition: _,
		result_union: _,
	}) = types.get_type_by_id(on)
	{
		let truthy = *truthy_result;
		let else_result = *else_result;
		set_property(
			truthy,
			publicity,
			under,
			new.clone(),
			environment,
			behavior,
			types,
			setter_position,
		)?;
		return set_property(
			else_result,
			publicity,
			under,
			new,
			environment,
			behavior,
			types,
			setter_position,
		);
	}

	if let Some(fact) = current_property {
		match fact {
			Logical::Error => {}
			Logical::Pure(og) => match og {
				PropertyValue::Deleted | PropertyValue::Value(..) => {
					let info = behavior.get_latest_info(environment);
					info.current_properties.entry(on).or_default().push((
						publicity,
						under.into_owned(),
						new.clone(),
					));
					info.events.push(Event::Setter {
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
				let info = behavior.get_latest_info(environment);
				info.current_properties.entry(on).or_default().push((
					publicity,
					under.into_owned(),
					new.clone(),
				));
				info.events.push(Event::Setter {
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
		behavior.get_latest_info(environment).register_property(
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
