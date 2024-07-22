use std::borrow::Cow;

use crate::{
	context::{
		information::InformationChain, CallCheckingBehavior, Logical, PossibleLogical,
		SetPropertyError,
	},
    diagnostics::TypeStringRepresentation,
	events::Event,
	features::{
		functions::{FunctionBehavior, ThisValue},
		objects::{Proxy, SpecialObjects},
	},
	subtyping::{State, SubTypeResult},
	types::{
		calling::{self, FunctionCallingError},
		generics::generic_type_arguments::GenericArguments,
		get_constraint, get_larger_type, substitute, FunctionType, GenericChain, GenericChainLink, printing,
		ObjectNature, PartiallyAppliedGenerics, PolyNature, SynthesisedArgument,
	},
	Constant, Environment, LocalInformation, TypeId,
};

use source_map::SpanWithSource;

use super::{calling::CalledWithNew, Constructor, Type, TypeStore};

#[derive(PartialEq)]
pub enum PropertyKind {
	Direct,
	Getter,
	Setter,
	/// TODO unsure
	Generic,
}

/// TODO explain usage
/// For [private properties](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes/Private_properties)
#[derive(Debug, Clone, Copy, PartialEq, Eq, binary_serialize_derive::BinarySerializable)]
pub enum Publicity {
	Private,
	Public,
}

/// TODO symbol
/// Implements basic definition equality, not type equality
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PropertyKey<'a> {
	String(Cow<'a, str>),
	Type(TypeId),
	// SomeThingLike(TypeId),
}

// Cannot derive BinarySerializable because lifetime
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
					// TODO is there a better way
					#[allow(clippy::float_cmp)]
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

	pub(crate) fn new_empty_property_key() -> Self {
		PropertyKey::String(Cow::Borrowed(""))
	}

	/// For quick things
	#[must_use]
	pub fn is_equal_to(&self, key: &str) -> bool {
		match self {
			PropertyKey::String(s) => s == key,
			PropertyKey::Type(_t) => false,
		}
	}

	/// TODO when is this used
	pub fn into_type(&self, types: &mut TypeStore) -> TypeId {
		match self {
			PropertyKey::String(s) => {
				types.new_constant_type(Constant::String(s.clone().into_owned()))
			}
			PropertyKey::Type(t) => *t,
		}
	}

	pub(crate) fn mapped_generic_id(&self, types: &TypeStore) -> Option<(TypeId, TypeId)> {
		match self {
			PropertyKey::String(_) => None,
			PropertyKey::Type(ty) => {
				let get_type_by_id = types.get_type_by_id(*ty);
				if let Type::RootPolyType(super::PolyNature::MappedGeneric {
					eager_fixed, ..
				}) = get_type_by_id
				{
					Some((*ty, *eager_fixed))
				} else {
					None
				}
			}
		}
	}
}

static NUMBERS: &str = "0123456789";

impl<'a> PropertyKey<'a> {
	/// For small array indexes
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
	/// TODO doesn't exist Deleted | Optional
	Deleted,
	ConditionallyExists {
		on: TypeId,
		truthy: Box<Self>,
	},
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
			PropertyValue::ConditionallyExists { truthy, .. } => {
				// TODO temp
				truthy.as_get_type()
			}
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
			PropertyValue::ConditionallyExists { truthy, .. } => {
				// TODO temp
				truthy.as_get_type()
			}
		}
	}
}

/// Also evaluates getter and binds `this`
///
/// *be aware this creates a new type every time, bc of this binding. could cache this bound
/// types at some point*
/// TODO: `optional_chain`
#[allow(clippy::too_many_arguments)]
pub(crate) fn get_property<E: CallCheckingBehavior>(
	on: TypeId,
	publicity: Publicity,
	under: &PropertyKey,
	with: Option<TypeId>,
	top_environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
	position: SpanWithSource,
	bind_this: bool,
) -> Option<(PropertyKind, TypeId)> {
	if on == TypeId::ERROR_TYPE
		|| matches!(under, PropertyKey::Type(under) if *under == TypeId::ERROR_TYPE)
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
			bind_this,
		)
	} else if top_environment.possibly_mutated_objects.contains(&on) {
		let Some(constraint) = top_environment.get_object_constraint(on) else {
			todo!("mutated property inference")
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
			bind_this,
		)
	} else {
		// if environment.get_poly_base(under, types).is_some() {
		// 	todo!()
		// }
		// TODO
		get_from_an_object(on, publicity, under, top_environment, behavior, types, bind_this)
	}
}

fn get_from_an_object<E: CallCheckingBehavior>(
	on: TypeId,
	publicity: Publicity,
	under: &PropertyKey,
	environment: &mut Environment,
	behavior: &mut E,
	types: &mut TypeStore,
	bind_this: bool,
) -> Option<(PropertyKind, TypeId)> {
	/// Generates closure arguments, values of this and more. Runs getters
	fn resolve_property_on_logical<E: CallCheckingBehavior>(
		logical: Logical<PropertyValue>,
		on: TypeId,
		generics: GenericChain,
		environment: &mut Environment,
		types: &mut TypeStore,
		behavior: &mut E,
		bind_this: bool,
	) -> Option<(PropertyKind, TypeId)> {
		match logical {
			Logical::Pure(property) => {
				match property {
					PropertyValue::Value(value) => {
						let ty = types.get_type_by_id(value);
						match ty {
							Type::SpecialObject(SpecialObjects::Function(func, _state)) => {
								let this_value = if bind_this {
									ThisValue::Passed(on)
								} else {
									ThisValue::UseParent
								};
								let func = types.register_type(Type::SpecialObject(
									SpecialObjects::Function(*func, this_value),
								));

								Some((PropertyKind::Direct, func))
							}
							Type::FunctionReference(_) => {
								let ty = if let Some(arguments) =
									generics.and_then(super::GenericChainLink::get_value).cloned()
								{
									// assert!(chain.parent.is_none());
									types.register_type(Type::PartiallyAppliedGenerics(
										PartiallyAppliedGenerics { on: value, arguments },
									))
								} else {
									value
								};

								Some((PropertyKind::Direct, ty))
							}
							// TODO if uses generics
							Type::SpecialObject(..)
							| Type::Object(..)
							| Type::RootPolyType { .. }
							| Type::Constant(..) => Some((PropertyKind::Direct, value)),
							Type::Class { .. }
							| Type::Interface { .. }
							| Type::And(_, _)
							| Type::Or(_, _) => {
								crate::utilities::notify!(
								    "property was {:?} {:?}, which should be NOT be able to be returned from a function",
								    property, ty
							    );

								let value = types.register_type(Type::RootPolyType(
									crate::types::PolyNature::Open(value),
								));
								Some((PropertyKind::Direct, value))
							}
							// For closures
							Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
								on: sg_on,
								arguments,
							}) => {
								// TODO not great... need less overhead
								if let Type::SpecialObject(SpecialObjects::Function(f, _p)) =
									types.get_type_by_id(*sg_on)
								{
									let arguments = arguments.clone();
									let f = types.register_type(Type::SpecialObject(
										SpecialObjects::Function(*f, ThisValue::Passed(on)),
									));
									let ty = types.register_type(Type::PartiallyAppliedGenerics(
										PartiallyAppliedGenerics { on: f, arguments },
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
							Ok(res) => {
								let application_result = calling::application_result_to_return_type(
									res.result,
									environment,
									types,
								);
								Some((PropertyKind::Getter, application_result))
							}
							Err(_) => {
								todo!()
							}
						}
					}
					PropertyValue::Setter(_) => todo!(),
					PropertyValue::Deleted => None,
					PropertyValue::ConditionallyExists { on, truthy } => {
						if let PropertyValue::Value(value) = *truthy {
							let value =
								types.new_conditional_type(on, value, TypeId::UNDEFINED_TYPE);
							Some((PropertyKind::Direct, value))
						} else {
							todo!()
						}
					}
				}
			}
			Logical::Or { left, right, condition: based_on } => left
				.map(|l| {
					resolve_property_on_logical(
						l,
						based_on,
						None,
						environment,
						types,
						behavior,
						bind_this,
					)
				})
				.or_else(|_| {
					right.map(|r| {
						resolve_property_on_logical(
							r,
							based_on,
							None,
							environment,
							types,
							behavior,
							bind_this,
						)
					})
				})
				.ok()
				.flatten(),
			Logical::Implies { on: log_on, antecedent } => {
				let generics = GenericChainLink::append(
					TypeId::UNIMPLEMENTED_ERROR_TYPE,
					generics.as_ref(),
					&antecedent,
				);
				resolve_property_on_logical(
					*log_on,
					on,
					generics,
					environment,
					types,
					behavior,
					bind_this,
				)
			}
		}
	}

	// TODO explain what happens around non constant strings
	// TODO this breaks for conditionals maybe #120 fixes it?
	if let Type::Constant(Constant::String(s)) = types.get_type_by_id(on) {
		if let Some(n) = under.as_number(types) {
			return s.chars().nth(n).map(|s| {
				(PropertyKind::Direct, types.new_constant_type(Constant::String(s.to_string())))
			});
		}
	}

	let result = get_property_unbound((on, None), (publicity, under, None), environment, types);

	match result {
		Ok(logical) => {
			resolve_property_on_logical(logical, on, None, environment, types, behavior, bind_this)
		}
		Err(err) => match err {
			crate::context::MissingOrToCalculate::Missing => None,
			crate::context::MissingOrToCalculate::Error => {
				// Don't return none because that will raise error!
				Some((PropertyKind::Direct, TypeId::ERROR_TYPE))
			}
			// Can get through set prototype..?
			crate::context::MissingOrToCalculate::Infer { .. } => {
				crate::utilities::notify!("TODO set infer");
				Some((PropertyKind::Direct, TypeId::ERROR_TYPE))
			}
			crate::context::MissingOrToCalculate::Proxy(Proxy { .. }) => {
				todo!(); // #33
				 // TODO pass down
			}
		},
	}
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
	bind_this: bool,
) -> Option<(PropertyKind, TypeId)> {
	fn resolve_logical_with_poly(
		fact: Logical<PropertyValue>,
		on: TypeId,
		under: PropertyKey,
		// TODO generic chain
		arguments: Option<&GenericArguments>,
		environment: &mut Environment,
		types: &mut TypeStore,
		bind_this: bool,
	) -> Option<TypeId> {
		match fact {
			Logical::Pure(og) => {
				Some(match og {
					PropertyValue::Value(value) => match types.get_type_by_id(value) {
						t @ (Type::And(_, _)
						| Type::Or(_, _)
						| Type::RootPolyType(_)
						| Type::Constructor(_)) => {
							let result = if let Some(arguments) = arguments {
								let arguments = arguments.into_substitutable();
								substitute(value, &arguments, environment, types)
							} else {
								crate::utilities::notify!("Here, getting property on {:?}", t);
								value
							};

							types.register_type(Type::Constructor(Constructor::Property {
								on,
								under: under.into_owned(),
								result,
								bind_this,
							}))
						}
						// Don't need to set this here. It is picked up from `on` during lookup
						Type::SpecialObject(SpecialObjects::Function(..))
						| Type::FunctionReference(..)
						| Type::AliasTo { .. }
						| Type::Object(ObjectNature::AnonymousTypeAnnotation)
						| Type::Interface { .. }
						| Type::Class { .. } => types.register_type(Type::Constructor(Constructor::Property {
							on,
							under: under.into_owned(),
							result: value,
							bind_this,
						})),
						Type::Constant(_)
						| Type::PartiallyAppliedGenerics(_)
						| Type::Object(ObjectNature::RealDeal)
						| Type::SpecialObject(..) => value,
					},
					PropertyValue::Getter(getter) => {
						// if is_open_poly {
						// 	crate::utilities::notify!("TODO evaluate getter...");
						// } else {
						// 	crate::utilities::notify!("TODO don't evaluate getter");
						// }
						// TODO : getter.return_type
						types.register_type(Type::Constructor(Constructor::Property {
							on,
							under: under.into_owned(),
							result: getter.return_type,
							bind_this: false,
						}))
					}
					PropertyValue::Setter(_) => todo!(),
					// Very important
					PropertyValue::Deleted => return None,
					PropertyValue::ConditionallyExists { on, truthy } => {
						// TODO can only get if optional ?
						if let PropertyValue::Value(value) = *truthy {
							types.new_conditional_type(on, value, TypeId::UNDEFINED_TYPE)
						} else {
							todo!()
						}
					}
				})
			}
			Logical::Or { condition: based_on, left, right } => {
				// let left = resolve_logical_with_poly(
				// 	*left,
				// 	on,
				// 	under.clone(),
				// 	arguments,
				// 	environment,
				// 	types,
				// );
				// let right =
				// 	resolve_logical_with_poly(*right, on, under, arguments, environment, types);

				// crate::utilities::notify!("lr = {:?}", (left, right));

				// TODO lots of information (and inference) lost here
				if let (Ok(lhs), Ok(rhs)) = (*left, *right) {
					let lhs = resolve_logical_with_poly(
						lhs,
						on,
						under.clone(),
						arguments,
						environment,
						types,
						bind_this,
					)?;
					let rhs = resolve_logical_with_poly(
						rhs,
						on,
						under,
						arguments,
						environment,
						types,
						bind_this,
					)?;
					Some(types.new_conditional_type(based_on, lhs, rhs))
				} else {
					crate::utilities::notify!("TODO emit some diagnostic about missing");
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
					bind_this,
				)
			}
		}
	}

	let fact =
		get_property_unbound((on, None), (publicity, &under, None), top_environment, types).ok()?;

	crate::utilities::notify!("unbound is is {:?}", fact);

	let value = resolve_logical_with_poly(
		fact,
		on,
		under.clone(),
		None,
		top_environment,
		types,
		bind_this,
	)?;

	behavior.get_latest_info(top_environment).events.push(Event::Getter {
		on,
		under: under.into_owned(),
		reflects_dependency: Some(value),
		publicity,
		position,
		bind_this,
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
	setter_position: SpanWithSource,
) -> Result<Option<TypeId>, SetPropertyError> {
	// TODO
	// if environment.is_not_writeable(on, under) {
	// 	return Err(SetPropertyError::NotWriteable);
	// }

	// if E::CHECK_PARAMETERS {
	if let Some(constraint) = environment.get_object_constraint(on) {
		let property_constraint =
			get_property_unbound((constraint, None), (publicity, under, None), environment, types);

		// crate::utilities::notify!("Property constraint .is_some() {:?}", property_constraint.is_some());

		// crate::utilities::notify!(
		// 	"Re-assignment constraint {}, prop={} {:?}",
		// 	print_type(constraint, types, environment, true),
		// 	print_type(under, types, environment, true),
		// 	property_constraint
		// );

		if let Ok(property_constraint) = property_constraint {
			// TODO ...?
			let mut state = State {
				already_checked: Default::default(),
				mode: Default::default(),
				contributions: Default::default(),
				others: Default::default(),
				object_constraints: Default::default(),
			};

			// TODO property value is readonly
			// TODO state is writeable etc?
			// TODO document difference with context.writeable
			match &property_constraint {
				Logical::Pure(PropertyValue::Value(p)) => {
					if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
						on: TypeId::READONLY_RESTRICTION,
						..
					}) = types.get_type_by_id(*p)
					{
						return Err(SetPropertyError::NotWriteable);
					}
				}
				Logical::Pure(_) => {}
				Logical::Or { .. } => todo!(),
				Logical::Implies { .. } => todo!(),
			}

			match new {
				PropertyValue::Value(value) => {
					let result = crate::subtyping::type_is_subtype_of_property(
						(&property_constraint, None),
						value,
						&mut state,
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
				PropertyValue::ConditionallyExists { .. } => todo!(),
			}

		// environment
		// 	.add_object_constraints(basic_subtyping.object_constraints.into_iter(), types);

		// environment.context_type.requests.append(todo!().parameter_constraint_request);
		} else {
			// TODO does not exist warning
			// return Err(SetPropertyError::DoesNotMeetConstraint(
			// 	new.as_get_type(),
			// 	todo!("no property"),
			// ));
		}
	}

	// crate::utilities::notify!(
	// 	"setting {:?} {:?} {:?}",
	// 	crate::types::printing::print_type(types, on, environment, true),
	// 	crate::types::printing::print_type(types, under, environment, true),
	// 	crate::types::printing::print_type(types, new.as_get_type(), environment, true),
	// );

	let current_property =
		get_property_unbound((on, None), (publicity, under, None), environment, types);

	// crate::utilities::notify!("(2) Made it here assigning to {:?}", types.get_type_by_id(on));

	// Cascade if it is a union (unsure tho)
	if let Type::Constructor(Constructor::ConditionalResult {
		truthy_result,
		otherwise_result,
		condition: _,
		result_union: _,
	}) = types.get_type_by_id(on)
	{
		let truthy = *truthy_result;
		let otherwise_result = *otherwise_result;
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
			otherwise_result,
			publicity,
			under,
			new,
			environment,
			behavior,
			types,
			setter_position,
		);
	}

	if let Ok(fact) = current_property { match fact {
		Logical::Pure(og) => { let result =
		run_setter_on_object( og, behavior, environment, on,
		publicity, under, new, types, setter_position, ); if
		let Err(result) = result {
					// TODO temp
					for error in result {
						match error {
							FunctionCallingError::InvalidArgumentType {
								parameter_type,
								argument_type: _,
								argument_position: _,
								parameter_position: _,
								restriction: _,
							} => {
								return Err(SetPropertyError::DoesNotMeetConstraint {
									property_constraint: parameter_type,
									reason: crate::subtyping::NonEqualityReason::Mismatch,
								})
							}
							FunctionCallingError::NeedsToBeCalledWithNewKeyword(_)
							| FunctionCallingError::NoLogicForIdentifier(..)
							| FunctionCallingError::NotCallable { .. }
							| FunctionCallingError::ExcessArguments { .. }
							| FunctionCallingError::ExcessTypeArguments { .. }
							| FunctionCallingError::MissingArgument { .. } => unreachable!(),
							FunctionCallingError::ReferenceRestrictionDoesNotMatch { .. } => {
								todo!()
							}
							FunctionCallingError::CyclicRecursion(_, _) => todo!(),
							FunctionCallingError::TDZ { .. } => todo!(),
							FunctionCallingError::SetPropertyConstraint { .. } => todo!(),
							FunctionCallingError::UnconditionalThrow { .. } => {
								todo!()
							}
							FunctionCallingError::MismatchedThis { .. } => {
								todo!()
							}
							FunctionCallingError::CannotCatch { .. } => todo!(),
						}
					}
				}
			}
			Logical::Or { .. } => todo!(),
			Logical::Implies { on: _implies_on, antecedent: _ } => {
				crate::utilities::notify!("Check that `implies_on` could be a setter here");
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

/// `Vec<FunctionCallingError>` from calling setter
#[allow(clippy::too_many_arguments)]
fn run_setter_on_object<E: CallCheckingBehavior>(
	og: PropertyValue,
	behavior: &mut E,
	environment: &mut Environment,
	on: TypeId,
	publicity: Publicity,
	under: &PropertyKey<'_>,
	new: PropertyValue,
	types: &mut TypeStore,
	setter_position: SpanWithSource,
) -> Result<(), Vec<FunctionCallingError>> {
	match og {
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

			Ok(())
		}
		PropertyValue::Getter(_) => todo!(),
		PropertyValue::Setter(setter) => {
			let arg = SynthesisedArgument {
				position: setter_position,
				spread: false,
				value: match new {
					PropertyValue::Value(type_id) => type_id,
					_ => todo!(),
				},
			};
			let result = setter.call(
				CalledWithNew::None,
				ThisValue::Passed(on),
				setter_position,
				&[arg],
				None,
				// TODO structure generics
				None,
				environment,
				behavior,
				types,
				false,
			);

			match result {
				// Ignore the result
				Ok(_ok) => Ok(()),
				Err(res) => Err(res.errors),
			}
		}
		PropertyValue::ConditionallyExists { .. } => todo!(),
	}
}

pub(crate) fn get_property_unbound(
	(on, on_type_arguments): (TypeId, GenericChain),
	(publicity, under, under_type_arguments): (Publicity, &PropertyKey, GenericChain),
	info_chain: &impl InformationChain,
	types: &TypeStore,
) -> PossibleLogical<PropertyValue> {
	/// Has to return `Logical` for mapped types
	fn resolver(
		(on, on_type_arguments): (TypeId, GenericChain),
		(publicity, under, under_type_arguments): (Publicity, &PropertyKey, GenericChain),
		info: &LocalInformation,
		types: &TypeStore,
	) -> Option<Logical<PropertyValue>> {
		// TODO if on == constant string and property == length. Need to be able to create types here

		info.current_properties.get(&on).and_then(|properties_on_on| {
			{
				let (on_properties, on_type_arguments) = (properties_on_on, on_type_arguments);
				let (required_publicity, want_key, want_type_arguments) =
					(publicity, under, under_type_arguments);

				// TODO conditional for conditional?
				// let _acc = ();

				// 'rev' is important
				for (publicity, key, value) in on_properties.iter().rev() {
					if *publicity != required_publicity {
						continue;
					}

					let key_matches = key_matches(
						(key, on_type_arguments),
						(want_key, want_type_arguments),
						types,
					);
					if key_matches {
						// TODO if conditional then continue to find then logical or
						let pure = Logical::Pure(value.clone());
						return Some(if let Some((_parameter, _)) = key.mapped_generic_id(types) {
							// Logical::Implies {
							// 	on: Box::new(pure),
							// 	antecedent: GenericArguments::ExplicitRestrictions(todo!(
							// 		"want TypeArgument::PropertyKey 😀"
							// 	)),
							// }
							crate::utilities::notify!(
								"TODO to set property key as a type argument?"
							);
							pure
						} else {
							pure
						});
					}
				}

				None
			}
		})
	}

	if on == TypeId::ERROR_TYPE {
		return Err(crate::context::MissingOrToCalculate::Error);
	}
	if on == TypeId::ANY_TYPE {
		// TODO any
		return Err(crate::context::MissingOrToCalculate::Infer { on });
	}

	match types.get_type_by_id(on) {
		Type::SpecialObject(SpecialObjects::Function(function_id, _)) => info_chain
			.get_chain_of_info()
			.find_map(|info| {
				resolver(
					(on, on_type_arguments),
					(publicity, under, under_type_arguments),
					info,
					types,
				)
				.or_else(|| {
					if let (true, FunctionBehavior::Function { prototype, .. }) = (
						under.is_equal_to("prototype"),
						&types.get_function_from_id(*function_id).behavior,
					) {
						Some(Logical::Pure(PropertyValue::Value(*prototype)))
					} else {
						None
					}
				})
				.or_else(|| {
					resolver(
						(TypeId::FUNCTION_TYPE, on_type_arguments),
						(publicity, under, under_type_arguments),
						info,
						types,
					)
				})
			})
			.ok_or(crate::context::MissingOrToCalculate::Missing),
		Type::FunctionReference(_) => info_chain
			.get_chain_of_info()
			.find_map(|info| {
				resolver(
					(TypeId::FUNCTION_TYPE, on_type_arguments),
					(publicity, under, under_type_arguments),
					info,
					types,
				)
			})
			.ok_or(crate::context::MissingOrToCalculate::Missing),
		Type::AliasTo { to, .. } => {
			get_property_unbound(
				(*to, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
			// TODO why would an alias have a property
			// let property_on_types = info_chain
			// 	.get_chain_of_info()
			// 	.find_map(|info| resolver(info, types, on, on_type_arguments,))
		}
		Type::And(left, right) => get_property_unbound(
			(*left, on_type_arguments),
			(publicity, under, under_type_arguments),
			info_chain,
			types,
		)
		.or_else(|_| {
			get_property_unbound(
				(*right, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			)
		}),
		Type::Or(left, right) => {
			let left = get_property_unbound(
				(*left, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			);
			let right = get_property_unbound(
				(*right, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			);

			// TODO throwaway if both Missing::None

			Ok(Logical::Or {
				condition: TypeId::BOOLEAN_TYPE,
				left: Box::new(left),
				right: Box::new(right),
			})
		}
		Type::RootPolyType(_nature) => {
			// Can assign to properties on parameters etc
			let aliases = get_constraint(on, types).expect("poly type with no constraint");

			info_chain
				.get_chain_of_info()
				.find_map(|info| {
					resolver(
						(on, on_type_arguments),
						(publicity, under, under_type_arguments),
						info,
						types,
					)
				})
				.ok_or(crate::context::MissingOrToCalculate::Missing)
				.or_else(|_| {
					get_property_unbound(
						(aliases, on_type_arguments),
						(publicity, under, under_type_arguments),
						info_chain,
						types,
					)
				})
		}
		Type::PartiallyAppliedGenerics(crate::types::PartiallyAppliedGenerics {
			on: base,
			arguments,
		}) => {
			let on_sg_type = if let GenericArguments::Closure(_) = arguments {
				info_chain
					.get_chain_of_info()
					.find_map(|info| {
						resolver(
							(on, on_type_arguments),
							(publicity, under, under_type_arguments),
							info,
							types,
						)
					})
					.ok_or(crate::context::MissingOrToCalculate::Missing)
			} else {
				Err(crate::context::MissingOrToCalculate::Missing)
			};

			on_sg_type.or_else(|_| {
				let on_type_arguments = crate::types::GenericChainLink::append(
					on,
					on_type_arguments.as_ref(),
					arguments,
				);

				crate::utilities::notify!("{:?}", on_type_arguments);

				get_property_unbound(
					(*base, on_type_arguments),
					(publicity, under, under_type_arguments),
					info_chain,
					types,
				)
				.map(|fact| Logical::Implies { on: Box::new(fact), antecedent: arguments.clone() })
			})
		}
		Type::Constructor(crate::types::Constructor::ConditionalResult {
			condition,
			truthy_result,
			otherwise_result,
			result_union: _,
		}) => {
			let left = get_property_unbound(
				(*truthy_result, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			);
			let right = get_property_unbound(
				(*otherwise_result, on_type_arguments),
				(publicity, under, under_type_arguments),
				info_chain,
				types,
			);

			// TODO throwaway if both Missing::None

			Ok(Logical::Or { condition: *condition, left: Box::new(left), right: Box::new(right) })
		}
		Type::Constructor(_constructor) => {
			let on_constructor_type = info_chain
				.get_chain_of_info()
				.find_map(|info| {
					resolver(
						(on, on_type_arguments),
						(publicity, under, under_type_arguments),
						info,
						types,
					)
				})
				.ok_or(crate::context::MissingOrToCalculate::Missing);

			let aliases = get_constraint(on, types).expect("no constraint for constructor");

			on_constructor_type.or_else(|_| {
				get_property_unbound(
					(aliases, on_type_arguments),
					(publicity, under, under_type_arguments),
					info_chain,
					types,
				)
			})
		}
		Type::Object(..) => {
			let object_constraint_structure_generics =
				crate::types::get_structure_arguments_based_on_object_constraint(
					on, info_chain, types,
				);

			let prototype =
				info_chain.get_chain_of_info().find_map(|facts| facts.prototypes.get(&on)).copied();

			let generics = if let Some(generics) = object_constraint_structure_generics {
				// TODO clone
				Some(generics.clone())
			} else if prototype
				.is_some_and(|prototype| types.lookup_generic_map.contains_key(&prototype))
			{
				Some(GenericArguments::LookUp { on })
			} else {
				None
			};

			info_chain
				.get_chain_of_info()
				.find_map(|info| {
					let on_self = resolver(
						(on, on_type_arguments),
						(publicity, under, under_type_arguments),
						info,
						types,
					);

					let result = if let (Some(prototype), None) = (prototype, &on_self) {
						resolver(
							(prototype, on_type_arguments),
							(publicity, under, under_type_arguments),
							info,
							types,
						)
					} else {
						on_self
					};

					result.map(|result| {
						if let Some(ref generics) = generics {
							// TODO clone
							Logical::Implies { on: Box::new(result), antecedent: generics.clone() }
						} else {
							result
						}
					})
				})
				.ok_or(crate::context::MissingOrToCalculate::Missing)
		}
		Type::Interface { .. } => info_chain
			.get_chain_of_info()
			.find_map(|env| {
				resolver(
					(on, on_type_arguments),
					(publicity, under, under_type_arguments),
					env,
					types,
				)
			})
			.ok_or(crate::context::MissingOrToCalculate::Missing)
			.or_else(|_| {
				// TODO class and class constructor extends etc
				if let Some(extends) = types.interface_extends.get(&on) {
					get_property_unbound(
						(*extends, on_type_arguments),
						(publicity, under, under_type_arguments),
						info_chain,
						types,
					)
				} else {
					Err(crate::context::MissingOrToCalculate::Missing)
				}
			}),
		Type::SpecialObject(SpecialObjects::ClassConstructor { .. }) | Type::Class { .. } => {
			info_chain
				.get_chain_of_info()
				.find_map(|env| {
					resolver(
						(on, on_type_arguments),
						(publicity, under, under_type_arguments),
						env,
						types,
					)
				})
				.ok_or(crate::context::MissingOrToCalculate::Missing)
				.or_else(|_| {
					if let Some(prototype) =
						info_chain.get_chain_of_info().find_map(|info| info.prototypes.get(&on))
					{
						get_property_unbound(
							(*prototype, on_type_arguments),
							(publicity, under, under_type_arguments),
							info_chain,
							types,
						)
					} else {
						Err(crate::context::MissingOrToCalculate::Missing)
					}
				})
		}
		Type::Constant(cst) => info_chain
			.get_chain_of_info()
			.find_map(|env| {
				resolver(
					(on, on_type_arguments),
					(publicity, under, under_type_arguments),
					env,
					types,
				)
			})
			.ok_or(crate::context::MissingOrToCalculate::Missing)
			.or_else(|_| {
				let backing_type = cst.get_backing_type_id();
				get_property_unbound(
					(backing_type, on_type_arguments),
					(publicity, under, under_type_arguments),
					info_chain,
					types,
				)
			}),
		Type::SpecialObject(SpecialObjects::Promise { .. }) => {
			todo!()
		}
		Type::SpecialObject(SpecialObjects::Import(..)) => {
			todo!()
		}
		Type::SpecialObject(SpecialObjects::Proxy(proxy)) => {
			Err(crate::context::MissingOrToCalculate::Proxy(*proxy))
		}
		Type::SpecialObject(SpecialObjects::Generator { .. }) => {
			todo!()
		}
		Type::SpecialObject(SpecialObjects::RegularExpression(..)) => {
			todo!()
		}
	}
}

/// Does lhs equal want
/// Aka is `want_key in { [lhs_key]: ... }`
pub(crate) fn key_matches(
	(key, key_type_arguments): (&PropertyKey<'_>, GenericChain),
	(want, want_type_arguments): (&PropertyKey<'_>, GenericChain),
	types: &TypeStore,
) -> bool {
	// crate::utilities::notify!(
	// 	"Key equality: have {:?} want {:?}",
	// 	(key, key_type_arguments),
	// 	(want, want_type_arguments)
	// );

	match (key, want) {
		(PropertyKey::String(left), PropertyKey::String(right)) => left == right,
		(PropertyKey::String(s), PropertyKey::Type(want)) => {
			if let Some(substituted_key) =
				want_type_arguments.and_then(|args| args.get_single_argument(*want))
			{
				crate::utilities::notify!("Here");
				return key_matches(
					(key, key_type_arguments),
					(&PropertyKey::Type(substituted_key), want_type_arguments),
					types,
				);
			}
			let want_ty = types.get_type_by_id(*want);
			crate::utilities::notify!("{:?} key_ty={:?}", s, want_ty);
			if let Type::Or(lhs, rhs) = want_ty {
				key_matches(
					(key, key_type_arguments),
					(&PropertyKey::Type(*lhs), key_type_arguments),
					types,
				) || key_matches(
					(key, key_type_arguments),
					(&PropertyKey::Type(*rhs), key_type_arguments),
					types,
				)
			} else if let Type::RootPolyType(PolyNature::MappedGeneric {
				eager_fixed: to, ..
			})
			| Type::AliasTo { to, .. } = want_ty
			{
				key_matches(
					(key, key_type_arguments),
					(&PropertyKey::Type(*to), want_type_arguments),
					types,
				)
			} else if let Type::Constant(c) = want_ty {
				crate::utilities::notify!("{:?}", c);
				// TODO
				*s == c.as_js_string()
			} else {
				false
			}
		}
		(PropertyKey::Type(key), PropertyKey::String(s)) => {
			if let Some(substituted_key) =
				key_type_arguments.and_then(|args| args.get_single_argument(*key))
			{
				return key_matches(
					(&PropertyKey::Type(substituted_key), key_type_arguments),
					(want, want_type_arguments),
					types,
				);
			}

			let key = *key;
			let key_type = types.get_type_by_id(key);

			if let Type::RootPolyType(PolyNature::MappedGeneric { eager_fixed: to, .. }) = key_type
			{
				crate::utilities::notify!("Special behavior?");
				return key_matches(
					(&PropertyKey::Type(*to), key_type_arguments),
					(want, want_type_arguments),
					types,
				);
			}

			if let Type::AliasTo { to, .. } = key_type {
				return key_matches(
					(&PropertyKey::Type(*to), key_type_arguments),
					(want, want_type_arguments),
					types,
				);
			}

			if let Type::Or(l, r) = key_type {
				return key_matches(
					(&PropertyKey::Type(*l), key_type_arguments),
					(want, want_type_arguments),
					types,
				) || key_matches(
					(&PropertyKey::Type(*r), key_type_arguments),
					(want, want_type_arguments),
					types,
				);
			}

			// TODO WIP
			if key == TypeId::ANY_TYPE {
				true
			} else if let Type::Constant(Constant::String(ks)) = key_type {
				ks == s
			} else if key == TypeId::BOOLEAN_TYPE {
				s == "true" || s == "false"
			} else if key == TypeId::NUMBER_TYPE {
				s.parse::<usize>().is_ok()
			} else if key == TypeId::STRING_TYPE {
				s.parse::<usize>().is_err()
			} else {
				false
			}
		}
		(PropertyKey::Type(left), PropertyKey::Type(right)) => {
			crate::utilities::notify!(
				"{:?} {:?}",
				types.get_type_by_id(*left),
				types.get_type_by_id(*right)
			);
			// TODO subtyping
			*left == get_larger_type(*right, types)
		}
	}
}

pub type Properties = Vec<(Publicity, PropertyKey<'static>, PropertyValue)>;

/// Get properties on a type (for printing and other non-one property uses)
///
/// - TODO prototypes
/// - TODO could this be an iterator
/// - TODO return whether it is fixed
/// - TODO doesn't evaluate properties
pub fn get_properties_on_single_type(
	base: TypeId,
	types: &TypeStore,
	info: &impl InformationChain,
) -> Properties {
	match types.get_type_by_id(base) {
		Type::Interface { .. } | Type::Class { .. } | Type::Object(_) => {
			let reversed_flattened_properties = info
				.get_chain_of_info()
				.filter_map(|info| info.current_properties.get(&base).map(|v| v.iter().rev()))
				.flatten();

			let mut deleted_or_existing_properties =
				std::collections::HashSet::<PropertyKey>::new();

			// This retains ordering here

			let mut properties = Vec::new();
			for (publicity, key, prop) in reversed_flattened_properties {
				if let PropertyValue::Deleted = prop {
					// TODO doesn't cover constants :(
					deleted_or_existing_properties.insert(key.clone());
				} else if deleted_or_existing_properties.insert(key.clone()) {
					properties.push((*publicity, key.to_owned(), prop.clone()));
				}
			}

			properties.reverse();
			properties
		}
		t @ (Type::SpecialObject(_)
		| Type::Constructor(_)
		| Type::RootPolyType(_)
		| Type::Or(..)
		| Type::PartiallyAppliedGenerics(_)
		| Type::Constant(_)
		| Type::AliasTo { .. }
		| Type::FunctionReference(_)
		| Type::And(_, _)) => panic!("Cannot get all properties on {t:?}"),
	}
}


pub fn get_property_as_string(property: &PropertyKey, types: &mut TypeStore, environment: &mut Environment) -> String {

    match property {
	PropertyKey::String(s) => return s.to_string(),
	PropertyKey::Type(t) => return printing::print_type(
	    *t,
	    types,
	    environment,
	    false,
	),
    }
}

pub fn get_property_key_names_on_a_single_type<'a>(
	base: TypeId,
	types: &mut TypeStore,
    info: &impl InformationChain,
    environment: &mut Environment
) -> Vec<String> {
    
    get_properties_on_single_type(base, types, info).into_iter().map(|property| {
	get_property_as_string(&property.1, types, environment)
    }
    ).collect()
}
