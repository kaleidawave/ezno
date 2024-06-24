mod access;
mod assignment;

pub use access::*;
pub use assignment::set_property;

use super::{Type, TypeStore};
use crate::{
	types::{get_larger_type, FunctionType, GenericChain, PolyNature},
	Constant, TypeId,
};
use std::borrow::Cow;

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
				Constant::Symbol { .. } => {
					// Okay I think?
					PropertyKey::Type(ty)
				}
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
			// crate::utilities::notify!("{:?} key_ty={:?}", s, want_ty);
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
			// crate::utilities::notify!(
			// 	"{:?} {:?}",
			// 	types.get_type_by_id(*left),
			// 	types.get_type_by_id(*right)
			// );
			// TODO subtyping
			*left == *right || *left == get_larger_type(*right, types)
		}
	}
}
