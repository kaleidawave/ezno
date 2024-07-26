mod access;
mod assignment;

pub use access::*;
pub use assignment::set_property;

use super::{Type, TypeStore};
use crate::{
	context::information::InformationChain,
	subtyping::{slice_matches_type, SliceArguments, SubTypingOptions},
	types::{
		calling::Callable, generics::contributions::Contributions, logical::*, properties,
		GenericChain, PolyNature,
	},
	Constant, Environment, FunctionId, TypeId,
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
	/// Use [`PropertyKey::from_type`] for canonicalisation (generating [`PropertyKey::String`] when possible)
	Type(TypeId),
	// SomeThingLike(TypeId),
}

impl From<&'static str> for PropertyKey<'static> {
	fn from(value: &'static str) -> Self {
		Self::String(Cow::Borrowed(value))
	}
}

// Cannot auto derive BinarySerializable because lifetime
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

	pub fn into_name_type(&self, types: &mut TypeStore) -> TypeId {
		match self {
			PropertyKey::String(s) => {
				types.new_constant_type(Constant::String(s.clone().into_owned()))
			}
			PropertyKey::Type(t) => {
				crate::utilities::notify!("TODO Symbol has different printing here");
				*t
			}
		}
	}

	pub(crate) fn substitute(
		&self,
		type_arguments: &super::SubstitutionArguments,
		top_environment: &Environment,
		types: &mut TypeStore,
	) -> Self {
		match self {
			PropertyKey::Type(under) => {
				let ty = super::substitute(*under, type_arguments, top_environment, types);
				PropertyKey::from_type(ty, types)
			}
			under @ PropertyKey::String(_) => under.clone(),
		}
	}
}

// WIP quick hack for static property keys under < 10
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

/// TODO getter, setter need a closure id (or implement using `Callable::Type`)
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum PropertyValue {
	Value(TypeId),
	/// These need to be [`Callable`], because the getter can be a type via `Object.defineProperty`
	Getter(Callable),
	/// These need to be [`Callable`], because the getter can be a type via `Object.defineProperty`
	Setter(Callable),
	/// TODO this could have a bit more information. Whether it came from a `delete` or from TS Optional annotation
	Deleted,
	/// This is the solution for ors. Can be a getter added, a value removed.
	ConditionallyExists {
		/// If `condition == TypeId::TRUE` then truthy is the value
		condition: TypeId,
		/// Should be always Value | Getter | Setter | Deleted
		truthy: Box<Self>,
	},
	/// TODO while this works it does mean that there can be long linked lists via Box<Self> (I think)
	Configured {
		/// This points to the previous. Can be conditionally via [`PropertyValue::ConditionallyExists`]
		on: Box<Self>,
		descriptor: Descriptor,
	},
}

/// TODO link to MDN
#[derive(Copy, Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct Descriptor {
	pub writable: TypeId,
	pub enumerable: TypeId,
	pub configurable: TypeId,
}

// The default descriptor for 'a' for `{ a: 2 }`
impl Default for Descriptor {
	fn default() -> Self {
		Self { writable: TypeId::TRUE, enumerable: TypeId::TRUE, configurable: TypeId::TRUE }
	}
}

impl PropertyValue {
	/// TODO wip
	#[must_use]
	pub fn as_get_type(&self) -> TypeId {
		match self {
			PropertyValue::Value(value) => *value,
			PropertyValue::Getter(_getter) => {
				crate::utilities::notify!("Here. Should pass down types");
				TypeId::ERROR_TYPE
			}
			// TODO unsure about these two
			PropertyValue::Setter(_) => TypeId::UNDEFINED_TYPE,
			PropertyValue::Deleted => TypeId::NEVER_TYPE,
			PropertyValue::ConditionallyExists { truthy, .. } => {
				// TODO temp
				truthy.as_get_type()
			}
			PropertyValue::Configured { on, .. } => on.as_get_type(),
		}
	}

	#[must_use]
	pub fn as_set_type(&self) -> TypeId {
		match self {
			PropertyValue::Value(value) => *value,
			PropertyValue::Setter(_setter) => {
				crate::utilities::notify!("Here. Should pass down types");
				TypeId::ERROR_TYPE
			}
			// TODO unsure about these two
			PropertyValue::Getter(_) => TypeId::UNDEFINED_TYPE,
			PropertyValue::Deleted => TypeId::NEVER_TYPE,
			PropertyValue::ConditionallyExists { truthy, .. } => {
				// TODO temp
				truthy.as_get_type()
			}
			PropertyValue::Configured { on, .. } => on.as_get_type(),
		}
	}

	// For printing and debugging
	pub fn inner_simple(&self) -> &Self {
		if let PropertyValue::ConditionallyExists { truthy: on, .. }
		| PropertyValue::Configured { on, descriptor: _ } = self
		{
			on.inner_simple()
		} else {
			self
		}
	}

	// For printing and debugging
	pub fn is_optional_simple(&self) -> bool {
		if let PropertyValue::ConditionallyExists { condition, truthy: _ } = self {
			// crate::utilities::notify!("condition={:?}", *condition);
			!matches!(*condition, TypeId::NON_OPTIONAL_KEY_ARGUMENT)
		} else {
			false
		}
	}

	// For printing and debugging
	pub fn is_writable_simple(&self) -> bool {
		if let PropertyValue::ConditionallyExists { condition: _, truthy } = self {
			truthy.is_writable_simple()
		} else if let PropertyValue::Configured { on: _, descriptor } = self {
			!matches!(descriptor.writable, TypeId::TRUE | TypeId::WRITABLE_KEY_ARGUMENT)
		} else {
			false
		}
	}
}

/// Does lhs equal want. Aka is `want_key in { [lhs_key]: ... }`
///
/// This is very much like [`type_is_subtype`] but for [`PropertyKey`]
pub(crate) fn key_matches(
	(key, key_type_arguments): (&PropertyKey<'_>, GenericChain),
	(want, want_type_arguments): (&PropertyKey<'_>, GenericChain),
	info_chain: &impl InformationChain,
	types: &TypeStore,
) -> (bool, SliceArguments) {
	{
		// crate::utilities::notify!(
		// 	"{{ [{:?}({:?})]: ... }}[{:?}({:?})]",
		// 	key,
		// 	key_type_arguments,
		// 	want,
		// 	want_type_arguments
		// );
	}

	match (key, want) {
		(PropertyKey::String(left), PropertyKey::String(right)) => {
			let matches = if let Some(transform) =
				key_type_arguments.and_then(|a| a.get_string_transform())
			{
				super::intrinsics::apply_string_intrinsic(transform, left).as_str() == right
			} else {
				left == right
			};
			(matches, SliceArguments::default())
		}
		(PropertyKey::String(s), PropertyKey::Type(want)) => {
			if let Some(substituted_key) =
				want_type_arguments.and_then(|args| args.get_single_argument(*want))
			{
				key_matches(
					(key, key_type_arguments),
					(&PropertyKey::Type(substituted_key), want_type_arguments),
					info_chain,
					types,
				)
			} else {
				let want_ty = types.get_type_by_id(*want);
				// crate::utilities::notify!("{:?} key_ty={:?}", s, want_ty);
				if let Type::Or(lhs, rhs) = want_ty {
					// TODO
					let matches = key_matches(
						(key, key_type_arguments),
						(&PropertyKey::Type(*lhs), key_type_arguments),
						info_chain,
						types,
					)
					.0 || key_matches(
						(key, key_type_arguments),
						(&PropertyKey::Type(*rhs), key_type_arguments),
						info_chain,
						types,
					)
					.0;
					(matches, SliceArguments::default())
				} else if let Type::RootPolyType(PolyNature::MappedGeneric {
					eager_fixed: to,
					..
				})
				| Type::AliasTo { to, .. } = want_ty
				{
					key_matches(
						(key, key_type_arguments),
						(&PropertyKey::Type(*to), want_type_arguments),
						info_chain,
						types,
					)
				} else if let Type::Constructor(crate::types::Constructor::KeyOf(on)) = want_ty {
					let matches = access::get_properties_on_single_type2(
						(*on, want_type_arguments),
						types,
						info_chain,
						TypeId::ANY_TYPE,
					)
					.iter()
					.all(|(rhs_key, _, _)| {
						key_matches(
							(key, key_type_arguments),
							(rhs_key, want_type_arguments),
							info_chain,
							types,
						)
						.0
					});
					(matches, Default::default())
				} else if let Type::Constant(c) = want_ty {
					// crate::utilities::notify!("{:?}", c);
					// TODO
					(*s == c.as_js_string(), SliceArguments::default())
				} else if *want == TypeId::NUMBER_TYPE {
					(s.parse::<usize>().is_ok(), SliceArguments::default())
				} else if *want == TypeId::STRING_TYPE {
					(s.parse::<usize>().is_err(), SliceArguments::default())
				} else {
					(false, SliceArguments::default())
				}
			}
		}
		(PropertyKey::Type(key), PropertyKey::String(s)) => {
			// crate::utilities::notify!(
			// 	"Key equality: have {:?} want {:?}",
			// 	(key, key_type_arguments),
			// 	(want, want_type_arguments)
			// );

			if let Some(substituted_key) =
				key_type_arguments.and_then(|args| args.get_single_argument(*key))
			{
				key_matches(
					(&PropertyKey::Type(substituted_key), key_type_arguments),
					(want, want_type_arguments),
					info_chain,
					types,
				)
			} else {
				let key = *key;
				// First some special bases just for property keys
				let mut contributions = SliceArguments::default();
				let result = slice_matches_type(
					(key, key_type_arguments),
					s.as_ref(),
					Some(&mut contributions),
					info_chain,
					types,
					true,
				);
				(result, contributions)
			}
		}
		(PropertyKey::Type(left), PropertyKey::Type(right)) => {
			let mut state = crate::types::subtyping::State {
				already_checked: Default::default(),
				mode: Default::default(),
				contributions: Some(Contributions::default()),
				object_constraints: Default::default(),
				others: SubTypingOptions::default(),
			};
			let result = crate::types::subtyping::type_is_subtype(
				*left, *right, &mut state, info_chain, types,
			);

			let contributions = state.contributions.unwrap();
			crate::utilities::notify!(
				"Here contributions {:?}",
				&contributions.staging_contravariant
			);

			(result.is_subtype(), contributions.staging_contravariant)
		}
	}
}

/// WIP
pub(crate) fn has_property(
	(publicity, key): (properties::Publicity, &properties::PropertyKey<'_>),
	rhs: TypeId,
	information: &impl InformationChain,
	types: &mut TypeStore,
) -> TypeId {
	let result = properties::get_property_unbound(
		(rhs, None),
		(publicity, key, None),
		false,
		information,
		types,
	);

	crate::utilities::notify!("Has got {:?} on {:?}", result, rhs);

	match result {
		Ok(LogicalOrValid::Logical(result)) => match result {
			Logical::Pure(_) => TypeId::TRUE,
			Logical::Or { .. } => {
				crate::utilities::notify!("or or implies `in`");
				TypeId::ERROR_TYPE
			}
			Logical::Implies { .. } => {
				crate::utilities::notify!("or or implies `in`");
				TypeId::ERROR_TYPE
			}
			Logical::BasedOnKey { .. } => {
				crate::utilities::notify!("mapped in");
				TypeId::ERROR_TYPE
			}
		},
		Ok(LogicalOrValid::NeedsCalculation(result)) => {
			crate::utilities::notify!("TODO {:?}", result);
			TypeId::ERROR_TYPE
		}
		Err(err) => {
			crate::utilities::notify!("TODO {:?}", err);
			TypeId::ERROR_TYPE
		} // match err {
		  // 	MissingOrToCalculate::Missing => {
		  // 		crate::utilities::notify!("If on poly this could be conditional");
		  // 		TypeId::FALSE
		  // 	}
		  // 	MissingOrToCalculate::Error => types.new_error_type(TypeId::BOOLEAN_TYPE),
		  // 	MissingOrToCalculate::Infer { .. } => types.new_error_type(TypeId::BOOLEAN_TYPE),
		  // 	MissingOrToCalculate::Proxy(_) => {
		  // 		crate::utilities::notify!("`has` on proxy");
		  // 		types.new_error_type(TypeId::BOOLEAN_TYPE)
		  // 	}
		  // },
	}
}
