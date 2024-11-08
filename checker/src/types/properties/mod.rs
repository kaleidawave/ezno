pub mod access;
pub mod assignment;
pub mod list;

pub use access::*;
pub use assignment::set_property;
pub use list::*;

use super::{Type, TypeStore};
use crate::{
	context::InformationChain,
	subtyping::{slice_matches_type, SliceArguments, SubTypingOptions},
	types::{
		calling::Callable, generics::contributions::Contributions, logical, GenericChain,
		PolyNature,
	},
	Constant, Environment, TypeId,
};
use std::borrow::Cow;

pub type Properties = Vec<(Publicity, PropertyKey<'static>, PropertyValue)>;

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

impl From<String> for PropertyKey<'static> {
	fn from(value: String) -> Self {
		Self::String(Cow::Owned(value))
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
				Constant::Boolean(b) => {
					PropertyKey::String(Cow::Borrowed(if *b { "true" } else { "false" }))
				}
				Constant::Symbol { .. } => {
					// Okay I think?
					PropertyKey::Type(ty)
				}
				Constant::NaN => PropertyKey::String(Cow::Borrowed("NaN")),
				Constant::Undefined => PropertyKey::String(Cow::Borrowed("undefined")),
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

/// For getting `length` and stuff
pub(crate) fn get_simple_property_value(
	ctx: &impl InformationChain,
	on: TypeId,
	property: &PropertyKey,
	types: &TypeStore,
) -> Option<TypeId> {
	fn get_logical(v: logical::Logical<PropertyValue>) -> Option<TypeId> {
		match v {
			logical::Logical::Pure(PropertyValue::Value(t)) => Some(t),
			logical::Logical::Implies { on, antecedent: _ } => get_logical(*on),
			_ => None,
		}
	}

	let value =
		get_property_unbound((on, None), (Publicity::Public, property, None), false, ctx, types)
			.ok()?;

	if let logical::LogicalOrValid::Logical(value) = value {
		get_logical(value)
	} else {
		None
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
	/// These need to be [`Callable`], because the getter can be a type via `Object.defineProperty`
	GetterAndSetter {
		getter: Callable,
		setter: Callable,
	},
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
	#[must_use]
	pub fn as_get_type(&self, types: &TypeStore) -> TypeId {
		match self {
			PropertyValue::Value(value) => *value,
			PropertyValue::GetterAndSetter { getter, setter: _ }
			| PropertyValue::Getter(getter) => {
				crate::utilities::notify!("Here. Should pass down types");
				getter.get_return_type(types)
			}
			// TODO unsure about these two
			PropertyValue::Setter(_) => TypeId::UNDEFINED_TYPE,
			PropertyValue::Deleted => TypeId::NEVER_TYPE,
			PropertyValue::ConditionallyExists { truthy, .. } => {
				// TODO temp
				truthy.as_get_type(types)
			}
			PropertyValue::Configured { on, .. } => on.as_get_type(types),
		}
	}

	#[must_use]
	pub fn as_set_type(&self, types: &TypeStore) -> TypeId {
		match self {
			PropertyValue::Value(value) => *value,
			PropertyValue::Getter(_) => TypeId::UNDEFINED_TYPE,
			PropertyValue::GetterAndSetter { getter: _, setter }
			| PropertyValue::Setter(setter) => {
				crate::utilities::notify!("Here. Should pass down types");
				setter.get_first_argument(types)
			}
			// TODO unsure about these two
			PropertyValue::Deleted => TypeId::NEVER_TYPE,
			PropertyValue::ConditionallyExists { truthy, .. } => {
				// TODO temp
				truthy.as_set_type(types)
			}
			PropertyValue::Configured { on, .. } => on.as_set_type(types),
		}
	}

	// For printing and debugging
	#[must_use]
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
	#[must_use]
	pub fn is_optional_simple(&self) -> bool {
		if let PropertyValue::ConditionallyExists { condition, truthy: _ } = self {
			// crate::utilities::notify!("condition={:?}", *condition);
			!matches!(*condition, TypeId::NON_OPTIONAL_KEY_ARGUMENT)
		} else {
			false
		}
	}

	// For printing and debugging
	#[must_use]
	pub fn is_writable_simple(&self) -> bool {
		if let PropertyValue::ConditionallyExists { condition: _, truthy } = self {
			truthy.is_writable_simple()
		} else if let PropertyValue::Configured { on: _, descriptor } = self {
			!matches!(descriptor.writable, TypeId::TRUE | TypeId::WRITABLE_KEY_ARGUMENT)
		} else {
			false
		}
	}

	#[must_use]
	pub fn is_configuable_simple(&self) -> bool {
		if let PropertyValue::ConditionallyExists { condition: _, truthy } = self {
			truthy.is_configuable_simple()
		} else if let PropertyValue::Configured { on: _, descriptor } = self {
			descriptor.configurable == TypeId::TRUE
		} else {
			true
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
		(PropertyKey::String(s), PropertyKey::Type(want)) => {
			// This is a special branch because it can refer to many properties
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
					if let matched @ (true, _) = key_matches(
						(key, key_type_arguments),
						(&PropertyKey::Type(*lhs), key_type_arguments),
						info_chain,
						types,
					) {
						matched
					} else {
						key_matches(
							(key, key_type_arguments),
							(&PropertyKey::Type(*rhs), key_type_arguments),
							info_chain,
							types,
						)
					}
				} else if let Type::RootPolyType(PolyNature::MappedGeneric {
					extends: to, ..
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
					let matches = get_properties_on_single_type2(
						(*on, want_type_arguments),
						types,
						info_chain,
						TypeId::ANY_TYPE,
					)
					.iter()
					.all(|(rhs_key, _, _)| {
						// TODO what about keys here
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
					// Nuance about symbol here. TODO
					(true, SliceArguments::default())
				} else {
					(false, SliceArguments::default())
				}
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

#[must_use]
pub fn get_property_as_string(
	property: &PropertyKey,
	types: &TypeStore,
	environment: &Environment,
) -> String {
	match property {
		PropertyKey::String(s) => s.to_string(),
		PropertyKey::Type(t) => crate::types::printing::print_type(*t, types, environment, false),
	}
}
