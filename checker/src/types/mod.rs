pub mod calling;
pub mod casts;
pub mod classes;
pub mod disjoint;
pub mod functions;
pub mod generics;
pub mod helpers;
pub mod intrinsics;
pub mod logical;
pub mod printing;
pub mod properties;
pub mod store;
pub mod subtyping;
mod terms;

pub(crate) use self::{
	casts::*,
	generics::contributions::CovariantContribution,
	generics::{
		chain::{GenericChain, GenericChainLink},
		substitution::*,
	},
	properties::AccessMode,
	properties::*,
};

pub use self::{
	functions::*, generics::generic_type_arguments::GenericArguments, store::TypeStore,
	terms::Constant,
};

pub use crate::features::objects::SpecialObject;

use crate::{
	context::InformationChain,
	events::RootReference,
	features::operations::{CanonicalEqualityAndInequality, MathematicalOrBitwiseOperation},
	subtyping::SliceArguments,
	Decidable, FunctionId,
};

use derive_debug_extras::DebugExtras;
use source_map::SpanWithSource;

pub type ExplicitTypeArgument = (TypeId, SpanWithSource);

/// Final
pub type TypeArguments = crate::Map<TypeId, TypeId>;
pub type TypeRestrictions = crate::Map<TypeId, ExplicitTypeArgument>;
pub type LookUpGenericMap = crate::Map<TypeId, LookUpGeneric>;

/// References [Type]
///
/// TODO maybe [`u32`] or [`u64`]
/// TODO maybe [`crate::SourceId`] like to reference a block, then [`u16`] references some offset
#[derive(PartialEq, Eq, Clone, Copy, DebugExtras, Hash)]
pub struct TypeId(pub(crate) u16);

/// TODO ids as macro as to not do in [`crate::context::RootContext`]
impl TypeId {
	/// Not to be confused with [`TypeId::NEVER_TYPE`]
	pub const ERROR_TYPE: Self = Self(0);
	pub const UNIMPLEMENTED_ERROR_TYPE: Self = Self::ERROR_TYPE;
	pub const IS_ASSIGNED_VALUE_LATER: Self = Self::ERROR_TYPE;

	pub const NEVER_TYPE: Self = Self(1);

	pub const ANY_TYPE: Self = Self(2);
	// TODO
	pub const ANY_TO_INFER_TYPE: Self = Self::ANY_TYPE;

	pub const BOOLEAN_TYPE: Self = Self(3);
	pub const NUMBER_TYPE: Self = Self(4);
	pub const STRING_TYPE: Self = Self(5);

	pub const UNDEFINED_TYPE: Self = Self(6);
	pub const NULL_TYPE: Self = Self(7);
	pub const VOID_TYPE: Self = Self(8);

	/// This is the inner version
	pub const ARRAY_TYPE: Self = Self(9);
	pub const PROMISE_TYPE: Self = Self(10);

	/// Used for Array and Promise
	pub const T_TYPE: Self = Self(11);

	pub const OBJECT_TYPE: Self = Self(12);
	pub const FUNCTION_TYPE: Self = Self(13);
	/// This points to the `RegExp` prototype
	pub const REGEXP_TYPE: Self = Self(14);
	/// This points to the ???
	pub const SYMBOL_TYPE: Self = Self(15);

	/// For direct constant and the rules
	pub const TRUE: Self = Self(16);
	pub const FALSE: Self = Self(17);
	pub const ZERO: Self = Self(18);
	pub const ONE: Self = Self(19);
	pub const NAN: Self = Self(20);
	pub const NEG_INFINITY: Self = Self(21);
	pub const INFINITY: Self = Self(22);
	pub const FLOAT_MIN: Self = Self(23);
	pub const FLOAT_MAX: Self = Self(24);
	pub const FLOAT_EPSILON: Self = Self(25);
	/// For bitwise negation
	pub const MAX_U32: Self = Self(26);
	/// ""
	pub const EMPTY_STRING: Self = Self(27);

	/// Shortcut for inferred this
	/// TODO remove
	pub const ANY_INFERRED_FREE_THIS: Self = Self(28);

	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target>
	pub const NEW_TARGET_ARG: Self = Self(29);

	pub const IMPORT_META: Self = Self(30);

	// known symbols
	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/iterator>
	pub const SYMBOL_ITERATOR: Self = Self(31);
	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/asyncIterator>
	pub const SYMBOL_ASYNC_ITERATOR: Self = Self(32);
	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/hasInstance>
	pub const SYMBOL_HAS_INSTANCE: Self = Self(33);
	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/toPrimitive>
	pub const SYMBOL_TO_PRIMITIVE: Self = Self(34);

	// TSC intrinsics
	pub const STRING_GENERIC: Self = Self(35);
	pub const STRING_UPPERCASE: Self = Self(36);
	pub const STRING_LOWERCASE: Self = Self(37);
	pub const STRING_CAPITALIZE: Self = Self(38);
	pub const STRING_UNCAPITALIZE: Self = Self(39);
	pub const NO_INFER: Self = Self(40);

	/// Might be a special type in TSC
	pub const READONLY_RESTRICTION: Self = Self(41);

	/// For mapped types
	pub const NON_OPTIONAL_KEY_ARGUMENT: Self = Self(42);
	/// For mapped types
	pub const WRITABLE_KEY_ARGUMENT: Self = Self(43);

	// Ezno intrinsics
	pub const NUMBER_GENERIC: Self = Self(44);
	pub const GREATER_THAN: Self = Self(45);
	pub const LESS_THAN: Self = Self(46);
	pub const MULTIPLE_OF: Self = Self(47);
	pub const NOT_NOT_A_NUMBER: Self = Self(48);
	pub const NUMBER_BUT_NOT_NOT_A_NUMBER: Self = Self(49);

	pub const LITERAL_RESTRICTION: Self = Self(50);
	pub const EXCLUSIVE_RESTRICTION: Self = Self(51);
	pub const NOT_RESTRICTION: Self = Self(52);

	/// This is needed for the TSC string intrinsics
	pub const CASE_INSENSITIVE: Self = Self(53);

	/// WIP
	pub const OPEN_BOOLEAN_TYPE: Self = Self(54);
	pub const OPEN_NUMBER_TYPE: Self = Self(55);

	/// For `+` operator
	pub const STRING_OR_NUMBER: Self = Self(56);

	/// Above add one (because [`TypeId`] starts at zero). Used to assert that the above is all correct
	pub(crate) const INTERNAL_TYPE_COUNT: usize = 57;
}

#[derive(Debug, binary_serialize_derive::BinarySerializable)]
pub enum Type {
	/// *Dependent equality types*
	Constant(crate::Constant),
	/// Technically could be just a function but...
	Object(ObjectNature),
	SpecialObject(SpecialObject),

	/// From a parameter, introduces a set of types > 1
	RootPolyType(PolyNature),
	/// Also a "Substituted constructor type"
	Constructor(Constructor),
	Narrowed {
		/// Either `RootPolyType` or `Constructor`
		from: TypeId,
		narrowed_to: TypeId,
	},
	/// For
	/// - `extends` interface part (for multiple it to is a `Type::And`)
	/// - Can be used for subtypes (aka N aliases number then more types on top)
	/// - **Does not imply .prototype = **
	AliasTo {
		name: String,
		parameters: Option<Vec<TypeId>>,
		to: TypeId,
	},
	/// Properties are in environment (for declaration merging)
	Interface {
		name: String,
		parameters: Option<Vec<TypeId>>,
		extends: Option<TypeId>,
	},
	/// Pretty much same as [`Type::Interface`]
	Class {
		name: String,
		type_parameters: Option<Vec<TypeId>>,
	},
	/// From a type annotation or .d.ts WITHOUT body. e.g. don't know effects TODO...
	FunctionReference(FunctionId),

	/// **e.g `Array<string>`
	PartiallyAppliedGenerics(PartiallyAppliedGenerics),

	/// Has properties of both sides
	And(TypeId, TypeId),
	/// Has properties of either of sides
	Or(TypeId, TypeId),
}

/// TODO difference between untyped and typed parameters and what about parameter based for any
///
/// Most of the difference here is just for debugging, printing etc
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum PolyNature {
	/// Regular parameters are consider generic in this type system
	/// This allows for
	/// - event application to work on known values
	/// - more accurate return types
	/// - `fixed_to` can point to [`PolyNature::FunctionGeneric`]
	Parameter { fixed_to: TypeId },
	/// This is on a structure (`class`, `interface` and `type` alias)
	StructureGeneric { name: String, extends: TypeId },
	/// From `infer U`.
	InferGeneric { name: String, extends: TypeId },
	/// For explicit generics (or on external definitions)
	FunctionGeneric { name: String, extends: TypeId },
	/// For mapped types
	MappedGeneric { name: String, extends: TypeId },
	/// An error occurred and it looks like
	Error(TypeId),
	/// This is generic types. Examples such as a fetch
	/// A set of known values cannot be made throughout the s
	Open(TypeId),
	/// For functions and for loops where something in the scope can mutate
	/// (so not constant) between runs.
	FreeVariable { reference: RootReference, based_on: TypeId },
	/// This function is called before it is
	RecursiveFunction(FunctionId, TypeId),
	/// ```ts
	/// } catch (error) {
	///          ^^^^^
	/// }
	/// ```
	CatchVariable(TypeId),
}

impl PolyNature {
	/// Whether <- can set a value
	#[must_use]
	pub fn is_substitutable(&self) -> bool {
		matches!(
			self,
			Self::Parameter { .. }
				| Self::FunctionGeneric { .. }
				| Self::InferGeneric { .. }
				| Self::MappedGeneric { .. }
		)
		// | Self::StructureGeneric { .. }
	}

	/// The constraint can be adjusted
	#[must_use]
	pub fn is_inferrable(&self) -> bool {
		matches!(
			self,
			Self::Parameter { fixed_to: to } | Self::FreeVariable { based_on: to, .. } | Self::CatchVariable(to)
			// TODO matches TypeId::unknown
			if matches!(*to, TypeId::ANY_TYPE)
		)
	}

	// TODO remove Option
	#[must_use]
	pub fn get_constraint(&self) -> TypeId {
		match self {
			PolyNature::Parameter { fixed_to: to }
			| PolyNature::FunctionGeneric { extends: to, .. }
			| PolyNature::MappedGeneric { extends: to, .. }
			| PolyNature::FreeVariable { based_on: to, .. }
			| PolyNature::RecursiveFunction(_, to)
			| PolyNature::StructureGeneric { extends: to, .. }
			| PolyNature::InferGeneric { extends: to, .. }
			| PolyNature::CatchVariable(to)
			| PolyNature::Open(to)
			| PolyNature::Error(to) => *to,
		}
	}
}

/// TODO split
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum ObjectNature {
	/// Actual allocated object
	RealDeal,
	/// From `x: { a: number }` etc
	AnonymousTypeAnnotation(Properties),
}

impl Type {
	/// These can be referenced by `<...>`
	pub(crate) fn get_parameters(&self) -> Option<Vec<TypeId>> {
		if let Type::Class { type_parameters: parameters, .. }
		| Type::Interface { parameters, .. }
		| Type::AliasTo { parameters, .. } = self
		{
			parameters.clone()
		} else {
			None
		}
	}

	/// TODO return is poly
	#[must_use]
	pub fn is_dependent(&self) -> bool {
		#[allow(clippy::match_same_arms)]
		match self {
			// TODO
			Type::PartiallyAppliedGenerics(..) => false,
			// Fine
			Type::Narrowed { .. } | Type::Constructor(_) | Type::RootPolyType(_) => true,
			// TODO what about if left or right
			Type::And(_, _) | Type::Or(_, _) => false,
			// TODO what about if it aliases dependent?
			Type::AliasTo { .. } => false,
			Type::Interface { .. } | Type::Class { .. } => false,
			Type::Constant(_)
			| Type::SpecialObject(_)
			| Type::FunctionReference(..)
			| Type::Object(_) => false,
		}
	}

	#[must_use]
	pub fn is_operator(&self) -> bool {
		matches!(
			self,
			Self::And(..) | Self::Or(..) | Self::Constructor(Constructor::ConditionalResult { .. })
		)
	}

	#[must_use]
	pub fn is_nominal(&self) -> bool {
		matches!(self, Self::Class { .. })
	}

	#[must_use]
	pub fn is_constant(&self) -> bool {
		matches!(
			self,
			Self::Constant(..) | Self::Object(ObjectNature::RealDeal) | Self::SpecialObject(..)
		)
	}
}

/// - Some of these can be specialised, others are only created via event specialisation
/// - Note that no || and && etc. This is handled using [`Constructor::ConditionalResult`]
/// - Unary operations are encoded via [`Constructor::BinaryOperator`] equivalents
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum Constructor {
	BinaryOperator {
		lhs: TypeId,
		operator: MathematicalOrBitwiseOperation,
		rhs: TypeId,
		/// for add + number intrinsics
		result: TypeId,
	},
	CanonicalRelationOperator {
		lhs: TypeId,
		operator: CanonicalEqualityAndInequality,
		rhs: TypeId,
	},
	/// JS type based operations
	TypeOperator(TypeOperator),
	/// TS operation
	TypeExtends(TypeExtends),
	/// TODO constraint is res
	ConditionalResult {
		/// TODO this can only be poly types
		condition: TypeId,
		truthy_result: TypeId,
		otherwise_result: TypeId,
		result_union: TypeId,
	},
	/// Output of a function where on is dependent (or sometimes for const functions one of `with` is dependent)
	Image {
		on: TypeId,
		// TODO I don't think this is necessary, maybe for debugging. In such case should be an Rc to share with events
		with: Box<[calling::SynthesisedArgument]>,
		result: TypeId,
	},
	/// Access
	Property {
		on: TypeId,
		under: properties::PropertyKey<'static>,
		result: TypeId,
		mode: AccessMode,
	},
	/// For await a poly type
	Awaited {
		on: TypeId,
		result: TypeId,
	},
	/// From TS `keyof`. Also
	KeyOf(TypeId),
}

impl Constructor {
	pub fn get_constraint(&self) -> TypeId {
		match self {
			Constructor::BinaryOperator { result, .. }
			| Constructor::Awaited { on: _, result }
			| Constructor::Image { on: _, with: _, result } => *result,
			Constructor::Property { on: _, under: _, result, mode: _ } => {
				// crate::utilities::notify!("Here, result of a property get");
				*result
			}
			Constructor::ConditionalResult { result_union, .. } => {
				// TODO dynamic and open poly
				*result_union
			}
			Constructor::TypeOperator(op) => match op {
				// TODO union of names
				TypeOperator::TypeOf(_) => TypeId::STRING_TYPE,
				TypeOperator::IsPrototype { .. } | TypeOperator::HasProperty(..) => {
					TypeId::BOOLEAN_TYPE
				}
			},
			Constructor::CanonicalRelationOperator { .. } => TypeId::BOOLEAN_TYPE,
			Constructor::TypeExtends(op) => {
				let crate::types::TypeExtends { .. } = op;
				TypeId::BOOLEAN_TYPE
			}
			Constructor::KeyOf(_) => TypeId::STRING_TYPE,
		}
	}
}

#[must_use]
pub fn as_logical_not(constructor: &Constructor, types: &TypeStore) -> Option<TypeId> {
	// TODO technically any falsy, truthy reverse pair is okay
	if let Constructor::ConditionalResult {
		condition,
		truthy_result: TypeId::FALSE,
		otherwise_result: TypeId::TRUE,
		result_union: _,
	} = constructor
	{
		Some(helpers::get_origin(*condition, types))
	} else {
		None
	}
}

#[must_use]
pub fn as_logical_and(constructor: &Constructor, types: &TypeStore) -> Option<(TypeId, TypeId)> {
	if let Constructor::ConditionalResult {
		condition,
		truthy_result,
		otherwise_result,
		result_union: _,
	} = constructor
	{
		let (condition, truthy_result, otherwise_result) = (
			helpers::get_origin(*condition, types),
			helpers::get_origin(*truthy_result, types),
			helpers::get_origin(*otherwise_result, types),
		);
		(condition == otherwise_result).then_some((truthy_result, otherwise_result))
	} else {
		None
	}
}

#[must_use]
pub fn as_logical_or(constructor: &Constructor, types: &TypeStore) -> Option<(TypeId, TypeId)> {
	if let Constructor::ConditionalResult {
		condition,
		truthy_result,
		otherwise_result,
		result_union: _,
	} = constructor
	{
		let (condition, truthy_result, otherwise_result) = (
			helpers::get_origin(*condition, types),
			helpers::get_origin(*truthy_result, types),
			helpers::get_origin(*otherwise_result, types),
		);
		(condition == truthy_result || truthy_result == TypeId::TRUE)
			.then_some((condition, otherwise_result))
	} else {
		None
	}
}

/// Closed over arguments
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct PartiallyAppliedGenerics {
	pub on: TypeId,
	pub arguments: GenericArguments,
}

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum TypeOperator {
	IsPrototype {
		lhs: TypeId,
		rhs_prototype: TypeId,
	},
	/// The `typeof` unary operator
	TypeOf(TypeId),
	HasProperty(TypeId, properties::PropertyKey<'static>),
}

/// This if from the type annotation `is`
#[derive(Clone, Copy, Debug, binary_serialize_derive::BinarySerializable)]
pub struct TypeExtends {
	pub item: TypeId,
	pub extends: TypeId,
}

impl TypeExtends {
	/// This does `typeof`, `===` and `instanceof`
	///
	/// Because of type representation and the fact this cannot generate types the following are not covered
	/// - negation. Cannot create `Not`
	/// - number intrinsincs. Cannot create `LessThan`, `GreaterThan`, etc
	/// - has property. Cannot create new object
	pub fn from_type(rhs: TypeId, types: &TypeStore) -> Result<Self, ()> {
		let rhs_ty = types.get_type_by_id(rhs);
		if let Type::Constructor(Constructor::CanonicalRelationOperator {
			lhs,
			operator: CanonicalEqualityAndInequality::StrictEqual,
			rhs,
		}) = rhs_ty
		{
			if let (
				Type::Constructor(Constructor::TypeOperator(TypeOperator::TypeOf(on))),
				Type::Constant(Constant::String(s)),
			) = (types.get_type_by_id(*lhs), types.get_type_by_id(*rhs))
			{
				if let Some(extends_ty_name) = crate::features::string_name_to_type(s) {
					Ok(Self { item: *on, extends: extends_ty_name })
				} else {
					Err(())
				}
			} else {
				// === rhs
				Ok(Self { item: *lhs, extends: *rhs })
			}
		} else if let Type::Constructor(Constructor::TypeOperator(TypeOperator::IsPrototype {
			lhs,
			rhs_prototype,
		})) = rhs_ty
		{
			// TODO this loses generics etc
			Ok(Self { item: *lhs, extends: *rhs_prototype })
		} else if let Type::Constructor(Constructor::TypeExtends(extends)) = rhs_ty {
			Ok(*extends)
		} else {
			Err(())
		}
	}

	#[must_use]
	pub fn equal_to_rhs(&self, rhs: TypeId, types: &TypeStore) -> bool {
		if let Ok(TypeExtends { item, extends }) = Self::from_type(rhs, types) {
			// TODO get origin on item
			item == self.item && extends == self.extends
		} else {
			// crate::utilities::notify!("Here {:?}", rhs_ty);
			false
		}
	}
}

/// If `assert x is y` annotation
pub fn type_is_assert_is_type(ty: TypeId, types: &TypeStore) -> Result<TypeExtends, ()> {
	if let Type::Constructor(Constructor::ConditionalResult {
		condition,
		truthy_result: _,
		otherwise_result: TypeId::NEVER_TYPE,
		result_union: _,
	}) = types.get_type_by_id(ty)
	{
		TypeExtends::from_type(*condition, types)
	} else {
		Err(())
	}
}

#[must_use]
pub fn is_type_truthy_falsy(id: TypeId, types: &TypeStore) -> Decidable<bool> {
	// These first two branches are just shortcuts.
	if id == TypeId::TRUE || id == TypeId::FALSE {
		Decidable::Known(id == TypeId::TRUE)
	} else if id == TypeId::NULL_TYPE || id == TypeId::UNDEFINED_TYPE {
		Decidable::Known(false)
	} else {
		let ty = types.get_type_by_id(id);
		match ty {
			Type::AliasTo { .. }
			| Type::And(_, _)
			| Type::Or(_, _)
			| Type::RootPolyType(_)
			| Type::Constructor(_)
			| Type::Interface { .. }
			| Type::PartiallyAppliedGenerics(..)
			| Type::Class { .. } => {
				// TODO some of these case are known
				Decidable::Unknown(id)
			}
			Type::FunctionReference(..) | Type::SpecialObject(_) | Type::Object(_) => {
				Decidable::Known(true)
			}
			Type::Constant(cst) => {
				// TODO strict casts
				Decidable::Known(cast_as_boolean(cst, false).unwrap())
			}
			Type::Narrowed { narrowed_to, .. } => is_type_truthy_falsy(*narrowed_to, types),
		}
	}
}

pub enum ArgumentOrLookup {
	Argument(TypeId),
	LookUpGeneric(LookUpGeneric),
}

// TODO maybe positions and extra information here
// SomeLiteralMismatch
// GenericParameterCollision
#[derive(Debug)]
pub enum NonEqualityReason {
	Mismatch,
	PropertiesInvalid {
		errors: Vec<(properties::PropertyKey<'static>, PropertyError)>,
	},
	TooStrict,
	/// TODO more information
	MissingParameter,
	GenericParameterMismatch,
	Excess,
}

#[derive(Debug)]
pub enum PropertyError {
	Missing,
	Invalid { expected: TypeId, found: TypeId, mismatch: NonEqualityReason },
}

/// TODO temp fix for printing
pub(crate) fn is_explicit_generic(on: TypeId, types: &TypeStore) -> bool {
	if let Type::RootPolyType(
		PolyNature::FunctionGeneric { .. } | PolyNature::StructureGeneric { .. },
	) = types.get_type_by_id(on)
	{
		true
	} else if let Type::Constructor(Constructor::Property { on, under, result: _, mode: _ }) =
		types.get_type_by_id(on)
	{
		is_explicit_generic(*on, types)
			|| matches!(under, properties::PropertyKey::Type(under) if is_explicit_generic(*under, types))
	} else {
		false
	}
}

/// Finds the constraint of poly types
///
/// **Also looks at possibly mutated things
pub(crate) fn get_constraint(on: TypeId, types: &TypeStore) -> Option<TypeId> {
	match types.get_type_by_id(on) {
		Type::RootPolyType(nature) => Some(nature.get_constraint()),
		Type::Constructor(constructor) => Some(constructor.get_constraint()),
		Type::Narrowed { from: _, narrowed_to } => Some(*narrowed_to),
		Type::Object(ObjectNature::RealDeal) => {
			// crate::utilities::notify!("Might be missing some mutations that are possible here");
			None
		}
		_ => None,
	}
}

/// TODO T on `Array`, U, V on `Map` etc. Works around not having `&mut TypeStore` and mutations in-between
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum LookUpGeneric {
	NumberPropertyOfSelf, // Property(Box<Self>, PropertyKey<'static>),
}

impl LookUpGeneric {
	#[allow(unreachable_patterns)]
	pub(crate) fn calculate_lookup(
		&self,
		info: &impl InformationChain,
		types: &TypeStore,
		on: TypeId,
	) -> Vec<TypeId> {
		match self {
			LookUpGeneric::NumberPropertyOfSelf => {
				info.get_chain_of_info()
					.filter_map(|info| info.current_properties.get(&on).map(|v| v.iter()))
					.flatten()
					.filter_map(|(_publicity, key, value)| {
						// TODO filter more
						if matches!(key, properties::PropertyKey::String(s) if s == "length") {
							None
						} else {
							Some(value.as_get_type(types))
						}
					})
					.collect()
			}
			_ => unreachable!(), // LookUpGeneric::Property(_, _) => todo!(),
		}
	}
}

pub trait TypeCombinable {
	fn combine(
		condition: TypeId,
		truthy_result: Self,
		otherwise_result: Self,
		types: &mut TypeStore,
	) -> Self;

	fn default() -> Self;
}

// For if-else branches
impl TypeCombinable for () {
	fn combine(
		_condition: TypeId,
		_truthy_result: Self,
		_otherwise_result: Self,
		_types: &mut TypeStore,
	) -> Self {
	}

	fn default() -> Self {}
}

// For ternary conditional operators
impl TypeCombinable for TypeId {
	fn combine(
		condition: TypeId,
		truthy_result: Self,
		otherwise_result: Self,
		types: &mut TypeStore,
	) -> Self {
		types.new_conditional_type(condition, truthy_result, otherwise_result)
	}

	fn default() -> Self {
		TypeId::UNDEFINED_TYPE
	}
}
