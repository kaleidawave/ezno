pub mod calling;
pub mod casts;
pub mod classes;
pub mod disjoint;
pub mod functions;
pub mod generics;
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
	features::operations::{CanonicalEqualityAndInequality, MathematicalAndBitwise, PureUnary},
	subtyping::SliceArguments,
	Decidable, FunctionId,
};

use derive_debug_extras::DebugExtras;
use derive_enum_from_into::EnumFrom;
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
	/// ""
	pub const EMPTY_STRING: Self = Self(26);

	/// Shortcut for inferred this
	/// TODO remove
	pub const ANY_INFERRED_FREE_THIS: Self = Self(27);

	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target>
	pub const NEW_TARGET_ARG: Self = Self(28);

	pub const IMPORT_META: Self = Self(29);

	// known symbols
	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/iterator>
	pub const SYMBOL_ITERATOR: Self = Self(30);
	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/asyncIterator>
	pub const SYMBOL_ASYNC_ITERATOR: Self = Self(31);
	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/hasInstance>
	pub const SYMBOL_HAS_INSTANCE: Self = Self(32);
	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/toPrimitive>
	pub const SYMBOL_TO_PRIMITIVE: Self = Self(33);

	// TSC intrinsics
	pub const STRING_GENERIC: Self = Self(34);
	pub const STRING_UPPERCASE: Self = Self(35);
	pub const STRING_LOWERCASE: Self = Self(36);
	pub const STRING_CAPITALIZE: Self = Self(37);
	pub const STRING_UNCAPITALIZE: Self = Self(38);
	pub const NO_INFER: Self = Self(39);

	/// Might be a special type in TSC
	pub const READONLY_RESTRICTION: Self = Self(40);

	/// For mapped types
	pub const NON_OPTIONAL_KEY_ARGUMENT: Self = Self(41);
	/// For mapped types
	pub const WRITABLE_KEY_ARGUMENT: Self = Self(42);

	// Ezno intrinsics

	/// Used in [`Self::INCLUSIVE_RANGE`], [`Self::EXCLUSIVE_RANGE`] and [`Self::MULTIPLE_OF`]
	pub const NUMBER_BOTTOM_GENERIC: Self = Self(43);
	pub const NUMBER_TOP_GENERIC: Self = Self(44);
	pub const INCLUSIVE_RANGE: Self = Self(45);
	pub const EXCLUSIVE_RANGE: Self = Self(46);
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
		to: TypeId,
		name: String,
		parameters: Option<Vec<TypeId>>,
	},
	/// For number and other rooted types
	///
	/// Although they all alias Object
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
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum Constructor {
	// TODO separate add?
	BinaryOperator {
		lhs: TypeId,
		operator: MathematicalAndBitwise,
		rhs: TypeId,
	},
	CanonicalRelationOperator {
		lhs: TypeId,
		operator: CanonicalEqualityAndInequality,
		rhs: TypeId,
	},
	UnaryOperator {
		operator: PureUnary,
		operand: TypeId,
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
	fn get_base(&self) -> Option<TypeId> {
		match self {
			Constructor::ConditionalResult { result_union: result, .. }
			| Constructor::Awaited { result, .. }
			| Constructor::Property { result, .. }
			| Constructor::Image { result, .. } => Some(*result),
			Constructor::BinaryOperator { .. }
			| Constructor::CanonicalRelationOperator { .. }
			| Constructor::UnaryOperator { .. }
			| Constructor::TypeExtends(_)
			| Constructor::TypeOperator(_) => None,
			// TODO or symbol
			Constructor::KeyOf(_) => Some(TypeId::STRING_TYPE),
		}
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
			| Type::Narrowed { .. }
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
		Type::Constructor(constructor) => match constructor.clone() {
			Constructor::BinaryOperator { lhs, operator, rhs } => {
				if let MathematicalAndBitwise::Add = operator {
					let lhs = get_larger_type(lhs, types);
					let rhs = get_larger_type(rhs, types);
					// TODO these need to be generated
					if let (TypeId::NUMBER_TYPE, TypeId::NUMBER_TYPE) = (lhs, rhs) {
						Some(TypeId::NUMBER_TYPE)
					} else if let (TypeId::STRING_TYPE, _) | (_, TypeId::STRING_TYPE) = (lhs, rhs) {
						Some(TypeId::STRING_TYPE)
					} else {
						crate::utilities::notify!("lhs = {:?}", types.get_type_by_id(lhs));
						crate::utilities::notify!("TODO use existing conditional");
						Some(TypeId::NUMBER_TYPE)
					}
				} else {
					Some(TypeId::NUMBER_TYPE)
				}
			}
			Constructor::UnaryOperator { operand: _, operator } => {
				Some(match operator {
					PureUnary::LogicalNot => TypeId::BOOLEAN_TYPE,
					PureUnary::Negation | PureUnary::BitwiseNot => TypeId::NUMBER_TYPE,
				})
				// if *constraint == TypeId::ANY_TYPE && mutable_context {
				// 	let (operand, operator) = (operand.clone(), operator.clone());
				// 	let constraint = to(self, data);
				// 	self.modify_type(
				// 		on,
				// 		Some(Type::Constructor(Constructor::UnaryOperator {
				// 			operator,
				// 			operand,
				// 								// 		})),
				// 	);
				// 	Some(constraint)
				// } else {
				// 	Some(*constraint)
				// }
			}
			Constructor::Awaited { on: _, result }
			| Constructor::Image { on: _, with: _, result } => Some(result),
			Constructor::Property { on: _, under: _, result, mode: _ } => {
				// crate::utilities::notify!("Here, result of a property get");
				Some(result)
			}
			Constructor::ConditionalResult { result_union, .. } => {
				// TODO dynamic and open poly
				Some(result_union)
			}
			Constructor::TypeOperator(op) => match op {
				// TODO union of names
				TypeOperator::TypeOf(_) => Some(TypeId::STRING_TYPE),
				TypeOperator::IsPrototype { .. } | TypeOperator::HasProperty(..) => {
					Some(TypeId::BOOLEAN_TYPE)
				}
			},
			Constructor::CanonicalRelationOperator { .. } => Some(TypeId::BOOLEAN_TYPE),
			Constructor::TypeExtends(op) => {
				let crate::types::TypeExtends { .. } = op;
				Some(TypeId::BOOLEAN_TYPE)
			}
			Constructor::KeyOf(_) => Some(TypeId::STRING_TYPE),
		},
		Type::Narrowed { from: _, narrowed_to } => Some(*narrowed_to),
		Type::Object(ObjectNature::RealDeal) => {
			// crate::utilities::notify!("Might be missing some mutations that are possible here");
			None
		}
		_ => None,
	}
}

/// Returns the constraint or base of a constant for a type. Otherwise just return the type
#[must_use]
pub fn get_larger_type(on: TypeId, types: &TypeStore) -> TypeId {
	if let Some(poly_base) = get_constraint(on, types) {
		poly_base
	} else if let Type::Constant(cst) = types.get_type_by_id(on) {
		cst.get_backing_type_id()
	} else {
		on
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

/// Used for **both** inference and narrowing
pub enum Confirmation {
	HasProperty { on: (), property: () },
	IsType { on: (), ty: () },
}

pub(crate) fn get_structure_arguments_based_on_object_constraint<'a, C: InformationChain>(
	object: TypeId,
	info_chain: &C,
	types: &'a TypeStore,
) -> Option<&'a GenericArguments> {
	if let Some(object_constraint) =
		info_chain.get_chain_of_info().find_map(|c| c.object_constraints.get(&object).copied())
	{
		let ty = types.get_type_by_id(object_constraint);
		if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { arguments, .. }) = ty {
			Some(arguments)
		} else {
			crate::utilities::notify!("Generics might be missed here {:?}", ty);
			None
		}
	} else {
		None
	}
}

pub(crate) fn tuple_like(id: TypeId, types: &TypeStore, environment: &crate::Environment) -> bool {
	// TODO should be `ObjectNature::AnonymousObjectType` or something else
	let ty = types.get_type_by_id(id);
	if let Type::Object(ObjectNature::RealDeal) = ty {
		environment
			.get_chain_of_info()
			.any(|info| info.prototypes.get(&id).is_some_and(|p| *p == TypeId::ARRAY_TYPE))
	} else if let Type::AliasTo { to, .. } = ty {
		tuple_like(*to, types, environment)
	} else {
		false
	}
}

pub(crate) fn _unfold_tuple(_ty: TypeId) -> TypeId {
	// return Type::PropertyOf()
	todo!()
}

pub(crate) fn _assign_to_tuple(_ty: TypeId) -> TypeId {
	todo!()
	// if let PropertyKey::Type(slice) =
}

fn get_array_length(
	ctx: &impl InformationChain,
	on: TypeId,
	types: &TypeStore,
) -> Result<ordered_float::NotNan<f64>, Option<TypeId>> {
	let length_property = properties::PropertyKey::String(std::borrow::Cow::Borrowed("length"));
	let id = get_simple_value(ctx, on, &length_property, types).ok_or(None)?;
	if let Type::Constant(Constant::Number(n)) = types.get_type_by_id(id) {
		Ok(*n)
	} else {
		Err(Some(id))
	}
}

/// TODO name?
#[derive(Clone, Copy, Debug)]
pub enum ArrayItem {
	Member(TypeId),
	Optional(TypeId),
	Wildcard(TypeId),
}

/// WIP
pub(crate) fn as_slice(
	ty: TypeId,
	types: &TypeStore,
	environment: &crate::Environment,
) -> Result<Vec<ArrayItem>, ()> {
	if tuple_like(ty, types, environment) {
		let ty = if let Type::AliasTo { to, .. } = types.get_type_by_id(ty) { *to } else { ty };
		let properties =
			environment.get_chain_of_info().find_map(|info| info.current_properties.get(&ty));
		if let Some(properties) = properties {
			Ok(properties
				.iter()
				.filter_map(|(_, key, value)| {
					let not_length_value = !key.is_equal_to("length");
					not_length_value.then(|| {
						crate::utilities::notify!("key (should be incremental) {:?}", key);
						if key.as_number(types).is_some() {
							if let crate::PropertyValue::ConditionallyExists { .. } = value {
								ArrayItem::Optional(value.as_get_type(types))
							} else {
								ArrayItem::Member(value.as_get_type(types))
							}
						} else {
							ArrayItem::Wildcard(value.as_get_type(types))
						}
					})
				})
				.collect())
		} else {
			crate::utilities::notify!("BAD");
			Err(())
		}
	} else {
		Err(())
	}
}

/// WIP for counting slice indexes
#[derive(EnumFrom, Clone, Copy, Debug)]
pub enum Counter {
	On(usize),
	AddTo(TypeId),
}

impl Counter {
	/// TODO &mut or Self -> Self?
	pub fn increment(&mut self, types: &mut TypeStore) {
		match self {
			Counter::On(value) => {
				*value += 1;
			}
			Counter::AddTo(value) => {
				*value = types.register_type(Type::Constructor(Constructor::BinaryOperator {
					lhs: *value,
					operator: MathematicalAndBitwise::Add,
					rhs: TypeId::ONE,
				}));
			}
		}
	}

	pub fn add_type(&mut self, ty: TypeId, types: &mut TypeStore) {
		let current = self.into_type(types);
		let new = types.register_type(Type::Constructor(Constructor::BinaryOperator {
			lhs: ty,
			operator: MathematicalAndBitwise::Add,
			rhs: current,
		}));
		*self = Counter::AddTo(new);
	}

	pub(crate) fn into_property_key(self) -> properties::PropertyKey<'static> {
		match self {
			Counter::On(value) => properties::PropertyKey::from_usize(value),
			Counter::AddTo(ty) => properties::PropertyKey::Type(ty),
		}
	}

	pub(crate) fn into_type(self, types: &mut TypeStore) -> TypeId {
		match self {
			Counter::On(value) => {
				types.new_constant_type(Constant::Number((value as f64).try_into().unwrap()))
			}
			Counter::AddTo(ty) => ty,
		}
	}
}

pub(crate) mod helpers {
	use super::{
		get_constraint, subtyping, Constructor, GenericArguments, GenericChain, InformationChain,
		PartiallyAppliedGenerics, PolyNature, Type, TypeId, TypeStore,
	};

	/// To fill in for TSC behavior for mapped types
	#[must_use]
	pub fn references_key_of(id: TypeId, types: &TypeStore) -> bool {
		match types.get_type_by_id(id) {
			Type::AliasTo { to, .. } => references_key_of(*to, types),
			Type::Or(lhs, rhs) | Type::And(lhs, rhs) => {
				references_key_of(*lhs, types) || references_key_of(*rhs, types)
			}
			Type::RootPolyType(c) => references_key_of(c.get_constraint(), types),
			Type::Constructor(c) => {
				if let Constructor::KeyOf(..) = c {
					true
				} else if let Constructor::BinaryOperator { lhs, rhs, operator: _ } = c {
					references_key_of(*lhs, types) || references_key_of(*rhs, types)
				} else {
					// TODO might have missed something here
					false
				}
			}
			Type::PartiallyAppliedGenerics(a) => {
				if let GenericArguments::ExplicitRestrictions(ref e) = a.arguments {
					e.0.iter().any(|(_, (lhs, _))| references_key_of(*lhs, types))
				} else {
					false
				}
			}
			Type::Interface { .. }
			| Type::Class { .. }
			| Type::Constant(_)
			| Type::Narrowed { .. }
			| Type::FunctionReference(_)
			| Type::Object(_)
			| Type::SpecialObject(_) => false,
		}
	}

	#[allow(clippy::match_like_matches_macro)]
	#[must_use]
	pub fn _type_is_error(ty: TypeId, types: &TypeStore) -> bool {
		if ty == TypeId::ERROR_TYPE {
			true
		} else if let Type::RootPolyType(PolyNature::Error(_)) = types.get_type_by_id(ty) {
			true
		} else {
			false
		}
	}

	/// TODO want to skip mapped generics because that would break subtyping
	#[must_use]
	pub fn get_conditional(ty: TypeId, types: &TypeStore) -> Option<(TypeId, TypeId, TypeId)> {
		match types.get_type_by_id(ty) {
			Type::Constructor(crate::types::Constructor::ConditionalResult {
				condition,
				truthy_result,
				otherwise_result,
				result_union: _,
			}) => Some((*condition, *truthy_result, *otherwise_result)),
			Type::Or(left, right) => Some((TypeId::OPEN_BOOLEAN_TYPE, *left, *right)),
			// For reasons !
			Type::RootPolyType(PolyNature::MappedGeneric { .. }) => None,
			_ => {
				if let Some(constraint) = get_constraint(ty, types) {
					get_conditional(constraint, types)
				} else {
					None
				}
			}
		}
	}

	/// TODO wip
	#[must_use]
	pub fn is_pseudo_continous((ty, generics): (TypeId, GenericChain), types: &TypeStore) -> bool {
		if let TypeId::NUMBER_TYPE | TypeId::STRING_TYPE = ty {
			true
		} else if let Some(arg) = generics.as_ref().and_then(|args| args.get_single_argument(ty)) {
			is_pseudo_continous((arg, generics), types)
		} else {
			let ty = types.get_type_by_id(ty);
			if let Type::Or(left, right) = ty {
				is_pseudo_continous((*left, generics), types)
					|| is_pseudo_continous((*right, generics), types)
			} else if let Type::And(left, right) = ty {
				is_pseudo_continous((*left, generics), types)
					&& is_pseudo_continous((*right, generics), types)
			} else if let Type::RootPolyType(PolyNature::MappedGeneric { extends, .. }) = ty {
				is_pseudo_continous((*extends, generics), types)
			} else {
				false
			}
		}
	}

	#[must_use]
	pub fn is_inferrable_type(ty: TypeId) -> bool {
		matches!(ty, TypeId::ANY_TO_INFER_TYPE | TypeId::OBJECT_TYPE)
	}

	/// For quick checking
	#[must_use]
	pub fn simple_subtype(
		expr_ty: TypeId,
		to_satisfy: TypeId,
		information: &impl InformationChain,
		types: &TypeStore,
	) -> bool {
		let mut state = subtyping::State {
			already_checked: Default::default(),
			mode: Default::default(),
			contributions: Default::default(),
			others: subtyping::SubTypingOptions { allow_errors: true },
			object_constraints: None,
		};

		subtyping::type_is_subtype(to_satisfy, expr_ty, &mut state, information, types).is_subtype()
	}

	// unfolds narrowing
	pub fn get_origin(ty: TypeId, types: &TypeStore) -> TypeId {
		if let Type::Narrowed { from, .. } = types.get_type_by_id(ty) {
			// Hopefully don't have a nested from
			*from
		} else {
			ty
		}
	}

	/// Temp fix for equality of narrowing stuff
	pub fn is_not_of_constant(ty: TypeId, types: &TypeStore) -> bool {
		if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::NOT_RESTRICTION,
			arguments,
		}) = types.get_type_by_id(ty)
		{
			let inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
			matches!(types.get_type_by_id(inner), Type::Constant(_))
		} else {
			false
		}
	}
}
