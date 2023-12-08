pub mod calling;
pub mod casts;
pub mod classes;
pub mod functions;
pub mod others;
pub mod poly_types;
pub mod printing;
pub mod properties;
pub mod store;
pub mod subtyping;
mod terms;

use derive_debug_extras::DebugExtras;

pub(crate) use poly_types::substitution::*;

pub(crate) use casts::*;
use source_map::{Span, SpanWithSource};
pub use store::TypeStore;
pub use terms::Constant;

use crate::{
	behavior::{
		functions::{ClosureId, ThisValue},
		objects::SpecialObjects,
		operations::{CanonicalEqualityAndInequality, MathematicalAndBitwise, PureUnary},
	},
	events::RootReference,
	Decidable, Environment,
};

pub use self::functions::*;
use self::{
	poly_types::{generic_type_arguments::StructureGenericArguments, SeedingContext},
	properties::PropertyKey,
	subtyping::type_is_subtype,
};
use crate::FunctionId;

/// References [Type]
///
/// TODO maybe u32 or u64
/// TODO maybe on environment rather than here
#[derive(PartialEq, Eq, Clone, Copy, DebugExtras, Hash)]
pub struct TypeId(pub(crate) u16);

// TODO ids as macro as to not do in [crate::RootEnvironment]
impl TypeId {
	/// Not to be confused with [`TypeId::NEVER_TYPE`]
	pub const ERROR_TYPE: Self = Self(0);
	pub const UNIMPLEMENTED_ERROR_TYPE: TypeId = TypeId::ERROR_TYPE;

	pub const NEVER_TYPE: Self = Self(1);

	pub const ANY_TYPE: Self = Self(2);

	pub const BOOLEAN_TYPE: Self = Self(3);
	pub const NUMBER_TYPE: Self = Self(4);
	pub const STRING_TYPE: Self = Self(5);

	pub const UNDEFINED_TYPE: Self = Self(6);
	pub const NULL_TYPE: Self = Self(7);

	/// This is the inner version
	pub const ARRAY_TYPE: Self = Self(8);

	/// Used for Array and Promise
	pub const T_TYPE: Self = Self(9);

	pub const OBJECT_TYPE: Self = Self(10);
	pub const FUNCTION_TYPE: Self = Self(11);
	pub const REGEXP_TYPE: Self = Self(12);
	pub const SYMBOL_TYPE: Self = Self(13);

	/// For more direct stuff and the rules
	pub const TRUE: Self = Self(14);
	pub const FALSE: Self = Self(15);
	pub const ZERO: Self = Self(16);
	pub const ONE: Self = Self(17);
	pub const NAN_TYPE: Self = Self(18);

	/// TODO remove. Shortcut for inferred this
	pub const ANY_INFERRED_FREE_THIS: Self = Self(19);
	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target>
	pub const NEW_TARGET_ARG: Self = Self(20);

	pub const SYMBOL_TO_PRIMITIVE: Self = Self(21);

	pub(crate) const INTERNAL_TYPE_COUNT: usize = 22;
}

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum Type {
	/// For
	/// - `extends` interface part (for multiple it to is a `Type::And`)
	/// - Can be used for subtypes (aka N aliases number then more types on top)
	/// - **Does not imply .prototype = **
	AliasTo {
		to: TypeId,
		name: String,
		parameters: Option<Vec<TypeId>>,
	},
	And(TypeId, TypeId),
	Or(TypeId, TypeId),
	RootPolyType(PolyNature),
	/// Also a "Substituted constructor type"
	Constructor(Constructor),
	/// For number and other rooted types
	///
	/// Although they all alias Object
	Interface {
		name: String,
		// Whether only values under this type can be matched
		nominal: bool,
		parameters: Option<Vec<TypeId>>,
	},
	/// *Dependent equality types*
	Constant(crate::Constant),
	/// Represents known functions
	Function(FunctionId, ThisValue),

	/// From a type annotation or .d.ts WITHOUT body. e.g. don't know effects TODO...
	FunctionReference(FunctionId, ThisValue),

	/// Technically could be just a function but...
	Object(ObjectNature),
	SpecialObject(SpecialObjects),
}

/// TODO difference between untyped and typed parameters and what about parameter based for any
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum PolyNature {
	Parameter {
		fixed_to: TypeId,
	},
	Generic {
		name: String,
		eager_fixed: TypeId,
	},
	Open(TypeId),
	/// For functions and for loops where something in the scope can mutate (so not constant)
	/// between runs.
	FreeVariable {
		reference: RootReference,
		based_on: TypeId,
	},
	RecursiveFunction(FunctionId, TypeId),
	// Object
}

// TODO
#[must_use]
pub fn is_primitive(ty: TypeId, types: &TypeStore) -> bool {
	if matches!(ty, TypeId::BOOLEAN_TYPE | TypeId::NUMBER_TYPE | TypeId::STRING_TYPE) {
		return true;
	}
	false
}

#[derive(Copy, Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum ObjectNature {
	/// Actual allocated object
	RealDeal,
	/// From `x: { a: number }` etc
	AnonymousTypeAnnotation,
}

impl Type {
	pub(crate) fn get_parameters(&self) -> Option<Vec<TypeId>> {
		if let Type::Interface { parameters, .. } | Type::AliasTo { parameters, .. } = self {
			parameters.clone()
		} else {
			None
		}
	}

	/// TODO return is poly
	pub(crate) fn is_dependent(&self) -> bool {
		#[allow(clippy::match_same_arms)]
		match self {
			// TODO
			Type::Constructor(Constructor::StructureGenerics(..)) => false,
			// Fine
			Type::Constructor(_) | Type::RootPolyType(_) => true,
			// TODO what about if left or right
			Type::And(_, _) | Type::Or(_, _) => false,
			// TODO what about if it aliases
			Type::AliasTo { .. } | Type::Interface { .. } => {
				// TODO not sure
				false
			}
			Type::Constant(_)
			| Type::Function(..)
			| Type::FunctionReference(..)
			| Type::Object(_) => false,
			Type::SpecialObject(_) => todo!(),
		}
	}
}

/// TODO work in progress types
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
	TypeOperator(TypeOperator),
	TypeRelationOperator(TypeRelationOperator),
	/// TODO constraint is res
	ConditionalResult {
		/// TODO this can only be poly types
		condition: TypeId,
		truthy_result: TypeId,
		else_result: TypeId,
		result_union: TypeId,
	},
	/// output of a function where on is dependent (or sometimes for const functions one of `with` is dependent)
	Image {
		on: TypeId,
		// TODO I don't think this is necessary, maybe for debugging. In such case should be an Rc to share with events
		with: Box<[SynthesisedArgument]>,
		result: TypeId,
	},
	Property {
		on: TypeId,
		under: PropertyKey<'static>,
		result: TypeId,
	},
	/// Might not be best place but okay.
	StructureGenerics(StructureGenerics),
}

impl Constructor {
	fn get_base(&self) -> Option<TypeId> {
		match self {
			Constructor::ConditionalResult { result_union: result, .. }
			| Constructor::Image { result, .. } => Some(*result),
			Constructor::BinaryOperator { .. }
			| Constructor::CanonicalRelationOperator { .. }
			| Constructor::UnaryOperator { .. }
			| Constructor::TypeOperator(_)
			| Constructor::TypeRelationOperator(_)
			| Constructor::Property { .. }
			| Constructor::StructureGenerics(_) => None,
		}
	}
}

/// Closed over arguments
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct StructureGenerics {
	pub on: TypeId,
	pub arguments: StructureGenericArguments,
}

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum TypeOperator {
	PrototypeOf(TypeId),
	PrimitiveTypeName(TypeId),
}

/// TODO instance of?
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum TypeRelationOperator {
	Extends { ty: TypeId, extends: TypeId },
}

pub(crate) fn new_logical_or_type(lhs: TypeId, rhs: TypeId, types: &mut TypeStore) -> TypeId {
	types.new_conditional_type(lhs, lhs, rhs)
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
			| Type::Interface { .. } => {
				// TODO some of these case are known
				Decidable::Unknown(id)
			}
			Type::Function(..)
			| Type::FunctionReference(..)
			| Type::SpecialObject(_)
			| Type::Object(_) => Decidable::Known(true),
			Type::Constant(cst) => {
				// TODO strict casts
				Decidable::Known(cast_as_boolean(cst, false).unwrap())
			}
		}
	}
}

/// TODO `add_property_restrictions` via const generics
pub struct BasicEquality {
	pub add_property_restrictions: bool,
	pub position: SpanWithSource,
}

/// For subtyping
pub trait SubtypeBehavior {
	/// If false will error out if T >: U
	const INFER_GENERICS: bool;

	fn set_type_argument(
		&mut self,
		parameter: TypeId,
		value: TypeId,
		restriction_mode: bool,
	) -> Result<(), NonEqualityReason>;

	fn add_property_restrictions(&self) -> bool;

	fn add_function_restriction(
		&mut self,
		environment: &mut Environment,
		function_id: FunctionId,
		function_type: FunctionType,
	);

	// TODO
	// object reference type needs to meet constraint
	// LHS is dependent + RHS argument
}

impl SubtypeBehavior for SeedingContext {
	// Want to match/infer, so false here
	const INFER_GENERICS: bool = true;

	/// Does not check thingy
	fn set_type_argument(
		&mut self,
		parameter: TypeId,
		value: TypeId,
		restriction_mode: bool,
	) -> Result<(), NonEqualityReason> {
		// TODO
		let (parameter_pos, parameter_idx) = self.argument_position_and_parameter_idx;
		self.set_id(parameter, (value, parameter_pos, parameter_idx), restriction_mode);
		Ok(())
	}

	fn add_property_restrictions(&self) -> bool {
		false
	}

	fn add_function_restriction(
		&mut self,
		_environment: &mut Environment,
		function_id: FunctionId,
		function_type: FunctionType,
	) {
		self.locally_held_functions.insert(function_id, function_type);
	}
}

impl SubtypeBehavior for BasicEquality {
	const INFER_GENERICS: bool = false;

	fn set_type_argument(
		&mut self,
		_parameter: TypeId,
		_value: TypeId,
		_restriction_mode: bool,
	) -> Result<(), NonEqualityReason> {
		Ok(())
	}

	fn add_property_restrictions(&self) -> bool {
		self.add_property_restrictions
	}

	fn add_function_restriction(
		&mut self,
		environment: &mut Environment,
		function_id: FunctionId,
		function_type: FunctionType,
	) {
		let result = environment
			.deferred_function_constraints
			.insert(function_id, (function_type, self.position));

		debug_assert!(result.is_none());
	}
}

#[derive(Debug)]
pub enum SubTypeResult {
	IsSubType,
	IsNotSubType(NonEqualityReason),
}

// impl SubTypeResult {
// 	type Error = NonEqualityReason;
// }

// TODO implement `?` on SupertypeResult

// TODO maybe positions and extra information here
// SomeLiteralMismatch
// GenericParameterCollision
#[derive(Debug)]
pub enum NonEqualityReason {
	Mismatch,
	PropertiesInvalid {
		errors: Vec<(PropertyKey<'static>, PropertyError)>,
	},
	TooStrict,
	/// TODO more information
	MissingParameter,
	GenericParameterMismatch,
}

#[derive(Debug)]
pub enum PropertyError {
	Missing,
	Invalid { expected: TypeId, found: TypeId, mismatch: NonEqualityReason },
}

/// TODO temp fix for printing
pub(crate) fn is_explicit_generic(on: TypeId, types: &TypeStore) -> bool {
	if let Type::RootPolyType(PolyNature::Generic { .. }) = types.get_type_by_id(on) {
		true
	} else if let Type::Constructor(Constructor::Property { on, under, result }) =
		types.get_type_by_id(on)
	{
		is_explicit_generic(*on, types)
			|| matches!(under, PropertyKey::Type(under) if is_explicit_generic(*under, types))
	} else {
		false
	}
}
