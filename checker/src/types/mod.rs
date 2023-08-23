pub mod calling;
mod casts;
pub mod functions;
pub mod indexing;
pub mod operations;
pub mod poly_types;
pub mod printing;
pub mod properties;
pub mod store;
pub mod subtyping;
mod terms;

use derive_debug_extras::DebugExtras;

pub(crate) use poly_types::specialization::*;

pub(crate) use casts::*;
pub use store::TypeStore;
pub use terms::Constant;

use crate::{context::InferenceBoundary, events::RootReference, structures::operators::*};

pub use self::functions::*;
use crate::FunctionId;
use std::fmt::Debug;

/// References [Type]
///
/// Not to be confused with [parser::TypeId]
///
/// TODO maybe u32 or u64
/// TODO maybe on environment rather than here
#[derive(PartialEq, Eq, Clone, Copy, DebugExtras, Hash)]
pub struct TypeId(pub(crate) u16);

// TODO ids as macro as to not do in [crate::RootEnvironment]
impl TypeId {
	/// Not to be confused with [TypeId::NEVER_TYPE]
	pub const ERROR_TYPE: Self = Self(0);

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
	/// For arrays
	pub const LENGTH_AS_STRING: Self = Self(19);

	/// For this_arg for type constraints only
	pub const THIS_ARG: Self = Self(20);
	/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target
	pub const NEW_TARGET_ARG: Self = Self(21);

	pub const SYMBOL_TO_PRIMITIVE: Self = Self(22);

	// This exists in TS
	pub const HTML_ELEMENT_TAG_NAME_MAP: Self = Self(23);

	pub(crate) const INTERNAL_TYPE_COUNT: usize = 25;
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
	/// TODO
	And(TypeId, TypeId),
	Or(TypeId, TypeId),
	RootPolyType(PolyNature),
	/// Also a "Specialized constructor type"
	Constructor(Constructor),
	/// For number and other rooted types
	///
	/// Although they all alias Object
	NamedRooted {
		name: String,
		parameters: Option<Vec<TypeId>>,
	},
	Constant(crate::Constant),
	/// TODO function type behind Rc
	Function(FunctionType, FunctionNature),
	Object(ObjectNature), // TODO Proxy,
}

/// TODO difference between untyped and typed parameters and what about parameter based for any
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum PolyNature {
	Parameter {
		fixed_to: PolyPointer,
	},
	Generic {
		name: String,
		/// This can be `Dynamic` for interface hoisting
		eager_fixed: PolyPointer,
	},
	Open(TypeId),
	/// For functions and for loops where something in the scope can mutate (so not constant)
	/// between runs.
	ParentScope {
		reference: RootReference,
		based_on: PolyPointer,
	},
	RecursiveFunction(FunctionId, PolyPointer),
	// Object
}

// TODO
pub fn is_primitive(ty: TypeId, types: &TypeStore) -> bool {
	if matches!(ty, TypeId::BOOLEAN_TYPE | TypeId::NUMBER_TYPE | TypeId::STRING_TYPE) {
		return true;
	}
	return false;
}

#[deprecated(note = "All will be fixed... based of `any`")]
#[derive(Copy, Clone, Debug)]
pub enum PolyPointer {
	Fixed(TypeId),
	Inferred(InferenceBoundary),
}

impl PolyNature {
	pub fn get_fixed_constraint(&self) -> Option<TypeId> {
		match self.get_poly_pointer() {
			PolyPointer::Fixed(ty) => Some(ty),
			PolyPointer::Inferred(_) => None,
		}
	}

	pub fn get_poly_pointer(&self) -> PolyPointer {
		match self {
			Self::Open(ty) => PolyPointer::Fixed(*ty),
			Self::Generic { eager_fixed: pp, .. }
			| Self::ParentScope { based_on: pp, .. }
			| Self::Parameter { fixed_to: pp, .. }
			| Self::RecursiveFunction(_, pp) => *pp,
		}
	}

	pub fn has_fixed_constraint(&self) -> bool {
		self.get_fixed_constraint().is_some()
	}

	pub fn is_open(&self) -> bool {
		matches!(self, Self::Open(_))
	}
}

#[derive(Copy, Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum ObjectNature {
	/// Actual allocated object
	RealDeal,
	/// From `x: { a: number }` etc
	AnonymousTypeAnnotation,
	/// Important that this is different as properties can be mutated
	ModifiableConstraint,
}

impl Type {
	pub(crate) fn get_parameters(&self) -> Option<Vec<TypeId>> {
		if let Type::NamedRooted { parameters, .. } | Type::AliasTo { parameters, .. } = self {
			parameters.clone()
		} else {
			None
		}
	}

	/// TODO return is poly
	pub(crate) fn is_dependent(&self) -> bool {
		match self {
			Type::Constructor(_) | Type::RootPolyType(_) => true,
			Type::And(_, _) | Type::Or(_, _) | Type::AliasTo { .. } | Type::NamedRooted { .. } => {
				// TODO not sure
				false
			}
			Type::Constant(_) | Type::Function(_, _) | Type::Object(_) => false,
		}
	}
}

/// TODO work in progress types
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum Constructor {
	BinaryOperator {
		lhs: TypeId,
		operator: BinaryOperator,
		rhs: TypeId,
	},
	RelationOperator {
		lhs: TypeId,
		operator: RelationOperator,
		rhs: TypeId,
	},
	LogicalOperator {
		lhs: TypeId,
		operator: LogicalOperator,
		rhs: TypeId,
	},
	UnaryOperator {
		operator: UnaryOperator,
		operand: TypeId,
	},
	TypeOperator(TypeOperator),
	TypeRelationOperator(TypeRelationOperator),
	/// TODO constraint is res
	ConditionalTernary {
		/// TODO this can only be poly types
		on: TypeId,
		true_res: TypeId,
		false_res: TypeId,
		// Currently necessary for getting the base == `true_res | false_res`
		result_union: TypeId,
	},
	///
	FunctionResult {
		on: TypeId,
		// TODO I don't think this is necessary, maybe for debugging. In such case should be an Rc to share with events
		with: Box<[SynthesizedArgument]>,
		result: PolyPointer,
	},
	Property {
		on: TypeId,
		under: TypeId,
	},
	/// Should really be base, but we will ignore for now
	StructureGenerics {
		on: TypeId,
		with: map_vec::Map<TypeId, TypeId>,
	},
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
