//! Contains function things:
//! - [FunctionPointer], [InternalFunctionId] used to determine function origin
//! - [FunctionType] the definition of functions and the return types stuff

use derive_enum_from_into::{EnumFrom, EnumTryInto};

use crate::{types::TypeId, FunctionId};

use std::{
	fmt::Debug,
	sync::atomic::{AtomicU16, Ordering},
};

static INTERNAL_FUNCTION_COUNTER: AtomicU16 = AtomicU16::new(1000);
static AUTO_CONSTRUCTOR_COUNTER: AtomicU16 = AtomicU16::new(0);

/// A id of a function that is internal
#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct InternalFunctionId(u16);

impl InternalFunctionId {
	pub fn new() -> Self {
		Self(INTERNAL_FUNCTION_COUNTER.fetch_add(1, Ordering::SeqCst))
	}

	pub(crate) const fn new_from_id(id: u16) -> Self {
		Self(id)
	}

	pub(crate) fn get_id(&self) -> u16 {
		self.0
	}

	// TODO expose more that may be useful for plugins
	pub const ASSERT_TYPE: Self = Self(800);
	pub const PRINT_EFFECTS: Self = Self(801);
	pub const DEBUG_ENVIRONMENT: Self = Self(569);
	pub const DEBUG: Self = Self(570);
	pub const PROXY_CONSTRUCTOR: Self = Self(580);
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct AutoConstructorId(pub(crate) u16);

impl AutoConstructorId {
	pub fn new() -> Self {
		Self(AUTO_CONSTRUCTOR_COUNTER.fetch_add(1, Ordering::SeqCst))
	}

	pub(crate) fn get_id(&self) -> u16 {
		self.0
	}
}

/// A reference to a function,
/// Includes spans for where defined and other information specific to the nature of the declaration

impl Eq for FunctionPointer {}

/// The type of `this` that a function has ....?
/// TODO needs for work
#[derive(Clone, Debug)]
pub enum ThisBinding {
	FixedTo(TypeId),
	Dynamic(TypeId),
}

impl ThisBinding {
	pub fn get_type(&self) -> TypeId {
		match self {
			ThisBinding::FixedTo(ty) | ThisBinding::Dynamic(ty) => *ty,
		}
	}
}

// pub type CallableFunctionType = FunctionType<SourceFunction>;
// Non callable
// pub type TypeAnnotationFunctionType = FunctionType<InterfaceFunction>;

/// Used for precise location of functions that bind ids.
/// Unification of internal and source functions
#[derive(
	Clone,
	Debug,
	PartialEq,
	EnumFrom,
	EnumTryInto,
	Hash,
	binary_serialize_derive::BinarySerializable,
)]
pub enum FunctionPointer {
	Function(FunctionId),
	/// Properties without a constructor
	AutoConstructor(AutoConstructorId),
	Internal(InternalFunctionId),
}
