//! Contains function things:
//! - [FunctionPointer], [InternalFunctionId] used to determine function origin
//! - [FunctionType] the definition of functions and the return types stuff

use derive_enum_from_into::{EnumFrom, EnumTryInto};

use source_map::Span;

use crate::{
	context::FunctionId,
	errors::TypeStringRepresentation,
	events::RootReference,
	types::{
		poly_types::{ResolveGenerics, TypeArguments},
		TypeId,
	},
	CheckingData,
};

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
// pub type TypeReferenceFunctionType = FunctionType<InterfaceFunction>;

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

/// TODO spread should of tuples should expand into `NonSpread`
/// TODO spread for non heterogenous things
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
#[non_exhaustive]
pub enum SynthesizedArgument {
	/// This is the get value of a argument
	NonSpread { ty: TypeId, position: Span },
	// TODO
	// Spread(Instance),
}

impl SynthesizedArgument {
	pub(crate) fn get_position(&self) -> Span {
		match self {
			SynthesizedArgument::NonSpread { ty: _, position } => position.clone(),
		}
	}

	pub(crate) fn into_type(&self) -> Result<TypeId, ()> {
		match self {
			SynthesizedArgument::NonSpread { ty, position: _ } => Ok(*ty),
		}
	}
}

// For when SynthesizedFunctionArgument are held on dependent types
impl ResolveGenerics for SynthesizedArgument {
	fn resolve_generics<T: crate::FSResolver>(
		self,
		type_arguments: &TypeArguments,
		checking_data: &mut CheckingData<T>,
	) -> Self {
		match self {
			SynthesizedArgument::NonSpread { ty, position: pos } => {
				todo!()
				// SynthesizedFunctionArgument::NonSpread(
				//     non_spread.resolve_generics(type_arguments, checking_data),
				// )
			} // SynthesizedFunctionArgument::Spread(_) => todo!(),
		}
	}
}

/// Errors from trying to call a function
pub enum FunctionCallingError {
	InvalidArgumentType {
		parameter_type: TypeStringRepresentation,
		argument_type: TypeStringRepresentation,
		argument_position: Span,
		parameter_position: Span,
		restriction: Option<(Span, TypeStringRepresentation)>,
	},
	MissingArgument {
		parameter_pos: Span,
	},
	ExtraArguments {
		count: usize,
		position: Span,
	},
	NotCallable {
		calling: TypeStringRepresentation,
	},
	ReferenceRestrictionDoesNotMatch {
		reference: RootReference,
		requirement: TypeStringRepresentation,
		found: TypeStringRepresentation,
	},
}
