//! Generics = dependent function types

pub mod generic_structure;
pub mod generic_type_arguments;
pub mod generic_type_parameters;

use std::collections::HashMap;

pub use generic_structure::*;
pub(crate) use generic_type_arguments::FunctionTypeArguments;
pub use generic_type_parameters::*;

use crate::{types::FunctionType, CheckingData, FunctionId};

pub(crate) struct UnmatchedGenericsError;

/// Checking and seeding
///
/// TODO bad place
pub(crate) struct SeedingContext {
	pub type_arguments: FunctionTypeArguments,
	/// For functions that might be called. Ones where they aren't called end being substituted
	/// and end up on environment
	pub locally_held_functions: HashMap<FunctionId, FunctionType>,
}
