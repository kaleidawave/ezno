//! Generics = dependent function types

pub mod generic_structure;
pub mod generic_type_arguments;
pub mod generic_type_parameters;

pub use generic_structure::*;
pub(crate) use generic_type_arguments::FunctionTypeArguments;
pub use generic_type_parameters::*;
