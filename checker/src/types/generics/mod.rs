//! Note that this not the conventional "dependent type theory" **in dependent means dependent on a term**.
//!
//! Here dependent means the type depends on another type or condition (sometimes called type constructors)

pub mod contributions;
pub mod generic_type_arguments;
pub mod generic_type_parameters;
pub mod substitution;

pub(crate) use generic_type_arguments::FunctionTypeArguments;
pub use generic_type_parameters::*;
