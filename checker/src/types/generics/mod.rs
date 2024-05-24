//! Note that this not the conventional "dependent type theory" **in dependent means dependent on a term**.
//!
//! Here dependent means the type depends on another type or condition (sometimes called type constructors)

pub mod contributions;
pub mod generic_type_arguments;
pub mod generic_type_parameters;
pub mod substitution;

pub(crate) use generic_type_arguments::FunctionTypeArguments;
pub use generic_type_parameters::*;
use source_map::SpanWithSource;

use crate::{types::SubstitutionArguments, TypeId};

pub struct ExplicitTypeArguments(pub crate::Map<TypeId, (TypeId, SpanWithSource)>);

impl ExplicitTypeArguments {
	pub(crate) fn into_substitution_arguments(self) -> SubstitutionArguments<'static> {
		SubstitutionArguments {
			arguments: self.0.into_iter().map(|(key, (v, _pos))| (key, v)).collect(),
			parent: None,
			closures: Vec::new(),
		}
	}
}
