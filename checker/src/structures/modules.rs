use std::{collections::HashMap, default, path::PathBuf};

use derive_enum_from_into::EnumFrom;

use crate::{context::facts::Facts, Diagnostic, TypeId, VariableId, VariableOrImport};

use super::variables::VariableMutability;

pub struct SynthesisedModule<M> {
	pub content: M,
	pub exported: Exported,
	/// TODO ...
	pub facts: Facts,
}

#[derive(Debug, Clone, Default)]
pub struct Exported {
	pub default: Option<TypeId>,
	pub named: HashMap<String, (VariableId, VariableMutability)>,
}

#[derive(Debug, EnumFrom)]
pub enum ModuleFromPathError {
	// ParseError
	ParseError(()),
	PathDoesNotExist(PathBuf),
	NoResolverForExtension(String),
}

impl From<ModuleFromPathError> for Diagnostic {
	fn from(err: ModuleFromPathError) -> Self {
		todo!()
	}
}
