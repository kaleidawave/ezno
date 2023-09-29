use std::{collections::HashMap, path::PathBuf};

use derive_enum_from_into::EnumFrom;

use crate::{Diagnostic, Variable};

pub struct Module {
	pub path: PathBuf,
}

pub struct SynthesisedModule {
	pub module: Module,
	// TODO this should not be on unchecked module
	// TODO export default
	pub(crate) exported_variables: HashMap<String, Variable>,
}

// TODO
// pub(crate) struct CheckedModule<TStage> { exported }

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

impl SynthesisedModule {
	pub fn get_exports(&self) -> &HashMap<String, Variable> {
		&self.exported_variables
	}
}
