use super::variables::VariableMutability;
use crate::{context::facts::Facts, Diagnostic, TypeId, VariableId, VariableOrImport};
use derive_enum_from_into::EnumFrom;
use source_map::Span;
use std::{collections::HashMap, default, path::PathBuf};

pub struct NamePair<'a> {
	pub value: &'a str,
	pub r#as: &'a str,
	pub position: Span,
}

pub enum ImportKind<'a, T: Iterator<Item = NamePair<'a>>> {
	Parts(T),
	All { under: &'a str, position: Span },
	SideEffect,
}

pub struct SynthesisedModule<M> {
	pub content: M,
	pub exported: Exported,
	/// TODO ...
	pub facts: Facts,
}

#[derive(Clone, Debug, Default, binary_serialize_derive::BinarySerializable)]
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

/// After a syntax error
pub struct InvalidModule;

pub type FinalModule<M> = Result<SynthesisedModule<M>, InvalidModule>;
