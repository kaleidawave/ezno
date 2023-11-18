use super::variables::VariableMutability;
use crate::{
	behavior::variables::VariableOrImport, context::facts::Facts, Diagnostic, TypeId, VariableId,
};
use derive_enum_from_into::EnumFrom;
use source_map::Span;
use std::{collections::HashMap, default, path::PathBuf};

#[derive(Debug)]
pub struct NamePair<'a> {
	pub value: &'a str,
	pub r#as: &'a str,
	pub position: Span,
}

pub enum ImportKind<'a, T: Iterator<Item = NamePair<'a>>> {
	Parts(T),
	All {
		under: &'a str,
		position: Span,
	},
	/// From `export * from ...`
	Everything,
}

pub struct SynthesisedModule<M> {
	pub content: M,
	pub exported: Exported,
	/// TODO ...
	pub facts: Facts,
}

/// TODO tidy
#[derive(Clone, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub struct Exported {
	pub default: Option<TypeId>,
	pub named: Vec<(String, (VariableId, VariableMutability))>,
	pub named_types: Vec<(String, TypeId)>,
}

pub enum TypeOrVariable {
	ExportedVariable((VariableId, VariableMutability)),
	Type(TypeId),
}

impl Exported {
	pub(crate) fn get_export(&self, want: &str) -> Option<TypeOrVariable> {
		self.named
			.iter()
			.find_map(|(export, value)| {
				(export == want)
					.then_some(TypeOrVariable::ExportedVariable((value.0, value.1.clone())))
			})
			.or_else(|| {
				self.named_types.iter().find_map(|(export, value)| {
					(export == want).then_some(TypeOrVariable::Type(*value))
				})
			})
	}
}

/// After a syntax error
pub struct InvalidModule;

pub type FinalModule<M> = Result<SynthesisedModule<M>, InvalidModule>;
