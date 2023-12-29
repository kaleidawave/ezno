use super::variables::VariableMutability;
use crate::{context::facts::Facts, TypeId, VariableId};

use source_map::Span;

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
	/// Mutability purely for the mutation thingy
	pub named: Vec<(String, (VariableId, VariableMutability))>,
	pub named_types: Vec<(String, TypeId)>,
}

pub type ExportedVariable = (VariableId, VariableMutability);

pub enum TypeOrVariable {
	ExportedVariable(),
	Type(TypeId),
}

impl Exported {
	pub(crate) fn get_export(
		&self,
		want: &str,
		type_only: bool,
	) -> (Option<ExportedVariable>, Option<TypeId>) {
		let variable = if type_only {
			None
		} else {
			self.named
				.iter()
				.find_map(|(export, value)| (export == want).then_some((value.0, value.1)))
		};

		let r#type =
			self.named_types.iter().find_map(|(export, value)| (export == want).then_some(*value));

		(variable, r#type)
	}
}

/// After a syntax error
pub struct InvalidModule;

pub type FinalModule<M> = Result<SynthesisedModule<M>, InvalidModule>;
