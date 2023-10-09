use super::generic_type_arguments::FunctionTypeArguments;
use crate::{CheckingData, DiagnosticsContainer, TypeId};

/// A instance of a generic typed object
#[derive(Clone, Debug)]
pub struct GenericStructure {
	pub generic_type: TypeId,
	pub arguments: GenericStructureTypeArguments,
}

#[derive(Clone, Debug, Default)]
pub struct GenericStructureTypeArguments(pub(crate) Vec<GenericStructureTypeArgument>);

#[derive(Clone, Debug)]
pub enum GenericStructureArgumentValue {
	Type(TypeId),
	Unknown,
}

impl From<TypeId> for GenericStructureArgumentValue {
	fn from(ty: TypeId) -> Self {
		Self::Type(ty)
	}
}

impl From<Option<TypeId>> for GenericStructureArgumentValue {
	fn from(ty: Option<TypeId>) -> Self {
		match ty {
			Some(ty) => Self::Type(ty),
			None => Self::Unknown,
		}
	}
}

impl GenericStructureTypeArguments {
	pub(crate) fn get_value_for_id(&self, id: TypeId) -> Option<&GenericStructureTypeArgument> {
		self.0.iter().find(|item| item.matching_id == id)
	}
}

#[derive(Clone, Debug)]
pub struct GenericStructureTypeArgument {
	pub(crate) matching_id: TypeId,
	pub(crate) ty: GenericStructureArgumentValue,
	// TODO fixed type constraint id
	// pub(crate) constraint: Option<Box<Type>>,
}

impl From<GenericStructureTypeArguments> for FunctionTypeArguments {
	fn from(instance_of_generics_arguments: GenericStructureTypeArguments) -> Self {
		todo!()
		// TypeArguments {
		// 	structure_arguments: todo!(),
		// 	local_arguments: todo!(),
		// 	environment: todo!(),
		// }
		//     Cow::Owned(
		//     instance_of_generics_arguments.into(),
		// ))
	}
}
