use super::{generic_type_arguments::FunctionTypeArguments, ResolveGenerics};
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

impl ResolveGenerics for GenericStructureArgumentValue {
	fn resolve_generics<T: crate::FSResolver>(
		self,
		type_arguments: &FunctionTypeArguments,
		checking_data: &mut CheckingData<T>,
	) -> Self {
		todo!()
		// match self {
		//     GenericStructureArgumentValue::Type(ty) => GenericStructureArgumentValue::Type(
		//         Box::new(ResolveGenerics::resolve_generics(*ty, type_arguments, checking_data)),
		//     ),
		//     GenericStructureArgumentValue::Unknown => GenericStructureArgumentValue::Unknown,
		// }
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

impl ResolveGenerics for GenericStructureTypeArgument {
	fn resolve_generics<T: crate::FSResolver>(
		self,
		type_arguments: &FunctionTypeArguments,
		checking_data: &mut CheckingData<T>,
	) -> Self {
		Self {
			matching_id: self.matching_id,
			ty: ResolveGenerics::resolve_generics(self.ty, type_arguments, checking_data),
			// constraint: todo!(),
			// Box::new(ResolveGenerics::resolve_generics(
			//     *self.constraint,
			//     type_arguments,
			//     checking_data,
			//     environment,
			// ))
		}
	}
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
