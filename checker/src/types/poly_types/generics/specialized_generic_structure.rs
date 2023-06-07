use super::{generic_type_arguments::TypeArguments, ResolveGenerics};
use crate::{CheckingData, DiagnosticsContainer, TypeId};

/// A instance of a generic typed object
///
/// aka a specialized generic type
#[derive(Clone, Debug)]
pub struct SpecializedGeneric {
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
		type_arguments: &TypeArguments,
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
		type_arguments: &TypeArguments,
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

impl SpecializedGeneric {
	/// Will return a empty if it is valid. Otherwise a [TypeCheckErrors] as to why it is invalid
	/// Will add missing generic type arguments to the arguments list
	pub(crate) fn _validate(&self, error_handler: &mut DiagnosticsContainer) {
		todo!()
		// let underlying_object_type =
		//     if let Type::Object(object_type) = &*self.ty { object_type } else { panic!() };
		// let type_parameters_borrow = underlying_object_type.generic_type_parameters.borrow_mut();
		// if type_parameters_borrow.is_empty() {
		//     return Err(TypeCheckError::TypeHasNoGenericParameters(&self.ty));
		// }
		// dbg!(&type_parameters_borrow.iter().map(|tp| tp.to_string()).collect::<Vec<_>>());
		// Ok(())
	}
}

impl From<GenericStructureTypeArguments> for TypeArguments {
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
