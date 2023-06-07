use crate::types::poly_types::TypeArguments;
use crate::{types::poly_types::ResolveGenerics, CheckingData};

use super::TypeId;

/// Intermediate type used for array destructuring and iteration (for of)
pub(crate) enum IndexableReturnExpression {
	SingularType(TypeId),
	/// Each element can have a different type. Represented in TS as "Tuples"
	Heterogeneous(Box<[TypeId]>),
}

impl IndexableReturnExpression {
	/// Will return None if index is out of bound on a tuple.
	/// TODO array destructuring can be longer than the array. Maybe returning a union with undefined...?
	pub(crate) fn get_type_at_index(&self, index: usize) -> Option<TypeId> {
		match self {
			IndexableReturnExpression::SingularType(ty) => Some(ty.clone()),
			IndexableReturnExpression::Heterogeneous(types) => types.get(index).cloned(),
		}
	}

	pub(crate) fn get_iterator_type(&self) -> TypeId {
		match self {
			IndexableReturnExpression::SingularType(ty) => ty.clone(),
			IndexableReturnExpression::Heterogeneous(_) => {
				todo!("Probably need to create a type union here")
			}
		}
	}
}

impl ResolveGenerics for IndexableReturnExpression {
	fn resolve_generics<T: crate::FSResolver>(
		self,
		type_arguments: &TypeArguments,
		checking_data: &mut CheckingData<T>,
	) -> Self {
		todo!()
		// match self {
		//     IndexableReturnExpression::SingularType(ty) => {
		//         IndexableReturnExpression::SingularType(ResolveGenerics::resolve_generics(
		//             ty,
		//             type_arguments,
		//             checking_data,
		//         ))
		//     }
		//     IndexableReturnExpression::Heterogeneous(_) => todo!(),
		// }
	}
}
