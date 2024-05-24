use std::{fmt::Debug, iter::FromIterator};

use crate::TypeId;

// Encompasses both generic types
#[derive(Default, Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub struct GenericTypeParameters(pub Vec<GenericTypeParameter>);

impl GenericTypeParameters {
	#[must_use]
	pub fn as_option(&self) -> Option<()> {
		todo!()
		// let borrow = self.0.borrow();
		// (!borrow.is_empty()).then(|| borrow)
	}
}

impl FromIterator<GenericTypeParameter> for GenericTypeParameters {
	fn from_iter<T: IntoIterator<Item = GenericTypeParameter>>(iter: T) -> Self {
		Self(iter.into_iter().collect())
	}
}

impl From<Vec<GenericTypeParameter>> for GenericTypeParameters {
	fn from(parameters: Vec<GenericTypeParameter>) -> Self {
		Self(parameters)
	}
}

/// A generic type parameter. Used in verifying generic constructs.
/// Ids used for parameter subtyping
/// TODO could redesign later
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct GenericTypeParameter {
	/// name is only for error displaying
	pub name: String,
	// Using its associated [`Type`], its restriction can be found
	pub type_id: TypeId,
	pub default: Option<TypeId>,
}

// impl ResolveGenerics<Type> for GenericTypeParameter {
//     fn resolve_generics<T: crate::FSResolver>(
//         self,
//         type_arguments: &GenericArgumentEnvironment,
//         checking_data: &mut CheckingData<T>,
//         environment: &crate::Environment,
//     ) -> Type {
//         get_type_from_arguments_using_generic_type_parameter_id(
//             self.get_generic_parameter_type_id(),
//             type_arguments,
//         )
//     }
// }

impl PartialEq for GenericTypeParameter {
	/// For type subtyping
	fn eq(&self, other: &Self) -> bool {
		let type_ids_equal = self.type_id == other.type_id;
		// TODO fallback to checking extends
		type_ids_equal || todo!("Check extends")
	}
}

// impl TypeDisplay for GenericTypeParameter {
// 	// fn fmt(
// 	// 	&self,
// 	// 	buf: &mut String,
// 	// 	_indent: usize,
// 	// 	_cycles: &mut collections::HashSet<usize>,
// 	// 	_environment: &GeneralContext
// 	// ) {
// 	// 	buf.push_str(&self.name)
// 	// }
// }

// impl ResolveGenerics<Type> for InferredGenericTypeParameter {
//     fn resolve_generics<T: crate::FSResolver>(
//         self,
//         type_arguments: &GenericArgumentEnvironment,
//         checking_data: &mut CheckingData<T>,
//         environment: &crate::Environment,
//     ) -> Type {

//     }
// }

// impl TypeDisplay for InferredGenericTypeParameter {
//     fn fmt(
//         &self,
//         buf: &mut String,
//         _indent: usize,
//         cycles: &mut std::collections::HashSet<usize>,
//         memory: &Memory,
//     ) {
//         cycles.insert((self as *const _) as usize);
//         buf.push_str(&format!("T{:?}", self.id))
//     }
// }
