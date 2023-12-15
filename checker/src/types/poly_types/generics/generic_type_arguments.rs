//! Contains wrappers for generic type arguments and their wrapping environments
//! TODO Some of these are a bit overkill and don't need wrapping objects **AND THEY BREAK FINALIZE THINGS REQUIRE CLONING**

use crate::{
	behavior::functions::{ClosureChain, ClosureId},
	context::facts::Facts,
	types::TypeStore,
	CheckingData, TypeId,
};

use map_vec::Map as SmallMap;
use source_map::{Span, SpanWithSource};

use std::{fmt::Debug, iter::FromIterator};

use super::{GenericStructureTypeArgument, GenericStructureTypeArguments};

impl FromIterator<GenericStructureTypeArgument> for GenericStructureTypeArguments {
	fn from_iter<I: IntoIterator<Item = GenericStructureTypeArgument>>(iter: I) -> Self {
		Self(iter.into_iter().collect())
	}
}

// impl CurriedGenericsValue {
// 	pub(crate) fn as_type(&self) -> TypeId {
// 		let (CurriedGenericsValue::Type(value) | CurriedGenericsValue::StructureGeneric(value)) =
// 			self;
// 		value
// 	}
// }

// #[derive(Debug, Clone)]
// pub enum CurriedGenericsValue {
// 	Type(TypeId),
// 	/// TODO this is different in that it can differ maybe... or is that closure values
// 	StructureGeneric(TypeId),
// }

// impl From<GenericStructureTypeArguments> for CurriedFunctionTypeArguments {
// 	fn from(args: GenericStructureTypeArguments) -> Self {
// 		Self(
// 			args.0
// 				.clone()
// 				.into_iter()
// 				.map(|GenericStructureTypeArgument { ref matching_id, ref ty, .. }| {
// 					(
// 						matching_id.clone().into(),
// 						todo!(),
// 						// CurriedGenericsValue::StructureGeneric(match ty.clone() {
// 						// 	super::GenericStructureArgumentValue::Type(ty) => ty,
// 						// 	super::GenericStructureArgumentValue::Unknown => todo!(),
// 						// }),
// 					)
// 				})
// 				.collect(),
// 		)
// 	}
// }

/// TODO working out environment thingy
#[derive(Debug)]
pub(crate) struct FunctionTypeArguments {
	pub structure_arguments: Option<StructureGenericArguments>,
	/// Might not be full
	pub local_arguments: SmallMap<TypeId, (TypeId, SpanWithSource)>,
	pub closure_id: Option<ClosureId>,
}

impl FunctionTypeArguments {
	pub(crate) fn set_id_from_reference(&mut self, id: TypeId, value: TypeId, types: &TypeStore) {
		self.local_arguments.insert(id, (value, SpanWithSource::NULL_SPAN));
	}

	pub(crate) fn new() -> Self {
		Self {
			structure_arguments: Default::default(),
			local_arguments: SmallMap::new(),
			closure_id: Default::default(),
		}
	}
}

pub(crate) trait TypeArgumentStore {
	/// Gets the value, not the constraint
	fn get_structure_argument(&self, id: TypeId) -> Option<TypeId>;

	fn get_local_argument(&self, id: TypeId) -> Option<TypeId>;

	fn get_argument(&self, id: TypeId) -> Option<TypeId> {
		self.get_local_argument(id).or_else(|| self.get_structure_argument(id))
	}

	fn get_structural_closures(&self) -> Option<Vec<ClosureId>>;

	fn to_structural_generic_arguments(&self) -> StructureGenericArguments;

	fn is_empty(&self) -> bool;
}

impl ClosureChain for FunctionTypeArguments {
	fn get_fact_from_closure<T, R>(&self, fact: &Facts, mut cb: T) -> Option<R>
	where
		T: Fn(ClosureId) -> Option<R>,
	{
		if let Some(ref closure_id) = self.closure_id {
			let res = cb(*closure_id);
			if res.is_some() {
				return res;
			}
		}
		if let Some(ref parent) = self.structure_arguments {
			for closure_id in &parent.closures {
				let res = cb(*closure_id);
				if res.is_some() {
					return res;
				}
			}
		}
		None
	}
}

impl TypeArgumentStore for FunctionTypeArguments {
	fn get_structure_argument(&self, id: TypeId) -> Option<TypeId> {
		self.structure_arguments.as_ref().and_then(|args| args.get_structure_argument(id))
	}

	fn get_local_argument(&self, id: TypeId) -> Option<TypeId> {
		self.local_arguments.get(&id).map(|(ty, _)| *ty)
	}

	fn get_structural_closures(&self) -> Option<Vec<ClosureId>> {
		None
	}

	fn to_structural_generic_arguments(&self) -> StructureGenericArguments {
		// self.structure_arguments.clone()
		match self.structure_arguments {
			Some(ref parent) => {
				let mut merged = parent.type_arguments.clone();
				let iter = self.local_arguments.clone();
				merged.extend(iter);

				StructureGenericArguments {
					type_arguments: merged,
					closures: parent.closures.iter().copied().chain(self.closure_id).collect(),
				}
			}
			None => StructureGenericArguments {
				type_arguments: self.local_arguments.clone(),
				closures: self.closure_id.into_iter().collect(),
			},
		}
	}

	fn is_empty(&self) -> bool {
		self.closure_id.is_none() && self.local_arguments.len() == 0
	}
}

// TODO temp: for type alias specialisation
impl TypeArgumentStore for SmallMap<TypeId, (TypeId, SpanWithSource)> {
	fn get_structure_argument(&self, id: TypeId) -> Option<TypeId> {
		self.get(&id).map(|(value, pos)| *value)
	}

	fn get_local_argument(&self, id: TypeId) -> Option<TypeId> {
		// ???
		self.get_structure_argument(id)
	}

	fn get_structural_closures(&self) -> Option<Vec<ClosureId>> {
		None
	}

	fn to_structural_generic_arguments(&self) -> StructureGenericArguments {
		crate::utils::notify!("to_structural_generic_arguments shouldn't be needed");
		StructureGenericArguments { type_arguments: self.clone(), closures: Vec::new() }
	}

	fn is_empty(&self) -> bool {
		false
	}
}

/// These are curried between structures
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct StructureGenericArguments {
	pub type_arguments: map_vec::Map<TypeId, (TypeId, SpanWithSource)>,
	pub closures: Vec<ClosureId>,
}

impl TypeArgumentStore for StructureGenericArguments {
	fn get_structure_argument(&self, id: TypeId) -> Option<TypeId> {
		self.type_arguments.get(&id).map(|(ty, _)| *ty)
	}

	fn get_local_argument(&self, id: TypeId) -> Option<TypeId> {
		None
	}

	fn get_structural_closures(&self) -> Option<Vec<ClosureId>> {
		Some(self.closures.clone())
	}

	fn to_structural_generic_arguments(&self) -> StructureGenericArguments {
		self.clone()
	}

	fn is_empty(&self) -> bool {
		self.closures.is_empty() && self.type_arguments.len() == 0
	}
}
