//! Contains wrappers for generic type arguments and their wrapping environments
//! TODO Some of these are a bit overkill and don't need wrapping objects **AND THEY BREAK FINALIZE THINGS REQUIRE CLONING**

use crate::{
	context::information::{InformationChain, LocalInformation},
	features::functions::{ClosureChain, ClosureId},
	types::{LookUpGenericMap, TypeArguments, TypeRestrictions, TypeStore},
	TypeId,
};

use map_vec::Map as SmallMap;
use source_map::{Nullable, SpanWithSource};

use std::{fmt::Debug, iter::FromIterator};

use super::{GenericStructureTypeArgument, GenericStructureTypeArguments};

impl FromIterator<GenericStructureTypeArgument> for GenericStructureTypeArguments {
	fn from_iter<I: IntoIterator<Item = GenericStructureTypeArgument>>(iter: I) -> Self {
		Self(iter.into_iter().collect())
	}
}

/// For when a function is called
#[derive(Debug)]
pub(crate) struct FunctionTypeArguments {
	/// Might not be full
	pub local_arguments: TypeArguments,
	pub closure_id: Vec<ClosureId>,
	pub call_site: SpanWithSource,
}

impl FunctionTypeArguments {
	/// TODO!! explain
	pub(crate) fn set_id_from_event_application(&mut self, id: TypeId, value: TypeId) {
		self.local_arguments.insert(id, value);
	}

	pub(crate) fn new_arguments_for_use_in_loop() -> Self {
		Self {
			local_arguments: SmallMap::new(),
			closure_id: Default::default(),
			call_site: SpanWithSource::NULL,
		}
	}
}

pub(crate) trait TypeArgumentStore {
	fn get_argument<C: InformationChain>(
		&self,
		under: TypeId,
		c: &C,
		types: &mut TypeStore,
	) -> Option<TypeId>;

	fn get_structural_closures(&self) -> Option<Vec<ClosureId>>;

	fn to_structural_generic_arguments(&self) -> StructureGenericArguments;

	fn is_empty(&self) -> bool;
}

impl ClosureChain for FunctionTypeArguments {
	fn get_fact_from_closure<T, R>(&self, _fact: &LocalInformation, cb: T) -> Option<R>
	where
		T: Fn(ClosureId) -> Option<R>,
	{
		for closure_id in &self.closure_id {
			let res = cb(*closure_id);
			if res.is_some() {
				return res;
			}
		}
		None
	}
}

impl TypeArgumentStore for FunctionTypeArguments {
	fn get_structural_closures(&self) -> Option<Vec<ClosureId>> {
		None
	}

	fn to_structural_generic_arguments(&self) -> StructureGenericArguments {
		let type_arguments_with_positions =
			self.local_arguments.iter().map(|(k, v)| (*k, (*v, self.call_site)));

		StructureGenericArguments {
			properties: SmallMap::new(),
			type_restrictions: type_arguments_with_positions.collect(),
			closures: self.closure_id.clone(),
		}
	}

	fn is_empty(&self) -> bool {
		self.closure_id.is_empty() && self.local_arguments.is_empty()
	}

	fn get_argument<C: InformationChain>(
		&self,
		under: TypeId,
		_c: &C,
		_types: &mut TypeStore,
	) -> Option<TypeId> {
		self.local_arguments.get(&under).copied()
	}
}

/// These are curried between structures
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct StructureGenericArguments {
	pub type_restrictions: TypeRestrictions,
	pub properties: LookUpGenericMap,
	pub closures: Vec<ClosureId>,
}

impl StructureGenericArguments {
	#[must_use]
	pub fn get_structure_restriction(&self, under: TypeId) -> Option<TypeId> {
		self.type_restrictions.get(&under).map(|(l, _)| *l)
	}
}

impl TypeArgumentStore for StructureGenericArguments {
	fn get_argument<C: InformationChain>(
		&self,
		under: TypeId,
		info: &C,
		types: &mut TypeStore,
	) -> Option<TypeId> {
		if let Some(restriction) = self.get_structure_restriction(under) {
			Some(restriction)
		} else if let Some(lookup) = self.properties.get(&under) {
			let res = lookup.calculate_lookup(info);
			let mut iter = res.into_iter();
			let first = iter.next().unwrap_or(TypeId::NEVER_TYPE);
			let mut acc = first;
			for ty in iter {
				acc = types.new_or_type(acc, ty);
			}
			Some(acc)
		} else {
			None
		}
	}

	fn get_structural_closures(&self) -> Option<Vec<ClosureId>> {
		Some(self.closures.clone())
	}

	fn to_structural_generic_arguments(&self) -> StructureGenericArguments {
		self.clone()
	}

	fn is_empty(&self) -> bool {
		self.closures.is_empty() && self.type_restrictions.len() == 0
	}
}
