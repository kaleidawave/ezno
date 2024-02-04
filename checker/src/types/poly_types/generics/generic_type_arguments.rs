//! Contains wrappers for generic type arguments and their wrapping environments
//! TODO Some of these are a bit overkill and don't need wrapping objects **AND THEY BREAK FINALIZE THINGS REQUIRE CLONING**

use crate::{
	context::information::{InformationChain, LocalInformation},
	features::functions::{ClosureChain, ClosureId},
	types::{LookUpGeneric, TypeArguments, TypeRestrictions, TypeStore},
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
	/// TODO this shouldn't be here
	pub structure_arguments: Option<StructureGenericArguments>,
	/// Might not be full
	pub local_arguments: TypeArguments,
	pub closure_id: Option<ClosureId>,
	pub call_site: SpanWithSource,
}

impl FunctionTypeArguments {
	/// TODO!! explain
	pub(crate) fn set_id_from_event_application(&mut self, id: TypeId, value: TypeId) {
		self.local_arguments.insert(id, value);
	}

	pub(crate) fn new_arguments_for_use_in_loop() -> Self {
		Self {
			structure_arguments: Default::default(),
			local_arguments: SmallMap::new(),
			closure_id: Default::default(),
			call_site: SpanWithSource::NULL,
		}
	}
}

pub(crate) trait TypeArgumentStore {
	/// Gets the constraint
	fn get_structure_argument<C: InformationChain>(
		&self,
		id: TypeId,
		info: &C,
		types: &mut TypeStore,
	) -> Option<TypeId>;

	fn get_structure_restriction(&self, id: TypeId) -> Option<TypeId>;

	fn get_local_argument(&self, id: TypeId) -> Option<TypeId>;

	fn get_argument<C: InformationChain>(
		&self,
		id: TypeId,
		info: &C,
		types: &mut TypeStore,
	) -> Option<TypeId> {
		self.get_local_argument(id).or_else(|| self.get_structure_argument(id, info, types))
	}

	fn get_structural_closures(&self) -> Option<Vec<ClosureId>>;

	fn to_structural_generic_arguments(&self) -> StructureGenericArguments;

	fn is_empty(&self) -> bool;
}

impl ClosureChain for FunctionTypeArguments {
	fn get_fact_from_closure<T, R>(&self, _fact: &LocalInformation, cb: T) -> Option<R>
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
	fn get_structure_restriction(&self, id: TypeId) -> Option<TypeId> {
		self.structure_arguments.as_ref().and_then(|args| args.get_structure_restriction(id))
	}

	fn get_structure_argument<C: InformationChain>(
		&self,
		id: TypeId,
		info: &C,
		types: &mut TypeStore,
	) -> Option<TypeId> {
		self.structure_arguments
			.as_ref()
			.and_then(|args| args.get_structure_argument(id, info, types))
	}

	fn get_local_argument(&self, id: TypeId) -> Option<TypeId> {
		self.local_arguments.get(&id).cloned()
	}

	fn get_structural_closures(&self) -> Option<Vec<ClosureId>> {
		None
	}

	fn to_structural_generic_arguments(&self) -> StructureGenericArguments {
		let type_arguments_with_positions =
			self.local_arguments.iter().map(|(k, v)| (*k, (*v, self.call_site)));

		match self.structure_arguments {
			Some(ref parent) => {
				let mut merged = parent.type_restrictions.clone();

				merged.extend(type_arguments_with_positions);

				StructureGenericArguments {
					type_restrictions: merged,
					properties: SmallMap::new(),
					closures: parent.closures.iter().copied().chain(self.closure_id).collect(),
				}
			}
			None => StructureGenericArguments {
				properties: SmallMap::new(),
				type_restrictions: type_arguments_with_positions.collect(),
				closures: self.closure_id.into_iter().collect(),
			},
		}
	}

	fn is_empty(&self) -> bool {
		self.closure_id.is_none()
			&& self.local_arguments.keys().any(|t| !matches!(*t, TypeId::NEW_TARGET_ARG))
	}
}

/// for type alias specialisation and inferred generic function type arguments
pub struct ExplicitTypeArguments<'a>(pub &'a mut map_vec::Map<TypeId, (TypeId, SpanWithSource)>);

impl<'a> TypeArgumentStore for ExplicitTypeArguments<'a> {
	fn get_structure_restriction(&self, id: TypeId) -> Option<TypeId> {
		self.0.get(&id).map(|(value, _pos)| *value)
	}

	fn get_structure_argument<C: InformationChain>(
		&self,
		id: TypeId,
		_info: &C,
		_types: &mut TypeStore,
	) -> Option<TypeId> {
		self.get_structure_restriction(id)
	}

	fn get_local_argument(&self, _id: TypeId) -> Option<TypeId> {
		None
	}

	fn get_structural_closures(&self) -> Option<Vec<ClosureId>> {
		None
	}

	fn to_structural_generic_arguments(&self) -> StructureGenericArguments {
		crate::utils::notify!("to_structural_generic_arguments shouldn't be needed");
		let type_restrictions = self.0.iter().map(|(a, (b, c))| (*a, (*b, *c))).collect();
		StructureGenericArguments {
			type_restrictions,
			properties: SmallMap::new(),
			closures: Vec::new(),
		}
	}

	fn is_empty(&self) -> bool {
		false
	}
}

/// These are curried between structures
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct StructureGenericArguments {
	pub type_restrictions: TypeRestrictions,
	pub properties: SmallMap<TypeId, LookUpGeneric>,
	pub closures: Vec<ClosureId>,
}

impl TypeArgumentStore for StructureGenericArguments {
	fn get_structure_argument<C: InformationChain>(
		&self,
		id: TypeId,
		info: &C,
		types: &mut TypeStore,
	) -> Option<TypeId> {
		if let Some(restriction) = self.get_structure_restriction(id) {
			Some(restriction)
		} else if let Some(lookup) = self.properties.get(&id) {
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

	fn get_local_argument(&self, _id: TypeId) -> Option<TypeId> {
		None
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

	fn get_structure_restriction(&self, id: TypeId) -> Option<TypeId> {
		self.type_restrictions.get(&id).map(|(ty, _)| *ty)
	}
}
