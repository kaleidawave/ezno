use super::ContextId;
use crate::{types::PolyPointer, TypeId};
use std::collections::HashMap;

impl crate::BinarySerializable for PolyPointer {
	fn serialize(self, buf: &mut Vec<u8>) {
		todo!()
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, buf_source: source_map::SourceId) -> Self {
		todo!()
	}
}

/// Any decedents of this context **can** mutate the restriction
#[derive(Debug, Clone, Copy)]
pub struct InferenceBoundary(pub(crate) ContextId);

/// Contains the constraint / bases of dynamic poly types
///
/// TODO generic as only environments should have mutable bases
#[derive(Default, Debug)]
pub(crate) struct Bases {
	pub(crate) immutable_bases: HashMap<TypeId, TypeId>,
	pub(crate) mutable_bases: HashMap<TypeId, (InferenceBoundary, TypeId)>,
}

impl Bases {
	pub(crate) fn does_type_have_mutable_base(&self, on: TypeId) -> bool {
		todo!()
	}

	pub(crate) fn merge(&mut self, mut bases: Bases, context_id: ContextId) {
		self.immutable_bases.extend(bases.immutable_bases.into_iter());
		for (ty, (ctx_ceil, base)) in bases.mutable_bases.into_iter() {
			let existing = if ctx_ceil.0 == context_id {
				self.immutable_bases.insert(ty, base).is_some()
			} else {
				self.mutable_bases.insert(ty, (ctx_ceil, base)).is_some()
			};
			if existing {
				crate::utils::notify!("Found existing constraint, should be safe to override");
			}
		}
	}

	/// INTERFACE extends HAPPEN AFTER THE TYPE HAS BEEN CRATED
	pub(crate) fn connect_extends(&mut self, on: TypeId, ty: TypeId) {
		let res = self.immutable_bases.insert(on, ty);
		debug_assert!(res.is_none());
	}

	pub(crate) fn get_local_type_base(&self, ty: TypeId) -> Option<TypeId> {
		self.mutable_bases.get(&ty).map(|b| b.1).or_else(|| self.immutable_bases.get(&ty).copied())
	}
}
