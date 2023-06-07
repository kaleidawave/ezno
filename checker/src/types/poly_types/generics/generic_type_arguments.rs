//! Contains wrappers for generic type arguments and their wrapping environments
//! TODO Some of these are a bit overkill and don't need wrapping objects **AND THEY BREAK FINALIZE THINGS REQUIRE CLONING**

use crate::{types::TypeStore, CheckingData, TypeId};

use map_vec::Map as SmallMap;
use source_map::Span;

use std::{fmt::Debug, iter::FromIterator};

use super::{GenericStructureTypeArgument, GenericStructureTypeArguments, ResolveGenerics};

// This is for `function x<T>(a: T): Array<T>`
impl ResolveGenerics for GenericStructureTypeArguments {
	fn resolve_generics<T: crate::FSResolver>(
		self,
		type_arguments: &TypeArguments,
		checking_data: &mut CheckingData<T>,
	) -> Self {
		self.0
			.iter()
			.cloned()
			.map(|generic_type_argument_pair| {
				ResolveGenerics::resolve_generics(
					generic_type_argument_pair,
					type_arguments,
					checking_data,
				)
			})
			.collect()
	}
}

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

#[derive(Debug, Clone)]
pub struct CurriedFunctionTypeArguments(pub map_vec::Map<TypeId, TypeId>);

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

#[derive(Debug)]
pub(crate) struct FunctionTypeArgument {
	pub value: Option<TypeId>,
	/// Via <> at call site. Note that backing types are held separately
	pub restriction: Option<(Span, TypeId)>,
}

/// TODO working out environment thingy
#[derive(Debug)]
pub(crate) struct TypeArguments {
	pub structure_arguments: Option<CurriedFunctionTypeArguments>,
	/// Might not be full
	pub local_arguments: SmallMap<TypeId, FunctionTypeArgument>,
}

impl TypeArguments {
	/// Gets the value, not the constraint
	pub(crate) fn get_arg(&self, id: TypeId) -> Option<TypeId> {
		self.local_arguments.get(&id).and_then(|arg| arg.value)
		// self.structure_arguments
		// 	.as_ref()
		// 	.and_then(|structure_arguments| structure_arguments.0.get(id).map(|v| v.as_type()))
		// 	.or_else(|| self.local_arguments.get(id).and_then(|v| v.value.as_ref()))
	}

	/// This is from <T>
	pub(crate) fn get_restriction_for_id(&self, id: TypeId) -> Option<(Span, TypeId)> {
		self.local_arguments.get(&id).and_then(|arg| arg.restriction.clone())
		// self.structure_arguments
		// 	.as_ref()
		// 	.and_then(|structure_arguments| structure_arguments.0.get(&id).map(|v| v.as_type()))
		// 	.or_else(|| self.local_arguments.get(&id).map(|v| v.restriction))
	}

	/// TODO check restriction here!
	/// TODO remove `Environment`
	pub(crate) fn set_id(&mut self, on: TypeId, arg: TypeId, _ts: &TypeStore) {
		// crate::utils::notify!(
		// 	"Setting argument {:?} to {:?}",
		// 	_ts.debug_type(on),
		// 	_ts.debug_type(arg)
		// );

		match self.local_arguments.entry(on) {
			map_vec::map::Entry::Occupied(mut exists) => {
				if let Some(value) = exists.get().value {
					todo!("check existing entry")
				} else {
					exists.get_mut().value = Some(arg);
				}
			}
			map_vec::map::Entry::Vacant(vacant) => {
				vacant.insert(FunctionTypeArgument {
					value: Some(arg),
					// TODO::
					restriction: None,
				});
			}
		}
	}

	// necessary_keys: &impl Iterator<Item = &'a DependentTypeId>,
	// Used for folding them between functions. e.g `Array.prototype.map`
	pub(crate) fn into_curried_function_type_arguments(&self) -> CurriedFunctionTypeArguments {
		crate::utils::notify!("TODO");
		CurriedFunctionTypeArguments(map_vec::Map::new())
		// self.local_arguments
	}

	pub(crate) fn set_this(&mut self, arg: TypeId) {
		self.local_arguments
			.insert(TypeId::THIS_ARG, FunctionTypeArgument { value: Some(arg), restriction: None });
	}
}
