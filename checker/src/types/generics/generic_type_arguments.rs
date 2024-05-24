//! Contains wrappers for generic type arguments and their wrapping environments
//! TODO Some of these are a bit overkill and don't need wrapping objects **AND THEY BREAK FINALIZE THINGS REQUIRE CLONING**

use crate::{
	context::information::{InformationChain, LocalInformation},
	features::functions::{ClosureChain, ClosureId},
	types::{StructureGenerics, SubstitutionArguments, TypeArguments, TypeRestrictions, TypeStore},
	Type, TypeId,
};

use source_map::{Nullable, SpanWithSource};

use std::fmt::Debug;

/// For when a function is called
#[derive(Debug)]
pub(crate) struct FunctionTypeArguments {
	/// Might not be full
	pub local_arguments: TypeArguments,
	pub closure_ids: Vec<ClosureId>,
	pub call_site: SpanWithSource,
}

impl FunctionTypeArguments {
	/// TODO!! explain
	pub(crate) fn set_id_from_event_application(&mut self, id: TypeId, value: TypeId) {
		self.local_arguments.insert(id, value);
	}

	pub(crate) fn new_arguments_for_use_in_loop() -> Self {
		Self {
			local_arguments: crate::Map::default(),
			closure_ids: Default::default(),
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

	fn curry_arguments(&self, types: &mut TypeStore, id: TypeId) -> TypeId;
}

impl ClosureChain for FunctionTypeArguments {
	fn get_fact_from_closure<T, R>(&self, _fact: &LocalInformation, cb: T) -> Option<R>
	where
		T: Fn(ClosureId) -> Option<R>,
	{
		for closure_id in &self.closure_ids {
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

	fn curry_arguments(&self, types: &mut TypeStore, on: TypeId) -> TypeId {
		// TODO some types might not have parameters, but still need specialising so this here will miss things out
		// types.get_type_by_id(id).get_parameters() {
		// 	let arguments = parameters
		// 		.into_iter()
		// .map(|p| (p, (self.local_arguments.get(&p).copied().unwrap_or(p), self.call_site)))
		// .collect();
		if let Type::Object(crate::types::ObjectNature::RealDeal) | Type::SpecialObject(..) =
			types.get_type_by_id(on)
		{
			if self.closure_ids.is_empty() {
				on
			} else {
				types.register_type(Type::Constructor(
					crate::types::Constructor::StructureGenerics(StructureGenerics {
						on,
						arguments: StructureGenericArguments::Closure(self.closure_ids.clone()),
					}),
				))
			}
		} else if self.local_arguments.is_empty() {
			on
		} else {
			types.register_type(Type::Constructor(crate::types::Constructor::StructureGenerics(
				StructureGenerics {
					on,
					arguments: StructureGenericArguments::ExplicitRestrictions(
						self.local_arguments
							.iter()
							.map(|(on, arg)| (*on, (*arg, self.call_site)))
							.collect(),
					),
				},
			)))
		}
	}

	// fn is_empty(&self) -> bool {
	// 	self.closure_id.is_empty() && self.local_arguments.is_empty()
	// }

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
pub enum StructureGenericArguments {
	/// This is from a specialised result
	ExplicitRestrictions(TypeRestrictions),
	Closure(Vec<ClosureId>),
	/// For example calling `.map` on `[1, 2, 3]`. We lookup `T` based on the elements of `on`. This can be passed through functions
	LookUp {
		on: TypeId,
	},
}

impl StructureGenericArguments {
	#[must_use]
	pub fn get_structure_restriction(&self, under: TypeId) -> Option<TypeId> {
		if let StructureGenericArguments::ExplicitRestrictions(type_restrictions) = self {
			// crate::utilities::notify!("under={:?}", under);
			type_restrictions.get(&under).map(|(l, _)| *l)
		} else {
			None
		}
	}

	/// Like [`StructureGenericArguments::get_argument`], but creates a list instead
	pub fn get_argument_as_list<C: InformationChain>(
		&self,
		under: TypeId,
		info: &C,
		types: &TypeStore,
	) -> Option<Vec<TypeId>> {
		match self {
			// | Self::ClosuresAndGenerics { restrictions, .. } => {
			StructureGenericArguments::ExplicitRestrictions(restrictions) => {
				restrictions.get(&under).map(|(ty, _)| vec![*ty])
			}
			StructureGenericArguments::Closure(_) => None,
			StructureGenericArguments::LookUp { on } => {
				let prototype =
					*info.get_chain_of_info().find_map(|env| env.prototypes.get(on)).unwrap();

				let lookup = types
					.lookup_generic_map
					.get(&prototype)
					.and_then(|lookup| lookup.get(&under))?;

				crate::utilities::notify!("Hopefully here {:?}", prototype);
				let res = lookup.calculate_lookup(info, *on);
				Some(res)
			}
		}
	}

	#[must_use]
	pub fn into_substitutable(&self) -> SubstitutionArguments<'static> {
		match self {
			StructureGenericArguments::ExplicitRestrictions(res) => SubstitutionArguments {
				parent: None,
				arguments: res.iter().map(|(k, (v, _))| (*k, *v)).collect(),
				closures: Default::default(),
			},
			StructureGenericArguments::Closure(closures) => SubstitutionArguments {
				parent: None,
				arguments: Default::default(),
				closures: closures.clone(),
			},
			StructureGenericArguments::LookUp { on } => SubstitutionArguments {
				parent: None,
				arguments: Default::default(),
				closures: Default::default(),
			},
		}
	}
}

impl TypeArgumentStore for StructureGenericArguments {
	fn get_argument<C: InformationChain>(
		&self,
		under: TypeId,
		info: &C,
		types: &mut TypeStore,
	) -> Option<TypeId> {
		// if let StructureGenericArguments::LookUp { on } = self {
		// 	let prototype =
		// 		*info.get_chain_of_info().find_map(|env| env.prototypes.get(on)).unwrap();

		// 	let lookup =
		// 		types.lookup_generic_map.get(&prototype).and_then(|lookup| lookup.get(&under))?;

		// 	crate::utilities::notify!("Hopefully here {:?}", prototype);
		// 	let res = lookup.calculate_lookup(info, *on);
		// 	let mut iter = res.into_iter();
		// 	let first = iter.next().unwrap_or(TypeId::NEVER_TYPE);
		// 	let mut acc = first;
		// 	for ty in iter {
		// 		acc = types.new_or_type(acc, ty);
		// 	}
		// 	Some(acc)
		// } else {
		self.get_structure_restriction(under)
		// }
	}

	fn get_structural_closures(&self) -> Option<Vec<ClosureId>> {
		if let StructureGenericArguments::Closure(closured_id) = self {
			Some(closured_id.clone())
		} else {
			None
		}
	}

	fn curry_arguments(&self, types: &mut TypeStore, id: TypeId) -> TypeId {
		types.register_type(Type::Constructor(crate::types::Constructor::StructureGenerics(
			StructureGenerics { on: id, arguments: self.clone() },
		)))
	}
}
