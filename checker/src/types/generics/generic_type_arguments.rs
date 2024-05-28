//! Contains wrappers for generic type arguments and their wrapping environments
//! TODO Some of these are a bit overkill and don't need wrapping objects **AND THEY BREAK FINALIZE THINGS REQUIRE CLONING**

use crate::{
	context::information::InformationChain,
	features::functions::ClosureId,
	types::{SubstitutionArguments, TypeRestrictions, TypeStore},
	TypeId,
};

use source_map::{Nullable, SpanWithSource};

use std::fmt::Debug;

/// These are curried between structures
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum GenericArguments {
	/// This is from a specialised result
	ExplicitRestrictions(TypeRestrictions),
	Closure(Vec<ClosureId>),
	/// For example calling `.map` on `[1, 2, 3]`. We lookup `T` based on the elements of `on`. This can be passed through functions
	LookUp {
		on: TypeId,
	},
}

impl GenericArguments {
	#[must_use]
	pub fn get_structure_restriction(&self, under: TypeId) -> Option<TypeId> {
		if let GenericArguments::ExplicitRestrictions(type_restrictions) = self {
			// crate::utilities::notify!("under={:?}", under);
			type_restrictions.get(&under).map(|(l, _)| *l)
		} else {
			None
		}
	}

	/// Like [`GenericArguments::get_argument`], but creates a list instead
	pub fn get_argument_as_list<C: InformationChain>(
		&self,
		under: TypeId,
		info: &C,
		types: &TypeStore,
	) -> Option<Vec<TypeId>> {
		match self {
			// | Self::ClosuresAndGenerics { restrictions, .. } => {
			GenericArguments::ExplicitRestrictions(restrictions) => {
				restrictions.get(&under).map(|(ty, _)| vec![*ty])
			}
			GenericArguments::Closure(_) => None,
			GenericArguments::LookUp { on } => {
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
			GenericArguments::ExplicitRestrictions(res) => SubstitutionArguments {
				parent: None,
				arguments: res.iter().map(|(k, (v, _))| (*k, *v)).collect(),
				closures: Default::default(),
			},
			GenericArguments::Closure(closures) => SubstitutionArguments {
				parent: None,
				arguments: Default::default(),
				closures: closures.clone(),
			},
			GenericArguments::LookUp { on } => SubstitutionArguments {
				parent: None,
				arguments: Default::default(),
				closures: Default::default(),
			},
		}
	}
}
