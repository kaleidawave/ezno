use source_map::SpanWithSource;

use crate::{
	subtyping::{type_is_subtype_with_generics, State, SubTypeResult},
	types::{GenericChain, TypeRestrictions, TypeStore},
	Environment, TypeId,
};

use super::generic_type_arguments::GenericArguments;

// pub type X<T, U> = crate::Map<T, U>;
pub type DoubleMap<T, U> = crate::Map<T, U>;
// pub type X2<T, U, V> = crate::Map<T, Vec<(U, V)>>;
pub type TriMap<T, U, V> = crate::Map<T, (U, V)>;

/// `staging_*` is to get around the fact that intersections cannot be during subtyping (as types
/// is immutable at this stage, rather than mutable)
///
/// `covariant :> contravariant`!!
///
/// This picks contributions. IT DOES NOT CREATE ARGUMENTS EAGERLY
///
#[derive(Default)]
pub struct Contributions<'a> {
	/// Contains **parent** constraints or some ways to lookup **parent** constraints (*more like arguments*)
	pub parent: Option<&'a GenericArguments>,

	/// Constraints constraints for the **current function call**
	pub call_site_type_arguments: Option<&'a TypeRestrictions>,

	// /// From other parameters
	// #[allow(unused)]
	// pub existing_covariant: &'a mut X<TypeId, TypeId>,
	/// Only for explicit generic parameters
	pub staging_covariant: TriMap<TypeId, TypeId, SpanWithSource>,
	pub staging_contravariant: TriMap<TypeId, TypeId, u8>,
}

impl<'a> Contributions<'a> {
	/// TODO return position?
	pub fn get_standard_restriction(&self, under: TypeId) -> Option<TypeId> {
		let cstr = self.call_site_type_arguments.and_then(|ta| ta.get(&under).map(|(c, _pos)| *c));
		if let cstr @ Some(_) = cstr {
			cstr
		} else {
			self.parent.and_then(|parent| parent.get_structure_restriction(under))
		}
	}

	fn _passes_under_current_covariant(
		&mut self,
		under: TypeId,
		argument: TypeId,
		state: &mut State,
		environment: &Environment,
		types: &TypeStore,
	) -> SubTypeResult {
		// else if let Some(lookup_restriction) = lookup_properties
		// 	.then(|| prototype.and_then(|prototype| self.get_lookup(prototype, under, types)))
		// 	.flatten()
		// {
		// 	// TODO don't create vec
		// 	for constraint in lookup_restriction.calculate_lookup(environment) {
		// 		if let e @ SubTypeResult::IsNotSubType(_) = type_is_subtype_with_generics(
		// 			constraint,
		// 			None,
		// 			argument,
		// 			None,
		// 			self,
		// 			environment,
		// 			types,
		// 			Default::default(),
		// 			already_checked,
		// 		) {
		// 			return e;
		// 		}
		// 	}

		// 	SubTypeResult::IsSubType
		// }

		// TODO staging_contravariant
		if let Some(constraint) = self.get_standard_restriction(under) {
			crate::utilities::notify!("Constraint is {:?}", constraint);
			type_is_subtype_with_generics(
				(constraint, GenericChain::None),
				(argument, GenericChain::None),
				state,
				environment,
				types,
			)
		} else {
			// TODO not sure
			let constraint = crate::types::get_constraint(under, types).unwrap();
			crate::utilities::notify!("Here, constraint={:?}", constraint);
			type_is_subtype_with_generics(
				(constraint, GenericChain::None),
				(argument, GenericChain::None),
				state,
				environment,
				types,
			)
		}
	}

	// pub fn try_set_contravariant(
	// 	&mut self,
	// 	on: TypeId,
	// 	argument: TypeId,
	// 	state: &mut State,
	// 	_environment: &Environment,
	// 	_types: &TypeStore,
	// ) -> SubTypeResult {
	// 	// {
	// 	// 	let lhs = crate::types::printing::print_type(on, types, environment, true);
	// 	// 	let rhs = crate::types::printing::print_type(argument, types, environment, true);
	// 	// 	crate::utilities::notify!("Here on=({}) :< arg=({})", lhs, rhs);
	// 	// }

	// 	// if let e @ SubTypeResult::IsNotSubType(_) =
	// 	// 	self.passes_under_current_covariant(on, argument, environment, state, types)
	// 	// {
	// 	// 	// TODO more detailed error
	// 	// 	return e;
	// 	// }

	// 	// TODO on state?
	// 	if let SubTypingMode::Contravariant { depth } = state.mode {
	// 		self.staging_covariant.push((on, argument, depth));
	// 	// self.staging_covariant.entry(on).or_default().push((argument, depth));
	// 	} else {
	// 		unreachable!()
	// 	}

	// 	SubTypeResult::IsSubType
	// }

	// pub fn try_set_covariant(
	// 	&mut self,
	// 	on: TypeId,
	// 	restriction: TypeId,
	// 	state: &mut State,
	// 	environment: &Environment,
	// 	types: &TypeStore,
	// ) -> SubTypeResult {
	// 	crate::utilities::notify!("TODO assert it meets existing_covariant and staging_covariant");
	// 	crate::utilities::notify!("TODO add to staging_covariant");

	// 	if let Some(under) = self.get_standard_restriction(on) {
	// 		type_is_subtype_with_generics(
	// 			(under, GenericChain::None),
	// 			(restriction, GenericChain::None),
	// 			state,
	// 			environment,
	// 			types,
	// 		)
	// 	} else {
	// 		SubTypeResult::IsSubType
	// 	}
	// }
}
