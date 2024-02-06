use source_map::SpanWithSource;

use crate::{
	subtyping::{type_is_subtype, SubTypeBehavior, SubTypeResult},
	types::{LookUpGeneric, TypeRestrictions, TypeStore},
	Environment, TypeId,
};

use super::generic_type_arguments::StructureGenericArguments;

/// `staging_*` is to get around the fact that intersections cannot be during subtyping (as types
/// is immutable at this stage, rather than mutable)
///
/// `covariant :> contravariant`!!
///
/// This picks contributions. IT DOES NOT CREATE ARGUMENTS EAGERLY
pub(crate) struct Contributions<'a> {
	/// Contains **parent** constraints or some ways to lookup **parent** constraints (*more like arguments*)
	pub(crate) parent: Option<&'a StructureGenericArguments>,

	/// Constraints constraints for the **current function call**
	pub(crate) call_site_type_arguments: Option<&'a TypeRestrictions>,

	pub(crate) existing_covariant: &'a mut map_vec::Map<TypeId, TypeId>,
	/// Only for explicit generic parameters
	pub(crate) staging_covariant: map_vec::Map<TypeId, Vec<(TypeId, u8)>>,
	pub(crate) staging_contravariant: map_vec::Map<TypeId, Vec<(TypeId, SpanWithSource)>>,
}

impl<'a> Contributions<'a> {
	/// TODO return position?
	fn get_standard_restrictions(&self, under: TypeId) -> Option<TypeId> {
		let cstr =
			self.call_site_type_arguments.and_then(|csta| csta.get(&under).map(|(c, _pos)| *c));
		if let cstr @ Some(_) = cstr {
			cstr
		} else {
			self.parent.and_then(|parent| parent.type_restrictions.get(&under).map(|(c, _pos)| *c))
		}
	}

	fn get_lookup(&self, under: TypeId) -> Option<LookUpGeneric> {
		self.parent.and_then(|parent| parent.properties.get(&under).cloned())
	}

	fn passes_under_current_contravariant(
		&mut self,
		under: TypeId,
		argument: TypeId,
		environment: &Environment,
		types: &TypeStore,
		lookup_properties: bool,
	) -> SubTypeResult {
		// TODO staging_contravariant
		if let Some(constraint) = self.get_standard_restrictions(under) {
			type_is_subtype(constraint, argument, self, environment, types)
		} else if let Some(lookup_restriction) =
			lookup_properties.then(|| self.get_lookup(under)).flatten()
		{
			// TODO don't create vec
			for constraint in lookup_restriction.calculate_lookup(environment) {
				if let e @ SubTypeResult::IsNotSubType(_) =
					type_is_subtype(constraint, argument, self, environment, types)
				{
					return e;
				}
			}

			SubTypeResult::IsSubType
		} else {
			crate::utils::notify!("Here 2");
			// TODO not sure
			let constraint = crate::types::get_constraint(under, types).unwrap();
			crate::subtyping::type_is_subtype(constraint, argument, self, environment, types)
		}
	}

	fn try_set_covariant(
		&mut self,
		on: TypeId,
		argument: TypeId,
		depth: u8,
		environment: &Environment,
		types: &TypeStore,
	) -> SubTypeResult {
		let lhs = crate::types::printing::print_type(on, types, environment, true);
		let rhs = crate::types::printing::print_type(argument, types, environment, true);
		crate::utils::notify!("Here on=({}) :< arg=({})", lhs, rhs);

		if let e @ SubTypeResult::IsNotSubType(_) =
			self.passes_under_current_contravariant(on, argument, environment, types, false)
		{
			// TODO more detailed error
			return e;
		}

		self.staging_covariant.entry(on).or_default().push((argument, depth));

		SubTypeResult::IsSubType
	}

	fn try_set_contravariant(
		&mut self,
		on: TypeId,
		restriction: TypeId,
		position: SpanWithSource,
		environment: &Environment,
		types: &TypeStore,
	) -> SubTypeResult {
		todo!()
		// // TODO chain
		// let current_covariant = self.existing_covariant.get(&on).cloned().into_iter();

		// // Check that the new argument, satisfies the restriction
		// for argument in current_covariant {
		// 	if let e @ SubTypeResult::IsNotSubType(..) =
		// 		type_is_subtype(restriction, argument, self, environment, types)
		// 	{
		// 		return e;
		// 	}
		// }

		// self.staging_contravariant.entry(on).or_default().push((restriction, position));

		// SubTypeResult::IsSubType
	}
}

impl<'a> SubTypeBehavior for Contributions<'a> {
	const INFER_GENERICS: bool = true;

	fn add_object_mutation_constraint(&mut self, _on: TypeId, _constraint: TypeId) {
		crate::utils::notify!("TODO");
	}

	fn add_function_restriction(
		&mut self,
		_environment: &mut Environment,
		_function_id: crate::FunctionId,
		_function_type: crate::types::FunctionType,
	) {
		todo!()
	}

	fn set_type_argument(
		&mut self,
		on: TypeId,
		argument: TypeId,
		depth: u8,
		environment: &Environment,
		types: &TypeStore,
	) -> SubTypeResult {
		self.try_set_covariant(on, argument, depth, environment, types)
	}

	fn try_set_contravariant(
		&mut self,
		on: TypeId,
		restriction: TypeId,
		position: SpanWithSource,
		environment: &Environment,
		types: &TypeStore,
	) -> SubTypeResult {
		self.try_set_contravariant(on, restriction, position, environment, types)
	}
}
