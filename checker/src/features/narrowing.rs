//! TODO WIP

use crate::{
	features::operations::CanonicalEqualityAndInequality,
	types::{Constructor, PolyNature, TypeStore},
	LocalInformation, Type, TypeId,
};

pub fn narrow_based_on_expression(
	condition: TypeId,
	into: &mut LocalInformation,
	types: &TypeStore,
) {
	if let Type::Constructor(Constructor::CanonicalRelationOperator {
		lhs,
		operator: CanonicalEqualityAndInequality::StrictEqual,
		rhs,
	}) = types.get_type_by_id(condition)
	{
		if let Type::RootPolyType(PolyNature::Parameter { .. }) = types.get_type_by_id(*lhs) {
			crate::utilities::notify!("lhs is {:?} with {:?}", lhs, types.get_type_by_id(*rhs));
		}

		// TODO reflexive ?
		into.narrowed_values.insert(*lhs, *rhs);
	}
}
