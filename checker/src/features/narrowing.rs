use crate::{
	features::operations::CanonicalEqualityAndInequality,
	types::{Constant, Constructor, PolyNature, TypeOperator, TypeStore},
	LocalInformation, Type, TypeId,
};

/// TODO
/// - Ors and ands
/// - Restrictions
pub fn narrow_based_on_expression(
	condition: TypeId,
	into: &mut LocalInformation,
	types: &mut TypeStore,
) {
	let r#type = types.get_type_by_id(condition);
	if let Type::Constructor(constructor) = r#type {
		match constructor {
			Constructor::CanonicalRelationOperator {
				lhs,
				operator: CanonicalEqualityAndInequality::StrictEqual,
				rhs,
			} => {
				if let Type::Constructor(Constructor::TypeOperator(TypeOperator::TypeOf(on))) =
					types.get_type_by_id(*lhs)
				{
					let from = *on;
					if let Type::Constant(Constant::String(c)) = types.get_type_by_id(*rhs) {
						let narrowed_to = match c.as_str() {
							"number" => TypeId::NUMBER_TYPE,
							"string" => TypeId::STRING_TYPE,
							"boolean" => TypeId::BOOLEAN_TYPE,
							rhs => {
								// TODO also never
								crate::utilities::notify!("typeof rhs={}", rhs);
								return;
							}
						};
						let narrowed = types.register_type(Type::Narrowed { from, narrowed_to });
						into.narrowed_values.insert(from, narrowed);
					} else {
						crate::utilities::notify!("Here?");
					}
				} else {
					if let Type::RootPolyType(PolyNature::Parameter { .. }) =
						types.get_type_by_id(*lhs)
					{
						crate::utilities::notify!(
							"lhs is {:?} with {:?}",
							lhs,
							types.get_type_by_id(*rhs)
						);
					}

					// TODO reflexive ?
					into.narrowed_values.insert(*lhs, *rhs);
				}
			}
			// TODO instance of
			// Constructor::TypeOperator(TypeOperator::TypeOf(on)) => {
			// }
			constructor => {
				crate::utilities::notify!("Here?, {:?}", constructor);
			}
		}
	} else if let Type::RootPolyType(rpt) = r#type {
		if rpt.get_constraint() == TypeId::BOOLEAN_TYPE {
			into.narrowed_values.insert(condition, TypeId::TRUE);
		} else {
			crate::utilities::notify!("Set, {:?} as truthy", r#type);
		}
	}
}
