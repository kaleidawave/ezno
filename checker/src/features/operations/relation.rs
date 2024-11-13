use super::MathematicalOrBitwiseOperation;
use crate::types::{
	cast_as_number, disjoint, get_constraint, helpers, intrinsics, Constant, Constructor,
	PartiallyAppliedGenerics, Type, TypeId,
};

/// Not canonical / reducible form of [`CanonicalEqualityAndInequality`].
/// (for examples `a > b` is equivalent to `b < a` (after side effects) and `a !== b` is equivalent to `!(a === b)`)
#[derive(Clone, Copy, Debug)]
pub enum EqualityAndInequality {
	StrictEqual,
	StrictNotEqual,
	Equal,
	NotEqual,
	GreaterThan,
	LessThan,
	LessThanOrEqual,
	GreaterThanOrEqual,
}

/// Canonical / irreducible form of [`EqualityAndInequality`].
#[derive(Clone, Copy, Debug, binary_serialize_derive::BinarySerializable)]
pub enum CanonicalEqualityAndInequality {
	StrictEqual,
	LessThan,
}

pub enum EqualityAndInequalityResultKind {
	Constant,
	Disjoint,
	Condition,
}

pub fn evaluate_equality_inequality_operation(
	mut lhs: TypeId,
	operator: &EqualityAndInequality,
	mut rhs: TypeId,
	info: &impl crate::context::InformationChain,
	types: &mut crate::types::TypeStore,
	strict_casts: bool,
) -> Result<(TypeId, EqualityAndInequalityResultKind), ()> {
	// `NaN == t` is always true
	if lhs == TypeId::NAN || rhs == TypeId::NAN {
		return Ok((TypeId::FALSE, EqualityAndInequalityResultKind::Constant));
	}

	match operator {
		EqualityAndInequality::StrictEqual => {
			// crate::utilities::notify!("{:?} === {:?}", lhs, rhs);

			if let (Type::Constant(lhs), Type::Constant(rhs)) =
				(types.get_type_by_id(lhs), types.get_type_by_id(rhs))
			{
				let result = if lhs == rhs { TypeId::TRUE } else { TypeId::FALSE };
				return Ok((result, EqualityAndInequalityResultKind::Constant));
			}

			if let (
				Type::Object(_) | Type::SpecialObject(_),
				Type::Object(_) | Type::SpecialObject(_),
			) = (types.get_type_by_id(lhs), types.get_type_by_id(rhs))
			{
				let result = if lhs == rhs { TypeId::TRUE } else { TypeId::FALSE };
				return Ok((result, EqualityAndInequalityResultKind::Constant));
			}

			// if is_dependent {
			if lhs == rhs
				&& intrinsics::is_not_not_a_number(lhs, types)
				&& intrinsics::is_not_not_a_number(rhs, types)
			{
				// I think this is okay
				return Ok((TypeId::TRUE, EqualityAndInequalityResultKind::Constant));
			}

			// Checks lhs and rhs type to see if they overlap
			if disjoint::types_are_disjoint(lhs, rhs, &mut Vec::new(), info, types) {
				return Ok((TypeId::FALSE, EqualityAndInequalityResultKind::Disjoint));
			}

			let left_dependent = types.get_type_by_id(lhs).is_dependent();

			// Sort if `*constant* == ...`. Ideally want constant type on the RHS
			let (lhs, rhs) = if left_dependent { (lhs, rhs) } else { (rhs, rhs) };
			let operator = CanonicalEqualityAndInequality::StrictEqual;
			let result_ty =
				types.register_type(Type::Constructor(Constructor::CanonicalRelationOperator {
					lhs,
					operator,
					rhs,
				}));

			Ok((result_ty, EqualityAndInequalityResultKind::Condition))

			// } else {
			// 	match attempt_constant_equality(lhs, rhs, types) {
			// 		Ok(ty) => Ok((
			// 			if ty { TypeId::TRUE } else { TypeId::FALSE },
			// 			EqualityAndInequalityResultKind::Constant,
			// 		)),
			// 		Err(()) => {
			// 			unreachable!(
			// 				"should have been caught `is_dependent` above, {:?} === {:?}",
			// 				types.get_type_by_id(lhs),
			// 				types.get_type_by_id(rhs)
			// 			)
			// 		}
			// 	}
			// }
		}
		EqualityAndInequality::LessThan => {
			fn attempt_less_than(
				lhs: TypeId,
				rhs: TypeId,
				types: &mut crate::types::TypeStore,
				strict_casts: bool,
			) -> Result<bool, ()> {
				// Similar but reversed semantics to add
				match (types.get_type_by_id(lhs), types.get_type_by_id(rhs)) {
					(
						Type::Constant(Constant::String(string1)),
						Type::Constant(Constant::String(string2)),
					) => {
						// Yah rust includes string alphanumerical equivalence of strings
						Ok(string1 < string2)
					}
					(Type::Constant(c1), Type::Constant(c2)) => {
						let lhs = cast_as_number(c1, strict_casts)?;
						let rhs = cast_as_number(c2, strict_casts)?;
						Ok(lhs < rhs)
					}
					(lhs, rhs) => {
						crate::utilities::notify!("{:?}", (lhs, rhs));
						// Ok(TypeId::OPEN_BOOLEAN_TYPE)
						Err(())
					}
				}
			}

			let either_is_dependent = types.get_type_by_id(lhs).is_dependent()
				|| types.get_type_by_id(rhs).is_dependent();

			if either_is_dependent {
				{
					if let Type::Constructor(Constructor::BinaryOperator {
						lhs: op_lhs,
						operator,
						rhs: op_rhs,
						result: _,
					}) = types.get_type_by_id(lhs)
					{
						if let (
							Type::Constant(Constant::Number(add)),
							MathematicalOrBitwiseOperation::Add,
							Type::Constant(Constant::Number(lt)),
						) = (types.get_type_by_id(*op_rhs), operator, types.get_type_by_id(rhs))
						{
							crate::utilities::notify!("Shifted LT");
							lhs = *op_lhs;
							rhs = types.register_type(Type::Constant(Constant::Number(lt - add)));
						}
					}
				}

				{
					if !helpers::simple_subtype(lhs, TypeId::NUMBER_TYPE, info, types)
						|| !helpers::simple_subtype(rhs, TypeId::NUMBER_TYPE, info, types)
					{
						return Err(());
					}

					let lhs = get_constraint(lhs, types).unwrap_or(lhs);
					let rhs = get_constraint(rhs, types).unwrap_or(rhs);

					// Tidies some things for counting loop iterations

					// Checking disjoint-ness for inequalities (TODO under option) via distribution
					if let ((Some(lhs_range), _), (Some(rhs_range), _)) = (
						intrinsics::get_range_and_mod_class(lhs, types),
						intrinsics::get_range_and_mod_class(rhs, types),
					) {
						crate::utilities::notify!("{:?}", (lhs_range, rhs_range));
						if lhs_range.below(rhs_range) {
							return Ok((TypeId::TRUE, EqualityAndInequalityResultKind::Constant));
						}
						if lhs_range.above(rhs_range) {
							return Ok((TypeId::FALSE, EqualityAndInequalityResultKind::Disjoint));
						}
					}

					// Transitivity
					// TODO extra from &
					if let (
						Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
							on: TypeId::LESS_THAN,
							arguments: larg,
						}),
						Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
							on: TypeId::GREATER_THAN,
							arguments: rarg,
						}),
					) = (types.get_type_by_id(lhs), types.get_type_by_id(rhs))
					{
						crate::utilities::notify!("{:?} {:?}", larg, rarg);
						// Only one level
						let transitivity = larg.get_structure_restriction(TypeId::NUMBER_GENERIC)
							== rarg.get_structure_restriction(TypeId::NUMBER_GENERIC);
						if transitivity {
							return Ok((TypeId::TRUE, EqualityAndInequalityResultKind::Constant));
						}
					}
				}

				let constructor = Constructor::CanonicalRelationOperator {
					lhs,
					operator: CanonicalEqualityAndInequality::LessThan,
					rhs,
				};
				Ok((
					types.register_type(crate::Type::Constructor(constructor)),
					EqualityAndInequalityResultKind::Condition,
				))
			} else {
				attempt_less_than(lhs, rhs, types, strict_casts).map(|value| {
					(
						if value { TypeId::TRUE } else { TypeId::FALSE },
						EqualityAndInequalityResultKind::Constant,
					)
				})
			}
		}
		// equal OR less than
		EqualityAndInequality::LessThanOrEqual => {
			let (equality_result, warning) = evaluate_equality_inequality_operation(
				lhs,
				&EqualityAndInequality::StrictEqual,
				rhs,
				info,
				types,
				strict_casts,
			)?;

			if equality_result == TypeId::TRUE {
				Ok((equality_result, warning))
			} else if equality_result == TypeId::FALSE {
				evaluate_equality_inequality_operation(
					lhs,
					&EqualityAndInequality::LessThan,
					rhs,
					info,
					types,
					strict_casts,
				)
			} else {
				let (less_than_result, warning) = evaluate_equality_inequality_operation(
					lhs,
					&EqualityAndInequality::LessThan,
					rhs,
					info,
					types,
					strict_casts,
				)?;
				Ok((types.new_logical_or_type(equality_result, less_than_result), warning))
			}
		}
		EqualityAndInequality::StrictNotEqual => {
			let (equality_result, kind) = evaluate_equality_inequality_operation(
				lhs,
				&EqualityAndInequality::StrictEqual,
				rhs,
				info,
				types,
				strict_casts,
			)?;
			if let EqualityAndInequalityResultKind::Condition = kind {
				Ok((types.new_logical_negation_type(equality_result), kind))
			} else {
				let negated = if let TypeId::TRUE = equality_result {
					TypeId::FALSE
				} else if let TypeId::FALSE = equality_result {
					TypeId::TRUE
				} else {
					todo!()
				};
				Ok((negated, kind))
			}
		}
		EqualityAndInequality::Equal => {
			crate::utilities::notify!("TODO equal operator");
			Ok((TypeId::OPEN_BOOLEAN_TYPE, EqualityAndInequalityResultKind::Condition))
		}
		EqualityAndInequality::NotEqual => {
			let (equality_result, kind) = evaluate_equality_inequality_operation(
				lhs,
				&EqualityAndInequality::Equal,
				rhs,
				info,
				types,
				strict_casts,
			)?;
			if let EqualityAndInequalityResultKind::Condition = kind {
				Ok((types.new_logical_negation_type(equality_result), kind))
			} else {
				let negated = if let TypeId::TRUE = equality_result {
					TypeId::FALSE
				} else if let TypeId::FALSE = equality_result {
					TypeId::TRUE
				} else {
					todo!()
				};
				Ok((negated, kind))
			}
		}
		// Swapping operands!
		EqualityAndInequality::GreaterThan => evaluate_equality_inequality_operation(
			rhs,
			&EqualityAndInequality::LessThan,
			lhs,
			info,
			types,
			strict_casts,
		),
		// Swapping operands!
		EqualityAndInequality::GreaterThanOrEqual => evaluate_equality_inequality_operation(
			rhs,
			&EqualityAndInequality::LessThanOrEqual,
			lhs,
			info,
			types,
			strict_casts,
		),
	}
}

#[allow(clippy::let_and_return)]
pub fn is_null_or_undefined(
	ty: TypeId,
	info: &impl crate::context::InformationChain,
	types: &mut crate::types::TypeStore,
) -> TypeId {
	let is_null = evaluate_equality_inequality_operation(
		ty,
		&EqualityAndInequality::StrictEqual,
		TypeId::NULL_TYPE,
		info,
		types,
		false,
	)
	.map_or(TypeId::ERROR_TYPE, |(left, _)| left);

	if let TypeId::TRUE = is_null {
		is_null
	} else {
		let is_undefined = evaluate_equality_inequality_operation(
			ty,
			&EqualityAndInequality::StrictEqual,
			TypeId::UNDEFINED_TYPE,
			info,
			types,
			false,
		)
		.map_or(TypeId::ERROR_TYPE, |(left, _)| left);

		if let TypeId::FALSE = is_null {
			is_undefined
		} else {
			types.new_logical_or_type(is_null, is_undefined)
		}
	}
}
