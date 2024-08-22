//! Contains logic for mathematical, bitwise and logical operators

use derive_enum_from_into::EnumFrom;
use source_map::{Span, SpanWithSource};

use crate::{
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	features::conditional::new_conditional_context,
	types::{
		cast_as_number, cast_as_string, get_constraint, intrinsics, is_type_truthy_falsy,
		Constructor, PartiallyAppliedGenerics, TypeStore,
	},
	CheckingData, Constant, Decidable, Environment, Type, TypeId,
};

use super::objects::SpecialObject;

/// For these **binary** operations both operands are synthesised
#[derive(Clone, Copy, Debug, binary_serialize_derive::BinarySerializable)]
pub enum MathematicalAndBitwise {
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulo,
	Exponent,
	BitwiseShiftLeft,
	BitwiseShiftRight,
	BitwiseShiftRightUnsigned,
	BitwiseAnd,
	BitwiseXOr,
	BitwiseOr,
}

#[derive(Clone, Copy, Debug, EnumFrom)]
pub enum PureBinaryOperation {
	MathematicalAndBitwise(MathematicalAndBitwise),
	// Some of these can be reduced
	EqualityAndInequality(EqualityAndInequality),
}

/// TODO report errors better here
pub fn evaluate_pure_binary_operation_handle_errors<
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
>(
	(lhs, lhs_pos): (TypeId, SpanWithSource),
	operator: PureBinaryOperation,
	(rhs, rhs_pos): (TypeId, SpanWithSource),
	checking_data: &mut CheckingData<T, A>,
	environment: &mut Environment,
) -> TypeId {
	match operator {
		PureBinaryOperation::MathematicalAndBitwise(operator) => {
			if let (MathematicalAndBitwise::Exponent, TypeId::ZERO) = (operator, rhs) {
				// This holds for NaN. Thus can do in every case
				return TypeId::ONE;
			}

			let result = evaluate_mathematical_operation(
				lhs,
				operator,
				rhs,
				&mut checking_data.types,
				checking_data.options.strict_casts,
			);
			match result {
				Ok(result) => result,
				Err(_err) => {
					let position = lhs_pos
						.without_source()
						.union(rhs_pos.without_source())
						.with_source(environment.get_source());

					checking_data.diagnostics_container.add_error(
						TypeCheckError::InvalidMathematicalOrBitwiseOperation {
							operator,
							lhs: TypeStringRepresentation::from_type_id(
								lhs,
								environment,
								&checking_data.types,
								false,
							),
							rhs: TypeStringRepresentation::from_type_id(
								rhs,
								environment,
								&checking_data.types,
								false,
							),
							position,
						},
					);
					TypeId::ERROR_TYPE
				}
			}
		}
		PureBinaryOperation::EqualityAndInequality(operator) => {
			// Cannot error, but can be always true or false
			evaluate_equality_inequality_operation(
				lhs,
				&operator,
				rhs,
				&mut checking_data.types,
				checking_data.options.strict_casts,
			)
		}
	}
}

/// TODO proper err
pub fn evaluate_mathematical_operation(
	lhs: TypeId,
	operator: MathematicalAndBitwise,
	rhs: TypeId,
	types: &mut TypeStore,
	strict_casts: bool,
) -> Result<TypeId, ()> {
	fn attempt_constant_math_operator(
		lhs: TypeId,
		operator: MathematicalAndBitwise,
		rhs: TypeId,
		types: &mut TypeStore,
		strict_casts: bool,
	) -> Result<TypeId, ()> {
		if let MathematicalAndBitwise::Add = operator {
			let constant = match (types.get_type_by_id(lhs), types.get_type_by_id(rhs)) {
				(Type::Constant(Constant::Number(lhs)), Type::Constant(Constant::Number(rhs))) => {
					Constant::Number(lhs + rhs)
				}
				(Type::Constant(lhs), Type::Constant(rhs)) => {
					let mut first = cast_as_string(lhs, strict_casts)?;
					let second = cast_as_string(rhs, strict_casts)?;
					// Concatenate strings
					first.push_str(&second);
					Constant::String(first)
				}
				_ => return Err(()),
			};
			Ok(types.new_constant_type(constant))
		} else {
			match (types.get_type_by_id(lhs), types.get_type_by_id(rhs)) {
				(Type::Constant(c1), Type::Constant(c2)) => {
					let lhs = cast_as_number(c1, strict_casts).unwrap_or(f64::NAN);
					let rhs = cast_as_number(c2, strict_casts).unwrap_or(f64::NAN);
					// TODO hopefully Rust implementation is the same as JS
					#[allow(clippy::cast_possible_truncation)]
					let value = match operator {
						MathematicalAndBitwise::Add => unreachable!(),
						MathematicalAndBitwise::Subtract => lhs - rhs,
						MathematicalAndBitwise::Multiply => lhs * rhs,
						MathematicalAndBitwise::Divide => lhs / rhs,
						MathematicalAndBitwise::Modulo => lhs % rhs,
						MathematicalAndBitwise::Exponent => lhs.powf(rhs),
						MathematicalAndBitwise::BitwiseShiftLeft => {
							f64::from((lhs as i32).checked_shl(rhs as u32).unwrap_or(0))
						}
						MathematicalAndBitwise::BitwiseShiftRight => {
							f64::from((lhs as i32).checked_shr(rhs as u32).unwrap_or(0))
						}
						MathematicalAndBitwise::BitwiseShiftRightUnsigned => {
							(lhs as i32).wrapping_shr(rhs as u32).into()
						}
						MathematicalAndBitwise::BitwiseAnd => {
							f64::from((lhs as i32) & (rhs as i32))
						}
						MathematicalAndBitwise::BitwiseXOr => {
							f64::from((lhs as i32) ^ (rhs as i32))
						}
						MathematicalAndBitwise::BitwiseOr => f64::from((lhs as i32) | (rhs as i32)),
					};
					let value = ordered_float::NotNan::try_from(value);
					let ty = match value {
						Ok(value) => types.new_constant_type(Constant::Number(value)),
						Err(_) => TypeId::NAN,
					};
					Ok(ty)
				}
				_ => Err(()),
			}
		}
	}

	{
		fn simple_is_type(matching: TypeId, on: TypeId, types: &TypeStore) -> bool {
			if on == matching {
				true
			} else {
				match types.get_type_by_id(on) {
					Type::Constant(c) => c.get_backing_type_id() == matching,
					Type::Or(left, right) => {
						simple_is_type(matching, *left, types)
							&& simple_is_type(matching, *right, types)
					}
					Type::And(left, right) => {
						simple_is_type(matching, *left, types)
							|| simple_is_type(matching, *right, types)
					}
					ty => {
						crate::utilities::notify!("{:?} not handled by simple is", ty);
						false
					}
				}
			}
		}

		let lhs = get_constraint(lhs, types).unwrap_or(lhs);
		let rhs = get_constraint(rhs, types).unwrap_or(rhs);

		// TODO proper type_is_subtype
		if let MathematicalAndBitwise::Add = operator {
			let want = if simple_is_type(TypeId::NUMBER_TYPE, lhs, types) {
				TypeId::NUMBER_TYPE
			} else if simple_is_type(TypeId::STRING_TYPE, lhs, types) {
				TypeId::STRING_TYPE
			} else {
				return Err(());
			};
			if !simple_is_type(want, rhs, types) {
				return Err(());
			}
		} else {
			if !simple_is_type(TypeId::NUMBER_TYPE, lhs, types)
				|| !simple_is_type(TypeId::NUMBER_TYPE, rhs, types)
			{
				return Err(());
			}
		}
	}

	if lhs == TypeId::ERROR_TYPE || rhs == TypeId::ERROR_TYPE {
		return Ok(TypeId::ERROR_TYPE);
	}

	let is_dependent =
		types.get_type_by_id(lhs).is_dependent() || types.get_type_by_id(rhs).is_dependent();

	// TODO check sides
	if is_dependent {
		let constructor = crate::types::Constructor::BinaryOperator { lhs, operator, rhs };
		Ok(types.register_type(crate::Type::Constructor(constructor)))
	} else {
		attempt_constant_math_operator(lhs, operator, rhs, types, strict_casts)
	}
}

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

pub fn evaluate_equality_inequality_operation(
	mut lhs: TypeId,
	operator: &EqualityAndInequality,
	mut rhs: TypeId,
	types: &mut TypeStore,
	strict_casts: bool,
) -> TypeId {
	// `NaN == t` is always true
	if lhs == TypeId::NAN || rhs == TypeId::NAN {
		return TypeId::FALSE;
	}

	match operator {
		EqualityAndInequality::StrictEqual => {
			// crate::utilities::notify!("{:?} === {:?}", lhs, rhs);

			let left_dependent = types.get_type_by_id(lhs).is_dependent();
			let is_dependent = left_dependent || types.get_type_by_id(rhs).is_dependent();

			// TODO check lhs and rhs type to see if they overlap
			if is_dependent {
				// Sort if `*constant* == ...`. Ideally want constant type on the RHS
				let (lhs, rhs) = if left_dependent { (lhs, rhs) } else { (rhs, rhs) };
				let constructor = crate::types::Constructor::CanonicalRelationOperator {
					lhs,
					operator: CanonicalEqualityAndInequality::StrictEqual,
					rhs,
				};

				types.register_type(crate::Type::Constructor(constructor))
			} else {
				match attempt_constant_equality(lhs, rhs, types) {
					Ok(ty) => ty,
					Err(()) => {
						unreachable!(
							"should have been caught `is_dependent` above, {:?} === {:?}",
							types.get_type_by_id(lhs),
							types.get_type_by_id(rhs)
						)
					}
				}
			}
		}
		EqualityAndInequality::LessThan => {
			fn attempt_less_than(
				lhs: TypeId,
				rhs: TypeId,
				types: &mut TypeStore,
				strict_casts: bool,
			) -> Result<TypeId, ()> {
				// Similar but reversed semantics to add
				match (types.get_type_by_id(lhs), types.get_type_by_id(rhs)) {
					(Type::Constant(Constant::String(a)), Type::Constant(Constant::String(b))) => {
						// Yah rust includes string alphanumerical equivalence of strings
						Ok(types.new_constant_type(Constant::Boolean(a < b)))
					}
					(Type::Constant(c1), Type::Constant(c2)) => {
						let lhs = cast_as_number(c1, strict_casts)?;
						let rhs = cast_as_number(c2, strict_casts)?;
						Ok(types.new_constant_type(Constant::Boolean(lhs < rhs)))
					}
					(lhs, rhs) => {
						crate::utilities::notify!("{:?}", (lhs, rhs));
						Ok(TypeId::OPEN_BOOLEAN_TYPE)
						// Err(())
					}
				}
			}

			let is_dependent = types.get_type_by_id(lhs).is_dependent()
				|| types.get_type_by_id(rhs).is_dependent();

			if is_dependent {
				// Tidies some things for counting loop iterations
				{
					if let Type::Constructor(Constructor::BinaryOperator {
						lhs: op_lhs,
						operator,
						rhs: op_rhs,
					}) = types.get_type_by_id(lhs)
					{
						if let (
							Type::Constant(Constant::Number(add)),
							MathematicalAndBitwise::Add,
							Type::Constant(Constant::Number(lt)),
						) = (types.get_type_by_id(*op_rhs), operator, types.get_type_by_id(rhs))
						{
							crate::utilities::notify!("Shifted LT");
							lhs = *op_lhs;
							rhs = types.register_type(Type::Constant(Constant::Number(lt - add)));
						}
					}
				}

				// Checking things (TODO under option) via distribution
				{
					// lhs < rhs (ceiling < rhs)
					if let (
						Type::Constant(Constant::Number(lhs)),
						Some(Type::Constant(Constant::Number(ceiling))),
					) = (
						types.get_type_by_id(lhs),
						intrinsics::get_less_than(rhs, types).map(|t| types.get_type_by_id(t)),
					) {
						// crate::utilities::notify!("{:?} < {:?}", lhs, ceiling);
						// We know that the rhs is always greater than ceiling
						if *lhs >= *ceiling {
							return TypeId::FALSE;
						}
					}

					if let (
						Type::Constant(Constant::Number(lhs)),
						Some(Type::Constant(Constant::Number(floor))),
					) = (
						types.get_type_by_id(lhs),
						intrinsics::get_greater_than(rhs, types).map(|t| types.get_type_by_id(t)),
					) {
						// crate::utilities::notify!("{:?} {:?}", lhs, floor);
						if *lhs <= *floor {
							return TypeId::TRUE;
						}
					}

					if let (
						Some(Type::Constant(Constant::Number(ceiling))),
						Type::Constant(Constant::Number(rhs)),
					) = (
						intrinsics::get_less_than(lhs, types).map(|t| types.get_type_by_id(t)),
						types.get_type_by_id(rhs),
					) {
						// We know that the lhs is always less than ceiling
						// crate::utilities::notify!("{:?} {:?}", rhs, ceiling);

						if *rhs >= *ceiling {
							return TypeId::TRUE;
						}
					}

					if let (
						Some(Type::Constant(Constant::Number(floor))),
						Type::Constant(Constant::Number(rhs)),
					) = (
						intrinsics::get_greater_than(lhs, types).map(|t| types.get_type_by_id(t)),
						types.get_type_by_id(rhs),
					) {
						// We know that the lhs is always less than ceiling
						// crate::utilities::notify!("{:?} {:?}", rhs, floor);

						if *rhs <= *floor {
							return TypeId::FALSE;
						}
					}

					// {
					// 	crate::utilities::notify!(
					// 		"Floors and ceilings: {:?} {:?} {:?} {:?}, lhs={:?}, rhs={:?}",
					// 		intrinsics::get_less_than(lhs, types).map(|t| types.get_type_by_id(t)),
					// 		intrinsics::get_less_than(rhs, types).map(|t| types.get_type_by_id(t)),
					// 		intrinsics::get_greater_than(lhs, types)
					// 			.map(|t| types.get_type_by_id(t)),
					// 		intrinsics::get_greater_than(rhs, types)
					// 			.map(|t| types.get_type_by_id(t)),
					// 		types.get_type_by_id(lhs),
					// 		types.get_type_by_id(rhs)
					// 	);
					// }
				}
				let constructor = Constructor::CanonicalRelationOperator {
					lhs,
					operator: CanonicalEqualityAndInequality::LessThan,
					rhs,
				};
				types.register_type(crate::Type::Constructor(constructor))
			} else {
				let less_than_result = attempt_less_than(lhs, rhs, types, strict_casts);
				if let Ok(result) = less_than_result {
					result
				} else {
					crate::utilities::notify!(
						"Less than unreachable {:?}",
						(types.get_type_by_id(lhs), types.get_type_by_id(rhs))
					);
					TypeId::ERROR_TYPE
				}
			}
		}
		// equal OR less than
		EqualityAndInequality::LessThanOrEqual => {
			let equality_result = evaluate_equality_inequality_operation(
				lhs,
				&EqualityAndInequality::StrictEqual,
				rhs,
				types,
				strict_casts,
			);

			if equality_result == TypeId::TRUE {
				equality_result
			} else if equality_result == TypeId::FALSE {
				evaluate_equality_inequality_operation(
					lhs,
					&EqualityAndInequality::LessThan,
					rhs,
					types,
					strict_casts,
				)
			} else {
				let less_than_result = evaluate_equality_inequality_operation(
					lhs,
					&EqualityAndInequality::LessThan,
					rhs,
					types,
					strict_casts,
				);
				types.new_logical_or_type(equality_result, less_than_result)
			}
		}
		EqualityAndInequality::StrictNotEqual => {
			let equality_result = evaluate_equality_inequality_operation(
				lhs,
				&EqualityAndInequality::StrictEqual,
				rhs,
				types,
				strict_casts,
			);
			evaluate_pure_unary_operator(
				PureUnary::LogicalNot,
				equality_result,
				types,
				strict_casts,
			)
		}
		EqualityAndInequality::Equal => {
			crate::utilities::notify!("TODO equal operator");
			TypeId::OPEN_BOOLEAN_TYPE
		}
		EqualityAndInequality::NotEqual => {
			let equality_result = evaluate_equality_inequality_operation(
				lhs,
				&EqualityAndInequality::Equal,
				rhs,
				types,
				strict_casts,
			);
			evaluate_pure_unary_operator(
				PureUnary::LogicalNot,
				equality_result,
				types,
				strict_casts,
			)
		}
		// Swapping operands!
		EqualityAndInequality::GreaterThan => evaluate_equality_inequality_operation(
			rhs,
			&EqualityAndInequality::LessThan,
			lhs,
			types,
			strict_casts,
		),
		// Swapping operands!
		EqualityAndInequality::GreaterThanOrEqual => evaluate_equality_inequality_operation(
			rhs,
			&EqualityAndInequality::LessThanOrEqual,
			lhs,
			types,
			strict_casts,
		),
	}
}

#[allow(clippy::let_and_return)]
pub fn is_null_or_undefined(ty: TypeId, types: &mut TypeStore) -> TypeId {
	let is_null = evaluate_equality_inequality_operation(
		ty,
		&EqualityAndInequality::StrictEqual,
		TypeId::NULL_TYPE,
		types,
		false,
	);
	// TODO temp to fix narrowing
	// let is_undefined = evaluate_equality_inequality_operation(
	// 	ty,
	// 	&EqualityAndInequality::StrictEqual,
	// 	TypeId::UNDEFINED_TYPE,
	// 	types,
	// 	false,
	// );

	// types.new_logical_or_type(is_null, is_undefined)
	is_null
}

#[derive(Copy, Clone, Debug)]
pub enum LogicalOperator {
	And,
	Or,
	/// TODO is this canonical?
	NullCoalescing,
}

/// TODO strict casts!
pub fn evaluate_logical_operation_with_expression<
	'a,
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
>(
	lhs: (TypeId, Span),
	operator: LogicalOperator,
	rhs: &'a A::Expression<'a>,
	checking_data: &mut CheckingData<T, A>,
	environment: &mut Environment,
	expecting: TypeId,
) -> Result<TypeId, ()> {
	match operator {
		LogicalOperator::And => Ok(new_conditional_context(
			environment,
			lhs,
			|env: &mut Environment, data: &mut CheckingData<T, A>| {
				A::synthesise_expression(rhs, expecting, env, data)
			},
			Some(|_env: &mut Environment, _data: &mut CheckingData<T, A>| lhs.0),
			checking_data,
		)),
		LogicalOperator::Or => Ok(new_conditional_context(
			environment,
			lhs,
			|env: &mut Environment, checking_data: &mut CheckingData<T, A>| {
				if let Some(constraint) = crate::types::get_constraint(lhs.0, &checking_data.types)
				{
					let mut result = Vec::new();
					super::narrowing::build_union_from_filter(
						constraint,
						super::narrowing::NOT_FASLY,
						&mut result,
						env,
						&checking_data.types,
					);
					let narrowed_to = checking_data.types.new_or_type_from_iterator(result);
					checking_data.types.register_type(Type::Narrowed { from: lhs.0, narrowed_to })
				} else {
					lhs.0
				}
			},
			Some(|env: &mut Environment, data: &mut CheckingData<T, A>| {
				A::synthesise_expression(rhs, expecting, env, data)
			}),
			checking_data,
		)),
		LogicalOperator::NullCoalescing => {
			let is_lhs_null_or_undefined = is_null_or_undefined(lhs.0, &mut checking_data.types);
			// Equivalent to: `(lhs is null or undefined) ? lhs : rhs`
			Ok(new_conditional_context(
				environment,
				(is_lhs_null_or_undefined, lhs.1),
				|env: &mut Environment, checking_data: &mut CheckingData<T, A>| {
					if let Some(constraint) =
						crate::types::get_constraint(lhs.0, &checking_data.types)
					{
						let mut result = Vec::new();
						super::narrowing::build_union_from_filter(
							constraint,
							super::narrowing::NOT_NULL_OR_UNDEFINED,
							&mut result,
							env,
							&checking_data.types,
						);
						let narrowed_to = checking_data.types.new_or_type_from_iterator(result);
						checking_data
							.types
							.register_type(Type::Narrowed { from: lhs.0, narrowed_to })
					} else {
						lhs.0
					}
				},
				Some(|env: &mut Environment, data: &mut CheckingData<T, A>| {
					A::synthesise_expression(rhs, expecting, env, data)
				}),
				checking_data,
			))
		}
	}
}

/// `typeof` and some others done elsewhere
#[derive(Clone, Copy, Debug, binary_serialize_derive::BinarySerializable)]
pub enum PureUnary {
	LogicalNot,
	Negation,
	BitwiseNot,
}

pub fn evaluate_pure_unary_operator(
	operator: PureUnary,
	operand: TypeId,
	types: &mut TypeStore,
	strict_casts: bool,
) -> TypeId {
	if operand == TypeId::ERROR_TYPE {
		return operand;
	}

	match operator {
		PureUnary::LogicalNot => {
			if let Decidable::Known(value) = is_type_truthy_falsy(operand, types) {
				if value {
					TypeId::FALSE
				} else {
					TypeId::TRUE
				}
			} else {
				types.new_logical_negation_type(operand)
			}
		}
		PureUnary::Negation | PureUnary::BitwiseNot => {
			if let Type::Constant(cst) = types.get_type_by_id(operand) {
				let value = cast_as_number(cst, strict_casts).expect("hmm");
				let value = match operator {
					PureUnary::LogicalNot => unreachable!(),
					PureUnary::Negation => -value,
					PureUnary::BitwiseNot => f64::from(!(value as i32)),
				};
				let value = ordered_float::NotNan::try_from(value);
				match value {
					Ok(value) => types.new_constant_type(Constant::Number(value)),
					Err(_) => TypeId::NAN,
				}
			} else {
				types.register_type(Type::Constructor(crate::types::Constructor::UnaryOperator {
					operator,
					operand,
				}))
			}
		}
	}
}

/// Returns whether lhs and rhs are always equal or never equal. TODO more
///
/// TODO return decidable.
fn attempt_constant_equality(
	lhs: TypeId,
	rhs: TypeId,
	types: &mut TypeStore,
) -> Result<TypeId, ()> {
	let are_equal = if lhs == rhs {
		true
	} else if matches!(lhs, TypeId::NULL_TYPE | TypeId::UNDEFINED_TYPE)
		|| matches!(rhs, TypeId::NULL_TYPE | TypeId::UNDEFINED_TYPE)
	{
		// If above `==`` failed => false (as always have same `TypeId`)
		false
	} else {
		let lhs = types.get_type_by_id(lhs);
		let rhs = types.get_type_by_id(rhs);
		if let (Type::Constant(cst1), Type::Constant(cst2)) = (lhs, rhs) {
			cst1 == cst2
		} else if let (Type::Object(..) | Type::SpecialObject(SpecialObject::Function(..)), _)
		| (_, Type::Object(..) | Type::SpecialObject(SpecialObject::Function(..))) = (lhs, rhs)
		{
			// Same objects and functions always have same type id. Poly case doesn't occur here
			false
		}
		// Temp fix for closures
		else if let (
			Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on: on_lhs, .. }),
			Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on: on_rhs, .. }),
		) = (lhs, rhs)
		{
			// TODO does this work?
			return attempt_constant_equality(*on_lhs, *on_rhs, types);
		} else {
			crate::utilities::notify!("{:?} === {:?} is apparently false", lhs, rhs);
			return Err(());
		}
	};

	Ok(types.new_constant_type(Constant::Boolean(are_equal)))
}
