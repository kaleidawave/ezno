use crate::{
	types::{cast_as_number, cast_as_string, helpers, intrinsics, Constant, Type},
	TypeId,
};

/// For these **binary** operations both operands are synthesised
#[derive(Clone, Copy, Debug, binary_serialize_derive::BinarySerializable)]
pub enum MathematicalOrBitwiseOperation {
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

/// TODO proper error type
pub fn evaluate_mathematical_operation(
	lhs: TypeId,
	operator: MathematicalOrBitwiseOperation,
	rhs: TypeId,
	info: &impl crate::context::InformationChain,
	types: &mut crate::TypeStore,
	strict_casts: bool,
	operate_on_number_intrinsics: bool,
) -> Result<TypeId, ()> {
	fn attempt_constant_math_operator(
		lhs: TypeId,
		operator: MathematicalOrBitwiseOperation,
		rhs: TypeId,
		types: &mut crate::TypeStore,
		strict_casts: bool,
	) -> Result<TypeId, ()> {
		if let MathematicalOrBitwiseOperation::Add = operator {
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
				_ => {
					crate::utilities::notify!("here");
					return Err(());
				}
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
						MathematicalOrBitwiseOperation::Add => unreachable!(),
						MathematicalOrBitwiseOperation::Subtract => lhs - rhs,
						MathematicalOrBitwiseOperation::Multiply => lhs * rhs,
						MathematicalOrBitwiseOperation::Divide => lhs / rhs,
						MathematicalOrBitwiseOperation::Modulo => lhs % rhs,
						MathematicalOrBitwiseOperation::Exponent => lhs.powf(rhs),
						MathematicalOrBitwiseOperation::BitwiseShiftLeft => {
							f64::from((lhs as i32).checked_shl(rhs as u32).unwrap_or(0))
						}
						MathematicalOrBitwiseOperation::BitwiseShiftRight => {
							f64::from((lhs as i32).checked_shr(rhs as u32).unwrap_or(0))
						}
						MathematicalOrBitwiseOperation::BitwiseShiftRightUnsigned => {
							(lhs as i32).wrapping_shr(rhs as u32).into()
						}
						MathematicalOrBitwiseOperation::BitwiseAnd => {
							f64::from((lhs as i32) & (rhs as i32))
						}
						MathematicalOrBitwiseOperation::BitwiseXOr => {
							f64::from((lhs as i32) ^ (rhs as i32))
						}
						MathematicalOrBitwiseOperation::BitwiseOr => {
							f64::from((lhs as i32) | (rhs as i32))
						}
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

	let lhs_ty = types.get_type_by_id(lhs);
	let rhs_ty = types.get_type_by_id(rhs);

	if lhs == TypeId::ERROR_TYPE || rhs == TypeId::ERROR_TYPE {
		return Ok(TypeId::ERROR_TYPE);
	}

	crate::utilities::notify!("lhs={:?}, rhs={:?}", lhs_ty, rhs_ty);
	let either_dependent = lhs_ty.is_dependent() || rhs_ty.is_dependent();

	if either_dependent {
		let can_be_string = if let MathematicalOrBitwiseOperation::Add = operator {
			let left_is_string = helpers::simple_subtype(lhs, TypeId::STRING_TYPE, info, types);
			let right_is_string = helpers::simple_subtype(lhs, TypeId::STRING_TYPE, info, types);
			let left_is_string_or_number =
				left_is_string || helpers::simple_subtype(lhs, TypeId::NUMBER_TYPE, info, types);
			let right_is_string_or_number =
				right_is_string || helpers::simple_subtype(rhs, TypeId::NUMBER_TYPE, info, types);
			if !left_is_string_or_number || !right_is_string_or_number {
				crate::utilities::notify!("Here");
				return Err(());
			}

			left_is_string || right_is_string
		} else {
			if !helpers::simple_subtype(lhs, TypeId::NUMBER_TYPE, info, types)
				|| !helpers::simple_subtype(rhs, TypeId::NUMBER_TYPE, info, types)
			{
				crate::utilities::notify!("Here :/");
				return Err(());
			}

			false
		};

		// :)
		if let (MathematicalOrBitwiseOperation::Exponent, TypeId::ZERO) = (operator, rhs) {
			// This holds for NaN. Thus can do in every case
			return Ok(TypeId::ONE);
		} else if let (MathematicalOrBitwiseOperation::Exponent, TypeId::ONE, true) =
			(operator, rhs, intrinsics::is_not_not_a_number(lhs, types))
		{
			return Ok(lhs);
		} else if let (MathematicalOrBitwiseOperation::Add, TypeId::ZERO)
		| (MathematicalOrBitwiseOperation::Multiply, TypeId::ONE) = (operator, rhs)
		{
			return Ok(lhs);
		} else if let (MathematicalOrBitwiseOperation::Add, TypeId::ZERO)
		| (MathematicalOrBitwiseOperation::Multiply, TypeId::ONE) = (operator, lhs)
		{
			return Ok(rhs);
		}

		let result = if can_be_string {
			TypeId::STRING_TYPE
		} else if let (
			MathematicalOrBitwiseOperation::Add | MathematicalOrBitwiseOperation::Multiply,
			(lhs_range, _lhs_modulo),
			(rhs_range, _rhs_modulo),
			true,
		) = (
			operator,
			intrinsics::get_range_and_mod_class(lhs, types),
			intrinsics::get_range_and_mod_class(rhs, types),
			operate_on_number_intrinsics,
		) {
			crate::utilities::notify!(
				"{:?} with {:?}",
				(lhs_range, _lhs_modulo),
				(rhs_range, _rhs_modulo)
			);
			if let (Some(lhs_range), Some(rhs_range)) = (lhs_range, rhs_range) {
				let range = match operator {
					MathematicalOrBitwiseOperation::Add => lhs_range.space_addition(rhs_range),
					MathematicalOrBitwiseOperation::Multiply => {
						lhs_range.space_multiplication(rhs_range)
					}
					_ => unreachable!(),
				};
				intrinsics::range_to_type(range, types)
			} else {
				TypeId::NUMBER_TYPE
			}
		} else {
			// TODO if or of constant types
			TypeId::NUMBER_TYPE
		};

		let constructor = crate::types::Constructor::BinaryOperator { lhs, operator, rhs, result };
		Ok(types.register_type(crate::Type::Constructor(constructor)))
	} else {
		attempt_constant_math_operator(lhs, operator, rhs, types, strict_casts)
	}
}
