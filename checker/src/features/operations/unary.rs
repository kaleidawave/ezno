use crate::{
	types::{
		cast_as_number, helpers::simple_subtype, is_type_truthy_falsy, Constant, Type, TypeId,
	},
	Decidable,
};

/// `typeof` and some others done elsewhere
#[derive(Clone, Copy, Debug, binary_serialize_derive::BinarySerializable)]
pub enum UnaryOperation {
	/// Treated as `(value ? false : true)`
	LogicalNot,
	/// Treated as `0 - value` (could also do -1 * value?)
	Negation,
	/// Treated as `value ^ 0xFFFF_FFFF`
	BitwiseNot,
}

/// Tries to evaluate unary operation for constant terms. Else delegates to binary operations that handle equivalent thing
pub fn evaluate_unary_operator(
	operator: UnaryOperation,
	operand: TypeId,
	info: &impl crate::context::InformationChain,
	types: &mut crate::TypeStore,
	strict_casts: bool,
) -> Result<TypeId, ()> {
	if operand == TypeId::ERROR_TYPE {
		return Ok(operand);
	}

	match operator {
		UnaryOperation::LogicalNot => {
			if let Decidable::Known(value) = is_type_truthy_falsy(operand, types) {
				if value {
					Ok(TypeId::FALSE)
				} else {
					Ok(TypeId::TRUE)
				}
			} else {
				let is_boolean = simple_subtype(operand, TypeId::BOOLEAN_TYPE, info, types);
				if is_boolean {
					Ok(types.new_logical_negation_type(operand))
				} else {
					Err(())
				}
			}
		}
		UnaryOperation::Negation | UnaryOperation::BitwiseNot => {
			if let Type::Constant(cst) = types.get_type_by_id(operand) {
				let value = cast_as_number(cst, strict_casts).expect("hmm");
				let value = match operator {
					UnaryOperation::BitwiseNot => f64::from(!(value as i32)),
					UnaryOperation::Negation => -value,
					UnaryOperation::LogicalNot => unreachable!(),
				};
				let value = ordered_float::NotNan::try_from(value);
				Ok(match value {
					Ok(value) => types.new_constant_type(Constant::Number(value)),
					Err(_) => TypeId::NAN,
				})
			} else {
				match operator {
					UnaryOperation::BitwiseNot => super::evaluate_mathematical_operation(
						TypeId::MAX_U32,
						super::MathematicalOrBitwiseOperation::BitwiseXOr,
						operand,
						info,
						types,
						strict_casts,
					),
					UnaryOperation::Negation => super::evaluate_mathematical_operation(
						TypeId::ZERO,
						super::MathematicalOrBitwiseOperation::Subtract,
						operand,
						info,
						types,
						strict_casts,
					),
					UnaryOperation::LogicalNot => unreachable!("handled above"),
				}
			}
		}
	}
}
