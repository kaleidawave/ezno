use derive_enum_from_into::EnumFrom;
use source_map::Span;

use crate::{
	types::{
		cast_as_number, cast_as_string, is_type_truthy_falsy, new_logical_or_type,
		StructureGenerics, TypeStore,
	},
	CheckingData, Constant, Environment, SynthesizableConditional, SynthesizableExpression,
	TruthyFalsy, Type, TypeId,
};

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum MathematicalAndBitwise {
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulo,
	Exponent,
	// Bitwise
	BitwiseShiftLeft,
	BitwiseShiftRight,
	BitwiseShiftRightUnsigned,
	BitwiseAnd,
	BitwiseXOr,
	BitwiseOr,
}

const STRICT_CASTS: bool = false;

#[derive(Clone, Debug, EnumFrom)]
pub enum PureBinaryOperation {
	MathematicalAndBitwise(MathematicalAndBitwise),
	// Some of these can be reduced
	EqualityAndInequality(EqualityAndInequality),
	TypePropertyOperator(TypePropertyOperator),
}

pub fn evaluate_pure_binary_operation_handle_errors<T: crate::FSResolver>(
	(lhs, lhs_pos): (TypeId, Span),
	operator: PureBinaryOperation,
	(rhs, rhs_pos): (TypeId, Span),
	checking_data: &mut CheckingData<T>,
	environment: &mut Environment,
) -> TypeId {
	// environment,
	// checking_data.settings.strict_casts,
	let result = match operator {
		PureBinaryOperation::MathematicalAndBitwise(operator) => {
			evaluate_mathematical_operation(lhs, operator, rhs, &mut checking_data.types)
		}
		PureBinaryOperation::EqualityAndInequality(operator) => {
			evaluate_equality_inequality_operation(lhs, operator, rhs, &mut checking_data.types)
		}
		PureBinaryOperation::TypePropertyOperator(_) => todo!(),
	};
	match result {
		Ok(ok) => ok,
		Err(error) => {
			todo!();
			// let error = match error {
			// 	BinaryOperatorError::InvalidMathematicalOperation(error) => {
			// 		TypeCheckError::InvalidMathematicalOperation(error)
			// 	}
			// 	BinaryOperatorError::NotDefined(op) => {
			// 		TypeCheckError::NotDefinedOperator(op, lhs.1.union(&rhs.1))
			// 	}
			// };
			// checking_data.diagnostics_container.add_error(error);
			TypeId::ERROR_TYPE
		}
	}
}

pub fn evaluate_mathematical_operation(
	lhs: TypeId,
	operator: MathematicalAndBitwise,
	rhs: TypeId,
	types: &mut TypeStore,
) -> Result<TypeId, ()> {
	fn attempt_constant_math_operator(
		lhs: TypeId,
		operator: MathematicalAndBitwise,
		rhs: TypeId,
		types: &mut TypeStore,
	) -> Result<TypeId, ()> {
		if let MathematicalAndBitwise::Add = operator {
			let constant = match (types.get_type_by_id(lhs), types.get_type_by_id(rhs)) {
				(Type::Constant(Constant::Number(lhs)), Type::Constant(Constant::Number(rhs))) => {
					Constant::Number(lhs + rhs)
				}
				(Type::Constant(c1), Type::Constant(c2)) => {
					// TODO temp
					let result = format!(
						"{}{}",
						cast_as_string(&c1, STRICT_CASTS).unwrap(),
						cast_as_string(&c2, STRICT_CASTS).unwrap()
					);
					Constant::String(result)
				}
				_ => return Err(()),
			};
			Ok(types.new_constant_type(constant))
		} else {
			match (types.get_type_by_id(lhs), types.get_type_by_id(rhs)) {
				(Type::Constant(c1), Type::Constant(c2)) => {
					let lhs = cast_as_number(c1, STRICT_CASTS).unwrap_or(f64::NAN);
					let rhs = cast_as_number(c2, STRICT_CASTS).unwrap_or(f64::NAN);
					// TODO hopefully Rust implementation is the same as JS
					let value = match operator {
						MathematicalAndBitwise::Add => unreachable!(),
						MathematicalAndBitwise::Subtract => lhs - rhs,
						MathematicalAndBitwise::Multiply => lhs * rhs,
						MathematicalAndBitwise::Divide => lhs / rhs,
						MathematicalAndBitwise::Modulo => lhs % rhs,
						MathematicalAndBitwise::Exponent => lhs.powf(rhs),
						MathematicalAndBitwise::BitwiseShiftLeft => {
							f64::from((lhs as i32) << (rhs as i32))
						}
						MathematicalAndBitwise::BitwiseShiftRight => {
							f64::from((lhs as i32) >> (rhs as i32))
						}
						MathematicalAndBitwise::BitwiseShiftRightUnsigned => todo!(),
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
						Err(_) => TypeId::NAN_TYPE,
					};
					Ok(ty)
				}
				_ => return Err(()),
			}
		}
	}

	let is_dependent =
		types.get_type_by_id(lhs).is_dependent() || types.get_type_by_id(rhs).is_dependent();

	// TODO check sides
	if is_dependent {
		let constructor = crate::types::Constructor::BinaryOperator { lhs, operator, rhs };
		return Ok(types.register_type(crate::Type::Constructor(constructor)));
	}

	match attempt_constant_math_operator(lhs, operator, rhs, types) {
		Ok(ty) => Ok(ty),
		Err(_) => {
			todo!("operator error")
		}
	}
}

/// Not canonical / reducible
#[derive(Clone, Debug)]
pub enum EqualityAndInequality {
	StrictEqual,
	StrictNotEqual,
	Equal,
	NotEqual,
	GreaterThan,
	LessThan,
	LessThanEqual,
	GreaterThanEqual,
}

/// Canonical / irreducible
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum CanonicalEqualityAndInequality {
	StrictEqual,
	LessThan,
}

pub fn evaluate_equality_inequality_operation(
	lhs: TypeId,
	operator: EqualityAndInequality,
	rhs: TypeId,
	types: &mut TypeStore,
) -> Result<TypeId, ()> {
	match operator {
		EqualityAndInequality::StrictEqual => {
			// crate::utils::notify!("{:?} === {:?}", lhs, rhs);

			let is_dependent = types.get_type_by_id(lhs).is_dependent()
				|| types.get_type_by_id(rhs).is_dependent();

			// TODO check lhs and rhs type to see if they overlap
			if is_dependent {
				let constructor = crate::types::Constructor::CanonicalRelationOperator {
					lhs,
					operator: CanonicalEqualityAndInequality::StrictEqual,
					rhs,
				};
				return Ok(types.register_type(crate::Type::Constructor(constructor)));
			}

			match attempt_constant_equality(lhs, rhs, types) {
				Ok(ty) => Ok(ty),
				Err(_) => todo!(),
			}
		}
		EqualityAndInequality::LessThan => {
			fn attempt_less_than(
				lhs: TypeId,
				rhs: TypeId,
				types: &mut TypeStore,
			) -> Result<TypeId, ()> {
				// Similar but reversed semantics to add
				Ok(types.new_constant_type(
					match (types.get_type_by_id(lhs), types.get_type_by_id(rhs)) {
						(
							Type::Constant(Constant::String(a)),
							Type::Constant(Constant::String(b)),
						) => Constant::Boolean(a < b),
						(Type::Constant(c1), Type::Constant(c2)) => {
							let lhs = cast_as_number(&c1, STRICT_CASTS).unwrap();
							let rhs = cast_as_number(&c2, STRICT_CASTS).unwrap();
							Constant::Boolean(lhs < rhs)
						}
						_ => return Err(()),
					},
				))
			}

			let is_dependent = types.get_type_by_id(lhs).is_dependent()
				|| types.get_type_by_id(rhs).is_dependent();

			if is_dependent {
				let constructor = crate::types::Constructor::CanonicalRelationOperator {
					lhs,
					operator: CanonicalEqualityAndInequality::LessThan,
					rhs,
				};
				return Ok(types.register_type(crate::Type::Constructor(constructor)));
			}

			match attempt_less_than(lhs, rhs, types) {
				Ok(ty) => Ok(ty),
				Err(_) => todo!(),
			}
		}
		EqualityAndInequality::StrictNotEqual => {
			let equality_result = evaluate_equality_inequality_operation(
				rhs,
				EqualityAndInequality::StrictEqual,
				lhs,
				types,
			)?;
			evaluate_pure_unary_operator(PureUnary::LogicalNot, equality_result, types)
		}
		EqualityAndInequality::Equal => todo!(),
		EqualityAndInequality::NotEqual => todo!(),
		EqualityAndInequality::GreaterThan => {
			evaluate_equality_inequality_operation(rhs, EqualityAndInequality::LessThan, lhs, types)
		}
		EqualityAndInequality::LessThanEqual => {
			let lhs = evaluate_equality_inequality_operation(
				rhs,
				EqualityAndInequality::StrictEqual,
				lhs,
				types,
			)?;

			if lhs == TypeId::TRUE {
				Ok(lhs)
			} else if lhs == TypeId::FALSE {
				evaluate_equality_inequality_operation(
					rhs,
					EqualityAndInequality::LessThan,
					lhs,
					types,
				)
			} else {
				let rhs = evaluate_equality_inequality_operation(
					rhs,
					EqualityAndInequality::LessThan,
					lhs,
					types,
				)?;
				Ok(new_logical_or_type(lhs, rhs, types))
			}
		}
		EqualityAndInequality::GreaterThanEqual => evaluate_equality_inequality_operation(
			rhs,
			EqualityAndInequality::LessThanEqual,
			lhs,
			types,
		),
	}
}

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum TypePropertyOperator {
	InstanceOf,
	In,
}

pub fn evaluate_type_property_operator(
	lhs: TypeId,
	operator: TypePropertyOperator,
	rhs: TypeId,
	types: &mut TypeStore,
) -> Result<TypeId, ()> {
	match operator {
		TypePropertyOperator::InstanceOf => todo!(),
		TypePropertyOperator::In => todo!(),
	}
}

#[derive(Clone, Debug)]
pub enum Logical {
	And,
	Or,
	NullCoalescing,
}

/// TODO strict casts!
pub fn evaluate_logical_operation_with_expression<T: crate::FSResolver>(
	lhs: TypeId,
	operator: Logical,
	rhs: &impl SynthesizableExpression,
	checking_data: &mut CheckingData<T>,
	environment: &mut Environment,
) -> Result<TypeId, ()> {
	enum TypeOrSynthesizable<'a, TExpr: SynthesizableExpression> {
		Type(TypeId),
		Expression(&'a TExpr),
	}

	impl<'a, TExpr: SynthesizableExpression> SynthesizableConditional
		for TypeOrSynthesizable<'a, TExpr>
	{
		type ExpressionResult = TypeId;

		fn synthesize_condition<T: crate::FSResolver>(
			self,
			environment: &mut Environment,
			checking_data: &mut CheckingData<T>,
		) -> Self::ExpressionResult {
			match self {
				TypeOrSynthesizable::Type(ty) => ty,
				TypeOrSynthesizable::Expression(expr) => {
					TExpr::synthesize_expression(&expr, environment, checking_data)
				}
			}
		}

		fn conditional_expression_result(
			condition: TypeId,
			truthy_result: Self::ExpressionResult,
			falsy_result: Self::ExpressionResult,
			types: &mut TypeStore,
		) -> Self::ExpressionResult {
			types.new_conditional_type(condition, truthy_result, falsy_result)
		}

		fn default_result() -> Self::ExpressionResult {
			unreachable!()
		}
	}

	match operator {
		Logical::And => Ok(environment.new_conditional_context(
			lhs,
			TypeOrSynthesizable::Expression(rhs),
			Some(TypeOrSynthesizable::Type(lhs)),
			checking_data,
		)),
		Logical::Or => Ok(environment.new_conditional_context(
			lhs,
			TypeOrSynthesizable::Type(lhs),
			Some(TypeOrSynthesizable::Expression(rhs)),
			checking_data,
		)),
		Logical::NullCoalescing => {
			let is_lhs_null = evaluate_equality_inequality_operation(
				lhs,
				EqualityAndInequality::StrictEqual,
				TypeId::NULL_TYPE,
				&mut checking_data.types,
			)?;
			Ok(environment.new_conditional_context(
				is_lhs_null,
				TypeOrSynthesizable::Expression(rhs),
				Some(TypeOrSynthesizable::Type(lhs)),
				checking_data,
			))
		}
	}
}

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum PureUnary {
	LogicalNot,
	Negation,
	BitwiseNot,
}

pub fn evaluate_pure_unary_operator(
	operator: PureUnary,
	operand: TypeId,
	types: &mut TypeStore,
) -> Result<TypeId, ()> {
	match operator {
		PureUnary::LogicalNot => {
			if let TruthyFalsy::Decidable(value) = is_type_truthy_falsy(operand, types) {
				if value {
					Ok(TypeId::FALSE)
				} else {
					Ok(TypeId::TRUE)
				}
			} else {
				Ok(types.new_logical_negation_type(operand))
			}
		}
		PureUnary::Negation | PureUnary::BitwiseNot => {
			if let Type::Constant(cst) = types.get_type_by_id(operand) {
				let value = cast_as_number(cst, STRICT_CASTS)?;
				let value = match operator {
					PureUnary::LogicalNot => unreachable!(),
					PureUnary::Negation => -value,
					PureUnary::BitwiseNot => f64::from(!(value as i32)),
				};
				let value = ordered_float::NotNan::try_from(value);
				Ok(match value {
					Ok(value) => types.new_constant_type(Constant::Number(value)),
					Err(_) => TypeId::NAN_TYPE,
				})
			} else {
				Ok(types.register_type(Type::Constructor(
					crate::types::Constructor::UnaryOperator { operator, operand },
				)))
			}
		}
	}
}

fn attempt_constant_equality(
	lhs: TypeId,
	rhs: TypeId,
	types: &mut TypeStore,
) -> Result<TypeId, ()> {
	let are_equal =
		if lhs == rhs {
			true
		} else {
			let lhs = types.get_type_by_id(lhs);
			let rhs = types.get_type_by_id(rhs);
			if let (Type::Constant(cst1), Type::Constant(cst2)) = (lhs, rhs) {
				cst1 == cst2
			} else if let (Type::Object(..), _) | (_, Type::Object(..)) = (lhs, rhs) {
				// Same objects always have same type id
				false
			}
			// Temp fix for closures
			else if let (
				Type::Constructor(crate::types::Constructor::StructureGenerics(
					StructureGenerics { on: on_lhs, .. },
				)),
				Type::Constructor(crate::types::Constructor::StructureGenerics(
					StructureGenerics { on: on_rhs, .. },
				)),
			) = (lhs, rhs)
			{
				// TODO
				return attempt_constant_equality(*on_lhs, *on_rhs, types);
			} else {
				// TODO also Err if unknown
				crate::utils::notify!("{:?} === {:?} is apparently false", lhs, rhs);
				return Err(());
			}
		};
	Ok(types.new_constant_type(Constant::Boolean(are_equal)))
}
