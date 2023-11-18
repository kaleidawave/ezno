use std::marker::PhantomData;

use derive_enum_from_into::EnumFrom;
use source_map::{Span, SpanWithSource};

use crate::{
	diagnostics::TypeCheckError,
	synthesis::EznoParser,
	types::{
		cast_as_number, cast_as_string, is_type_truthy_falsy, new_logical_or_type,
		StructureGenerics, TypeStore,
	},
	ASTImplementation, CheckingData, Constant, Environment, TruthyFalsy, Type, TypeId,
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

/// TODO report errors better here
pub fn evaluate_pure_binary_operation_handle_errors<T: crate::ReadFromFS, M: ASTImplementation>(
	(lhs, lhs_pos): (TypeId, SpanWithSource),
	operator: PureBinaryOperation,
	(rhs, rhs_pos): (TypeId, SpanWithSource),
	checking_data: &mut CheckingData<T, M>,
	environment: &mut Environment,
) -> TypeId {
	// environment,
	// checking_data.settings.strict_casts,
	match operator {
		PureBinaryOperation::MathematicalAndBitwise(operator) => {
			let result =
				evaluate_mathematical_operation(lhs, operator, rhs, &mut checking_data.types);
			match result {
				Ok(result) => result,
				Err(_) => {
					// TODO
					// checking_data.diagnostics_container.add_error(TypeCheckError::InvalidMathematicalOperation(()))
					TypeId::ERROR_TYPE
				}
			}
		}
		PureBinaryOperation::EqualityAndInequality(operator) => {
			evaluate_equality_inequality_operation(lhs, operator, rhs, &mut checking_data.types)
				.unwrap_or(TypeId::ERROR_TYPE)
		}
		PureBinaryOperation::TypePropertyOperator(operator) => {
			evaluate_type_property_operator(lhs, operator, rhs, &checking_data.types, &environment)
				.unwrap_or(TypeId::ERROR_TYPE)
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
				(Type::Constant(lhs), Type::Constant(rhs)) => {
					let mut first = cast_as_string(lhs, STRICT_CASTS)?;
					let second = cast_as_string(rhs, STRICT_CASTS)?;
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
						Err(_) => TypeId::NAN_TYPE,
					};
					Ok(ty)
				}
				_ => Err(()),
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

	attempt_constant_math_operator(lhs, operator, rhs, types)
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
				Err(_) => {
					unreachable!("should have been caught by above")
				}
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
							let lhs = cast_as_number(c1, STRICT_CASTS)?;
							let rhs = cast_as_number(c2, STRICT_CASTS)?;
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

			attempt_less_than(lhs, rhs, types)
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
		EqualityAndInequality::Equal => {
			crate::utils::notify!("TODO equal operator");
			Err(())
		}
		EqualityAndInequality::NotEqual => {
			let equality_result = evaluate_equality_inequality_operation(
				rhs,
				EqualityAndInequality::Equal,
				lhs,
				types,
			)?;
			evaluate_pure_unary_operator(PureUnary::LogicalNot, equality_result, types)
		}
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
	types: &TypeStore,
	environment: &Environment,
) -> Result<TypeId, ()> {
	todo!("bad function definition")
	// match operator {
	// 	TypePropertyOperator::InstanceOf => {
	// 		crate::utils::notify!("instanceof operator");
	// 		Err(())
	// 	}
	// 	TypePropertyOperator::In => {
	// 		// TODO privacy
	// 		// let result = environment.get_property_unbound(
	// 		// 	rhs,
	// 		// 	lhs,
	// 		// 	crate::context::facts::PublicityKind::Public,
	// 		// 	types,
	// 		// );
	// 		// match result {
	// 		// 	Some(crate::context::Logical::Pure(..)) => Ok(TypeId::TRUE),
	// 		// 	Some(_) => {
	// 		// 		crate::utils::notify!("Unsure of property on 'in' operator implementation");
	// 		// 		Err(())
	// 		// 	}
	// 		// 	None => Ok(TypeId::FALSE),
	// 		// }
	// 	}
	// }
}

#[derive(Clone, Debug)]
pub enum Logical {
	And,
	Or,
	NullCoalescing,
}

/// TODO strict casts!
pub fn evaluate_logical_operation_with_expression<
	T: crate::ReadFromFS,
	M: crate::ASTImplementation,
>(
	lhs: TypeId,
	operator: Logical,
	rhs: &M::Expression,
	checking_data: &mut CheckingData<T, M>,
	environment: &mut Environment,
) -> Result<TypeId, ()> {
	enum TypeOrSynthesisable<'a, M: crate::ASTImplementation> {
		Type(TypeId),
		Expression(&'a M::Expression),
	}

	// impl<'a, M: crate::ASTImplementation> SynthesisableConditional<M> for TypeOrSynthesisable<'a, M> {
	// 	type ExpressionResult = TypeId;

	// 	fn synthesise_condition<T: crate::ReadFromFS>(
	// 		self,
	// 		environment: &mut Environment,
	// 		checking_data: &mut CheckingData<T, M>,
	// 	) -> Self::ExpressionResult {
	// 		match self {
	// 			TypeOrSynthesisable::Type(ty) => ty,
	// 			TypeOrSynthesisable::Expression(expr, _) => {
	// 				M::synthesise_expression(expr, TypeId::ANY_TYPE, environment, checking_data)
	// 			}
	// 		}
	// 	}

	// 	fn conditional_expression_result(
	// 		condition: TypeId,
	// 		truthy_result: Self::ExpressionResult,
	// 		falsy_result: Self::ExpressionResult,
	// 		types: &mut TypeStore,
	// 	) -> Self::ExpressionResult {
	// 		types.new_conditional_type(condition, truthy_result, falsy_result)
	// 	}

	// 	fn default_result() -> Self::ExpressionResult {
	// 		unreachable!()
	// 	}
	// }

	match operator {
		Logical::And => Ok(environment.new_conditional_context(
			lhs,
			|env: &mut Environment, data: &mut CheckingData<T, M>| {
				M::synthesise_expression(rhs, TypeId::ANY_TYPE, env, data)
			},
			Some(|_env: &mut Environment, _data: &mut CheckingData<T, M>| lhs),
			checking_data,
		)),
		Logical::Or => Ok(environment.new_conditional_context(
			lhs,
			|_env: &mut Environment, _data: &mut CheckingData<T, M>| lhs,
			Some(|env: &mut Environment, data: &mut CheckingData<T, M>| {
				M::synthesise_expression(rhs, TypeId::ANY_TYPE, env, data)
			}),
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
				|env: &mut Environment, data: &mut CheckingData<T, M>| {
					M::synthesise_expression(rhs, TypeId::ANY_TYPE, env, data)
				},
				Some(|_env: &mut Environment, _data: &mut CheckingData<T, M>| lhs),
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
			} else if let (Type::Object(..) | Type::Function(..), _)
			| (_, Type::Object(..) | Type::Function(..)) = (lhs, rhs)
			{
				// Same objects and functions always have same type id. Poly case doesn't occur here
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
				// TODO does this work?
				return attempt_constant_equality(*on_lhs, *on_rhs, types);
			} else {
				crate::utils::notify!("{:?} === {:?} is apparently false", lhs, rhs);
				return Err(());
			}
		};

	Ok(types.new_constant_type(Constant::Boolean(are_equal)))
}
