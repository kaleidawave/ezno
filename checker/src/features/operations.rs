use derive_enum_from_into::EnumFrom;
use source_map::{Span, SpanWithSource};

use crate::{
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	types::{
		cast_as_number, cast_as_string, is_type_truthy_falsy, new_logical_or_type, Constructor,
		StructureGenerics, TypeStore,
	},
	CheckingData, Constant, Decidable, Environment, Type, TypeId,
};

use super::objects::SpecialObjects;

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
							position: lhs_pos
								.without_source()
								.union(rhs_pos.without_source())
								.with_source(environment.get_source()),
						},
					);
					TypeId::ERROR_TYPE
				}
			}
		}
		PureBinaryOperation::EqualityAndInequality(operator) => {
			evaluate_equality_inequality_operation(
				lhs,
				&operator,
				rhs,
				&mut checking_data.types,
				checking_data.options.strict_casts,
			)
			.unwrap_or(TypeId::ERROR_TYPE)
		}
	}
}

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

	if lhs == TypeId::ERROR_TYPE || rhs == TypeId::ERROR_TYPE {
		return Ok(TypeId::ERROR_TYPE);
	}

	let is_dependent =
		types.get_type_by_id(lhs).is_dependent() || types.get_type_by_id(rhs).is_dependent();

	// TODO check sides
	if is_dependent {
		let constructor = crate::types::Constructor::BinaryOperator { lhs, operator, rhs };
		return Ok(types.register_type(crate::Type::Constructor(constructor)));
	}

	attempt_constant_math_operator(lhs, operator, rhs, types, strict_casts)
}

/// Not canonical / reducible
#[derive(Clone, Copy, Debug)]
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
				Err(()) => {
					unreachable!(
						"should have been caught `is_dependent` above, {:?} === {:?}",
						types.get_type_by_id(lhs),
						types.get_type_by_id(rhs)
					)
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
				Ok(types.new_constant_type(
					match (types.get_type_by_id(lhs), types.get_type_by_id(rhs)) {
						(
							Type::Constant(Constant::String(a)),
							Type::Constant(Constant::String(b)),
						) => Constant::Boolean(a < b),
						(Type::Constant(c1), Type::Constant(c2)) => {
							let lhs = cast_as_number(c1, strict_casts)?;
							let rhs = cast_as_number(c2, strict_casts)?;
							Constant::Boolean(lhs < rhs)
						}
						_ => return Err(()),
					},
				))
			}

			let is_dependent = types.get_type_by_id(lhs).is_dependent()
				|| types.get_type_by_id(rhs).is_dependent();

			if is_dependent {
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
						crate::utils::notify!("Shifted LT");
						lhs = *op_lhs;
						rhs = types.register_type(Type::Constant(Constant::Number(lt - add)));
					}
				}
				let constructor = Constructor::CanonicalRelationOperator {
					lhs,
					operator: CanonicalEqualityAndInequality::LessThan,
					rhs,
				};
				return Ok(types.register_type(crate::Type::Constructor(constructor)));
			}

			attempt_less_than(lhs, rhs, types, strict_casts)
		}
		EqualityAndInequality::StrictNotEqual => {
			let equality_result = evaluate_equality_inequality_operation(
				rhs,
				&EqualityAndInequality::StrictEqual,
				lhs,
				types,
				strict_casts,
			)?;
			evaluate_pure_unary_operator(
				PureUnary::LogicalNot,
				equality_result,
				types,
				strict_casts,
			)
		}
		EqualityAndInequality::Equal => {
			crate::utils::notify!("TODO equal operator");
			Err(())
		}
		EqualityAndInequality::NotEqual => {
			let equality_result = evaluate_equality_inequality_operation(
				rhs,
				&EqualityAndInequality::Equal,
				lhs,
				types,
				strict_casts,
			)?;
			evaluate_pure_unary_operator(
				PureUnary::LogicalNot,
				equality_result,
				types,
				strict_casts,
			)
		}
		EqualityAndInequality::GreaterThan => evaluate_equality_inequality_operation(
			rhs,
			&EqualityAndInequality::LessThan,
			lhs,
			types,
			strict_casts,
		),
		EqualityAndInequality::LessThanEqual => {
			let lhs = evaluate_equality_inequality_operation(
				rhs,
				&EqualityAndInequality::StrictEqual,
				lhs,
				types,
				strict_casts,
			)?;

			if lhs == TypeId::TRUE {
				Ok(lhs)
			} else if lhs == TypeId::FALSE {
				evaluate_equality_inequality_operation(
					rhs,
					&EqualityAndInequality::LessThan,
					lhs,
					types,
					strict_casts,
				)
			} else {
				let rhs = evaluate_equality_inequality_operation(
					rhs,
					&EqualityAndInequality::LessThan,
					lhs,
					types,
					strict_casts,
				)?;
				Ok(new_logical_or_type(lhs, rhs, types))
			}
		}
		EqualityAndInequality::GreaterThanEqual => evaluate_equality_inequality_operation(
			rhs,
			&EqualityAndInequality::LessThanEqual,
			lhs,
			types,
			strict_casts,
		),
	}
}

#[derive(Copy, Clone, Debug)]
pub enum Logical {
	And,
	Or,
	NullCoalescing,
}

/// TODO strict casts!
pub fn evaluate_logical_operation_with_expression<
	'a,
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
>(
	lhs: (TypeId, Span),
	operator: Logical,
	rhs: &'a A::Expression<'a>,
	checking_data: &mut CheckingData<T, A>,
	environment: &mut Environment,
) -> Result<TypeId, ()> {
	match operator {
		Logical::And => Ok(environment.new_conditional_context(
			lhs,
			|env: &mut Environment, data: &mut CheckingData<T, A>| {
				A::synthesise_expression(rhs, TypeId::ANY_TYPE, env, data)
			},
			Some(|_env: &mut Environment, _data: &mut CheckingData<T, A>| lhs.0),
			checking_data,
		)),
		Logical::Or => Ok(environment.new_conditional_context(
			lhs,
			|_env: &mut Environment, _data: &mut CheckingData<T, A>| lhs.0,
			Some(|env: &mut Environment, data: &mut CheckingData<T, A>| {
				A::synthesise_expression(rhs, TypeId::ANY_TYPE, env, data)
			}),
			checking_data,
		)),
		Logical::NullCoalescing => {
			let is_lhs_null = evaluate_equality_inequality_operation(
				lhs.0,
				&EqualityAndInequality::StrictEqual,
				TypeId::NULL_TYPE,
				&mut checking_data.types,
				checking_data.options.strict_casts,
			)?;
			Ok(environment.new_conditional_context(
				(is_lhs_null, lhs.1),
				|env: &mut Environment, data: &mut CheckingData<T, A>| {
					A::synthesise_expression(rhs, TypeId::ANY_TYPE, env, data)
				},
				Some(|_env: &mut Environment, _data: &mut CheckingData<T, A>| lhs.0),
				checking_data,
			))
		}
	}
}

/// `typeof` done elsewhere
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
) -> Result<TypeId, ()> {
	if operand == TypeId::ERROR_TYPE {
		return Ok(operand);
	}

	match operator {
		PureUnary::LogicalNot => {
			if let Decidable::Known(value) = is_type_truthy_falsy(operand, types) {
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
				let value = cast_as_number(cst, strict_casts)?;
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
	let are_equal = if lhs == rhs {
		true
	} else {
		let lhs = types.get_type_by_id(lhs);
		let rhs = types.get_type_by_id(rhs);
		if let (Type::Constant(cst1), Type::Constant(cst2)) = (lhs, rhs) {
			cst1 == cst2
		} else if let (Type::Object(..) | Type::SpecialObject(SpecialObjects::Function(..)), _)
		| (_, Type::Object(..) | Type::SpecialObject(SpecialObjects::Function(..))) = (lhs, rhs)
		{
			// Same objects and functions always have same type id. Poly case doesn't occur here
			false
		}
		// Temp fix for closures
		else if let (
			Type::Constructor(crate::types::Constructor::StructureGenerics(StructureGenerics {
				on: on_lhs,
				..
			})),
			Type::Constructor(crate::types::Constructor::StructureGenerics(StructureGenerics {
				on: on_rhs,
				..
			})),
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
