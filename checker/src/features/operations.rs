//! Contains logic for mathematical, bitwise and logical operators

use derive_enum_from_into::EnumFrom;
use source_map::{Span, SpanWithSource};

use crate::{
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	features::conditional::new_conditional_context,
	types::{
		cast_as_number, cast_as_string, helpers::simple_subtype, intrinsics, is_type_truthy_falsy,
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
				environment,
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
			let result = evaluate_equality_inequality_operation(
				lhs,
				&operator,
				rhs,
				environment,
				&mut checking_data.types,
				checking_data.options.strict_casts,
			);

			if let Ok((result, warning)) = result {
				if let EqualityAndInequalityResultKind::Disjoint = warning {
					let position = lhs_pos
						.without_source()
						.union(rhs_pos.without_source())
						.with_source(environment.get_source());

					checking_data.diagnostics_container.add_warning(
						crate::TypeCheckWarning::DisjointEquality {
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
				}

				result
			} else {
				let position = lhs_pos
					.without_source()
					.union(rhs_pos.without_source())
					.with_source(environment.get_source());

				checking_data.diagnostics_container.add_error(
					crate::TypeCheckError::InvalidEqualityOperation {
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
}

/// TODO proper error type
pub fn evaluate_mathematical_operation(
	lhs: TypeId,
	operator: MathematicalAndBitwise,
	rhs: TypeId,
	info: &impl crate::context::InformationChain,
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

	if lhs == TypeId::ERROR_TYPE || rhs == TypeId::ERROR_TYPE {
		return Ok(TypeId::ERROR_TYPE);
	}

	let is_dependent =
		types.get_type_by_id(lhs).is_dependent() || types.get_type_by_id(rhs).is_dependent();

	if is_dependent {
		let can_be_string = if let MathematicalAndBitwise::Add = operator {
			let left_is_string = simple_subtype(lhs, TypeId::STRING_TYPE, info, types);
			let right_is_string = simple_subtype(lhs, TypeId::STRING_TYPE, info, types);
			let left_is_string_or_number =
				left_is_string || simple_subtype(lhs, TypeId::NUMBER_TYPE, info, types);
			let right_is_string_or_number =
				right_is_string || simple_subtype(rhs, TypeId::NUMBER_TYPE, info, types);
			if !left_is_string_or_number || !right_is_string_or_number {
				return Err(());
			}
			left_is_string || right_is_string
		} else {
			let left_is_number = simple_subtype(lhs, TypeId::NUMBER_TYPE, info, types);
			if !left_is_number || !simple_subtype(rhs, TypeId::NUMBER_TYPE, info, types) {
				return Err(());
			}
			false
		};

		// :)
		if let (MathematicalAndBitwise::Exponent, TypeId::ONE, true) =
			(operator, rhs, intrinsics::is_not_not_a_number(lhs, types))
		{
			return Ok(lhs);
		} else if let (MathematicalAndBitwise::Add, TypeId::ZERO)
		| (MathematicalAndBitwise::Multiply, TypeId::ONE) = (operator, rhs)
		{
			return Ok(lhs);
		} else if let (MathematicalAndBitwise::Add, TypeId::ZERO)
		| (MathematicalAndBitwise::Multiply, TypeId::ONE) = (operator, lhs)
		{
			return Ok(rhs);
		}

		let result = if can_be_string {
			TypeId::STRING_TYPE
		} else if let (
			MathematicalAndBitwise::Add | MathematicalAndBitwise::Multiply,
			Some(lhs_range),
			Some(rhs_range),
		) = (operator, intrinsics::get_range(lhs, types), intrinsics::get_range(rhs, types))
		{
			match operator {
				MathematicalAndBitwise::Add => {
					intrinsics::range_to_type(lhs_range.space_addition(rhs_range), types)
				}
				MathematicalAndBitwise::Multiply => {
					intrinsics::range_to_type(lhs_range.space_multiplication(rhs_range), types)
				}
				_ => unreachable!(),
			}
		} else {
			TypeId::NUMBER_TYPE
		};

		let constructor = crate::types::Constructor::BinaryOperator { lhs, operator, rhs, result };
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
	types: &mut TypeStore,
	strict_casts: bool,
) -> Result<(TypeId, EqualityAndInequalityResultKind), ()> {
	// `NaN == t` is always true
	if lhs == TypeId::NAN || rhs == TypeId::NAN {
		return Ok((TypeId::FALSE, EqualityAndInequalityResultKind::Constant));
	}

	match operator {
		EqualityAndInequality::StrictEqual => {
			// crate::utilities::notify!("{:?} === {:?}", lhs, rhs);

			let left_dependent = types.get_type_by_id(lhs).is_dependent();
			let is_dependent = left_dependent || types.get_type_by_id(rhs).is_dependent();

			if is_dependent {
				if lhs == rhs
					&& intrinsics::is_not_not_a_number(lhs, types)
					&& intrinsics::is_not_not_a_number(rhs, types)
				{
					// I think this is okay
					return Ok((TypeId::TRUE, EqualityAndInequalityResultKind::Constant));
				}

				// Checks lhs and rhs type to see if they overlap
				if crate::types::disjoint::types_are_disjoint(
					lhs,
					rhs,
					&mut Vec::new(),
					info,
					types,
				) {
					return Ok((TypeId::FALSE, EqualityAndInequalityResultKind::Disjoint));
				}

				// Sort if `*constant* == ...`. Ideally want constant type on the RHS
				let (lhs, rhs) = if left_dependent { (lhs, rhs) } else { (rhs, rhs) };
				let constructor = crate::types::Constructor::CanonicalRelationOperator {
					lhs,
					operator: CanonicalEqualityAndInequality::StrictEqual,
					rhs,
				};

				Ok((
					types.register_type(crate::Type::Constructor(constructor)),
					EqualityAndInequalityResultKind::Condition,
				))
			} else {
				match attempt_constant_equality(lhs, rhs, types) {
					Ok(ty) => Ok((
						if ty { TypeId::TRUE } else { TypeId::FALSE },
						EqualityAndInequalityResultKind::Constant,
					)),
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

			let is_dependent = types.get_type_by_id(lhs).is_dependent()
				|| types.get_type_by_id(rhs).is_dependent();

			if is_dependent {
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

				{
					// let lhs = get_constraint(lhs, types).unwrap_or(lhs);
					// let rhs = get_constraint(rhs, types).unwrap_or(rhs);

					if !simple_subtype(lhs, TypeId::NUMBER_TYPE, info, types)
						|| !simple_subtype(rhs, TypeId::NUMBER_TYPE, info, types)
					{
						return Err(());
					}

					// Tidies some things for counting loop iterations

					// Checking disjoint-ness for inequalities (TODO under option) via distribution
					if let (Some(lhs_range), Some(rhs_range)) =
						(intrinsics::get_range(lhs, types), intrinsics::get_range(rhs, types))
					{
						if lhs_range.below(rhs_range) {
							return Ok((TypeId::TRUE, EqualityAndInequalityResultKind::Constant));
						}
						if lhs_range.above(rhs_range) {
							return Ok((TypeId::FALSE, EqualityAndInequalityResultKind::Disjoint));
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
	types: &mut TypeStore,
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
			Some(|env: &mut Environment, checking_data: &mut CheckingData<T, A>| {
				if let Some(constraint) = crate::types::get_constraint(lhs.0, &checking_data.types)
				{
					let mut result = Vec::new();
					super::narrowing::build_union_from_filter(
						constraint,
						super::narrowing::FASLY,
						&mut result,
						env,
						&checking_data.types,
					);
					let narrowed_to = checking_data.types.new_or_type_from_iterator(result);
					checking_data.types.register_type(Type::Narrowed { from: lhs.0, narrowed_to })
				} else {
					lhs.0
				}
			}),
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
			let is_lhs_null_or_undefined =
				is_null_or_undefined(lhs.0, environment, &mut checking_data.types);
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
	types: &mut TypeStore,
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
					UnaryOperation::BitwiseNot => evaluate_mathematical_operation(
						TypeId::MAX_U32,
						MathematicalAndBitwise::BitwiseXOr,
						operand,
						info,
						types,
						strict_casts,
					),
					UnaryOperation::Negation => evaluate_mathematical_operation(
						TypeId::ZERO,
						MathematicalAndBitwise::Subtract,
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

/// Returns whether lhs and rhs are always equal or never equal. TODO more
///
/// TODO return decidable.
fn attempt_constant_equality(lhs: TypeId, rhs: TypeId, types: &mut TypeStore) -> Result<bool, ()> {
	if lhs == rhs {
		Ok(true)
	} else if matches!(lhs, TypeId::NULL_TYPE | TypeId::UNDEFINED_TYPE)
		|| matches!(rhs, TypeId::NULL_TYPE | TypeId::UNDEFINED_TYPE)
	{
		// If above `==`` failed => false (as always have same `TypeId`)
		Ok(false)
	} else {
		let lhs = types.get_type_by_id(lhs);
		let rhs = types.get_type_by_id(rhs);
		if let (Type::Constant(cst1), Type::Constant(cst2)) = (lhs, rhs) {
			Ok(cst1 == cst2)
		} else if let (Type::Object(..) | Type::SpecialObject(SpecialObject::Function(..)), _)
		| (_, Type::Object(..) | Type::SpecialObject(SpecialObject::Function(..))) = (lhs, rhs)
		{
			// Same objects and functions always have same type id. Poly case doesn't occur here
			Ok(false)
		}
		// Temp fix for closures
		else if let (
			Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on: on_lhs, .. }),
			Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on: on_rhs, .. }),
		) = (lhs, rhs)
		{
			// TODO does this work?
			attempt_constant_equality(*on_lhs, *on_rhs, types)
		} else {
			crate::utilities::notify!("{:?} === {:?} is apparently false", lhs, rhs);
			Err(())
		}
	}
}
