use source_map::Span;

use crate::{
	context::Environment,
	diagnostics::{InvalidMathematicalOperation, TypeCheckError, TypeStringRepresentation},
	structures::operators::*,
	types::{functions::SynthesizedArgument, TypeStore},
	CheckingData, TypeId,
};

pub fn evaluate_binary_operator_handle_errors<T: crate::FSResolver>(
	operator: BinaryOperator,
	lhs: (TypeId, Span),
	rhs: (TypeId, Span),
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) -> TypeId {
	let result = evaluate_binary_operator(
		operator,
		lhs.0,
		rhs.0,
		environment,
		checking_data.settings.strict_casts,
		&mut checking_data.types,
	);
	match result {
		Ok(ok) => ok,
		Err(error) => {
			let error = match error {
				BinaryOperatorError::InvalidMathematicalOperation(error) => {
					TypeCheckError::InvalidMathematicalOperation(error)
				}
				BinaryOperatorError::NotDefined(op) => {
					TypeCheckError::NotDefinedOperator(op, lhs.1.union(&rhs.1))
				}
			};
			checking_data.diagnostics_container.add_error(error);
			TypeId::ERROR_TYPE
		}
	}
}

pub enum BinaryOperatorError {
	InvalidMathematicalOperation(InvalidMathematicalOperation),
	NotDefined(&'static str),
}

/// TODO pass position and settings
/// TODO Settings needs to be clauses on the branch
pub fn evaluate_binary_operator(
	operator: BinaryOperator,
	lhs: TypeId,
	rhs: TypeId,
	environment: &mut Environment,
	strict_casts: bool,
	types: &mut TypeStore,
) -> Result<TypeId, BinaryOperatorError> {
	let is_dependent =
		types.get_type_by_id(lhs).is_dependent() || types.get_type_by_id(rhs).is_dependent();

	if is_dependent {
		// TODO restriction
		return Ok(types.register_type(crate::Type::Constructor(
			super::Constructor::BinaryOperator { lhs, operator, rhs },
		)));
	}

	let operators = &environment.get_root().context_type.operators;
	// TODO automate with a macro
	let function = match operator {
		BinaryOperator::Add => operators.add.as_ref(),
		BinaryOperator::Multiply => operators.add.as_ref(),
		BinaryOperator::Modulo => None,
		BinaryOperator::Exponent => None,
		BinaryOperator::BitwiseOperators(_) => None,
		BinaryOperator::RelationOperator(relation) => match relation {
			RelationOperator::Equal => operators.equal.as_ref(),
			RelationOperator::GreaterThan => None,
		},
		BinaryOperator::Subtract => operators.mul.as_ref(),
		BinaryOperator::Divide => None,
		BinaryOperator::LogicalOperator(_) => None,
		BinaryOperator::InstanceOf => None,
	};

	let function = if let Some(function) = function {
		// TODO function calling issue
		function.clone()
	} else {
		let op = match operator {
			BinaryOperator::Add => "Add",
			BinaryOperator::Multiply => "Multiply",
			BinaryOperator::Modulo => "Modulo",
			BinaryOperator::Exponent => "Exponent",
			BinaryOperator::BitwiseOperators(_) => "BitwiseOperators",
			BinaryOperator::RelationOperator(_) => "RelationOperator",
			BinaryOperator::Subtract => "Subtract",
			BinaryOperator::Divide => "Divide",
			BinaryOperator::LogicalOperator(_) => "LogicalOperator",
			BinaryOperator::InstanceOf => "InstanceOf",
		};
		return Err(BinaryOperatorError::NotDefined(op));
	};

	let arguments = &[
		SynthesizedArgument::NonSpread { ty: lhs, position: Span::NULL_SPAN },
		SynthesizedArgument::NonSpread { ty: rhs, position: Span::NULL_SPAN },
	];
	let result = function.call(
		crate::events::CalledWithNew::None,
		None,
		None,
		&None,
		arguments,
		Span::NULL_SPAN,
		types,
		environment,
	);

	result
		.map(|op| op.returned_type)
		.map_err(|errors| {
			use crate::events::FunctionCallingError;
			match errors.into_iter().next().unwrap() {
				FunctionCallingError::InvalidArgumentType {
					parameter_type,
					argument_type,
					argument_position,
					parameter_position,
					restriction,
				} => {
					crate::utils::notify!("{} {}", parameter_type, argument_type);
					InvalidMathematicalOperation {
						lhs: TypeStringRepresentation::from_type_id(
							lhs,
							&environment.into_general_context(),
							&types,
							false,
						),
						rhs: TypeStringRepresentation::from_type_id(
							rhs,
							&environment.into_general_context(),
							&types,
							false,
						),
						operator,
						position: Span::NULL_SPAN, // lhs.1.union(&rhs.1),
					}
				}
				FunctionCallingError::MissingArgument { .. } => {
					unreachable!("binary operator should accept two operands")
				}
				FunctionCallingError::ExcessArguments { .. } => {
					unreachable!("binary operator should have two operands")
				}
				FunctionCallingError::NotCallable { .. } => {
					unreachable!("operator should be callable")
				}
				FunctionCallingError::ReferenceRestrictionDoesNotMatch { .. } => {
					unreachable!("...")
				}
			}
		})
		.map_err(BinaryOperatorError::InvalidMathematicalOperation)
}

pub fn evaluate_unary_operator(
	operator: UnaryOperator,
	operand: TypeId,
	environment: &mut Environment,
	strict_casts: bool,
	types: &mut TypeStore,
) -> Result<TypeId, ()> {
	todo!()
}
