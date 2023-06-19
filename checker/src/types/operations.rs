use source_map::Span;

use crate::{
	context::Environment,
	errors::{InvalidMathematicalOperation, TypeCheckError, TypeStringRepresentation},
	structures::{functions::SynthesizedArgument, operators::*},
	types::TypeStore,
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
			checking_data
				.diagnostics_container
				.add_error(TypeCheckError::InvalidMathematicalOperation(error));
			TypeId::ERROR_TYPE
		}
	}
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
) -> Result<TypeId, InvalidMathematicalOperation> {
	// TODO the function should be stored on Root environment on registration rather
	// than looking up a function using a key here
	let op_type = match operator {
		BinaryOperator::Add => {
			let key = types.new_constant_type(crate::Constant::String("Add".to_owned()));
			environment.get_property_unbound(TypeId::OPERATORS_SPECIAL, key, types)
		}
		BinaryOperator::Multiply => {
			let key = types.new_constant_type(crate::Constant::String("Multiply".to_owned()));
			environment.get_property_unbound(TypeId::OPERATORS_SPECIAL, key, types)
		}
		BinaryOperator::Modulo => todo!(),
		BinaryOperator::Exponent => todo!(),
		BinaryOperator::BitwiseOperators(_) => todo!(),
		BinaryOperator::RelationOperator(operator) => match operator {
			RelationOperator::Equal => {
				let key =
					types.new_constant_type(crate::Constant::String("StrictEqual".to_owned()));
				environment.get_property_unbound(TypeId::OPERATORS_SPECIAL, key, types)
			}
			RelationOperator::GreaterThan => todo!(),
		},
		BinaryOperator::Subtract => todo!(),
		BinaryOperator::Divide => todo!(),
		BinaryOperator::LogicalOperator(_) => todo!(),
	};
	// TODO handle things and convert to bin exprs
	super::calling::call_type(
		op_type.unwrap().prop_to_type(),
		// TODO faster!
		vec![
			SynthesizedArgument::NonSpread { ty: lhs, position: Span::NULL_SPAN },
			SynthesizedArgument::NonSpread { ty: rhs, position: Span::NULL_SPAN },
		],
		None,
		None,
		environment,
		types,
		crate::events::CalledWithNew::None,
	)
	.map(|op| op.returned_type)
	.map_err(|errors| {
		// TODO temp
		match errors.into_iter().next().unwrap() {
			crate::structures::functions::FunctionCallingError::InvalidArgumentType { parameter_type, argument_type, argument_position, parameter_position, restriction } => {
				crate::utils::notify!("{} {}", parameter_type, argument_type);
				return InvalidMathematicalOperation {
					lhs: TypeStringRepresentation::from_type_id(
						lhs,
						&environment.into_general_environment(),
						&types,
						false,
					),
					rhs: TypeStringRepresentation::from_type_id(
						rhs,
						&environment.into_general_environment(),
						&types,
						false,
					),
					operator,
					position: Span::NULL_SPAN, // lhs.1.union(&rhs.1),
				};
			},
			crate::structures::functions::FunctionCallingError::MissingArgument { .. } => unreachable!("binary operator should accept two operands"),
			crate::structures::functions::FunctionCallingError::ExtraArguments { .. } => unreachable!("binary operator should have two operands"),
			crate::structures::functions::FunctionCallingError::NotCallable { .. } => unreachable!("operator should be callable"),
			crate::structures::functions::FunctionCallingError::ReferenceRestrictionDoesNotMatch { .. } => unreachable!("..."),
		}
	})
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
