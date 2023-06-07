use source_map::Span;

use crate::{
	context::Environment,
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
		Err(errors) => {
			// TODO
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
) -> Result<TypeId, ()> {
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
	};
	// TODO handle things and convert to bin exprs
	super::calling::call_type(
		op_type.unwrap().to_type(),
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
	.map(|x| x.returned_type)
	.map_err(|err| {
		for error in err {
			match error {
				crate::structures::functions::FunctionCallingError::InvalidArgumentType { parameter_type, argument_type, argument_position, parameter_position, restriction } => {
					crate::utils::notify!("{} {}", types.debug_type(parameter_type), types.debug_type(argument_type));
				},
				crate::structures::functions::FunctionCallingError::MissingArgument { parameter_pos } => todo!(),
				crate::structures::functions::FunctionCallingError::ExtraArgument { idx, position } => todo!(),
				crate::structures::functions::FunctionCallingError::NotCallable { calling } => todo!(),
				crate::structures::functions::FunctionCallingError::ReferenceRestrictionDoesNotMatch { reference, requirement, found } => todo!(),
			}
		}
		()
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
