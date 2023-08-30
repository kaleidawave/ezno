use crate::diagnostics::InvalidMathematicalAndBitwiseOperation;

pub enum BinaryOperatorError {
	InvalidMathematicalOperation(InvalidMathematicalAndBitwiseOperation),
	NotDefined(&'static str),
}

// / TODO pass position and settings
// / TODO Settings needs to be clauses on the branch
// pub fn evaluate_binary_operator(
// 	operator: CanonicalBinaryOperator,
// 	lhs: TypeId,
// 	rhs: TypeId,
// 	environment: &mut Environment,
// 	strict_casts: bool,
// 	types: &mut TypeStore,
// ) -> Result<TypeId, BinaryOperatorError> {
// 	unimplemented!()
// let is_dependent =
// 	types.get_type_by_id(lhs).is_dependent() || types.get_type_by_id(rhs).is_dependent();

// if is_dependent {
// 	// TODO restriction
// 	return Ok(types.register_type(crate::Type::Constructor(
// 		super::Constructor::BinaryOperator { lhs, operator, rhs },
// 	)));
// }

// // TODO automate with a macro
// let function = match operator {
// 	CanonicalBinaryOperator::Add => operators.add.as_ref(),
// 	CanonicalBinaryOperator::Multiply => operators.mul.as_ref(),
// 	CanonicalBinaryOperator::Modulo => None,
// 	CanonicalBinaryOperator::Exponent => None,
// 	CanonicalBinaryOperator::BitwiseOperators(_) => None,
// 	CanonicalBinaryOperator::RelationOperator(relation) => match relation {
// 		RelationOperator::Equal => operators.equal.as_ref(),
// 		RelationOperator::GreaterThan => None,
// 	},
// 	CanonicalBinaryOperator::Subtract => operators.sub.as_ref(),
// 	CanonicalBinaryOperator::Divide => None,
// 	CanonicalBinaryOperator::LogicalOperator(_) => None,
// 	CanonicalBinaryOperator::InstanceOf => None,
// };

// let function = if let Some(function) = function {
// 	// TODO function calling issue
// 	function.clone()
// } else {
// 	let op = match operator {
// 		CanonicalBinaryOperator::Add => "Add",
// 		CanonicalBinaryOperator::Multiply => "Multiply",
// 		CanonicalBinaryOperator::Modulo => "Modulo",
// 		CanonicalBinaryOperator::Exponent => "Exponent",
// 		CanonicalBinaryOperator::BitwiseOperators(_) => "BitwiseOperators",
// 		CanonicalBinaryOperator::RelationOperator(_) => "RelationOperator",
// 		CanonicalBinaryOperator::Subtract => "Subtract",
// 		CanonicalBinaryOperator::Divide => "Divide",
// 		CanonicalBinaryOperator::LogicalOperator(_) => "LogicalOperator",
// 		CanonicalBinaryOperator::InstanceOf => "InstanceOf",
// 	};
// 	return Err(BinaryOperatorError::NotDefined(op));
// };

// let arguments = &[
// 	SynthesizedArgument::NonSpread { ty: lhs, position: Span::NULL_SPAN },
// 	SynthesizedArgument::NonSpread { ty: rhs, position: Span::NULL_SPAN },
// ];
// let result = function.call(
// 	crate::events::CalledWithNew::None,
// 	None,
// 	None,
// 	&None,
// 	arguments,
// 	Span::NULL_SPAN,
// 	types,
// 	environment,
// );

// result
// 	.map(|op| op.returned_type)
// 	.map_err(|errors| {
// 		use crate::events::FunctionCallingError;
// 		match errors.into_iter().next().unwrap() {
// 			FunctionCallingError::InvalidArgumentType {
// 				parameter_type,
// 				argument_type,
// 				argument_position,
// 				parameter_position,
// 				restriction,
// 			} => {
// 				crate::utils::notify!("{} {}", parameter_type, argument_type);
// 				InvalidMathematicalOperation {
// 					lhs: TypeStringRepresentation::from_type_id(
// 						lhs,
// 						&environment.into_general_context(),
// 						&types,
// 						false,
// 					),
// 					rhs: TypeStringRepresentation::from_type_id(
// 						rhs,
// 						&environment.into_general_context(),
// 						&types,
// 						false,
// 					),
// 					operator,
// 					position: Span::NULL_SPAN, // lhs.1.union(&rhs.1),
// 				}
// 			}
// 			FunctionCallingError::MissingArgument { .. } => {
// 				unreachable!("binary operator should accept two operands")
// 			}
// 			FunctionCallingError::ExcessArguments { .. } => {
// 				unreachable!("binary operator should have two operands")
// 			}
// 			FunctionCallingError::NotCallable { .. } => {
// 				unreachable!("operator should be callable")
// 			}
// 			FunctionCallingError::ReferenceRestrictionDoesNotMatch { .. } => {
// 				unreachable!("...")
// 			}
// 		}
// 	})
// 	.map_err(BinaryOperatorError::InvalidMathematicalOperation)
// }
