//! Synthesis is the aim of giving types to AST structures
//! **Synthesis and checking are split between two passes**
//! The aim is a AST cannot known to be valid until it everything has been attempted to be given a type
//! Unknown types are [types::Meta]

mod assignments;
pub mod block;
pub mod classes;
pub mod declarations;
pub mod definitions;
pub mod expressions;
mod extensions;
pub mod functions;
pub mod hoisting;
pub mod interfaces;
pub mod module;
pub mod object_literal;
pub mod statements;
pub mod type_references;
pub mod variables;

pub(crate) use expressions::{synthesize_expression, synthesize_multiple_expression};
pub(crate) use functions::synthesize_function;
pub(crate) use statements::synthesize_statement;

use block::synthesize_block;
use parser::{
	operators::{BinaryOperator as ParserBinaryOperator, UnaryOperator as ParserUnaryOperator},
	PropertyKey,
};
pub use std::convert::TryFrom;

use crate::{
	context::{Context, ContextType},
	structures::operators::{BinaryOperator, UnaryOperator},
	types::TypeStore,
	Constant, TypeId,
};

pub(crate) fn property_key_as_type<S: ContextType>(
	property_key: &PropertyKey,
	environment: &mut Context<S>,
	types: &mut TypeStore,
) -> TypeId {
	match property_key {
		PropertyKey::StringLiteral(value, _, _) | PropertyKey::Ident(value, _, _) => {
			types.new_constant_type(Constant::String(value.clone()))
		}
		PropertyKey::NumberLiteral(number, _, _) => {
			types.new_constant_type(Constant::Number(f64::from(*number).try_into().unwrap()))
		}
		PropertyKey::Computed(_, _, _) => todo!(),
	}
}

impl From<parser::ParseError> for Diagnostic {
	fn from(parse_error: parser::ParseError) -> Self {
		Diagnostic::Position { reason: parse_error.reason, pos: parse_error.position }
	}
}

/// TODO not all of these fit so design WIP
pub fn parser_binary_operator_to_others(operator: ParserBinaryOperator) -> BinaryOperator {
	match operator {
		ParserBinaryOperator::StrictEqual => todo!(),
		ParserBinaryOperator::StrictNotEqual => todo!(),
		ParserBinaryOperator::Equal => todo!(),
		ParserBinaryOperator::NotEqual => todo!(),
		ParserBinaryOperator::Add => BinaryOperator::Add,
		ParserBinaryOperator::Subtract => todo!(),
		ParserBinaryOperator::Multiply => todo!(),
		ParserBinaryOperator::Divide => todo!(),
		ParserBinaryOperator::Modulo => todo!(),
		ParserBinaryOperator::Exponent => todo!(),
		ParserBinaryOperator::GreaterThan => todo!(),
		ParserBinaryOperator::LessThan => todo!(),
		ParserBinaryOperator::LessThanEqual => todo!(),
		ParserBinaryOperator::GreaterThanEqual => todo!(),
		ParserBinaryOperator::InstanceOf => todo!(),
		ParserBinaryOperator::In => todo!(),
		ParserBinaryOperator::BitwiseShiftLeft => todo!(),
		ParserBinaryOperator::BitwiseShiftRight => todo!(),
		ParserBinaryOperator::BitwiseShiftRightUnsigned => todo!(),
		ParserBinaryOperator::BitwiseAnd => todo!(),
		ParserBinaryOperator::BitwiseXOr => todo!(),
		ParserBinaryOperator::BitwiseOr => todo!(),
		ParserBinaryOperator::LogicalAnd => todo!(),
		ParserBinaryOperator::LogicalOr => todo!(),
		ParserBinaryOperator::NullCoalescing => todo!(),
		ParserBinaryOperator::Divides => todo!(),
		ParserBinaryOperator::Pipe => todo!(),
		ParserBinaryOperator::Compose => todo!(),
	}
}

/// TODO not all of these fit so design WIP
pub fn parser_unary_operator_to_others(operator: ParserUnaryOperator) -> UnaryOperator {
	todo!()
}
