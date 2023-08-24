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
pub mod statements;
pub mod type_annotations;
pub mod variables;

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
	Constant, Diagnostic, TypeId,
};

pub(super) fn property_key_as_type<S: ContextType, P: parser::property_key::PropertyKeyKind>(
	property_key: &PropertyKey<P>,
	environment: &mut Context<S>,
	types: &mut TypeStore,
) -> TypeId {
	match property_key {
		PropertyKey::StringLiteral(value, _) | PropertyKey::Ident(value, _, _) => {
			types.new_constant_type(Constant::String(value.clone()))
		}
		PropertyKey::NumberLiteral(number, _) => {
			types.new_constant_type(Constant::Number(f64::from(*number).try_into().unwrap()))
		}
		PropertyKey::Computed(_, _) => todo!(),
	}
}

impl From<parser::ParseError> for Diagnostic {
	fn from(parse_error: parser::ParseError) -> Self {
		Diagnostic::Position {
			reason: parse_error.reason,
			position: parse_error.position,
			kind: crate::diagnostics::DiagnosticKind::Error,
		}
	}
}

/// TODO not all of these fit so design WIP
pub fn parser_binary_operator_to_others(operator: ParserBinaryOperator) -> BinaryOperator {
	match operator {
		ParserBinaryOperator::StrictEqual => {
			BinaryOperator::RelationOperator(crate::structures::operators::RelationOperator::Equal)
		}
		ParserBinaryOperator::StrictNotEqual => todo!(),
		ParserBinaryOperator::Equal => todo!(),
		ParserBinaryOperator::NotEqual => todo!(),
		ParserBinaryOperator::Add => BinaryOperator::Add,
		ParserBinaryOperator::Subtract => BinaryOperator::Subtract,
		ParserBinaryOperator::Multiply => BinaryOperator::Multiply,
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
	match operator {
		ParserUnaryOperator::Plus => todo!(),
		ParserUnaryOperator::Negation => UnaryOperator::Negation,
		ParserUnaryOperator::BitwiseNot => todo!(),
		ParserUnaryOperator::LogicalNot => todo!(),
		ParserUnaryOperator::Await => todo!(),
		ParserUnaryOperator::TypeOf => todo!(),
		ParserUnaryOperator::Void => todo!(),
		ParserUnaryOperator::Delete => todo!(),
		ParserUnaryOperator::Yield => todo!(),
		ParserUnaryOperator::DelegatedYield => todo!(),
	}
}

pub enum Performs<'a> {
	Block(&'a parser::Block),
	Const(String),
	None,
}

impl<'a> From<Option<&'a parser::types::AnnotationPerforms>> for Performs<'a> {
	fn from(value: Option<&'a parser::types::AnnotationPerforms>) -> Self {
		match value {
			Some(parser::types::AnnotationPerforms::PerformsConst {
				performs_keyword,
				identifier,
			}) => Performs::Const(identifier.clone()),
			Some(parser::types::AnnotationPerforms::PerformsStatements {
				performs_keyword,
				statements,
			}) => Performs::Block(statements),
			None => Performs::None,
		}
	}
}
