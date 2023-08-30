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
use parser::PropertyKey;

use crate::{
	context::{Context, ContextType},
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
