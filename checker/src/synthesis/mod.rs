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

use block::synthesise_block;
use parser::PropertyKey;
use source_map::SourceId;

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

impl From<(parser::ParseError, SourceId)> for Diagnostic {
	fn from(parse_error: (parser::ParseError, SourceId)) -> Self {
		Diagnostic::Position {
			reason: parse_error.0.reason,
			position: parse_error.0.position.with_source(parse_error.1),
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

pub mod interactive {
	use std::mem;

	use crate::{types::printing::print_type, CheckingData, DiagnosticsContainer, RootContext};

	use super::{
		block::{synthesise_block, synthesize_declaration},
		expressions::{synthesise_expression, synthesise_multiple_expression},
		statements::synthesise_statement,
	};

	pub struct State<'a, T: crate::FSResolver> {
		checking_data: CheckingData<'a, T>,
		root: RootContext,
		source_id: source_map::SourceId,
	}

	impl<'a, T: crate::FSResolver> State<'a, T> {
		pub fn new(resolver: &'a T, source_id: source_map::SourceId) -> Self {
			Self {
				checking_data: CheckingData::new(Default::default(), &resolver),
				root: RootContext::new_with_primitive_references(),
				source_id,
			}
		}

		pub fn check_item(
			&mut self,
			item: &parser::Module,
		) -> Result<(Option<String>, DiagnosticsContainer), DiagnosticsContainer> {
			let (ty, ..) = self.root.new_lexical_environment_fold_into_parent(
				crate::Scope::PassThrough { source: self.source_id },
				&mut self.checking_data,
				|environment, checking_data| {
					if let Some(parser::StatementOrDeclaration::Statement(
						parser::Statement::Expression(expression),
					)) = item.items.last()
					{
						synthesise_block(
							&item.items[..(item.items.len() - 1)],
							environment,
							checking_data,
						);
						let result =
							synthesise_multiple_expression(expression, environment, checking_data);
						Some(print_type(
							result,
							&checking_data.types,
							&environment.as_general_context(),
							false,
						))
					} else {
						synthesise_block(&item.items, environment, checking_data);

						None
					}
				},
			);
			let dc = mem::take(&mut self.checking_data.diagnostics_container);
			if dc.has_error() {
				Err(dc)
			} else {
				Ok((ty, dc))
			}
		}
	}
}
