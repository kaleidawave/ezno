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
pub mod statements;
pub mod type_annotations;
pub mod variables;

use block::synthesise_block;
use parser::PropertyKey;
use source_map::SourceId;

use crate::{
	behavior::modules::Exported,
	context::{environment, Context, ContextType, Names},
	types::TypeStore,
	Constant, Diagnostic, Environment, Facts, RootContext, TypeId,
};

pub(super) fn property_key_as_type<S: ContextType, P: parser::property_key::PropertyKeyKind>(
	property_key: &PropertyKey<P>,
	environment: &mut Context<S>,
	types: &mut TypeStore,
) -> TypeId {
	match property_key {
		PropertyKey::StringLiteral(value, ..) | PropertyKey::Ident(value, _, _) => {
			types.new_constant_type(Constant::String(value.clone()))
		}
		PropertyKey::NumberLiteral(number, _) => {
			types.new_constant_type(Constant::Number(f64::from(number.clone()).try_into().unwrap()))
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

impl crate::GenericTypeParameter for parser::GenericTypeConstraint {
	fn get_name(&self) -> &str {
		self.name()
	}
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

pub struct EznoParser;

impl crate::ASTImplementation for EznoParser {
	type ParseOptions = parser::ParseOptions;
	type ParseError = (parser::ParseError, SourceId);
	type Module = parser::Module;
	type DefinitionFile = parser::TypeDefinitionModule;
	type TypeAnnotation = parser::TypeAnnotation;
	type TypeParameter = parser::GenericTypeConstraint;

	fn module_from_string(
		source_id: SourceId,
		string: String,
		options: &Self::ParseOptions,
	) -> Result<Self::Module, Self::ParseError> {
		<parser::Module as parser::ASTNode>::from_string(string, options.clone(), source_id, None)
			.map_err(|err| (err, source_id))
	}

	fn definition_module_from_string(
		source_id: SourceId,
		string: String,
	) -> Result<Self::DefinitionFile, Self::ParseError> {
		let options = Default::default();
		parser::TypeDefinitionModule::from_string(string, options, source_id)
			.map_err(|err| (err, source_id))
	}

	fn synthesize_module<T: crate::ReadFromFS>(
		module: &Self::Module,
		source_id: SourceId,
		module_environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) {
		synthesise_block(&module.items, module_environment, checking_data)
	}

	fn type_definition_file<T: crate::ReadFromFS>(
		tdm: parser::TypeDefinitionModule,
		root: &crate::RootContext,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> (Names, Facts) {
		definitions::type_definition_file(tdm, checking_data, root)
	}

	fn type_parameter_name(parameter: &Self::TypeParameter) -> &str {
		parameter.name()
	}
}

pub mod interactive {
	use std::{collections::HashSet, mem, path::PathBuf};

	use source_map::{MapFileStore, SourceId, WithPathMap};

	use crate::{
		add_definition_files_to_root, types::printing::print_type, CheckingData,
		DiagnosticsContainer, RootContext,
	};

	use super::{
		block::{synthesise_block, synthesize_declaration},
		expressions::{synthesise_expression, synthesise_multiple_expression},
		statements::synthesise_statement,
	};

	pub struct State<'a, T: crate::ReadFromFS> {
		checking_data: CheckingData<'a, T, super::EznoParser>,
		root: RootContext,
	}

	impl<'a, T: crate::ReadFromFS> State<'a, T> {
		pub fn new(
			resolver: &'a T,
			type_definition_files: HashSet<PathBuf>,
		) -> Result<Self, (DiagnosticsContainer, MapFileStore<WithPathMap>)> {
			let mut root = RootContext::new_with_primitive_references();
			let entry_point = PathBuf::from("CLI");
			let mut checking_data = CheckingData::new(
				Default::default(),
				resolver,
				entry_point,
				Default::default(),
				None,
			);

			add_definition_files_to_root(type_definition_files, &mut root, &mut checking_data);

			if checking_data.diagnostics_container.has_error() {
				Err((checking_data.diagnostics_container, checking_data.modules.files))
			} else {
				Ok(Self { checking_data, root })
			}
		}

		pub fn check_item(
			&mut self,
			item: &parser::Module,
		) -> Result<(Option<String>, DiagnosticsContainer), DiagnosticsContainer> {
			let source = self.checking_data.modules.entry_point;
			let (ty, ..) = self.root.new_lexical_environment_fold_into_parent(
				crate::Scope::PassThrough { source },
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

		pub fn get_fs_ref(&self) -> &MapFileStore<WithPathMap> {
			&self.checking_data.modules.files
		}

		pub fn get_fs_mut(&mut self) -> &mut MapFileStore<WithPathMap> {
			&mut self.checking_data.modules.files
		}

		pub fn get_source_id(&self) -> SourceId {
			self.checking_data.modules.entry_point
		}
	}
}
