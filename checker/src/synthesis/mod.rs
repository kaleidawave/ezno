//! Synthesis is the aim of giving types to AST structures
//! **Synthesis and checking are split between two passes**
//! The aim is a AST cannot known to be valid until it everything has been attempted to be given a type
//! Unknown types are [`types::Meta`]

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
use parser::{ASTNode, ParseOptions, PropertyKey as ParserPropertyKey};
use source_map::SourceId;

use crate::{
	context::{Names, VariableRegisterArguments},
	types::properties::PropertyKey,
	CheckingData, Diagnostic, Environment, LocalInformation, RootContext, TypeId,
};

use self::{
	declarations::synthesise_variable_declaration,
	expressions::{synthesise_expression, synthesise_multiple_expression},
	hoisting::hoist_variable_declaration,
	type_annotations::synthesise_type_annotation,
	variables::register_variable,
};

pub struct EznoParser;

impl crate::ASTImplementation for EznoParser {
	type ParseOptions = parser::ParseOptions;
	type ParseError = (parser::ParseError, SourceId);
	type ParserRequirements = ();

	type Module<'_a> = parser::Module;
	type OwnedModule = parser::Module;
	type DefinitionFile<'_a> = parser::Module;

	type TypeAnnotation<'_a> = parser::TypeAnnotation;
	type TypeParameter<'_a> = parser::TypeParameter;
	type Expression<'_a> = parser::Expression;
	type MultipleExpression<'_a> = parser::expressions::MultipleExpression;
	type ClassMethod<'_a> = parser::FunctionBase<parser::ast::ClassFunctionBase>;

	type VariableField<'_a> = parser::VariableField;

	type ForStatementInitiliser<'_a> = parser::statements::ForLoopStatementInitializer;

	fn module_from_string(
		// TODO remove
		source_id: SourceId,
		string: String,
		options: Self::ParseOptions,
		_parser_requirements: &mut Self::ParserRequirements,
	) -> Result<Self::Module<'static>, Self::ParseError> {
		<parser::Module as parser::ASTNode>::from_string(string, options)
			.map_err(|err| (err, source_id))
	}

	fn definition_module_from_string(
		// TODO remove
		source_id: SourceId,
		string: String,
		_parser_requirements: &mut Self::ParserRequirements,
	) -> Result<Self::DefinitionFile<'static>, Self::ParseError> {
		let options = ParseOptions { type_definition_module: true, ..Default::default() };

		<parser::Module as parser::ASTNode>::from_string(string, options)
			.map_err(|err| (err, source_id))
	}

	fn synthesise_module<T: crate::ReadFromFS>(
		module: &Self::Module<'_>,
		_source_id: SourceId,
		module_environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) {
		synthesise_block(&module.items, module_environment, checking_data);
	}

	fn synthesise_expression<U: crate::ReadFromFS>(
		expression: &Self::Expression<'_>,
		expecting: TypeId,
		environment: &mut Environment,
		checking_data: &mut CheckingData<U, Self>,
	) -> TypeId {
		synthesise_expression(expression, environment, checking_data, expecting)
	}

	fn expression_position<'_a>(expression: &'_a Self::Expression<'_a>) -> source_map::Span {
		*ASTNode::get_position(expression)
	}

	fn type_parameter_name<'_a>(parameter: &'_a Self::TypeParameter<'_a>) -> &'_a str {
		&parameter.name
	}

	fn synthesise_type_annotation<T: crate::ReadFromFS>(
		annotation: &Self::TypeAnnotation<'_>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId {
		synthesise_type_annotation(annotation, environment, checking_data)
	}

	type DefinitionFile<'a> = parser::TypeDefinitionModule;

	fn synthesise_definition_file<T: crate::ReadFromFS>(
		file: Self::DefinitionFile<'_>,
    source: SourceId,
		root: &RootContext,
		checking_data: &mut CheckingData<T, Self>,
	) -> (Names, LocalInformation) {
		definitions::type_definition_file(file, source, checking_data, root)
	}

	fn parse_options(is_js: bool, parse_comments: bool, lsp_mode: bool) -> Self::ParseOptions {
		parser::ParseOptions {
			comments: if parse_comments {
				parser::Comments::JustDocumentation
			} else {
				parser::Comments::None
			},
			type_annotations: !is_js,
			partial_syntax: lsp_mode,
			..Default::default()
		}
	}

	fn owned_module_from_module(m: Self::Module<'static>) -> Self::OwnedModule {
		m
	}

	fn synthesise_multiple_expression<'_a, T: crate::ReadFromFS>(
		expression: &'_a Self::MultipleExpression<'_a>,
		expected_type: TypeId,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId {
		synthesise_multiple_expression(expression, environment, checking_data, expected_type)
	}

	fn synthesise_for_loop_initialiser<'_a, T: crate::ReadFromFS>(
		for_loop_initialiser: &'_a Self::ForStatementInitiliser<'_a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) {
		match for_loop_initialiser {
			parser::statements::ForLoopStatementInitializer::VariableDeclaration(declaration) => {
				// TODO is this correct & the best
				hoist_variable_declaration(declaration, environment, checking_data);
				synthesise_variable_declaration(declaration, environment, checking_data, false);
			}
			parser::statements::ForLoopStatementInitializer::VarStatement(_) => todo!(),
			parser::statements::ForLoopStatementInitializer::Expression(_) => todo!(),
		}
	}

	fn declare_and_assign_to_fields<'a, T: crate::ReadFromFS>(
		field: &'a Self::VariableField<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
		value: TypeId,
	) {
		register_variable(
			field,
			environment,
			checking_data,
			VariableRegisterArguments {
				// TODO
				constant: true,
				space: None,
				initial_value: Some(value),
			},
		);
	}
}

pub(super) fn parser_property_key_to_checker_property_key<
	P: parser::property_key::PropertyKeyKind,
	T: crate::ReadFromFS,
>(
	property_key: &ParserPropertyKey<P>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, EznoParser>,
) -> PropertyKey<'static> {
	match property_key {
		ParserPropertyKey::StringLiteral(value, ..) | ParserPropertyKey::Ident(value, ..) => {
			PropertyKey::String(std::borrow::Cow::Owned(value.clone()))
		}
		ParserPropertyKey::NumberLiteral(number, _) => {
			let result = f64::try_from(number.clone());
			match result {
				Ok(v) => {
					// TODO is there a better way
					#[allow(clippy::float_cmp)]
					if v.floor() == v {
						PropertyKey::from_usize(v as usize)
					} else {
						// TODO
						PropertyKey::String(std::borrow::Cow::Owned(v.to_string()))
					}
				}
				// TODO
				Err(()) => todo!(),
			}
		}
		ParserPropertyKey::Computed(expression, _) => {
			let key_type =
				synthesise_expression(expression, environment, checking_data, TypeId::ANY_TYPE);
			PropertyKey::from_type(key_type, &checking_data.types)
		}
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

impl crate::GenericTypeParameter for parser::TypeParameter {
	fn get_name(&self) -> &str {
		&self.name
	}
}

impl<'a> From<Option<&'a parser::types::AnnotationPerforms>> for Performs<'a> {
	fn from(value: Option<&'a parser::types::AnnotationPerforms>) -> Self {
		match value {
			Some(parser::types::AnnotationPerforms::PerformsConst { identifier }) => {
				Performs::Const(identifier.clone())
			}
			Some(parser::types::AnnotationPerforms::PerformsStatements { body: statements }) => {
				Performs::Block(statements)
			}
			None => Performs::None,
		}
	}
}

/// For the REPL in Ezno's CLI
pub mod interactive {
	use std::{collections::HashSet, mem, path::PathBuf};

	use source_map::{FileSystem, MapFileStore, SourceId, WithPathMap};

	use crate::{
		add_definition_files_to_root, types::printing::print_type, CheckingData,
		DiagnosticsContainer, RootContext, TypeId,
	};

	use super::{block::synthesise_block, expressions::synthesise_multiple_expression};

	pub struct State<'a, T: crate::ReadFromFS> {
		checking_data: CheckingData<'a, T, super::EznoParser>,
		root: RootContext,
		source: SourceId,
	}

	impl<'a, T: crate::ReadFromFS> State<'a, T> {
		pub fn new(
			resolver: &'a T,
			type_definition_files: HashSet<PathBuf>,
		) -> Result<Self, (DiagnosticsContainer, MapFileStore<WithPathMap>)> {
			let mut root = RootContext::new_with_primitive_references();
			let mut checking_data =
				CheckingData::new(Default::default(), resolver, Default::default(), ());

			add_definition_files_to_root(type_definition_files, &mut root, &mut checking_data);

			if checking_data.diagnostics_container.has_error() {
				Err((checking_data.diagnostics_container, checking_data.modules.files))
			} else {
				let source =
					checking_data.modules.files.new_source_id("CLI.tsx".into(), String::default());
				Ok(Self { checking_data, root, source })
			}
		}

		pub fn check_item(
			&mut self,
			item: &parser::Module,
		) -> Result<(Option<String>, DiagnosticsContainer), DiagnosticsContainer> {
			let (ty, ..) = self.root.new_lexical_environment_fold_into_parent(
				crate::Scope::PassThrough { source: self.source },
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
						let result = synthesise_multiple_expression(
							expression,
							environment,
							checking_data,
							TypeId::ANY_TYPE,
						);
						Some(print_type(result, &checking_data.types, environment, false))
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

		#[must_use]
		pub fn get_source_id(&self) -> SourceId {
			self.source
		}

		#[must_use]
		pub fn get_fs_ref(&self) -> &MapFileStore<WithPathMap> {
			&self.checking_data.modules.files
		}

		pub fn get_fs_mut(&mut self) -> &mut MapFileStore<WithPathMap> {
			&mut self.checking_data.modules.files
		}
	}
}
