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
use parser::{ASTNode, PropertyKey as ParserPropertyKey};
use source_map::SourceId;

use crate::{
	behavior::modules::Exported,
	context::{environment, Context, ContextType, Names},
	types::{properties::PropertyKey, TypeStore},
	CheckingData, Constant, Diagnostic, Environment, Facts, RootContext, TypeId,
};

use self::{
	declarations::synthesise_variable_declaration,
	expressions::{synthesise_expression, synthesise_multiple_expression},
	hoisting::hoist_variable_declaration,
	type_annotations::synthesise_type_annotation,
	variables::register_variable,
};

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

// Clippy suggests a fix that breaks the code
#[allow(clippy::needless_lifetimes)]
impl crate::ASTImplementation for EznoParser {
	type ParseOptions = parser::ParseOptions;
	type ParseError = (parser::ParseError, SourceId);

	type Module<'a> = parser::Module;
	type OwnedModule = parser::Module;

	type TypeAnnotation<'a> = parser::TypeAnnotation;
	type TypeParameter<'a> = parser::GenericTypeConstraint;
	type Expression<'a> = parser::Expression;
	type MultipleExpression<'a> = parser::expressions::MultipleExpression;
	type ClassMethod<'a> = parser::FunctionBase<parser::ast::ClassFunctionBase>;

	type VariableField<'a> = parser::VariableField<parser::VariableFieldInSourceCode>;

	type ForStatementInitiliser<'a> = parser::statements::ForLoopStatementInitializer;

	fn module_from_string(
		source_id: SourceId,
		string: String,
		options: Self::ParseOptions,
	) -> Result<Self::Module<'static>, Self::ParseError> {
		<parser::Module as parser::ASTNode>::from_string(string, options, source_id, None)
			.map_err(|err| (err, source_id))
	}

	fn definition_module_from_string(
		source_id: SourceId,
		string: String,
	) -> Result<Self::DefinitionFile<'static>, Self::ParseError> {
		let options = Default::default();
		parser::TypeDefinitionModule::from_string(&string, options, source_id)
			.map_err(|err| (err, source_id))
	}

	fn synthesise_module<'a, T: crate::ReadFromFS>(
		module: &Self::Module<'a>,
		source_id: SourceId,
		module_environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) {
		synthesise_block(&module.items, module_environment, checking_data);
	}

	fn synthesise_expression<'a, U: crate::ReadFromFS>(
		expression: &Self::Expression<'a>,
		expecting: TypeId,
		environment: &mut Environment,
		checking_data: &mut CheckingData<U, Self>,
	) -> TypeId {
		synthesise_expression(expression, environment, checking_data, expecting)
	}

	fn expression_position<'a>(expression: &'a Self::Expression<'a>) -> source_map::Span {
		*ASTNode::get_position(expression)
	}

	fn type_parameter_name<'a>(parameter: &'a Self::TypeParameter<'a>) -> &'a str {
		parameter.name()
	}

	fn synthesise_type_annotation<'a, T: crate::ReadFromFS>(
		annotation: &Self::TypeAnnotation<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId {
		synthesise_type_annotation(annotation, environment, checking_data)
	}

	type DefinitionFile<'a> = parser::TypeDefinitionModule;

	fn synthesise_definition_file<'a, T: crate::ReadFromFS>(
		file: Self::DefinitionFile<'a>,
		root: &RootContext,
		checking_data: &mut CheckingData<T, Self>,
	) -> (Names, Facts) {
		definitions::type_definition_file(file, checking_data, root)
	}

	fn parse_options(_is_js: bool) -> Self::ParseOptions {
		// TODO
		parser::ParseOptions::default()
	}

	fn owned_module_from_module(m: Self::Module<'static>) -> Self::OwnedModule {
		m
	}

	fn synthesise_multiple_expression<'a, T: crate::ReadFromFS>(
		expression: &'a Self::MultipleExpression<'a>,
		expected_type: TypeId,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId {
		synthesise_multiple_expression(expression, environment, checking_data, expected_type)
	}

	fn synthesise_for_loop_initialiser<'a, T: crate::ReadFromFS>(
		for_loop_initialiser: &'a Self::ForStatementInitiliser<'a>,
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
}

pub mod interactive {
	use std::{collections::HashSet, mem, path::PathBuf};

	use source_map::{MapFileStore, SourceId, WithPathMap};

	use crate::{
		add_definition_files_to_root, types::printing::print_type, CheckingData,
		DiagnosticsContainer, RootContext, TypeId,
	};

	use super::{
		block::{synthesise_block, synthesise_declaration},
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
			let mut checking_data =
				CheckingData::new(Default::default(), resolver, Default::default());

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
			let source = self.checking_data.modules.entry_point.unwrap();
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
						let result = synthesise_multiple_expression(
							expression,
							environment,
							checking_data,
							TypeId::ANY_TYPE,
						);
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

		#[must_use]
		pub fn get_fs_ref(&self) -> &MapFileStore<WithPathMap> {
			&self.checking_data.modules.files
		}

		pub fn get_fs_mut(&mut self) -> &mut MapFileStore<WithPathMap> {
			&mut self.checking_data.modules.files
		}

		#[must_use]
		pub fn get_source_id(&self) -> SourceId {
			self.checking_data.modules.entry_point.unwrap()
		}
	}
}
