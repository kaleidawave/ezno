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
pub mod interactive;
pub mod interfaces;
pub mod statements;
pub mod type_annotations;
pub mod variables;

use block::synthesise_block;
use parser::{
	ASTNode, ExpressionPosition, ParseOptions, PropertyKey as ParserPropertyKey, StatementPosition,
};
use source_map::SourceId;

use crate::{
	context::{Environment, LocalInformation, Names, VariableRegisterArguments},
	types::properties::PropertyKey,
	CheckingData, Diagnostic, RootContext, TypeId, VariableId,
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
	type Block<'_a> = parser::Block;
	type MultipleExpression<'_a> = parser::expressions::MultipleExpression;
	type ClassMethod<'_a> = parser::FunctionBase<parser::ast::ClassFunctionBase>;

	type VariableField<'_a> = parser::VariableField;

	type ForStatementInitiliser<'_a> = parser::statements::ForLoopStatementInitialiser;

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

	fn synthesise_definition_module<T: crate::ReadFromFS>(
		module: &Self::DefinitionFile<'_>,
		source: SourceId,
		root: &RootContext,
		checking_data: &mut CheckingData<T, Self>,
	) -> (Names, LocalInformation) {
		definitions::type_definition_file(module, source, checking_data, root)
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
		ASTNode::get_position(expression)
	}

	fn multiple_expression_position<'_a>(
		expression: &'_a Self::MultipleExpression<'_a>,
	) -> source_map::Span {
		ASTNode::get_position(expression)
	}

	fn type_parameter_name<'_a>(parameter: &'_a Self::TypeParameter<'_a>) -> &'_a str {
		&parameter.name
	}

	fn synthesise_type_parameter_extends<T: crate::ReadFromFS>(
		parameter: &Self::TypeParameter<'_>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId {
		if let Some(ref extends) = parameter.extends {
			synthesise_type_annotation(extends, environment, checking_data)
		} else {
			TypeId::ANY_TYPE
		}
	}

	fn type_annotation_position<'_a>(
		annotation: &'_a Self::TypeAnnotation<'_a>,
	) -> source_map::Span {
		ASTNode::get_position(annotation)
	}

	fn synthesise_type_annotation<T: crate::ReadFromFS>(
		annotation: &Self::TypeAnnotation<'_>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId {
		synthesise_type_annotation(annotation, environment, checking_data)
	}

	fn parse_options(
		is_js: bool,
		extra_syntax: bool,
		parse_comments: bool,
		lsp_mode: bool,
	) -> Self::ParseOptions {
		parser::ParseOptions {
			comments: if parse_comments {
				parser::Comments::JustDocumentation
			} else {
				parser::Comments::None
			},
			type_annotations: !is_js,
			partial_syntax: lsp_mode,
			// TODO
			retain_blank_lines: lsp_mode,
			is_expressions: extra_syntax,
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
			parser::statements::ForLoopStatementInitialiser::VariableDeclaration(declaration) => {
				// TODO is this correct & the best
				hoist_variable_declaration(declaration, environment, checking_data);
				synthesise_variable_declaration(
					declaration,
					environment,
					checking_data,
					false,
					// IMPORTANT!
					checking_data.options.infer_sensible_constraints_in_for_loops,
				);
			}
			parser::statements::ForLoopStatementInitialiser::VarStatement(stmt) => {
				checking_data.raise_unimplemented_error(
					"var in for statement initiliser",
					stmt.get_position().with_source(environment.get_source()),
				);
			}
			parser::statements::ForLoopStatementInitialiser::Expression(expr) => {
				checking_data.raise_unimplemented_error(
					"expression as for statement initiliser",
					expr.get_position().with_source(environment.get_source()),
				);
			}
		}
	}

	fn declare_and_assign_to_fields<'a, T: crate::ReadFromFS>(
		field: &'a Self::VariableField<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
		arguments: VariableRegisterArguments,
	) {
		register_variable(field, environment, checking_data, arguments);
	}

	fn parameter_constrained<'a>(parameter: &'a Self::TypeParameter<'a>) -> bool {
		parameter.extends.is_some()
	}

	fn synthesise_block<'a, T: crate::ReadFromFS>(
		block: &'a Self::Block<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) {
		synthesise_block(&block.0, environment, checking_data);
	}
}

/// `perform_side_effect_computed` is used for hoisting
pub(super) fn parser_property_key_to_checker_property_key<
	P: parser::property_key::PropertyKeyKind,
	T: crate::ReadFromFS,
>(
	property_key: &ParserPropertyKey<P>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, EznoParser>,
	perform_side_effect_computed: bool,
) -> PropertyKey<'static> {
	match property_key {
		ParserPropertyKey::StringLiteral(value, ..) | ParserPropertyKey::Identifier(value, ..) => {
			PropertyKey::String(std::borrow::Cow::Owned(value.clone()))
		}
		ParserPropertyKey::NumberLiteral(number, pos) => {
			let result = f64::try_from(number.clone());
			if let Ok(v) = result {
				// TODO is there a better way
				#[allow(clippy::float_cmp)]
				if v.floor() == v {
					PropertyKey::from_usize(v as usize)
				} else {
					// TODO
					PropertyKey::String(std::borrow::Cow::Owned(v.to_string()))
				}
			} else {
				checking_data.raise_unimplemented_error(
					"big int as property key",
					pos.with_source(environment.get_source()),
				);
				PropertyKey::Type(TypeId::UNIMPLEMENTED_ERROR_TYPE)
			}
		}
		ParserPropertyKey::Computed(expression, _) => {
			if perform_side_effect_computed {
				let key_type =
					synthesise_expression(expression, environment, checking_data, TypeId::ANY_TYPE);
				PropertyKey::from_type(key_type, &checking_data.types)
			} else {
				PropertyKey::Type(TypeId::ANY_TYPE)
			}
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

pub trait StatementOrExpressionVariable {
	fn get_variable_id(&self, under: SourceId) -> Option<VariableId>;
}

impl StatementOrExpressionVariable for StatementPosition {
	fn get_variable_id(&self, under: SourceId) -> Option<VariableId> {
		match self.identifier {
			parser::VariableIdentifier::Standard(_, pos) => Some(VariableId(under, pos.start)),
			parser::VariableIdentifier::Marker(_, _) => None,
		}
	}
}

impl StatementOrExpressionVariable for ExpressionPosition {
	fn get_variable_id(&self, _under: SourceId) -> Option<VariableId> {
		None
	}
}
