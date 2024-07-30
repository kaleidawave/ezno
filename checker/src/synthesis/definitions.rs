use parser::{
	ASTNode, Declaration, Expression, ExpressionOrStatementPosition, Statement,
	StatementOrDeclaration,
};
use source_map::SourceId;

use super::{
	classes::{register_statement_class_with_members, synthesise_class_declaration},
	type_annotations::synthesise_type_annotation,
	EznoParser,
};
use crate::{
	context::{Environment, LocalInformation, Names, RootContext, VariableRegisterArguments},
	diagnostics::TypeCheckWarning,
	features::functions::synthesise_declare_statement_function,
	types::InternalFunctionEffect,
	TypeId, VariableId,
};

const FUNCTION_REASSIGNMENT_CONSTANT: bool = true;

/// Interprets a definition module (.d.ts) and produces a [Environment]. Consumes the [`TypeDefinitionModule`]
/// TODO remove unwraps here and add to the existing error handler
pub(super) fn type_definition_file<T: crate::ReadFromFS>(
	definition: parser::Module,
	source: SourceId,
	checking_data: &mut crate::CheckingData<T, super::EznoParser>,
	root: &RootContext,
) -> (Names, LocalInformation) {
	use parser::declarations::{DeclareVariableDeclaration, TypeAlias};

	let mut environment = root.new_lexical_environment(crate::Scope::DefinitionModule { source });

	// Hoisting names of interfaces, namespaces and types
	for statement in &definition.items {
		// TODO classes and exports
		match statement {
			StatementOrDeclaration::Declaration(Declaration::Interface(interface)) => {
				let ty = environment.register_interface(
					interface.on.name.identifier.as_option_str().unwrap_or_default(),
					interface.on.is_nominal,
					interface.on.type_parameters.as_deref(),
					interface.on.extends.as_deref(),
					interface.on.position.with_source(source),
					checking_data,
				);
				checking_data
					.local_type_mappings
					.types_to_types
					.push(interface.on.get_position(), ty);
			}
			StatementOrDeclaration::Declaration(Declaration::Class(class)) => {
				let ty = environment.register_class::<EznoParser>(
					class.on.name.as_option_str().unwrap_or_default(),
					class.on.type_parameters.as_deref(),
					class.on.extends.as_deref(),
					&mut checking_data.types,
				);
				checking_data.local_type_mappings.types_to_types.push(class.on.get_position(), ty);
			}
			StatementOrDeclaration::Declaration(Declaration::TypeAlias(alias)) => {
				let ty = environment.new_alias(
					alias.name.identifier.as_option_str().unwrap_or_default(),
					alias.parameters.as_deref(),
					&alias.references,
					alias.get_position(),
					checking_data,
				);
				checking_data.local_type_mappings.types_to_types.push(alias.get_position(), ty);
			}
			_ => {}
		}
	}

	for declaration in &definition.items {
		// TODO more
		match declaration {
			StatementOrDeclaration::Declaration(Declaration::DeclareVariable(
				DeclareVariableDeclaration { keyword: _, declarations, position: _, decorators: _ },
			)) => {
				for declaration in declarations {
					// TODO is it ever `None`...?
					let constraint = declaration.type_annotation.as_ref().map_or(
						TypeId::ANY_TYPE,
						|annotation| {
							synthesise_type_annotation(annotation, &mut environment, checking_data)
						},
					);

					let initial_value = Some(checking_data.types.register_type(
						crate::Type::RootPolyType(crate::types::PolyNature::Open(constraint)),
					));
					crate::synthesis::variables::register_variable(
						declaration.name.get_ast_ref(),
						&mut environment,
						checking_data,
						VariableRegisterArguments {
							constant: true,
							space: None,
							initial_value,
							allow_reregistration: false,
						},
					);
				}
			}
			StatementOrDeclaration::Declaration(Declaration::Interface(interface)) => {
				let ty = *checking_data
					.local_type_mappings
					.types_to_types
					.get(interface.on.get_position().start)
					.unwrap();

				super::interfaces::synthesise_signatures(
					interface.on.type_parameters.as_deref(),
					interface.on.extends.as_deref(),
					&interface.on.members,
					super::interfaces::OnToType(ty),
					&mut environment,
					checking_data,
				);
			}
			StatementOrDeclaration::Declaration(Declaration::Class(class)) => {
				register_statement_class_with_members(&class.on, &mut environment, checking_data);
			}
			StatementOrDeclaration::Declaration(Declaration::Function(function)) => {
				crate::synthesis::variables::register_variable_identifier(
					&function.on.name.identifier,
					&mut environment,
					checking_data,
					VariableRegisterArguments {
						constant: FUNCTION_REASSIGNMENT_CONSTANT,
						space: None,
						initial_value: None,
						allow_reregistration: false,
					},
				);
			}
			StatementOrDeclaration::Declaration(Declaration::TypeAlias(TypeAlias { .. })) => {
				crate::utilities::notify!("Don't think anything needed here");
			}
			StatementOrDeclaration::Statement(
				Statement::Comment(..) | Statement::Empty(..) | Statement::AestheticSemiColon(..),
			) => {}
			StatementOrDeclaration::Statement(Statement::MultiLineComment(..)) => {
				crate::utilities::notify!("TODO add information to item?");
			}
			item => {
				crate::utilities::notify!("unknown {:?}", item);
				checking_data.diagnostics_container.add_warning(
					TypeCheckWarning::InvalidOrUnimplementedDefinitionFileItem(
						item.get_position().with_source(environment.get_source()),
					),
				);
			}
		}
	}

	for declaration in definition.items {
		match declaration {
			StatementOrDeclaration::Declaration(Declaration::Class(class)) => {
				let class_type = synthesise_class_declaration(
					&class.on,
					TypeId::ANY_TYPE,
					&mut environment,
					checking_data,
				);
				let variable_id = VariableId(
					environment.get_source(),
					class.on.name.identifier.get_position().start,
				);
				environment.info.variable_current_value.insert(variable_id, class_type);
			}
			StatementOrDeclaration::Declaration(Declaration::Function(function)) => {
				if !function.on.name.declare {
					crate::utilities::notify!("TODO warning");
				}

				let variable_id = crate::VariableId(
					environment.get_source(),
					function.on.name.identifier.get_position().start,
				);

				let is_async = function.on.header.is_async();
				let is_generator = function.on.header.is_generator();
				let location = function.on.header.get_location().map(|location| match location {
					parser::functions::FunctionLocationModifier::Server => "server".to_owned(),
					parser::functions::FunctionLocationModifier::Worker => "worker".to_owned(),
				});

				let internal_marker = get_internal_function_effect_from_decorators(
					&function.decorators,
					function.on.name.as_option_str().unwrap(),
					&environment,
				);

				synthesise_declare_statement_function(
					variable_id,
					// TODO
					false,
					is_async,
					is_generator,
					location,
					function.on.name.as_option_str().unwrap_or_default().to_owned(),
					internal_marker,
					&function.on,
					&mut environment,
					checking_data,
				);
			}
			_ => {}
		}
	}

	let Environment { named_types, info, variable_names, variables, .. } = environment;
	(Names { variables, named_types, variable_names }, info)
}

pub(crate) fn get_internal_function_effect_from_decorators(
	decorators: &[parser::Decorator],
	function_name: &str,
	environment: &Environment,
) -> Option<InternalFunctionEffect> {
	decorators.iter().find_map(|decorator| {
		if decorator.name.len() == 1 {
			let decorator_name = decorator.name.first().map(String::as_str)?;
			if matches!(decorator_name, "Constant" | "InputOutput") {
				let (identifier, may_throw) =
					if let Some(arguments) = decorator.arguments.as_ref() {
						let identifier = if let Some(Expression::StringLiteral(identifier, _, _)) =
							arguments.first()
						{
							identifier.clone()
						} else {
							panic!("first argument to constant or input output should be string literal");
						};
						let may_throw = if let Some(Expression::VariableReference(identifier, _)) =
							arguments.get(1)
						{
							Some(
								environment
									.get_type_from_name(identifier)
									.expect("could not find thrown type"),
							)
						} else {
							None
						};
						(identifier, may_throw)
					} else {
						(function_name.to_owned(), None)
					};
				Some(match decorator_name {
					"Constant" => InternalFunctionEffect::Constant { identifier, may_throw },
					"InputOutput" => InternalFunctionEffect::InputOutput { identifier, may_throw },
					_ => unreachable!(),
				})
			} else {
				crate::utilities::notify!("Unknown decorator {:?}", decorator_name);
				None
			}
		} else {
			None
		}
	})
}

pub(crate) fn _decorators_to_context(decorators: &[parser::Decorator]) -> Option<String> {
	decorators.iter().find_map(|dec| {
		matches!(dec.name.first().map(String::as_str), Some("Server" | "Client"))
			.then(|| dec.name.first().unwrap().to_owned())
	})
}
