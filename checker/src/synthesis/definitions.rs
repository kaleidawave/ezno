use parser::{
	ASTNode, Declaration, ExpressionOrStatementPosition, Statement, StatementOrDeclaration,
};
use source_map::SourceId;

use crate::{
	context::{Names, RootContext, VariableRegisterArguments},
	diagnostics::TypeCheckWarning,
	features::functions::synthesise_declare_statement_function,
	synthesis::{
		classes::{register_class_and_members, synthesise_class_declaration},
		type_annotations::synthesise_type_annotation,
		EznoParser,
	},
	types::InternalFunctionEffect,
	Environment, LocalInformation, TypeId, VariableId,
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
						VariableRegisterArguments { constant: true, space: None, initial_value },
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
				register_class_and_members(&class.on, &mut environment, checking_data)
			}
			StatementOrDeclaration::Declaration(Declaration::TypeAlias(TypeAlias {
				name: _,
				references: _,
				parameters,
				position: _,
			})) => {
				// To remove when implementing
				#[allow(clippy::redundant_pattern_matching)]
				if let Some(_) = parameters {
					todo!()
				// let ty = if let Some(type_parameters) = type_parameters {
				//     let mut root = env.new_lexical_root();
				//     let type_parameters = generic_type_parameters_from_generic_type_constraints(
				//         type_parameters,
				//         &mut env,
				//         error_handler,
				//         type_mappings,
				//     );
				//     let borrow = type_parameters.0.borrow();
				//     for parameter in borrow.iter().cloned() {
				//         env.declare_generic_type_parameter(parameter);
				//     }
				//     env.get_type(&type_expression, error_handler, type_mappings).unwrap()
				// } else {
				//     env.get_type(&type_expression, error_handler, type_mappings).unwrap()
				// };
				// todo!("This should have two passes with a empty type");
				} else {
					// todo!("Modify alias")
					// let ty = env.get_type_handle_errors(&type_expression, checking_data);
					// env.register_type(ty);
				}
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
					},
				);
			}
			StatementOrDeclaration::Statement(Statement::Comment(..) | Statement::Empty(..)) => {}
			item => checking_data.diagnostics_container.add_warning(
				TypeCheckWarning::InvalidOrUnimplementedDefinitionFileItem(
					item.get_position().with_source(environment.get_source()),
				),
			),
		}
	}

	for declaration in definition.items {
		match declaration {
			StatementOrDeclaration::Declaration(Declaration::Class(class)) => {
				let class_type =
					synthesise_class_declaration(&class.on, &mut environment, checking_data);
				let variable_id = VariableId(
					environment.get_source(),
					class.on.name.identifier.get_position().start,
				);
				environment.info.variable_current_value.insert(variable_id, class_type);
			}
			StatementOrDeclaration::Declaration(Declaration::Function(function)) => {
				if !function.on.name.declare {
					crate::utils::notify!("TODO warning");
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
				);

				synthesise_declare_statement_function(
					variable_id,
					is_async,
					is_generator,
					location,
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
) -> Option<InternalFunctionEffect> {
	decorators.iter().find_map(|d| {
		if d.name.len() == 1 {
			let name = d.name.first().map(String::as_str)?;
			match name {
				"Constant" => Some(InternalFunctionEffect::Constant(function_name.to_owned())),
				"InputOutput" => {
					Some(InternalFunctionEffect::InputOutput(function_name.to_owned()))
				}
				_ => None,
			}
		} else {
			None
		}
	})
}

pub(crate) fn _decorators_to_context(decorators: &[parser::Decorator]) -> Option<String> {
	decorators.iter().find_map(|dec| {
		matches!(dec.name.first().map(String::as_str), Some("server" | "client"))
			.then(|| dec.name.first().unwrap().to_owned())
	})
}
