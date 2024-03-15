use parser::{
	ASTNode, Declaration, ExpressionOrStatementPosition, Statement, StatementOrDeclaration,
};
use source_map::SourceId;

use crate::{
	context::{Names, RootContext, VariableRegisterArguments},
	diagnostics::TypeCheckWarning,
	synthesis::type_annotations::synthesise_type_annotation,
	Environment, LocalInformation, TypeId,
};

/// Interprets a definition module (.d.ts) and produces a [Environment]. Consumes the [`TypeDefinitionModule`]
/// TODO remove unwraps here and add to the existing error handler
pub(super) fn type_definition_file<T: crate::ReadFromFS>(
	definition: parser::Module,
	source: SourceId,
	checking_data: &mut crate::CheckingData<T, super::EznoParser>,
	root: &RootContext,
) -> (Names, LocalInformation) {
	use std::collections::HashMap;

	use parser::declarations::{DeclareVariableDeclaration, TypeAlias};

	let mut idx_to_types = HashMap::new();

	let mut environment = root.new_lexical_environment(crate::Scope::DefinitionModule { source });

	// Hoisting names of interfaces, namespaces and types
	for statement in &definition.items {
		// TODO classes and exports
		match statement {
			StatementOrDeclaration::Declaration(Declaration::Interface(interface)) => {
				let ty = environment.register_interface(
					interface.on.name.as_option_str().unwrap_or_default(),
					interface.on.is_nominal,
					interface.on.type_parameters.as_deref(),
					interface.on.extends.as_deref(),
					interface.on.position.with_source(source),
					checking_data,
				);
				idx_to_types.insert(interface.on.position.start, ty);
			}
			StatementOrDeclaration::Declaration(Declaration::TypeAlias(alias)) => {
				environment.new_alias(
					&alias.name.identifier.as_option_str().map_or_else(String::new, str::to_owned),
					alias.parameters.as_deref(),
					&alias.references,
					alias.get_position(),
					checking_data,
				);
			}
			_ => {}
		}
	}

	for declaration in definition.items {
		// TODO more
		match declaration {
			StatementOrDeclaration::Declaration(Declaration::DeclareVariable(
				DeclareVariableDeclaration { keyword: _, declarations, position: _, decorators: _ },
			)) => {
				for declaration in &declarations {
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
				let ty = idx_to_types.remove(&interface.on.position.start).unwrap();
				super::interfaces::synthesise_signatures(
					interface.on.type_parameters.as_deref(),
					interface.on.extends.as_deref(),
					&interface.on.members,
					super::interfaces::OnToType(ty),
					&mut environment,
					checking_data,
				);
			}
			StatementOrDeclaration::Declaration(Declaration::TypeAlias(TypeAlias { .. })) => {
				todo!()
			}
			StatementOrDeclaration::Statement(Statement::Comment(..) | Statement::Empty(..)) => {}
			item => checking_data.diagnostics_container.add_warning(
				TypeCheckWarning::InvalidOrUnimplementedDefinitionFileItem(
					item.get_position().with_source(environment.get_source()),
				),
			),
		}
	}

	let Environment { named_types, info, variable_names, variables, .. } = environment;
	(Names { variables, named_types, variable_names }, info)
}

pub(crate) fn _decorators_to_context(decorators: &[parser::Decorator]) -> Option<String> {
	decorators.iter().find_map(|dec| {
		if dec.name.first() == Some(&"server".to_owned()) {
			Some("server".to_owned())
		} else if dec.name.first() == Some(&"client".to_owned()) {
			Some("client".to_owned())
		} else {
			None
		}
	})
}

#[cfg(feature = "declaration-synthesis")]
pub fn definition_file_to_buffer<T: crate::ReadFromFS>(
	handler: &T,
	cwd: &std::path::Path,
	file: &std::path::Path,
) -> Result<Vec<u8>, String> {
	// 	ModuleData::new_with_custom_module_resolvers(Default::default(), , cwd.to_owned());

	let mut checking_data = crate::CheckingData::new(Default::default(), handler);

	let definition_file = if let Some(source) = handler(file) {
		let now = std::time::Instant::now();
		let from_string = parser::TypeDefinitionModule::from_string(source, Default::default());

		println!("Parsing {:?}", now.elapsed());
		match from_string {
			Ok((definition_file, _)) => definition_file,
			Err(err) => {
				return Err(format!("Error parsing {}, {:?}", file.display(), err));
			}
		}
	} else {
		return Err(format!("Could not find file {}", file.display()));
	};

	let now = std::time::Instant::now();
	let env = type_definition_file(definition_file, &mut checking_data);
	println!("Synthesis {:?}", now.elapsed());

	if checking_data.diagnostics_container.has_error() {
		todo!();
	}

	Ok(env.serialize())
}

// TODO temp
// pub fn root_context_from_bytes(file: Vec<u8>) -> RootContext {
// 	let now = std::time::Instant::now();
// 	let ctx = RootContext::deserialize(file, source_map::source_map::Nullable::NULL).unwrap();
// 	println!("From binary {:?}", now.elapsed());
// 	ctx
// }
