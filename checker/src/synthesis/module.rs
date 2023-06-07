use crate::{
	context::Environment, synthesis::variables::hoist_variable_identifier, types::Type,
	CheckingData, TypeId,
};
use parser::{
	ASTNode, Chain, ChainVariable, Declaration, Statement, StatementOrDeclaration,
	VariableIdentifier,
};
use temporary_annex::Annex;

use super::{
	declarations::synthesize_variable_declaration, synthesize_function, synthesize_statement,
};

/// TODO temp
pub fn synthesize_module_root<T: crate::FSResolver>(
	module: &mut parser::Module,
	resolver: T,
) -> (crate::DiagnosticsContainer, Vec<crate::events::Event>, Vec<(TypeId, Type)>) {
	let default_settings = Default::default();
	let mut checking_data = CheckingData::new(default_settings, &resolver);

	let mut root = crate::context::Root::new_with_primitive_references();

	let (_, stuff, _) = root.new_lexical_environment_fold_into_parent(
		crate::environment::Scope::Block {},
		&mut checking_data,
		|environment, checking_data| synthesize_module(module, environment, checking_data),
	);

	let CheckingData {
		diagnostics_container,
		type_mappings,
		modules,
		settings,
		parse_settings,
		existing_contexts,
		types,
	} = checking_data;

	(diagnostics_container, stuff.unwrap().0, types.into_vec_temp())
}

pub fn synthesize_module<T: crate::FSResolver>(
	module: &mut parser::Module,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) {
	let block_id = module.block_id;
	let mut new_with_initial =
		Chain::new_with_initial(ChainVariable::UnderModule(block_id, module.source_id));

	let chain = &mut Annex::new(&mut new_with_initial);

	crate::utils::notify!("Started module hoisting");

	for item in module.items.iter() {
		if let StatementOrDeclaration::Declaration(declaration) = item {
			match declaration {
				Declaration::Class(_) => todo!(),
				Declaration::Enum(_) => todo!(),
				Declaration::Interface(interface) => {
					environment.new_interface(
						&interface.on.name,
						interface.get_position().into_owned(),
						&mut checking_data.types,
					);
				}
				Declaration::DeclareInterface(_) => todo!(),
				Declaration::TypeAlias(_) => todo!(),
				Declaration::Variable(_) => todo!(),
				Declaration::Function(_) => todo!(),
				Declaration::DeclareVariable(_)
				| Declaration::DeclareFunction(_)
				| Declaration::Import(_)
				| Declaration::Export(_) => {}
			}
		}
	}

	// Variable and type names
	for item in module.items.iter() {
		match item {
			StatementOrDeclaration::Declaration(Declaration::Variable(variable)) => {
				use parser::declarations::VariableDeclaration;
				match variable {
					VariableDeclaration::ConstDeclaration { declarations, .. } => {
						declarations.iter().for_each(|declaration| {
							super::variables::hoist_variable_declaration(
								declaration.name.get_ast(),
								environment,
								true,
							)
						});
					}
					VariableDeclaration::LetDeclaration { declarations, .. } => {
						declarations.iter().for_each(|declaration| {
							super::variables::hoist_variable_declaration(
								declaration.name.get_ast(),
								environment,
								false,
							)
						});
					}
				}
			}
			StatementOrDeclaration::Declaration(Declaration::Function(func)) => {
				let is_constant = false;
				hoist_variable_identifier(&func.on.name, environment, is_constant);
			}
			_ => {}
		}
	}

	for item in module.items.iter_mut() {
		match item {
			StatementOrDeclaration::Declaration(declaration) => match declaration {
				Declaration::Variable(variable) => {}
				Declaration::Function(function) => {
					let function_type =
						synthesize_function(&mut function.on, environment, checking_data);

					let constructor =
						crate::types::FunctionNature::Source(function.on.function_id, None, None);
					let r#type =
						checking_data.types.new_type(Type::Function(function_type, constructor));

					let VariableIdentifier::Standard(name, variable_id, name_position) = &function.on.name else { todo!() };

					environment.variable_current_value.insert(*variable_id, r#type);
				}
				Declaration::Class(_) => todo!(),
				Declaration::Enum(_) => todo!(),
				Declaration::Interface(_) => todo!(),
				Declaration::TypeAlias(_) => todo!(),
				Declaration::DeclareVariable(_) => todo!(),
				Declaration::DeclareFunction(_) => todo!(),
				Declaration::DeclareInterface(_) => todo!(),
				Declaration::Import(_) => todo!(),
				Declaration::Export(_) => todo!(),
			},
			StatementOrDeclaration::Statement(Statement::VarVariable(_)) => todo!(),
			_ => {}
		}
	}

	crate::utils::notify!("Finished module hoisting");

	for item in module.items.iter_mut() {
		match item {
			StatementOrDeclaration::Declaration(declaration) => match declaration {
				Declaration::Variable(declaration) => {
					synthesize_variable_declaration(declaration, environment, checking_data, chain);
				}
				Declaration::Class(_) => todo!(),
				Declaration::Enum(_) => todo!(),
				Declaration::Interface(_) => todo!(),
				Declaration::TypeAlias(_) => todo!(),
				Declaration::DeclareVariable(_) => todo!(),
				Declaration::DeclareFunction(_) => todo!(),
				Declaration::DeclareInterface(_) => todo!(),
				Declaration::Import(_) => todo!(),
				Declaration::Export(_) => todo!(),
				// Hoisted away
				Declaration::Function(_) => {}
			},
			StatementOrDeclaration::Statement(statement) => {
				synthesize_statement(statement, environment, checking_data, chain)
			}
		}
	}
}
