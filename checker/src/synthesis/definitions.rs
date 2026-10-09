use parser::extensions::decorators::Decorator;
use parser::{ASTNode, Expression};
use source_map::SourceId;

use super::classes::synthesise_class_declaration;

use crate::{
	TypeId,
	context::{Environment, LocalInformation, Names, RootContext},
	types::InternalFunctionEffect,
};

/// Interprets a definition module (.d.ts) and produces a [Environment]. Consumes the [`TypeDefinitionModule`]
/// TODO remove unwraps here and add to the existing error handler
pub(super) fn type_definition_file<T: crate::ReadFromFS>(
	definition: &parser::Module,
	source: SourceId,
	checking_data: &mut crate::CheckingData<T, super::EznoParser>,
	root: &RootContext,
) -> (Names, LocalInformation) {
	let mut environment = root.new_lexical_environment(crate::Scope::DefinitionModule { source });
	super::hoisting::hoist_statements(&definition.items, &mut environment, checking_data);

	// Left over classes
	for item in &definition.items {
		if let parser::StatementOrDeclaration::Class(item) = item {
			use super::StatementOrExpressionVariable;

			let class = &item.on.item;

			let class_type = *checking_data
				.local_type_mappings
				.types_to_types
				.get_exact(class.name.identifier.get_position())
				.expect("class type not lifted");

			let constructor = synthesise_class_declaration(
				class,
				Some(class_type),
				TypeId::ANY_TYPE,
				&mut environment,
				checking_data,
			);

			if let Some(variable) = class.name.get_variable_id(environment.get_source()) {
				environment.info.variable_current_value.insert(variable, constructor);
			}
		}
	}

	let Environment { named_types, info, variable_names, variables, .. } = environment;
	(Names { variables, named_types, variable_names }, info)
}

pub(crate) fn get_internal_function_effect_from_decorators(
	decorators: &[Decorator],
	function_name: &str,
	environment: &Environment,
) -> Option<InternalFunctionEffect> {
	decorators.iter().find_map(|decorator| {
		if let Expression::FunctionCall { function, arguments, .. } = &decorator.0
			&& let Expression::VariableReference(name, _) = &**function
		{
			if let "Constant" | "InputOutput" = name.as_str() {
				let (identifier, may_throw) = if arguments.is_empty() {
					(function_name.to_owned(), None)
				} else {
					let identifier = if let Some(argument) = arguments.first()
						&& let (Expression::StringLiteral(identifier, _, _), false) =
							argument.value_and_spread_ref()
					{
						identifier.clone()
					} else {
						panic!(
							"first argument to constant or input output should be string literal"
						);
					};

					let may_throw = if let Some(argument) = arguments.get(1)
						&& let (Expression::VariableReference(identifier, _), false) =
							argument.value_and_spread_ref()
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
				};

				let effect = match name.as_str() {
					"Constant" => InternalFunctionEffect::Constant { identifier, may_throw },
					"InputOutput" => InternalFunctionEffect::InputOutput { identifier, may_throw },
					_ => unreachable!(),
				};
				Some(effect)
			} else {
				crate::utilities::notify!("Unknown decorator {:?}", name);
				None
			}
		} else {
			None
		}
	})
}

pub(crate) fn _decorators_to_context(decorators: &[Decorator]) -> Option<String> {
	decorators.iter().find_map(|dec| {
		if let Expression::VariableReference(name, _) = &dec.0
			&& matches!(name.as_str(), "Server" | "Client")
		{
			Some(name.to_owned())
		} else {
			None
		}
	})
}
