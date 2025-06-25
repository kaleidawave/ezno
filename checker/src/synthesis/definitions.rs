use parser::{ast::ExportDeclaration, ASTNode, Expression, StatementOrDeclaration};
use source_map::SourceId;

use super::classes::synthesise_class_declaration;

use crate::{
	context::{Environment, LocalInformation, Names, RootContext},
	types::InternalFunctionEffect,
	TypeId,
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

	for item in &definition.items {
		if let Some((class, _decorators)) = as_class(item) {
			use super::StatementOrExpressionVariable;

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

pub(crate) fn as_class(
	item: &parser::StatementOrDeclaration,
) -> Option<(&parser::statements_and_declarations::ClassDeclarationStatement, &[parser::Decorator])>
{
	if let parser::StatementOrDeclaration::Class(decorated) = item {
		Some((&decorated.on, &decorated.decorators))
	} else if let parser::StatementOrDeclaration::Export(decorated) = item {
		if let parser::statements_and_declarations::export::ExportDeclaration::Item {
			exported,
			position: _,
		} = &decorated.on
		{
			if let parser::statements_and_declarations::export::Exportable::Class(item) =
				&**exported
			{
				Some((item, &decorated.decorators))
			} else {
				None
			}
		} else {
			None
		}
	} else {
		None
	}
}

pub(crate) fn as_function(
	item: &parser::StatementOrDeclaration,
) -> Option<(&parser::statements_and_declarations::StatementFunction, &[parser::Decorator])> {
	if let StatementOrDeclaration::Function(decorated) = item {
		Some((&decorated.on, &decorated.decorators))
	} else if let StatementOrDeclaration::Export(decorated) = item {
		if let ExportDeclaration::Item { exported, position: _ } = &decorated.on {
			if let parser::statements_and_declarations::export::Exportable::Function(item) =
				&**exported
			{
				Some((item, &decorated.decorators))
			} else {
				None
			}
		} else {
			None
		}
	} else {
		None
	}
}

pub(crate) fn as_enum(
	item: &parser::StatementOrDeclaration,
) -> Option<(&parser::statements_and_declarations::EnumDeclaration, &[parser::Decorator])> {
	if let StatementOrDeclaration::Enum(decorated) = item {
		Some((&decorated.on, &decorated.decorators))
	} else if let StatementOrDeclaration::Export(decorated) = item {
		if let ExportDeclaration::Item { exported, position: _ } = &decorated.on {
			if let parser::statements_and_declarations::export::Exportable::Enum(item) = &**exported
			{
				Some((item, &decorated.decorators))
			} else {
				None
			}
		} else {
			None
		}
	} else {
		None
	}
}

pub(crate) fn as_interface(
	item: &parser::StatementOrDeclaration,
) -> Option<(&parser::statements_and_declarations::InterfaceDeclaration, &[parser::Decorator])> {
	if let StatementOrDeclaration::Interface(decorated) = item {
		Some((&decorated.on, &decorated.decorators))
	} else if let StatementOrDeclaration::Export(decorated) = item {
		if let ExportDeclaration::Item { exported, position: _ } = &decorated.on {
			if let parser::statements_and_declarations::export::Exportable::Interface(item) =
				&**exported
			{
				Some((item, &decorated.decorators))
			} else {
				None
			}
		} else {
			None
		}
	} else {
		None
	}
}

pub(crate) fn as_type_alias(
	item: &parser::StatementOrDeclaration,
) -> Option<(&parser::statements_and_declarations::TypeAlias, &[parser::Decorator])> {
	if let StatementOrDeclaration::TypeAlias(alias) = item {
		Some((alias, &[]))
	} else if let StatementOrDeclaration::Export(decorated) = item {
		if let ExportDeclaration::Item { exported, position: _ } = &decorated.on {
			if let parser::statements_and_declarations::export::Exportable::TypeAlias(item) =
				&**exported
			{
				Some((item, &decorated.decorators))
			} else {
				None
			}
		} else {
			None
		}
	} else {
		None
	}
}

pub(crate) fn as_variable_declaration(
	item: &parser::StatementOrDeclaration,
) -> Option<(&parser::statements_and_declarations::VariableDeclaration, &[parser::Decorator])> {
	if let StatementOrDeclaration::Variable(item) = item {
		Some((item, &[]))
	} else if let StatementOrDeclaration::Export(decorated) = item {
		if let ExportDeclaration::Item { exported, position: _ } = &decorated.on {
			if let parser::statements_and_declarations::export::Exportable::Variable(item) =
				&**exported
			{
				Some((item, &decorated.decorators))
			} else {
				None
			}
		} else {
			None
		}
	} else {
		None
	}
}

pub(crate) fn get_internal_function_effect_from_decorators(
	decorators: &[parser::Decorator],
	function_name: &str,
	environment: &Environment,
) -> Option<InternalFunctionEffect> {
	decorators.iter().find_map(|decorator| {
		if decorator.name.len() == 1 {
			let decorator_name = decorator.name.first().map(String::as_str)?;
			if let "Constant" | "InputOutput" = decorator_name {
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
