use parser::ASTNode;

use crate::{
	context::{Names, RootContext},
	synthesis::{functions::type_function_reference, EznoParser},
	Environment, Facts, TypeId,
};

const DEFINITION_VAR_IS_CONSTANT: bool = true;

/// Interprets a definition module (.d.ts) and produces a [Environment]. Consumes the [TypeDefinitionModule]
/// TODO remove unwraps here and add to the existing error handler
pub(super) fn type_definition_file<T: crate::ReadFromFS>(
	mut definition: parser::TypeDefinitionModule,
	checking_data: &mut crate::CheckingData<T, super::EznoParser>,
	root: &RootContext,
) -> (Names, Facts) {
	use std::collections::HashMap;

	use parser::{
		declarations::{DeclareVariableDeclaration, TypeAlias},
		TypeDeclaration, TypeDefinitionModuleDeclaration,
	};

	use crate::{
		diagnostics::TypeCheckError, synthesis::type_annotations::synthesise_type_annotation,
	};

	let mut idx_to_types = HashMap::new();
	let source = definition.source;
	let mut env = root.new_lexical_environment(crate::Scope::DefinitionModule { source });

	// Hoisting names of interfaces, namespaces and types
	// At some point with binaries could remove this pass
	for statement in definition.declarations.iter() {
		match statement {
			TypeDefinitionModuleDeclaration::Interface(interface) => {
				let ty = env.new_interface::<EznoParser>(
					&interface.on.name,
					interface.on.nominal_keyword.is_some(),
					interface.on.type_parameters.as_deref(),
					interface.on.extends.as_deref(),
					interface.on.position.clone().with_source(source).clone(),
					&mut checking_data.types,
				);
				idx_to_types.insert(interface.on.position.start, ty);
			}
			TypeDefinitionModuleDeclaration::Class(class) => {
				todo!();
				// (
				// 	class.type_id,
				// 	// env.register_type(&class.name, class.type_parameters.is_some(), None),
				// 	env.register_type(Type::NamedRooted { name class.name.clone())),
				// ),
			}
			TypeDefinitionModuleDeclaration::TypeAlias(type_alias) => {
				if type_alias.type_name.type_parameters.is_some() {
					todo!()
				}
				let to = synthesise_type_annotation(
					&type_alias.type_expression,
					&mut env,
					checking_data,
				);

				// idx_to_types.insert(
				// 	interface.on.position.start,
				// 	(&interface.on, &mut env, checking_data),
				// );
				env.new_alias(&type_alias.type_name.name, to, &mut checking_data.types);
				// checking_data
				// 	.raise_unimplemented_error("type alias", type_alias.type_name.position.clone());
			}
			_ => {}
		}
	}

	for declaration in definition.declarations.into_iter() {
		match declaration {
			TypeDefinitionModuleDeclaration::Function(func) => {
				// TODO abstract
				let declared_at = func.get_position().clone().with_source(source);
				let base = type_function_reference(
					&func.type_parameters,
					&func.parameters,
					func.return_type.as_ref(),
					&mut env,
					checking_data,
					func.performs.as_ref().into(),
					declared_at.clone(),
					crate::types::FunctionKind::Arrow,
					None,
				);

				let base = checking_data.types.new_function_type_annotation(
					base.type_parameters,
					base.parameters,
					base.return_type,
					// TODO
					declared_at,
					base.effects,
					base.constant_id,
				);

				let behavior = crate::context::VariableRegisterBehavior::Declare { base };

				let res = env.register_variable_handle_error(
					func.name.as_str(),
					// TODO
					func.get_position().clone().with_source(source),
					behavior,
					checking_data,
				);
			}
			TypeDefinitionModuleDeclaration::Variable(DeclareVariableDeclaration {
				keyword,
				declarations,
				position,
			}) => {
				for declaration in declarations.iter() {
					let constraint = declaration.type_annotation.as_ref().map(|annotation| {
						synthesise_type_annotation(annotation, &mut env, checking_data)
					});

					// TODO warning here
					let behavior = crate::context::VariableRegisterBehavior::Declare {
						base: constraint.unwrap_or(TypeId::ANY_TYPE),
					};

					crate::synthesis::variables::register_variable(
						declaration.name.get_ast_ref(),
						&mut env,
						checking_data,
						behavior,
						constraint,
					);
				}
			}
			TypeDefinitionModuleDeclaration::Interface(interface) => {
				let ty = idx_to_types.remove(&interface.on.position.start).unwrap();
				super::interfaces::synthesise_signatures(
					interface.on.type_parameters.as_deref(),
					&interface.on.members,
					super::interfaces::OnToType(ty),
					&mut env,
					checking_data,
				);
			}
			// TODO handle locals differently, (maybe squash ast as well)
			TypeDefinitionModuleDeclaration::LocalTypeAlias(TypeAlias {
				type_name,
				type_expression,
				..
			})
			| TypeDefinitionModuleDeclaration::TypeAlias(TypeAlias {
				type_name,
				type_expression,
				..
			}) => {
				let TypeDeclaration { name, type_parameters, .. } = &type_name;

				if let Some(_) = type_parameters {
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
			TypeDefinitionModuleDeclaration::Namespace(_) => unimplemented!(),
			TypeDefinitionModuleDeclaration::LocalVariableDeclaration(_) => {
				unimplemented!()
			}
			TypeDefinitionModuleDeclaration::Comment(comment) => {}
			TypeDefinitionModuleDeclaration::Class(class) => {
				todo!();
				// let existing_type =
				//     checking_data.type_mappings.get_type_declaration(&class.type_id).unwrap();
				// if let Some(extends_type) = &class.extends {
				//     let extending_type = root
				//         .get_type(
				//             extends_type,
				//             checking_data,
				//             &crate::root::GetTypeFromReferenceSettings::Default,
				//         )
				//         .expect("Class should have been initialised");
				//     todo!();
				//     // match existing_type {
				//     //     TypeDeclaration::NonGenericType(ngt) => {
				//     //         // *ngt.extends.borrow_mut() = ExtendsType::Single(extending_type);
				//     //     }
				//     //     TypeDeclaration::GenericType(gt) => {
				//     //         // *gt.extends.borrow_mut() = ExtendsType::Single(extending_type);
				//     //     }
				//     //     TypeDeclaration::PrimitiveType(_) => unreachable!(),
				//     // }
				// }
				// TODO parse members
				// for member in class.
			}
		}
	}

	let Environment { named_types, facts, variable_names, variables, .. } = env;
	(Names { named_types, variable_names, variables }, facts)
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
		// TODO
		let cursors = Default::default();
		let from_string = parser::TypeDefinitionModule::from_string(
			source,
			Default::default(),
			source_map::SourceId::NULL,
			cursors,
		);

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
// 	let ctx = RootContext::deserialize(file, source_map::SourceId::NULL).unwrap();
// 	println!("From binary {:?}", now.elapsed());
// 	ctx
// }
