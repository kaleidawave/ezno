use parser::ASTNode;

use crate::{context::Root, synthesis::functions::type_function_reference};

const DEFINITION_VAR_IS_CONSTANT: bool = true;

/// Interprets a definition module (.d.ts) and produces a [Environment]. Consumes the [TypeDefinitionModule]
/// TODO remove unwraps here and add to the existing error handler
pub(super) fn type_definition_file<T: crate::FSResolver>(
	mut definition: parser::TypeDefinitionModule,
	checking_data: &mut crate::CheckingData<T>,
) -> Root {
	use std::collections::HashMap;

	use parser::{
		declarations::{DeclareVariableDeclaration, TypeAlias},
		TypeDeclaration, TypeDefinitionModuleDeclaration,
	};

	use crate::{
		diagnostics::TypeCheckError, synthesis::type_annotations::synthesize_type_annotation,
	};

	let mut idx_to_types = HashMap::new();
	let mut root = Root::new_with_primitive_references();

	// Hoisting names of interfaces, namespaces and types
	// At some point with binaries could remove this pass
	for statement in definition.declarations.iter() {
		match statement {
			TypeDefinitionModuleDeclaration::Interface(interface) => {
				let ty = root.new_interface(
					&interface.on.name,
					interface.on.position.clone(),
					&mut checking_data.types,
				);
				idx_to_types.insert(interface.on.position.start, ty);
			}
			TypeDefinitionModuleDeclaration::Class(class) => {
				todo!();
				// (
				// 	class.type_id,
				// 	// environment.register_type(&class.name, class.type_parameters.is_some(), None),
				// 	environment.register_type(Type::NamedRooted { name class.name.clone())),
				// ),
			}
			TypeDefinitionModuleDeclaration::TypeAlias(type_alias) => {
				if type_alias.type_name.type_parameters.is_some() {
					todo!()
				}
				let to = synthesize_type_annotation(
					&type_alias.type_expression,
					&mut root,
					checking_data,
				);

				// idx_to_types.insert(
				// 	interface.on.position.start,
				// 	(&interface.on, &mut root, checking_data),
				// );
				root.new_alias(&type_alias.type_name.name, to, &mut checking_data.types);
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
				let base = type_function_reference(
					&func.type_parameters,
					&func.parameters,
					func.return_type.as_ref(),
					&mut root,
					checking_data,
					func.performs.as_ref().into(),
					func.position.clone(),
					crate::types::FunctionKind::Arrow,
					None,
				);

				let base = checking_data.types.register_type(crate::Type::Function(
					base,
					crate::types::FunctionNature::Reference,
				));

				let behavior = crate::context::VariableRegisterBehavior::Declare { base };

				let res = root.register_variable_handle_error(
					func.name.as_str(),
					func.get_position().into_owned(),
					behavior,
					checking_data,
				);
			}
			TypeDefinitionModuleDeclaration::Variable(DeclareVariableDeclaration {
				name,
				type_restriction,
				decorators,
				position,
			}) => {
				// TODO tidy up
				let variable_ty =
					synthesize_type_annotation(&type_restriction, &mut root, checking_data);

				// // TODO not sure...
				// if let Some(frozen) = environment.is_frozen(variable_ty) {
				// 	environment.frozen.insert(var_type, frozen);
				// }

				let declare_variable = root.declare_variable(
					&name,
					position.clone(),
					variable_ty,
					&mut checking_data.types,
				);

				checking_data
					.type_mappings
					.variables_to_constraints
					.0
					.insert(crate::VariableId(position.source, position.start), variable_ty);

				if let Err(error) = declare_variable {
					checking_data.diagnostics_container.add_error(
						TypeCheckError::CannotRedeclareVariable {
							name: error.name.to_owned(),
							position,
						},
					)
				}
			}
			TypeDefinitionModuleDeclaration::Interface(interface) => {
				let ty = idx_to_types.remove(&interface.on.position.start).unwrap();
				super::interfaces::synthesize_signatures(
					&interface.on.members,
					super::interfaces::OnToType(ty),
					&mut root,
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
				//     let mut environment = environment.new_lexical_environment();
				//     let type_parameters = generic_type_parameters_from_generic_type_constraints(
				//         type_parameters,
				//         &mut environment,
				//         error_handler,
				//         type_mappings,
				//     );
				//     let borrow = type_parameters.0.borrow();
				//     for parameter in borrow.iter().cloned() {
				//         environment.declare_generic_type_parameter(parameter);
				//     }
				//     environment.get_type(&type_expression, error_handler, type_mappings).unwrap()
				// } else {
				//     environment.get_type(&type_expression, error_handler, type_mappings).unwrap()
				// };
				// todo!("This should have two passes with a empty type");
				} else {
					// todo!("Modify alias")
					// let ty = environment.get_type_handle_errors(&type_expression, checking_data);
					// environment.register_type(ty);
				}
			}
			TypeDefinitionModuleDeclaration::Namespace(_) => unimplemented!(),
			TypeDefinitionModuleDeclaration::LocalVariableDeclaration(_) => unimplemented!(),
			TypeDefinitionModuleDeclaration::Comment(comment) => {}
			TypeDefinitionModuleDeclaration::Class(class) => {
				todo!();
				// let existing_type =
				//     checking_data.type_mappings.get_type_declaration(&class.type_id).unwrap();
				// if let Some(extends_type) = &class.extends {
				//     let extending_type = environment
				//         .get_type(
				//             extends_type,
				//             checking_data,
				//             &crate::environment::GetTypeFromReferenceSettings::Default,
				//         )
				//         .expect("Class should have been initialized");
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
	root
}

#[cfg(feature = "declaration-synthesis")]
pub fn definition_file_to_buffer<T: crate::FSResolver>(
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

pub fn root_context_from_bytes(file: Vec<u8>) -> Root {
	let now = std::time::Instant::now();
	let ctx = Root::deserialize(file, source_map::SourceId::NULL).unwrap();
	println!("From binary {:?}", now.elapsed());
	ctx
}
