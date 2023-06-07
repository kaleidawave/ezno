use crate::{
	context::Root,
	synthesis::{functions::type_function_reference, interfaces::type_interface_declaration},
	types::TypeId,
	CheckingData,
};

use parser::{
	declarations::{DeclareFunctionDeclaration, DeclareVariableDeclaration, TypeAlias},
	Expression, TypeDeclaration as ParserTypeDeclaration, TypeDefinitionModule,
};

const DEFINITION_VAR_IS_CONSTANT: bool = true;

/// Interprets a definition module (.d.ts) and produces a [Environment]. Consumes the [TypeDefinitionModule]
/// TODO remove unwraps here and add to the existing error handler
#[cfg(feature = "declaration-synthesis")]
pub(crate) fn type_definition_file<T: crate::FSResolver>(
	mut definition: TypeDefinitionModule,
	checking_data: &mut CheckingData<T>,
) -> Root {
	use parser::{Decorated, TypeDefinitionModuleDeclaration};

	use crate::synthesis::interfaces::hoist_interface_name;

	let mut environment = Root::new_with_primitive_references();

	// Hoisting names of interfaces, namespaces and types
	// At some point with binaries could remove this pass
	for statement in definition.declarations.iter_mut() {
		fn get_id_via_decorators(decorators: &Vec<parser::Decorator>) -> Option<TypeId> {
			fn expression_as_number(expression: &Expression) -> f64 {
				if let Expression::NumberLiteral(number, _, _) = expression {
					number.clone().into()
				} else {
					panic!()
				}
			}

			decorators.iter().find_map(|decorator| match decorator.name.as_str() {
				"TypeId" => Some(
					TypeId(expression_as_number(
						&decorator.arguments.iter().flatten().next().unwrap(),
					) as u16)
					.into(),
				),
				_ => None,
			})
		}

		let (parser_type_id, checker_type_id) = match statement {
			TypeDefinitionModuleDeclaration::Interface(Decorated { on: interface, .. }) => {
				hoist_interface_name(interface, &mut environment, checking_data)
			}
			TypeDefinitionModuleDeclaration::Class(class) => {
				todo!();
				// (
				// 	class.type_id,
				// 	// environment.new_type(&class.name, class.type_parameters.is_some(), None),
				// 	environment.new_type(Type::NamedRooted { name class.name.clone())),
				// ),
			}
			TypeDefinitionModuleDeclaration::TypeAlias(type_alias) => {
				todo!();
				// (
				// 	type_alias.type_id,
				// 	environment.new_type(
				// 		// TODO super annoying but have to alias the type later
				// 		Type::AliasTo(TypeId::ERROR_TYPE, type_alias.type_name.name.clone()),
				// 	),
				// ),
			}
			_ => {
				continue;
			}
		};
		checking_data.type_mappings.types_to_types.insert(parser_type_id, checker_type_id);
	}

	// Hoisting of generic type parameters, generic function parameters and type statements now
	// all types exist in system.
	// This allows for: because need to know constraints and matching ids
	// ```
	// interface Map<K, V> {
	//     new(k: K, v: V): Map<K, V>
	// }
	// ```
	// for statement in definition.declarations.iter() {
	// 	match statement {
	// 		TypeDefinitionModuleDeclaration::InterfaceDeclaration(interface) => {
	// 			if let Some(ref type_parameters) = interface.type_parameters {
	// 				let interface_type_identifier = *checking_data
	// 					.type_mappings
	// 					.types_to_types
	// 					.get(&interface.type_id)
	// 					.unwrap();

	// 				let information =
	// 					InterfaceDeclarationMetadata::from_decorators(&interface.decorators);

	// 				// let key = interface_type_identifier.try_into().unwrap();
	// 				// checking_data.memory.unpaired_generic_type_parameters.insert(
	// 				// 	key,
	// 				// 	// TODO optional parameters with default here and constrains here
	// 				// 	match information.generic_type_parameter_ids {
	// 				// 		Some(values) => {
	// 				// 			values.into_iter().map(|id| DependentTypeId(id)).collect()
	// 				// 		}
	// 				// 		None => {
	// 				// 			type_parameters.iter().map(|_tp| DependentTypeId::new()).collect()
	// 				// 		}
	// 				// 	},
	// 				// );
	// 				todo!()
	// 			}
	// 		}
	// 		TypeDefinitionModuleDeclaration::TypeAlias(_type_alias)
	// 		| TypeDefinitionModuleDeclaration::LocalTypeAlias(_type_alias) => {

	// 			// TODO Scan object literal references
	// 			// ast_to_type_declarers.insert(interface.type_declared_id.clone(), declared_type);
	// 		}
	// 		TypeDefinitionModuleDeclaration::ClassDeclaration(class) => {
	// 			if let Some(_type_parameters) = &class.type_parameters {
	// 				todo!()
	// 			}
	// 		}
	// 		_ => {}
	// 	}
	// }

	for declaration in definition.declarations.into_iter() {
		match declaration {
			TypeDefinitionModuleDeclaration::Function(DeclareFunctionDeclaration {
				type_parameters,
				parameters,
				return_type,
				name,
				variable_id,
				position,
				decorators,
			}) => {
				let ty = todo!(); // checking_data.types.new_constant_type(Constant::FunctionReference(pointer));

				let function_type = type_function_reference(
					&type_parameters,
					&parameters,
					return_type.as_ref(),
					&mut environment,
					checking_data,
					position.clone(),
					// TODO think is okay
					crate::structures::functions::FunctionNature::Arrow,
				);

				todo!();

				// environment.functions_on_type.insert(ty, function_type);

				// environment
				// 	.declare_variable(
				// 		name.as_str(),
				// 		None,
				// 		ty,
				// 		variable_id,
				// 		DEFINITION_VAR_IS_CONSTANT,
				// 		position,
				// 	)
				// 	.expect("TODO re-declared");
			}
			TypeDefinitionModuleDeclaration::Variable(DeclareVariableDeclaration {
				name,
				type_restriction,
				variable_id,
				decorators,
				position,
			}) => {
				// TODO tidy up
				let variable_ty =
					environment.get_type_handle_errors(&type_restriction, checking_data);

				// // TODO not sure...
				// if let Some(frozen) = environment.is_frozen(variable_ty) {
				// 	environment.frozen.insert(var_type, frozen);
				// }

				environment
					.register_variable(
						&name,
						// TODO
						variable_id,
						position.clone(),
						crate::context::VariableRegisterBehavior::Declare { base: variable_ty },
						&mut checking_data.types,
					)
					.expect("TODO re-declared");
			}
			TypeDefinitionModuleDeclaration::Interface(interface) => {
				type_interface_declaration(&interface, &mut environment, checking_data)
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
				let ParserTypeDeclaration { name, type_parameters, .. } = &type_name;

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
					todo!("Modify alias")
					// let ty = environment.get_type_handle_errors(&type_expression, checking_data);
					// environment.new_type(ty);
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
				//             &mut crate::environment::GetTypeFromReferenceSettings::Default,
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
	environment
}

use std::path::Path;

use crate::{context::Root, CheckingData};

use parser::{ASTNode, SourceId, TypeDefinitionModule};

pub fn definition_file_to_buffer<T: crate::FSResolver>(
	handler: &T,
	cwd: &Path,
	file: &Path,
) -> Result<Vec<u8>, String> {
	use crate::synthesis::definitions::type_definition_file;

	// 	ModuleData::new_with_custom_module_resolvers(Default::default(), , cwd.to_owned());

	let mut checking_data = CheckingData::new(Default::default(), handler);

	let definition_file = if let Some((source, cursors)) = handler(file) {
		let now = std::time::Instant::now();
		let from_string =
			TypeDefinitionModule::from_string(source, Default::default(), SourceId::NULL, cursors);

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
	let ctx = Root::deserialize(file, SourceId::NULL).unwrap();
	println!("From binary {:?}", now.elapsed());
	ctx
}
