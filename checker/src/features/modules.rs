use std::path::{Path, PathBuf};

use super::variables::{VariableMutability, VariableOrImport};
use crate::{
	context::{
		information::{get_value_of_constant_import_variable, LocalInformation},
		VariableRegisterArguments,
	},
	parse_source, CheckingData, Environment, Instance, Map, Scope, TypeId, TypeMappings,
	VariableId,
};

use simple_json_parser::{JSONKey, RootJSONValue};
use source_map::{FileSystem, Span};

/// For imports and exports
#[derive(Debug)]
pub struct NamePair<'a> {
	pub value: &'a str,
	pub r#as: &'a str,
	pub position: Span,
}

pub enum ImportKind<'a, T: Iterator<Item = NamePair<'a>>> {
	Parts(T),
	All {
		under: &'a str,
		position: Span,
	},
	/// From `export * from ...`
	Everything,
}

/// A module once it has been type checked (note could have type errors that have been raised) and all information has been resolved about it
pub struct SynthesisedModule<M> {
	pub content: M,
	pub exported: Exported,
	/// TODO ...
	pub info: LocalInformation,
	pub mappings: TypeMappings,
}

impl<M> SynthesisedModule<M> {
	pub fn get_instance_at_position(&self, pos: u32) -> Option<&Instance> {
		self.mappings.expressions_to_instances.get(pos)
	}

	/// Returns the instance + the span of the matched
	pub fn get_instance_at_position_with_span(
		&self,
		pos: u32,
	) -> Option<(&Instance, std::ops::Range<u32>)> {
		self.mappings.expressions_to_instances.get_with_range(pos)
	}
}

/// TODO tidy
#[derive(Clone, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub struct Exported {
	pub default: Option<TypeId>,
	/// Mutability purely for the mutation thingy
	pub named: Map<String, (VariableId, VariableMutability)>,
	pub named_types: Map<String, TypeId>,
}

pub type ExportedVariable = (VariableId, VariableMutability);

impl Exported {
	#[must_use]
	pub fn get_export(
		&self,
		want: &str,
		type_only: bool,
	) -> (Option<ExportedVariable>, Option<TypeId>) {
		let variable = if type_only {
			None
		} else {
			self.named.get(want).map(|(name, mutability)| (*name, *mutability))
		};

		let r#type = self.named_types.get(want).copied();

		(variable, r#type)
	}

	pub fn keys(&self) -> impl Iterator<Item = &str> {
		self.named.keys().chain(self.named_types.keys()).map(AsRef::as_ref)
	}
}

/// After a syntax error
pub struct InvalidModule;

/// The result of synthesising a module
pub type FinalModule<M> = Result<SynthesisedModule<M>, InvalidModule>;

#[derive(Debug, Clone)]
pub struct CouldNotOpenFile(pub PathBuf);

/// Given the current environment for a module:
/// - Parse the `partial_import_path` to a resolved module
/// - Type check the module (if not already done)
/// - Import names by `kind` (and another information)
#[allow(clippy::too_many_arguments)]
pub fn import_items<
	'b,
	P: Iterator<Item = NamePair<'b>>,
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
>(
	environment: &mut Environment,
	partial_import_path: &str,
	import_position: Span,
	default_import: Option<(&str, Span)>,
	kind: ImportKind<'b, P>,
	checking_data: &mut CheckingData<T, A>,
	also_export: bool,
	type_only: bool,
) {
	if !matches!(environment.context_type.scope, crate::Scope::Module { .. }) {
		checking_data.diagnostics_container.add_error(
			crate::diagnostics::TypeCheckError::NotTopLevelImport(
				import_position.with_source(environment.get_source()),
			),
		);
		return;
	}

	let exports = import_file(partial_import_path, environment, checking_data);

	if let Err(ref err) = exports {
		checking_data.diagnostics_container.add_error(
			crate::diagnostics::TypeCheckError::CannotOpenFile {
				file: err.clone(),
				import_position: Some(import_position.with_source(environment.get_source())),
				possibles: checking_data
					.modules
					.files
					.get_paths()
					.keys()
					.filter_map(|path| path.to_str())
					.collect(),
				partial_import_path,
			},
		);
	}

	let current_source = environment.get_source();

	if let Some((default_name, position)) = default_import {
		if let Ok(Ok(ref exports)) = exports {
			if let Some(item) = &exports.default {
				let id = crate::VariableId(current_source, position.start);
				let v = VariableOrImport::ConstantImport {
					to: None,
					import_specified_at: position.with_source(current_source),
				};
				environment.info.variable_current_value.insert(id, *item);
				let existing = environment.variables.insert(default_name.to_owned(), v);
				if let Some(existing) = existing {
					checking_data.diagnostics_container.add_error(
						crate::diagnostics::TypeCheckError::DuplicateImportName {
							import_position: position.with_source(current_source),
							existing_position: match existing {
								VariableOrImport::Variable { declared_at, .. } => declared_at,
								VariableOrImport::MutableImport { import_specified_at, .. }
								| VariableOrImport::ConstantImport {
									import_specified_at, ..
								} => import_specified_at,
							},
						},
					);
				}
			} else {
				checking_data.diagnostics_container.add_error(
					crate::diagnostics::TypeCheckError::NoDefaultExport {
						position: position.with_source(current_source),
						partial_import_path,
					},
				);
			}
		} else {
			environment.register_variable_handle_error(
				default_name,
				VariableRegisterArguments {
					constant: true,
					initial_value: Some(TypeId::ERROR_TYPE),
					space: None,
					allow_reregistration: false,
				},
				position.with_source(current_source),
				&mut checking_data.diagnostics_container,
				&mut checking_data.local_type_mappings,
				checking_data.options.record_all_assignments_and_reads,
			);
		}
	}

	match kind {
		ImportKind::Parts(parts) => {
			for part in parts {
				// Here in nested because want to type variables as error otherwise
				if let Ok(Ok(ref exports)) = exports {
					crate::utilities::notify!("{:?}", part);
					let (exported_variable, exported_type) =
						exports.get_export(part.value, type_only);

					if exported_variable.is_none() && exported_type.is_none() {
						let possibles = {
							let mut possibles =
								crate::get_closest(exports.keys(), part.value).unwrap_or(vec![]);
							possibles.sort_unstable();
							possibles
						};
						let position = part.position.with_source(current_source);
						checking_data.diagnostics_container.add_error(
							crate::diagnostics::TypeCheckError::FieldNotExported {
								file: partial_import_path,
								position,
								importing: part.value,
								possibles,
							},
						);

						// Register error
						environment.register_variable_handle_error(
							part.r#as,
							VariableRegisterArguments {
								constant: true,
								space: None,
								initial_value: Some(TypeId::ERROR_TYPE),
								allow_reregistration: false,
							},
							position,
							&mut checking_data.diagnostics_container,
							&mut checking_data.local_type_mappings,
							checking_data.options.record_all_assignments_and_reads,
						);
					}

					// add variable to scope
					if let Some((variable, mutability)) = exported_variable {
						let constant = match mutability {
							VariableMutability::Constant => {
								let k = crate::VariableId(current_source, part.position.start);
								let v =
									get_value_of_constant_import_variable(variable, environment);
								environment.info.variable_current_value.insert(k, v);
								true
							}
							VariableMutability::Mutable { reassignment_constraint: _ } => false,
						};

						let v = VariableOrImport::MutableImport {
							of: variable,
							constant,
							import_specified_at: part
								.position
								.with_source(environment.get_source()),
						};
						crate::utilities::notify!("{:?}", part.r#as.to_owned());
						let existing = environment.variables.insert(part.r#as.to_owned(), v);
						if let Some(existing) = existing {
							checking_data.diagnostics_container.add_error(
								crate::diagnostics::TypeCheckError::DuplicateImportName {
									import_position: part
										.position
										.with_source(environment.get_source()),
									existing_position: match existing {
										VariableOrImport::Variable { declared_at, .. } => {
											declared_at
										}
										VariableOrImport::MutableImport {
											import_specified_at,
											..
										}
										| VariableOrImport::ConstantImport {
											import_specified_at,
											..
										} => import_specified_at,
									},
								},
							);
						}
						if also_export {
							if let Scope::Module { ref mut exported, .. } =
								environment.context_type.scope
							{
								exported.named.insert(part.r#as.to_owned(), (variable, mutability));
							}
						}
					}

					// add type to scope
					if let Some(ty) = exported_type {
						let existing = environment.named_types.insert(part.r#as.to_owned(), ty);
						assert!(existing.is_none(), "TODO exception");
					}
				} else {
					// This happens if imported is an invalid file (syntax issue, doesn't exist etc)
					// Don't need to emit an error here
					let declared_at = part.position.with_source(environment.get_source());
					environment.register_variable_handle_error(
						part.r#as,
						VariableRegisterArguments {
							constant: true,
							space: None,
							initial_value: Some(TypeId::ERROR_TYPE),
							allow_reregistration: false,
						},
						declared_at,
						&mut checking_data.diagnostics_container,
						&mut checking_data.local_type_mappings,
						checking_data.options.record_all_assignments_and_reads,
					);
				}
			}
		}
		ImportKind::All { under, position } => {
			let value = if let Ok(Ok(ref exports)) = exports {
				let import_object = crate::Type::SpecialObject(
					crate::features::objects::SpecialObject::Import(exports.clone()),
				);
				checking_data.types.register_type(import_object)
			} else {
				crate::utilities::notify!("TODO :?");
				TypeId::UNIMPLEMENTED_ERROR_TYPE
			};
			environment.register_variable_handle_error(
				under,
				VariableRegisterArguments {
					constant: true,
					space: None,
					initial_value: Some(value),
					allow_reregistration: false,
				},
				position.with_source(current_source),
				&mut checking_data.diagnostics_container,
				&mut checking_data.local_type_mappings,
				checking_data.options.record_all_assignments_and_reads,
			);
		}
		ImportKind::Everything => {
			if let Ok(Ok(ref exports)) = exports {
				for (name, (variable, mutability)) in exports.named.iter() {
					// TODO are variables put into scope?
					if let Scope::Module { ref mut exported, .. } = environment.context_type.scope {
						exported.named.insert(name.clone(), (*variable, *mutability));
					}
				}
			} else {
				// TODO ??
			}
		}
	}
}

/// TODO better name.
pub fn import_file<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	to_import: &str,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
) -> Result<Result<Exported, InvalidModule>, CouldNotOpenFile> {
	fn get_module<'a, T: crate::ReadFromFS, A: crate::ASTImplementation>(
		full_importer: &Path,
		_definition_file: Option<&Path>,
		environment: &mut Environment,
		checking_data: &'a mut CheckingData<T, A>,
	) -> Option<Result<&'a SynthesisedModule<A::OwnedModule>, A::ParseError>> {
		let existing = checking_data.modules.files.get_source_at_path(full_importer);
		if let Some(existing) = existing {
			Some(Ok(checking_data
				.modules
				.synthesised_modules
				.get(&existing)
				.expect("existing file, but not synthesised")))
		} else {
			let content = checking_data.modules.file_reader.read_file(full_importer);
			if let Some(content) = content {
				let content = String::from_utf8(content).expect("invalid entry point encoding");
				let source = checking_data
					.modules
					.files
					.new_source_id(full_importer.to_path_buf(), content.clone());
				let module = parse_source(full_importer, source, content, checking_data);

				match module {
					Ok(module) => {
						let root = &environment.get_root();
						let new_module_context =
							root.new_module_context(source, module, checking_data);
						Some(Ok(new_module_context))
					}
					Err(err) => Some(Err(err)),
				}
			} else {
				None
			}
		}
	}

	fn get_package_from_node_modules<T: crate::ReadFromFS>(
		name: &str,
		cwd: &Path,
		fs_reader: &T,
	) -> Result<(PathBuf, Option<PathBuf>), ()> {
		// TODO support non `node_modules` or is that over ?
		let package_directory = cwd.join("node_modules");
		let package_root = package_directory.join(name);
		let package_json_path = package_root.join("package.json");
		// TODO error
		let package_json = fs_reader.read_file(&PathBuf::from(&package_json_path)).ok_or(())?;
		let package_json = String::from_utf8(package_json).unwrap();

		let (mut file_path, mut definition_file_path) = (None::<PathBuf>, None::<PathBuf>);

		// TODO JSON parse error
		let _res = simple_json_parser::parse_with_exit_signal(
			&package_json,
			|path, value| {
				// if let Some(ref export) = export {
				// 	todo!()
				// } else {
				if let [JSONKey::Slice("main")] = path {
					if let RootJSONValue::String(s) = value {
						file_path = Some(s.to_owned().into());
					} else {
						// invalid type
					}
				} else if let [JSONKey::Slice("types")] = path {
					if let RootJSONValue::String(s) = value {
						definition_file_path = Some(s.to_owned().into());
					} else {
						// invalid type
					}
				}
				// }
				file_path.is_some() && definition_file_path.is_some()
			},
			false,
			true,
		);

		file_path.ok_or(()).map(|entry| {
			(package_root.join(entry), definition_file_path.map(|dfp| package_root.join(dfp)))
		})
	}

	let result = if to_import.starts_with('.') {
		let from_path = checking_data.modules.files.get_file_path(environment.get_source());
		let from = PathBuf::from(to_import);
		let mut full_importer =
			path_absolutize::Absolutize::absolutize_from(&from, from_path.parent().unwrap())
				.unwrap()
				.to_path_buf();

		if full_importer.extension().is_some() {
			get_module(&full_importer, None, environment, checking_data)
		} else {
			let mut result = None;
			for ext in ["ts", "tsx", "js"] {
				full_importer.set_extension(ext);
				// TODO change parse options based on extension
				result = get_module(&full_importer, None, environment, checking_data);
				if result.is_some() {
					break;
				}
			}
			result
		}
	} else {
		crate::utilities::notify!("Here {}", to_import);
		let result = get_package_from_node_modules(
			to_import,
			&checking_data.modules.current_working_directory,
			checking_data.modules.file_reader,
		);
		if let Ok((path, definition_file)) = result {
			crate::utilities::notify!("Reading path from package {}", path.display());
			get_module(&path, definition_file.as_deref(), environment, checking_data)
		} else {
			None
		}
	};

	match result {
		Some(Ok(synthesised_module)) => {
			environment.info.extend_ref(&synthesised_module.info);
			Ok(Ok(synthesised_module.exported.clone()))
		}
		Some(Err(error)) => {
			checking_data.diagnostics_container.add_error(error);
			Ok(Err(InvalidModule))
		}
		None => Err(CouldNotOpenFile(PathBuf::from(to_import.to_owned()))),
	}
}

// pub fn get_possibles_message_for_imports(possibles: &[&str]) -> Vec<String> {
// 	possibles
// 		.iter()
// 		.filter(|file| !file.ends_with(".d.ts"))
// 		.filter_map(|file| file.strip_suffix(".ts"))
// 		.map(|file| {
// 			if file.starts_with("./") || file.starts_with("../") {
// 				file.to_string()
// 			} else {
// 				"./".to_string() + file
// 			}
// 		})
// 		.collect::<Vec<String>>()
// }
