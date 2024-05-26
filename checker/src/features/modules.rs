use std::path::{Path, PathBuf};

use super::variables::{VariableMutability, VariableOrImport};
use crate::{
	context::{
		information::{get_value_of_constant_import_variable, LocalInformation},
		VariableRegisterArguments,
	},
	parse_source, CheckingData, Environment, Instance, Scope, Type, TypeId, TypeMappings,
	VariableId,
};

use simple_json_parser::{JSONKey, RootJSONValue};
use source_map::{FileSystem, Span};

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
	pub named: Vec<(String, (VariableId, VariableMutability))>,
	pub named_types: Vec<(String, TypeId)>,
}

pub type ExportedVariable = (VariableId, VariableMutability);

pub enum TypeOrVariable {
	ExportedVariable(),
	Type(TypeId),
}

impl Exported {
	pub(crate) fn get_export(
		&self,
		want: &str,
		type_only: bool,
	) -> (Option<ExportedVariable>, Option<TypeId>) {
		let variable = if type_only {
			None
		} else {
			self.named
				.iter()
				.find_map(|(export, value)| (export == want).then_some((value.0, value.1)))
		};

		let r#type =
			self.named_types.iter().find_map(|(export, value)| (export == want).then_some(*value));

		(variable, r#type)
	}
}

/// After a syntax error
pub struct InvalidModule;

pub type FinalModule<M> = Result<SynthesisedModule<M>, InvalidModule>;

#[derive(Debug, Clone)]
pub struct CouldNotOpenFile(pub PathBuf);

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
				position: import_position.with_source(environment.get_source()),
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
				if let Some(_existing) = existing {
					todo!("diagnostic")
				}
			} else {
				todo!("emit 'no default export' diagnostic")
			}
		} else {
			environment.register_variable_handle_error(
				default_name,
				VariableRegisterArguments {
					constant: true,
					initial_value: Some(TypeId::ERROR_TYPE),
					space: None,
				},
				position.with_source(current_source),
				&mut checking_data.diagnostics_container,
			);
		}
	}

	match kind {
		ImportKind::Parts(parts) => {
			for part in parts {
				if let Ok(Ok(ref exports)) = exports {
					let (exported_variable, exported_type) =
						exports.get_export(part.value, type_only);

					if exported_variable.is_none() && exported_type.is_none() {
						let position = part.position.with_source(current_source);
						checking_data.diagnostics_container.add_error(
							crate::diagnostics::TypeCheckError::FieldNotExported {
								file: partial_import_path,
								position,
								importing: part.value,
							},
						);

						environment.register_variable_handle_error(
							part.r#as,
							VariableRegisterArguments {
								constant: true,
								space: None,
								initial_value: Some(TypeId::ERROR_TYPE),
							},
							position,
							&mut checking_data.diagnostics_container,
						);
					}
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
						let existing = environment.variables.insert(part.r#as.to_owned(), v);
						if let Some(_existing) = existing {
							todo!("diagnostic")
						}
						if also_export {
							if let Scope::Module { ref mut exported, .. } =
								environment.context_type.scope
							{
								exported.named.push((part.r#as.to_owned(), (variable, mutability)));
							}
						}
					}

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
						},
						declared_at,
						&mut checking_data.diagnostics_container,
					);
				}
			}
		}
		ImportKind::All { under, position } => {
			if let Ok(Ok(ref exports)) = exports {
				let value = checking_data.types.register_type(Type::SpecialObject(
					crate::features::objects::SpecialObjects::Import(exports.clone()),
				));

				environment.register_variable_handle_error(
					under,
					VariableRegisterArguments {
						constant: true,
						space: None,
						initial_value: Some(value),
					},
					position.with_source(current_source),
					&mut checking_data.diagnostics_container,
				);
			} else {
				crate::utilities::notify!("TODO :?");
				environment.register_variable_handle_error(
					under,
					VariableRegisterArguments {
						constant: true,
						space: None,
						initial_value: Some(TypeId::ERROR_TYPE),
					},
					position.with_source(current_source),
					&mut checking_data.diagnostics_container,
				);
			}
		}
		ImportKind::Everything => {
			if let Ok(Ok(ref exports)) = exports {
				for (name, (variable, mutability)) in &exports.named {
					// TODO are variables put into scope?
					if let Scope::Module { ref mut exported, .. } = environment.context_type.scope {
						exported.named.push((name.clone(), (*variable, *mutability)));
					}
				}
			} else {
				// TODO ??
			}
		}
	}
}

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
		let _res = simple_json_parser::parse_with_exit_signal(&package_json, |path, value| {
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
		});

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
