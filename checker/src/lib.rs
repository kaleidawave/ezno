#![doc = include_str!("../README.md")]
#![allow(
	unreachable_code,
	unused_variables,
	unused_imports,
	unused_mut,
	dead_code,
	irrefutable_let_patterns,
	deprecated,
	clippy::new_without_default,
	clippy::too_many_lines,
	clippy::result_unit_err
)]

pub mod behavior;
pub mod context;
pub mod diagnostics;
pub mod events;
mod options;
pub mod range_map;
mod serialization;
pub mod structures;
mod type_mappings;
pub mod types;
mod utils;

pub const INTERNAL_DEFINITION_FILE_PATH: &str = "internal.d.ts";
pub const INTERNAL_DEFINITION_FILE: &str = include_str!("../definitions/internal.d.ts");

#[cfg(feature = "ezno-parser")]
pub mod synthesis;

use context::{environment, Names};
use diagnostics::{TypeCheckError, TypeCheckWarning};
pub(crate) use serialization::BinarySerializable;

use behavior::{
	functions::SynthesisableFunction,
	modules::{Exported, InvalidModule, SynthesisedModule},
};
use indexmap::IndexMap;
use source_map::{FileSystem, MapFileStore, SpanWithSource, WithPathMap};
use std::{
	collections::{HashMap, HashSet},
	path::{Path, PathBuf},
};

use types::{
	subtyping::{check_satisfies, type_is_subtype, BasicEquality, SubTypeResult},
	TypeStore,
};

pub use context::{GeneralContext, RootContext};
pub use diagnostics::{Diagnostic, DiagnosticKind, DiagnosticsContainer};
pub use options::TypeCheckOptions;
pub use structures::jsx::*;
pub use types::{calling::call_type_handle_errors, poly_types::GenericTypeParameters, subtyping};

pub use type_mappings::*;
pub use types::{properties::PropertyValue, Constant, Type, TypeId};

pub use context::{facts::Facts, Environment, Scope};

pub trait ReadFromFS: Fn(&std::path::Path) -> Option<String> {}

impl<T> ReadFromFS for T where T: Fn(&std::path::Path) -> Option<String> {}

pub use source_map::{self, SourceId, Span};

/// Contains all the modules and mappings for import statements
///
/// TODO could files and `synthesised_modules` be merged? (with a change to the source map crate)
pub struct ModuleData<'a, FileReader, ModuleAST: ASTImplementation> {
	pub(crate) file_reader: &'a FileReader,
	pub(crate) current_working_directory: PathBuf,
	/// Set after started
	pub(crate) entry_point: Option<SourceId>,
	/// Contains the text content of files (for source maps and diagnostics)
	pub(crate) files: MapFileStore<WithPathMap>,
	/// To catch cyclic imports
	pub(crate) currently_checking_modules: HashSet<PathBuf>,
	/// The result of checking. Includes exported variables and facts
	pub(crate) synthesised_modules: HashMap<SourceId, SynthesisedModule<ModuleAST::OwnedModule>>,
}

pub trait ASTImplementation: Sized {
	type ParseOptions;
	type ParseError: Into<Diagnostic>;

	type Module<'a>;
	/// TODO temp item. Some modules can have references
	type OwnedModule;

	type DefinitionFile<'a>;

	type TypeAnnotation<'a>;
	type TypeParameter<'a>;
	type Expression<'a>;

	type ClassMethod<'a>: SynthesisableFunction<Self>;

	/// # Errors
	/// TODO
	fn module_from_string(
		source_id: SourceId,
		string: String,
		options: Self::ParseOptions,
	) -> Result<Self::Module<'static>, Self::ParseError>;

	/// # Errors
	/// TODO
	fn definition_module_from_string(
		source_id: SourceId,
		string: String,
	) -> Result<Self::DefinitionFile<'static>, Self::ParseError>;

	fn synthesise_module<'a, T: crate::ReadFromFS>(
		module: &Self::Module<'a>,
		source_id: SourceId,
		module_context: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	);

	fn synthesise_definition_file<'a, T: crate::ReadFromFS>(
		file: Self::DefinitionFile<'a>,
		root: &RootContext,
		checking_data: &mut CheckingData<T, Self>,
	) -> (Names, Facts);

	/// Expected is used for eagerly setting function parameters
	fn synthesise_expression<'a, T: crate::ReadFromFS>(
		expression: &'a Self::Expression<'a>,
		expected_type: TypeId,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId;

	fn synthesise_type_annotation<'a, T: crate::ReadFromFS>(
		annotation: &'a Self::TypeAnnotation<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId;

	fn expression_position<'a>(expression: &'a Self::Expression<'a>) -> Span;

	fn type_parameter_name<'a>(parameter: &'a Self::TypeParameter<'a>) -> &'a str;

	fn parse_options(is_js: bool) -> Self::ParseOptions;

	fn owned_module_from_module(m: Self::Module<'static>) -> Self::OwnedModule;
}

impl<'a, T: crate::ReadFromFS, ModuleAST: ASTImplementation> ModuleData<'a, T, ModuleAST> {
	pub(crate) fn new(
		mut file_resolver: &'a T,
		current_working_directory: PathBuf,
		files: Option<MapFileStore<WithPathMap>>,
	) -> Self {
		Self {
			files: files.unwrap_or_default(),
			entry_point: None,
			synthesised_modules: Default::default(),
			currently_checking_modules: Default::default(),
			// custom_module_resolvers,
			file_reader: file_resolver,
			current_working_directory,
		}
	}

	pub(crate) fn get_file(&mut self, path: &Path) -> Option<(SourceId, String)> {
		if let Some(source) = self.files.get_source_at_path(path) {
			Some((source, self.files.get_file_content(source)))
		} else {
			let content = (self.file_reader)(path)?;
			let source_id = self.files.new_source_id(path.to_path_buf(), content.clone());
			Some((source_id, content))
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, binary_serialize_derive::BinarySerializable)]
pub struct VariableId(pub SourceId, pub u32);

/// TODO split for annotations based functions
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, binary_serialize_derive::BinarySerializable)]
pub struct FunctionId(pub SourceId, pub u32);

impl FunctionId {
	pub const AUTO_CONSTRUCTOR: Self = FunctionId(SourceId::NULL, 0);
}

pub enum Decidable<T> {
	Known(T),
	/// Points to poly type
	Unknown(TypeId),
}

/// TODO
pub enum PredicateBound<T> {
	Always(T),
}

pub trait GenericTypeParameter {
	fn get_name(&self) -> &str;
}

/// Contains logic for **checking phase** (none of the later steps)
/// All data is global, non local to current scope
/// TODO some of these should be mutex / ref cell
pub struct CheckingData<'a, FSResolver, ModuleAST: ASTImplementation> {
	// pub(crate) type_resolving_visitors: [Box<dyn TypeResolvingExpressionVisitor>],
	// pub(crate) pre_type_visitors: FirstPassVisitors,
	/// Type checking errors
	pub diagnostics_container: DiagnosticsContainer,
	/// TODO temp pub
	pub type_mappings: TypeMappings,
	/// All module information
	pub(crate) modules: ModuleData<'a, FSResolver, ModuleAST>,
	/// Options for checking
	pub(crate) options: TypeCheckOptions,
	// pub(crate) parse_options: parser::ParseOptions,

	// pub(crate) events: EventsStore,
	pub types: TypeStore,

	/// Do not repeat emitting unimplemented parts
	unimplemented_items: HashSet<&'static str>,
}

#[derive(Debug, Clone)]
pub struct CouldNotOpenFile(pub PathBuf);

impl<'a, T: crate::ReadFromFS, A: crate::ASTImplementation> CheckingData<'a, T, A> {
	// TODO improve on this function
	pub fn new(
		options: TypeCheckOptions,
		resolver: &'a T,
		existing_files: Option<MapFileStore<WithPathMap>>,
	) -> Self {
		// let custom_file_resolvers = HashMap::default();
		let cwd = Default::default();
		let modules = ModuleData::new(resolver, cwd, existing_files);

		Self {
			options,
			type_mappings: Default::default(),
			diagnostics_container: Default::default(),
			modules,
			types: Default::default(),
			unimplemented_items: Default::default(),
		}
	}

	pub fn import_file(
		&mut self,
		from: SourceId,
		importing_path: &str,
		environment: &mut Environment,
	) -> Result<Result<Exported, InvalidModule>, CouldNotOpenFile> {
		if importing_path.starts_with('.') {
			let from_path = self.modules.files.get_file_path(from);
			let from = PathBuf::from(importing_path);
			let mut full_importer =
				path_absolutize::Absolutize::absolutize_from(&from, from_path.parent().unwrap())
					.unwrap()
					.to_path_buf();

			fn get_module<'a, T: crate::ReadFromFS, A: crate::ASTImplementation>(
				full_importer: PathBuf,
				environment: &mut Environment,
				checking_data: &'a mut CheckingData<T, A>,
			) -> Option<Result<&'a SynthesisedModule<A::OwnedModule>, A::ParseError>> {
				let existing = checking_data.modules.files.get_source_at_path(&full_importer);
				if let Some(existing) = existing {
					Some(Ok(checking_data
						.modules
						.synthesised_modules
						.get(&existing)
						.expect("existing file, but not synthesised")))
				} else {
					let content = (checking_data.modules.file_reader)(full_importer.as_ref());
					if let Some(content) = content {
						let source = checking_data
							.modules
							.files
							.new_source_id(full_importer.to_path_buf(), content.clone());

						let parse_options = A::parse_options(
							full_importer
								.extension()
								.and_then(|s| s.to_str())
								.map_or(false, |s| s.ends_with("ts")),
						);

						match A::module_from_string(source, content, parse_options) {
							Ok(module) => {
								let new_module_context = environment.get_root().new_module_context(
									source,
									module,
									checking_data,
								);
								Some(Ok(new_module_context))
							}
							Err(err) => Some(Err(err)),
						}
					} else {
						None
					}
				}
			}

			let result = if full_importer.extension().is_some() {
				get_module(full_importer.clone(), environment, self)
			} else {
				let mut result = None;
				for ext in ["ts", "tsx", "js"] {
					full_importer.set_extension(ext);
					// TODO change parse options based on extension
					result = get_module(full_importer.clone(), environment, self);
					if result.is_some() {
						break;
					}
				}
				result
			};

			match result {
				Some(Ok(synthesised_module)) => {
					environment.facts.extend_ref(&synthesised_module.facts);
					Ok(Ok(synthesised_module.exported.clone()))
				}
				Some(Err(error)) => {
					self.diagnostics_container.add_error(error);
					Ok(Err(InvalidModule))
				}
				None => Err(CouldNotOpenFile(full_importer)),
			}
		} else {
			// TODO temp + bad position
			self.raise_unimplemented_error(
				"non relative import (aka npm)",
				SpanWithSource { source: from, start: 0, end: 1 },
			);
			Ok(Err(InvalidModule))
		}
	}

	/// TODO temp, needs better place
	pub fn raise_decidable_result_error(&mut self, span: SpanWithSource, value: bool) {
		self.diagnostics_container.add_error(TypeCheckWarning::DeadBranch {
			expression_span: span,
			expression_value: value,
		});
	}

	/// TODO temp, needs better place
	pub fn raise_unimplemented_error(&mut self, item: &'static str, span: SpanWithSource) {
		if self.unimplemented_items.insert(item) {
			self.diagnostics_container
				.add_warning(TypeCheckWarning::Unimplemented { thing: item, at: span });
		}
	}

	pub fn add_expression_mapping(&mut self, span: SpanWithSource, instance: Instance) {
		self.type_mappings.expressions_to_instances.push(span, instance);
	}

	pub fn check_satisfies(
		&mut self,
		expr_ty: TypeId,
		to_satisfy: TypeId,
		at: SpanWithSource,
		environment: &mut Environment,
	) {
		if !check_satisfies(expr_ty, to_satisfy, &self.types, environment) {
			let expected = diagnostics::TypeStringRepresentation::from_type_id(
				to_satisfy,
				&environment.as_general_context(),
				&self.types,
				false,
			);
			let found = diagnostics::TypeStringRepresentation::from_type_id(
				expr_ty,
				&environment.as_general_context(),
				&self.types,
				false,
			);
			self.diagnostics_container.add_error(TypeCheckError::NotSatisfied {
				at,
				expected,
				found,
			});
		}
	}
}

/// Used for transformers and other things after checking!!!!
pub struct PostCheckData<A: crate::ASTImplementation> {
	pub type_mappings: crate::TypeMappings,
	pub types: crate::types::TypeStore,
	pub module_contents: MapFileStore<WithPathMap>,
	pub modules: HashMap<SourceId, SynthesisedModule<A::OwnedModule>>,
	pub entry_source: SourceId,
}

pub fn check_project<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	entry_point: PathBuf,
	type_definition_files: HashSet<PathBuf>,
	resolver: T,
	options: Option<TypeCheckOptions>,
) -> (crate::DiagnosticsContainer, Result<PostCheckData<A>, MapFileStore<WithPathMap>>) {
	let mut checking_data = CheckingData::<T, A>::new(options.unwrap_or_default(), &resolver, None);

	let mut root = crate::context::RootContext::new_with_primitive_references();

	add_definition_files_to_root(type_definition_files, &mut root, &mut checking_data);

	let entry_content = (checking_data.modules.file_reader)(entry_point.as_ref());
	let module = if let Some(content) = entry_content {
		let source =
			checking_data.modules.files.new_source_id(entry_point.clone(), content.clone());

		checking_data.modules.entry_point = Some(source);

		let is_js = false;
		let parse_options = A::parse_options(is_js);

		let module = A::module_from_string(source, content, parse_options);
		match module {
			Ok(module) => Some(root.new_module_context(source, module, &mut checking_data)),
			Err(err) => {
				checking_data.diagnostics_container.add_error(err);
				return (checking_data.diagnostics_container, Err(checking_data.modules.files));
			}
		}
	} else {
		checking_data.diagnostics_container.add_error(TypeCheckError::CannotOpenFile {
			file: CouldNotOpenFile(entry_point),
			position: None,
		});
		return (checking_data.diagnostics_container, Err(checking_data.modules.files));
	};

	if checking_data.diagnostics_container.has_error() {
		return (checking_data.diagnostics_container, Err(checking_data.modules.files));
	}

	let CheckingData {
		diagnostics_container,
		type_mappings,
		modules,
		options,
		types,
		unimplemented_items,
	} = checking_data;

	if diagnostics_container.has_error() {
		(diagnostics_container, Err(modules.files))
	} else {
		let post_check_data = Ok(PostCheckData {
			type_mappings,
			types,
			module_contents: modules.files,
			modules: modules.synthesised_modules,
			entry_source: modules.entry_point.unwrap(),
		});
		(diagnostics_container, post_check_data)
	}
}

pub(crate) fn add_definition_files_to_root<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	type_definition_files: HashSet<PathBuf>,
	root: &mut RootContext,
	checking_data: &mut CheckingData<T, A>,
) {
	for path in type_definition_files {
		let Some((source_id, content)) = checking_data.modules.get_file(&path) else {
			checking_data.diagnostics_container.add_error(Diagnostic::Global {
				reason: format!("could not find {}", path.display()),
				kind: crate::DiagnosticKind::Error,
			});
			continue;
		};

		// TODO U::new_tdm_from_string

		let result = A::definition_module_from_string(source_id, content);

		match result {
			Ok(tdm) => {
				let (names, facts) = A::synthesise_definition_file(tdm, root, checking_data);
				root.variables.extend(names.variables);
				root.named_types.extend(names.named_types);
				root.variable_names.extend(names.variable_names);

				root.facts.extend(facts, None);
			}
			Err(err) => {
				checking_data.diagnostics_container.add_error(err);
				continue;
			}
		}
	}
}

pub trait TypeCombinable {
	fn combine(
		condition: TypeId,
		truthy_result: Self,
		else_result: Self,
		types: &mut TypeStore,
	) -> Self;

	fn default() -> Self;
}

// For if-else branches
impl TypeCombinable for () {
	fn combine(
		_condition: TypeId,
		_truthy_result: Self,
		_else_result: Self,
		_types: &mut TypeStore,
	) -> Self {
	}

	fn default() -> Self {}
}

// For ternary conditional operators
impl TypeCombinable for TypeId {
	fn combine(
		condition: TypeId,
		truthy_result: Self,
		else_result: Self,
		types: &mut TypeStore,
	) -> Self {
		types.new_conditional_type(condition, truthy_result, else_result)
	}

	fn default() -> Self {
		TypeId::UNDEFINED_TYPE
	}
}
