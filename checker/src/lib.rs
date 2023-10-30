#![doc = include_str!("../README.md")]
#![allow(
	unreachable_code,
	unused_variables,
	unused_imports,
	unused_mut,
	dead_code,
	irrefutable_let_patterns,
	deprecated
)]

mod behavior;
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

// TODO temp pub
#[cfg(feature = "ezno-parser")]
pub mod synthesis;

use context::{environment, Names};
use diagnostics::{TypeCheckError, TypeCheckWarning};
pub(crate) use serialization::BinarySerializable;

use indexmap::IndexMap;
use source_map::{FileSystem, MapFileStore, SpanWithSource, WithPathMap};
use std::{
	collections::{HashMap, HashSet},
	path::{Path, PathBuf},
};
use structures::{functions::AutoConstructorId, modules::Exported};

use types::{
	subtyping::{check_satisfies, type_is_subtype, BasicEquality, SubTypeResult},
	TypeStore,
};

pub use behavior::{
	assignments::{
		Assignable, AssignmentKind, AssignmentReturnStatus, IncrementOrDecrement, Reference,
		SynthesisableExpression,
	},
	functions::{
		MethodKind, RegisterAsType, RegisterOnExisting, RegisterOnExistingObject,
		SynthesisableFunction,
	},
	variables::check_variable_initialization,
};
pub use context::{GeneralContext, RootContext};
pub use diagnostics::{Diagnostic, DiagnosticKind, DiagnosticsContainer};
pub use options::TypeCheckOptions;
pub use structures::{
	functions::{FunctionPointer, InternalFunctionId},
	jsx::*,
	modules::SynthesisedModule,
	variables::VariableOrImport,
};
pub use types::{calling::call_type_handle_errors, poly_types::GenericTypeParameters, subtyping};

pub use type_mappings::*;
pub use types::{properties::Property, Constant, Type, TypeId};

pub use context::{facts::Facts, Environment, Scope};
pub(crate) use structures::modules::ModuleFromPathError;

pub trait ReadFromFS: Fn(&std::path::Path) -> Option<String> {}

impl<T> ReadFromFS for T where T: Fn(&std::path::Path) -> Option<String> {}

// TODO should this be pub
pub use source_map::{SourceId, Span};

/// Contains all the modules and mappings for import statements
///
/// TODO could files and synthesized_modules be merged? (with a change to )
pub struct ModuleData<'a, FileReader, ModuleAST: ASTImplementation> {
	pub(crate) file_reader: &'a FileReader,
	pub(crate) current_working_directory: PathBuf,
	// TODO
	pub(crate) entry_point: SourceId,

	pub(crate) files: MapFileStore<WithPathMap>,
	pub(crate) currently_checking_modules: HashSet<PathBuf>,

	/// Includes exported variables and facts
	pub(crate) synthesised_modules: HashMap<SourceId, SynthesisedModule<ModuleAST::Module>>,

	pub(crate) parsing_options: ModuleAST::ParseOptions,
	// pub(crate) custom_module_resolvers: HashMap<String, Box<dyn CustomModuleResolver>>,
}

pub trait ASTImplementation: Sized {
	type ParseOptions;
	type ParseError: Into<Diagnostic>;

	type Module;
	type DefinitionFile;

	type TypeAnnotation;
	type TypeParameter;

	fn module_from_string(
		source_id: SourceId,
		string: String,
		options: &Self::ParseOptions,
	) -> Result<Self::Module, Self::ParseError>;

	fn definition_module_from_string(
		source_id: SourceId,
		string: String,
	) -> Result<Self::DefinitionFile, Self::ParseError>;

	fn synthesize_module<T: crate::ReadFromFS>(
		module: &Self::Module,
		source_id: SourceId,
		root: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	);

	fn type_definition_file<T: crate::ReadFromFS>(
		file: Self::DefinitionFile,
		root: &RootContext,
		checking_data: &mut CheckingData<T, Self>,
	) -> (Names, Facts);

	fn type_parameter_name(parameter: &Self::TypeParameter) -> &str;
}

impl<'a, T: crate::ReadFromFS, ModuleAST: ASTImplementation> ModuleData<'a, T, ModuleAST> {
	pub(crate) fn new_with_custom_module_resolvers(
		// custom_module_resolvers: HashMap<String, Box<dyn CustomModuleResolver>>,
		mut file_resolver: &'a T,
		current_working_directory: PathBuf,
		entry_point: PathBuf,
		files: Option<MapFileStore<WithPathMap>>,
		parsing_options: ModuleAST::ParseOptions,
	) -> Self {
		let mut files = files.unwrap_or_default();
		// TODO get from files sometimes, abstract below
		let content = (file_resolver)(&entry_point).expect("TODO ahhh");
		let entry_point = files.new_source_id(entry_point, content);
		Self {
			files,
			entry_point,
			synthesised_modules: Default::default(),
			currently_checking_modules: Default::default(),
			// custom_module_resolvers,
			file_reader: file_resolver,
			current_working_directory,
			parsing_options,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, binary_serialize_derive::BinarySerializable)]
pub struct FunctionId(pub SourceId, pub u32);

pub enum TruthyFalsy {
	Decidable(bool),
	/// Poly types
	Unknown,
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
	// pub(crate) parse_settings: parser::ParseSettings,

	// pub(crate) events: EventsStore,
	pub types: TypeStore,

	/// Do not repeat emitting unimplemented parts
	unimplemented_items: HashSet<&'static str>,
}

pub enum FromFileError<M: ASTImplementation> {
	SyntaxError(M::ParseError),
	CouldNotOpen(PathBuf),
}

impl<'a, T: crate::ReadFromFS, M: ASTImplementation> CheckingData<'a, T, M> {
	// TODO improve on this function
	pub fn new(
		settings: TypeCheckOptions,
		resolver: &'a T,
		entry_point: PathBuf,
		parse_options: M::ParseOptions,
		existing_files: Option<MapFileStore<WithPathMap>>,
	) -> Self {
		// let custom_file_resolvers = HashMap::default();
		let cwd = Default::default();
		let modules = ModuleData::new_with_custom_module_resolvers(
			resolver,
			cwd,
			entry_point,
			existing_files,
			parse_options,
		);

		Self {
			options: settings,
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
	) -> Result<Exported, FromFileError<M>> {
		if importing_path.starts_with('.') {
			let from_path = self.modules.files.get_file_path(from);
			let from = PathBuf::from(importing_path);
			let mut full_importer =
				path_absolutize::Absolutize::absolutize_from(&from, from_path.parent().unwrap())
					.unwrap()
					.to_path_buf();

			fn get_module<'a, T: crate::ReadFromFS, M: ASTImplementation>(
				full_importer: PathBuf,
				environment: &mut Environment,
				checking_data: &'a mut CheckingData<T, M>,
			) -> Option<Result<&'a SynthesisedModule<M::Module>, M::ParseError>> {
				let existing = checking_data.modules.files.get_source_at_path(&full_importer);
				if let Some(existing) = existing {
					Some(Ok(checking_data.modules.synthesised_modules.get(&existing).unwrap()))
				} else {
					let content = (checking_data.modules.file_reader)(full_importer.as_ref());
					if let Some(content) = content {
						let source = checking_data
							.modules
							.files
							.new_source_id(full_importer.to_path_buf(), content.clone());

						match M::module_from_string(
							source,
							content,
							&checking_data.modules.parsing_options,
						) {
							Ok(module) => Some(Ok(environment.get_root().new_module_context(
								source,
								module,
								checking_data,
							))),
							Err(err) => Some(Err(err)),
						}
					} else {
						None
					}
				}
			}

			let synthesised_module = if full_importer.extension().is_some() {
				let result = get_module(full_importer.clone(), environment, self);
				match result {
					Some(Ok(result)) => result,
					Some(Err(err)) => return Err(FromFileError::SyntaxError(err)),
					None => return Err(FromFileError::CouldNotOpen(full_importer)),
				}
			} else {
				let mut result = None;
				for ext in ["ts", "tsx", "js"] {
					full_importer.set_extension(ext);
					// TODO change settings based on extension
					result = get_module(full_importer.clone(), environment, self);
					if result.is_some() {
						break;
					}
				}
				if let Some(result) = result {
					result.map_err(FromFileError::SyntaxError)?
				} else {
					// TODO some message about extensions?
					return Err(FromFileError::CouldNotOpen(full_importer));
				}
			};

			environment.facts.extend_ref(&synthesised_module.facts);
			Ok(synthesised_module.exported.clone())
		} else {
			todo!()
		}
	}

	/// TODO temp, needs better place
	pub fn raise_decidable_result_error(&mut self, span: SpanWithSource, value: bool) {
		self.diagnostics_container.add_error(TypeCheckWarning::DeadBranch {
			expression_span: span,
			expression_value: value,
		})
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
pub struct PostCheckData<M: ASTImplementation> {
	pub type_mappings: crate::TypeMappings,
	pub types: crate::types::TypeStore,
	pub module_contents: MapFileStore<WithPathMap>,
	pub modules: HashMap<SourceId, SynthesisedModule<M::Module>>,
	pub entry_source: SourceId,
}

pub fn check_project<T: crate::ReadFromFS, M: ASTImplementation>(
	entry_point: PathBuf,
	type_definition_files: HashSet<PathBuf>,
	resolver: T,
	options: Option<TypeCheckOptions>,
	parse_options: M::ParseOptions,
) -> (crate::DiagnosticsContainer, Result<PostCheckData<M>, MapFileStore<WithPathMap>>) {
	let mut checking_data = CheckingData::<T, M>::new(
		options.unwrap_or_default(),
		&resolver,
		entry_point,
		parse_options,
		None,
	);

	let entry_point_source_id = checking_data.modules.entry_point;
	let file = checking_data.modules.files.get_file_content(entry_point_source_id);

	let module = match M::module_from_string(
		entry_point_source_id,
		file,
		&checking_data.modules.parsing_options,
	) {
		Ok(module) => module,
		Err(err) => {
			checking_data.diagnostics_container.add_error(err);
			return (checking_data.diagnostics_container, Err(checking_data.modules.files));
		}
	};

	let mut root = crate::context::RootContext::new_with_primitive_references();

	add_definition_files_to_root(type_definition_files, &mut root, &mut checking_data);

	if checking_data.diagnostics_container.has_error() {
		return (checking_data.diagnostics_container, Err(checking_data.modules.files));
	}

	root.new_module_context(entry_point_source_id, module, &mut checking_data);

	let CheckingData {
		diagnostics_container,
		type_mappings,
		modules,
		options: settings,
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
			entry_source: modules.entry_point,
		});
		(diagnostics_container, post_check_data)
	}
}

pub(crate) fn add_definition_files_to_root<T: crate::ReadFromFS, M: crate::ASTImplementation>(
	type_definition_files: HashSet<PathBuf>,
	root: &mut RootContext,
	checking_data: &mut CheckingData<T, M>,
) {
	for path in type_definition_files {
		let (source_id, content) = match checking_data.modules.get_file(&path) {
			Some(result) => result,
			None => {
				checking_data.diagnostics_container.add_error(Diagnostic::Global {
					reason: format!("could not find {}", path.display()),
					kind: crate::DiagnosticKind::Error,
				});
				continue;
			}
		};

		// TODO U::new_tdm_from_string

		let result = M::definition_module_from_string(source_id, content);

		match result {
			Ok(tdm) => {
				let (names, facts) = M::type_definition_file(tdm, root, checking_data);
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

pub trait SynthesisableConditional<M: ASTImplementation> {
	/// For conditional expressions (`a ? b : c`) as they return a type.
	/// **Not for return in conditional if blocks**
	type ExpressionResult;

	fn synthesise_condition<T: crate::ReadFromFS>(
		self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	) -> Self::ExpressionResult;

	fn conditional_expression_result(
		condition: TypeId,
		truthy_result: Self::ExpressionResult,
		falsy_result: Self::ExpressionResult,
		types: &mut TypeStore,
	) -> Self::ExpressionResult;

	fn default_result() -> Self::ExpressionResult;
}

impl<'a, M: ASTImplementation, T: crate::SynthesisableExpression<M>>
	crate::SynthesisableConditional<M> for &'a T
{
	type ExpressionResult = TypeId;

	fn synthesise_condition<U: crate::ReadFromFS>(
		self,
		environment: &mut crate::Environment,
		checking_data: &mut crate::CheckingData<U, M>,
	) -> Self::ExpressionResult {
		self.synthesise_expression(environment, checking_data)
	}

	fn conditional_expression_result(
		condition: TypeId,
		truthy_result: Self::ExpressionResult,
		else_result: Self::ExpressionResult,
		types: &mut TypeStore,
	) -> Self::ExpressionResult {
		types.new_conditional_type(condition, truthy_result, else_result)
	}

	fn default_result() -> Self::ExpressionResult {
		unreachable!("If was reachable it should be TypeID::Undefined")
	}
}
