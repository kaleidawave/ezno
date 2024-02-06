#![doc = include_str!("../README.md")]
#![allow(deprecated, clippy::new_without_default, clippy::too_many_lines, clippy::result_unit_err)]

pub mod context;
pub mod diagnostics;
pub mod events;
pub mod features;
mod options;
pub mod range_map;
mod serialization;
mod type_mappings;
pub mod types;
mod utils;

pub const INTERNAL_DEFINITION_FILE_PATH: &str = "internal.ts.d.bin";
pub const INTERNAL_DEFINITION_FILE: &[u8] = include_bytes!("../definitions/internal.ts.d.bin");

#[cfg(feature = "ezno-parser")]
pub mod synthesis;

use context::Names;
pub use map_vec::Map as SmallMap;

use diagnostics::{TypeCheckError, TypeCheckWarning};
pub(crate) use serialization::BinarySerializable;

use features::{
	functions::SynthesisableFunction,
	modules::{Exported, InvalidModule, SynthesisedModule},
};

use source_map::{FileSystem, MapFileStore, Nullable, SpanWithSource, WithPathMap};
use std::{
	collections::{HashMap, HashSet},
	path::{Path, PathBuf},
};

use types::{printing::print_type, TypeStore};

pub use context::{GeneralContext, RootContext};
pub use diagnostics::{Diagnostic, DiagnosticKind, DiagnosticsContainer};
pub use options::TypeCheckOptions;
pub use types::{calling::call_type_handle_errors, poly_types::GenericTypeParameters, subtyping};

pub use type_mappings::*;
pub use types::{properties::PropertyValue, Constant, Type, TypeId};

pub use context::{information::LocalInformation, Environment, Scope};

pub trait ReadFromFS {
	fn read_file(&self, path: &std::path::Path) -> Option<Vec<u8>>;
}

impl<T, U> ReadFromFS for T
where
	T: Fn(&std::path::Path) -> Option<U>,
	U: Into<Vec<u8>>,
{
	fn read_file(&self, path: &std::path::Path) -> Option<Vec<u8>> {
		(self)(path).map(Into::into)
	}
}

pub use source_map::{self, SourceId, Span};

/// Contains all the modules and mappings for import statements
///
/// TODO could files and `synthesised_modules` be merged? (with a change to the source map crate)
pub struct ModuleData<'a, FileReader, AST: ASTImplementation> {
	pub(crate) file_reader: &'a FileReader,
	pub(crate) parser_requirements: AST::ParserRequirements,
	pub(crate) _current_working_directory: PathBuf,
	/// Contains the text content of files (for source maps and diagnostics)
	pub(crate) files: MapFileStore<WithPathMap>,
	/// To catch cyclic imports
	pub(crate) _currently_checking_modules: HashSet<PathBuf>,
	/// The result of checking. Includes exported variables and info
	pub(crate) synthesised_modules: HashMap<SourceId, SynthesisedModule<AST::OwnedModule>>,
}

pub trait ASTImplementation: Sized {
	type ParseOptions;
	/// Custom allocator etc
	type ParserRequirements;

	type ParseError: Into<Diagnostic>;

	type Module<'a>;
	/// TODO temp item. Some modules can have references
	type OwnedModule;

	type DefinitionFile<'a>;

	type TypeAnnotation<'a>;
	type TypeParameter<'a>;
	type Expression<'a>;
	type MultipleExpression<'a>;
	type ForStatementInitiliser<'a>;

	/// Used in `for of`, `for in` and function parameters
	type VariableField<'a>;

	type ClassMethod<'a>: SynthesisableFunction<Self>;

	fn module_from_string(
		source_id: SourceId,
		string: String,
		options: Self::ParseOptions,
		parser_requirements: &mut Self::ParserRequirements,
	) -> Result<Self::Module<'static>, Self::ParseError>;

	fn definition_module_from_string(
		source_id: SourceId,
		string: String,
		parser_requirements: &mut Self::ParserRequirements,
	) -> Result<Self::DefinitionFile<'static>, Self::ParseError>;

	#[allow(clippy::needless_lifetimes)]
	fn synthesise_module<'a, T: crate::ReadFromFS>(
		module: &Self::Module<'a>,
		source_id: SourceId,
		module_context: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	);

	#[allow(clippy::needless_lifetimes)]
	fn synthesise_definition_file<'a, T: crate::ReadFromFS>(
		file: Self::DefinitionFile<'a>,
		source: SourceId,
		root: &RootContext,
		checking_data: &mut CheckingData<T, Self>,
	) -> (Names, LocalInformation);

	/// Expected is used for eagerly setting function parameters
	fn synthesise_expression<'a, T: crate::ReadFromFS>(
		expression: &'a Self::Expression<'a>,
		expected_type: TypeId,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId;

	/// Expected is used for eagerly setting function parameters
	fn synthesise_multiple_expression<'a, T: crate::ReadFromFS>(
		expression: &'a Self::MultipleExpression<'a>,
		expected_type: TypeId,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId;

	fn synthesise_type_annotation<'a, T: crate::ReadFromFS>(
		annotation: &'a Self::TypeAnnotation<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId;

	/// Don't need to return anything. All information recorded via changed to `environment`
	fn synthesise_for_loop_initialiser<'a, T: crate::ReadFromFS>(
		for_loop_initialiser: &'a Self::ForStatementInitiliser<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	);

	fn expression_position<'a>(expression: &'a Self::Expression<'a>) -> Span;

	fn type_parameter_name<'a>(parameter: &'a Self::TypeParameter<'a>) -> &'a str;

	fn parse_options(is_js: bool, parse_comments: bool, lsp_mode: bool) -> Self::ParseOptions;

	fn owned_module_from_module(m: Self::Module<'static>) -> Self::OwnedModule;

	/// For `for in` and `for of loops`
	fn declare_and_assign_to_fields<'a, T: crate::ReadFromFS>(
		field: &'a Self::VariableField<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
		value: TypeId,
	);
}

impl<'a, T, A> ModuleData<'a, T, A>
where
	T: crate::ReadFromFS,
	A: ASTImplementation,
{
	pub(crate) fn new(
		file_resolver: &'a T,
		current_working_directory: PathBuf,
		files: Option<MapFileStore<WithPathMap>>,
		parser_requirements: A::ParserRequirements,
	) -> Self {
		Self {
			files: files.unwrap_or_default(),
			synthesised_modules: Default::default(),
			_currently_checking_modules: Default::default(),
			// custom_module_resolvers,
			file_reader: file_resolver,
			_current_working_directory: current_working_directory,
			parser_requirements,
		}
	}

	pub(crate) fn get_file(&mut self, path: &Path) -> Option<File> {
		// TODO only internal code should be able to do this
		if let Some("bin") = path.extension().and_then(|s| s.to_str()) {
			return self.file_reader.read_file(path).map(|s| File::Binary(s.to_vec()));
		}

		if let Some(source) = self.files.get_source_at_path(path) {
			Some(File::Source(source, self.files.get_file_content(source)))
		} else {
			// Load into system
			let content = self.file_reader.read_file(path)?;
			let content = String::from_utf8(content);
			match content {
				Ok(content) => {
					let source_id = self.files.new_source_id(path.to_path_buf(), content);
					Some(File::Source(source_id, self.files.get_file_content(source_id)))
				}
				Err(_) => {
					eprintln!("{} is not valid Utf-8", path.display());
					None
				}
			}
		}
	}
}

pub enum File {
	Binary(Vec<u8>),
	Source(SourceId, String),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, binary_serialize_derive::BinarySerializable)]
pub struct VariableId(pub SourceId, pub u32);

/// TODO split for annotations based functions
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, binary_serialize_derive::BinarySerializable)]
pub struct FunctionId(pub SourceId, pub u32);

impl FunctionId {
	pub const AUTO_CONSTRUCTOR: Self = FunctionId(source_map::Nullable::NULL, 0);
}

#[derive(Debug)]
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

impl<'a, T, A> CheckingData<'a, T, A>
where
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
{
	// TODO improve on this function
	pub fn new(
		options: TypeCheckOptions,
		resolver: &'a T,
		existing_files: Option<MapFileStore<WithPathMap>>,
		parser_requirements: A::ParserRequirements,
	) -> Self {
		// let custom_file_resolvers = HashMap::default();
		let cwd = Default::default();
		let modules = ModuleData::new(resolver, cwd, existing_files, parser_requirements);

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
		fn get_module<'a, T: crate::ReadFromFS, A: crate::ASTImplementation>(
			full_importer: &Path,
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
					let (source, module) = get_source(
						checking_data,
						full_importer,
						String::from_utf8(content).unwrap(),
					);

					match module {
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

		if importing_path.starts_with('.') {
			let from_path = self.modules.files.get_file_path(from);
			let from = PathBuf::from(importing_path);
			let mut full_importer =
				path_absolutize::Absolutize::absolutize_from(&from, from_path.parent().unwrap())
					.unwrap()
					.to_path_buf();

			let result = if full_importer.extension().is_some() {
				get_module(&full_importer, environment, self)
			} else {
				let mut result = None;
				for ext in ["ts", "tsx", "js"] {
					full_importer.set_extension(ext);
					// TODO change parse options based on extension
					result = get_module(&full_importer, environment, self);
					if result.is_some() {
						break;
					}
				}
				result
			};

			match result {
				Some(Ok(synthesised_module)) => {
					environment.info.extend_ref(&synthesised_module.info);
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
		pub(crate) fn check_satisfies(
			expr_ty: TypeId,
			to_satisfy: TypeId,
			types: &TypeStore,
			environment: &mut Environment,
		) -> bool {
			// TODO `behavior.allow_error = true` would be better
			if expr_ty == TypeId::ERROR_TYPE {
				false
			} else {
				let mut basic_equality = subtyping::BasicEquality {
					add_property_restrictions: false,
					position: source_map::Nullable::NULL,
					object_constraints: Default::default(),
				};
				let result = subtyping::type_is_subtype(
					to_satisfy,
					expr_ty,
					&mut basic_equality,
					environment,
					types,
				);

				matches!(result, subtyping::SubTypeResult::IsSubType)
			}
		}

		if !check_satisfies(expr_ty, to_satisfy, &self.types, environment) {
			let expected = diagnostics::TypeStringRepresentation::from_type_id(
				to_satisfy,
				environment,
				&self.types,
				false,
			);
			let found = diagnostics::TypeStringRepresentation::from_type_id(
				expr_ty,
				environment,
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
pub struct CheckOutput<A: crate::ASTImplementation> {
	pub type_mappings: crate::TypeMappings,
	pub types: crate::types::TypeStore,
	pub module_contents: MapFileStore<WithPathMap>,
	pub modules: HashMap<SourceId, SynthesisedModule<A::OwnedModule>>,
	pub diagnostics: crate::DiagnosticsContainer,
	pub top_level_information: crate::LocalInformation,
}

impl<A: crate::ASTImplementation> CheckOutput<A> {
	#[must_use]
	pub fn get_type_at_position(&self, _path: &str, pos: u32, debug: bool) -> Option<String> {
		// TODO TypeMappings need to be per file
		self.type_mappings.expressions_to_instances.get(pos).map(|instance| {
			print_type(instance.get_value_on_ref(), &self.types, &self.top_level_information, debug)
		})
	}
}

#[allow(clippy::needless_pass_by_value)]
pub fn check_project<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	entry_points: Vec<PathBuf>,
	type_definition_files: HashSet<PathBuf>,
	resolver: T,
	options: TypeCheckOptions,
	parser_requirements: A::ParserRequirements,
) -> CheckOutput<A> {
	let mut checking_data =
		CheckingData::<T, A>::new(options, &resolver, None, parser_requirements);

	let mut root = crate::context::RootContext::new_with_primitive_references();

	crate::utils::notify!("--- Reading definition files from {:?} ---", type_definition_files);
	add_definition_files_to_root(type_definition_files, &mut root, &mut checking_data);

	if checking_data.diagnostics_container.has_error() {
		return CheckOutput {
			type_mappings: checking_data.type_mappings,
			types: checking_data.types,
			module_contents: checking_data.modules.files,
			modules: Default::default(),
			diagnostics: checking_data.diagnostics_container,
			top_level_information: Default::default(),
		};
	}

	crate::utils::notify!("--- Finished definition file ---");

	for point in &entry_points {
		let entry_content = checking_data.modules.file_reader.read_file(point);
		if let Some(content) = entry_content {
			let (source, module) =
				get_source(&mut checking_data, point, String::from_utf8(content).unwrap());

			match module {
				Ok(module) => {
					root.new_module_context(source, module, &mut checking_data);
				}
				Err(err) => {
					checking_data.diagnostics_container.add_error(err);
				}
			}
		} else {
			checking_data.diagnostics_container.add_error(TypeCheckError::CannotOpenFile {
				file: CouldNotOpenFile(point.clone()),
				position: None,
			});
		}
	}

	let CheckingData {
		diagnostics_container,
		type_mappings,
		modules,
		options: _,
		types,
		unimplemented_items: _,
	} = checking_data;

	CheckOutput {
		type_mappings,
		types,
		module_contents: modules.files,
		modules: modules.synthesised_modules,
		diagnostics: diagnostics_container,
		top_level_information: root.info,
	}
}

fn get_source<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	checking_data: &mut CheckingData<T, A>,
	path: &Path,
	content: String,
) -> (
	SourceId,
	Result<<A as ASTImplementation>::Module<'static>, <A as ASTImplementation>::ParseError>,
) {
	let source = checking_data.modules.files.new_source_id(path.to_path_buf(), content.clone());

	// TODO abstract using similar to import logic
	let is_js = path.extension().and_then(|s| s.to_str()).map_or(false, |s| s.ends_with("js"));

	let parse_options = A::parse_options(
		is_js,
		checking_data.options.parse_comments,
		checking_data.options.lsp_mode,
	);

	let module = A::module_from_string(
		source,
		content,
		parse_options,
		&mut checking_data.modules.parser_requirements,
	);

	(source, module)
}

const CACHE_MARKER: &[u8] = b"ezno-cache-file";

#[derive(binary_serialize_derive::BinarySerializable)]
pub(crate) struct Cache {
	pub(crate) variables: HashMap<String, features::variables::VariableOrImport>,
	pub(crate) named_types: HashMap<String, TypeId>,
	pub(crate) info: LocalInformation,
	pub(crate) types: TypeStore,
	// /// Retains position information
	// pub(crate) content: String,
}

pub(crate) fn add_definition_files_to_root<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	type_definition_files: HashSet<PathBuf>,
	root: &mut RootContext,
	checking_data: &mut CheckingData<T, A>,
) {
	let length = type_definition_files.len();
	for path in type_definition_files {
		let Some(file) = checking_data.modules.get_file(&path) else {
			checking_data.diagnostics_container.add_error(Diagnostic::Global {
				reason: format!("could not find {}", path.display()),
				kind: crate::DiagnosticKind::Error,
			});
			continue;
		};

		match file {
			File::Binary(mut content) => {
				crate::utils::notify!("Using cache :)");
				assert_eq!(length, 1, "only a single cache is current supported");

				let vec = content[CACHE_MARKER.len()..(CACHE_MARKER.len() + U32_BYTES as usize)]
					.to_owned();

				let at_end =
					<u32 as BinarySerializable>::deserialize(&mut vec.into_iter(), SourceId::NULL);

				// Get source and content which is at the end.
				let mut drain =
					content.drain((CACHE_MARKER.len() + U32_BYTES as usize + at_end as usize)..);
				// Okay as end
				let (_source_id, path) = <(SourceId, String) as BinarySerializable>::deserialize(
					&mut drain,
					SourceId::NULL,
				);

				// Collect from end
				let source_content = String::from_utf8(drain.collect::<Vec<_>>()).unwrap();

				// TODO unsafe set here
				// checking_data.modules.files.update_file(id, content)
				// todo!();
				let source_id =
					checking_data.modules.files.new_source_id(path.into(), source_content);

				let mut bytes = content.drain((CACHE_MARKER.len() + U32_BYTES as usize)..);

				// TODO WIP
				let Cache { variables, named_types, info, types } =
					Cache::deserialize(&mut bytes, source_id);

				root.variables = variables;
				root.named_types = named_types;
				root.info = info;
				checking_data.types = types;
			}
			File::Source(source_id, content) => {
				let result = A::definition_module_from_string(
					source_id,
					content,
					&mut checking_data.modules.parser_requirements,
				);

				match result {
					Ok(tdm) => {
						let (names, info) =
							A::synthesise_definition_file(tdm, source_id, root, checking_data);
						root.variables.extend(names.variables);
						root.named_types.extend(names.named_types);
						root.variable_names.extend(names.variable_names);
						root.info.extend(info, None);
					}
					Err(err) => {
						checking_data.diagnostics_container.add_error(err);
						continue;
					}
				}
			}
		}
	}
}

const U32_BYTES: u32 = u32::BITS / u8::BITS;

pub fn generate_cache<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	on: PathBuf,
	read: T,
	parser_requirements: A::ParserRequirements,
) -> Vec<u8> {
	let mut checking_data =
		CheckingData::<T, A>::new(Default::default(), &read, None, parser_requirements);

	let mut root = crate::context::RootContext::new_with_primitive_references();

	add_definition_files_to_root(HashSet::from_iter([on.clone()]), &mut root, &mut checking_data);

	if checking_data.diagnostics_container.has_error() {
		panic!(
			"found error in definition file {:#?}",
			checking_data.diagnostics_container.get_diagnostics()
		);
	}

	let mut buf = CACHE_MARKER.to_vec();

	buf.extend_from_slice(&0_u32.to_le_bytes());

	let cache = Cache {
		variables: root.variables,
		named_types: root.named_types,
		info: root.info,
		types: checking_data.types,
	};
	cache.serialize(&mut buf);

	let cache_len: usize = buf.len() - CACHE_MARKER.len() - U32_BYTES as usize;
	// Set length
	buf[CACHE_MARKER.len()..(CACHE_MARKER.len() + U32_BYTES as usize)]
		.copy_from_slice(&(cache_len as u32).to_le_bytes());

	// TODO not great
	let Some(File::Source(source, content)) = checking_data.modules.get_file(&on) else { panic!() };

	let path = on.to_str().unwrap().to_owned();
	(source, path).serialize(&mut buf);
	buf.extend_from_slice(content.as_bytes());

	buf
}
