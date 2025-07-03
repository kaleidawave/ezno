#![doc = include_str!("../README.md")]
#![allow(deprecated, clippy::new_without_default, clippy::too_many_lines, clippy::result_unit_err)]
#![warn(clippy::must_use_candidate)]
// #![allow(unused)]

pub mod context;
pub mod diagnostics;
pub mod events;
pub mod features;
mod options;
mod type_mappings;
pub mod types;
pub mod utilities;

#[cfg(feature = "ezno-parser")]
pub mod api;

#[cfg(feature = "ezno-parser")]
pub mod synthesis;

use std::{
	collections::{HashMap, HashSet},
	path::{Path, PathBuf},
	time::Duration,
};

use context::{
	information::{LocalInformation, ModuleInformation},
	Names,
};

use diagnostics::{TypeCheckError, TypeCheckWarning};
pub(crate) use utilities::{map::Map, range_map::RangeMap, serialization::BinarySerializable};

use features::{
	functions::SynthesisableFunction, modules::CouldNotOpenFile, modules::SynthesisedModule,
};

pub use context::{Environment, GeneralContext, RootContext, Scope, VariableRegisterArguments};
pub use diagnostics::{Diagnostic, DiagnosticKind, DiagnosticsContainer};
pub use options::TypeCheckOptions;
pub use types::{
	calling::call_type_handle_errors, generics::GenericTypeParameters, properties::PropertyValue,
	subtyping, Constant, Type, TypeId, TypeStore,
};

pub use source_map::{self, SourceId, Span};
use source_map::{FileSystem, MapFileStore, Nullable, SpanWithSource, WithPathMap};
pub use type_mappings::*;

pub const INTERNAL_DEFINITION_FILE_PATH: &str = "internal.ts.d.bin";
pub const INTERNAL_DEFINITION_FILE: &[u8] = include_bytes!("../definitions/internal.ts.d.bin");

pub trait ReadFromFS {
	/// Returns `Vec<u8>` as this callback can return binary file
	/// TODO this shouldn't take `&self`. Should be just `T::read_file`, doesn't need any data
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
	/// List of statements and declarations
	type Block<'a>;
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
	fn synthesise_module<T: crate::ReadFromFS>(
		module: &Self::Module<'_>,
		source_id: SourceId,
		module_context: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	);

	#[allow(clippy::needless_lifetimes)]
	fn synthesise_definition_module<T: crate::ReadFromFS>(
		module: &Self::DefinitionFile<'_>,
		source: SourceId,
		root: &RootContext,
		checking_data: &mut CheckingData<T, Self>,
	) -> (Names, LocalInformation);

	fn synthesise_expression<T: crate::ReadFromFS>(
		expression: &Self::Expression<'_>,
		expected_type: TypeId,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId;

	fn synthesise_type_parameter_extends<T: crate::ReadFromFS>(
		parameter: &Self::TypeParameter<'_>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId;

	fn synthesise_type_annotation<'a, T: crate::ReadFromFS>(
		annotation: &'a Self::TypeAnnotation<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId;

	fn synthesise_block<'a, T: crate::ReadFromFS>(
		block: &'a Self::Block<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	);

	/// Don't need to return anything. All information recorded via changed to `environment`
	fn synthesise_for_loop_initialiser<'a, T: crate::ReadFromFS>(
		for_loop_initialiser: &'a Self::ForStatementInitiliser<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	);

	fn expression_position<'a>(expression: &'a Self::Expression<'a>) -> Span;

	fn type_parameter_name<'a>(parameter: &'a Self::TypeParameter<'a>) -> &'a str;

	fn type_annotation_position<'a>(annotation: &'a Self::TypeAnnotation<'a>) -> Span;

	fn parameter_constrained<'a>(parameter: &'a Self::TypeParameter<'a>) -> bool;

	#[allow(clippy::fn_params_excessive_bools)]
	fn parse_options(
		is_js: bool,
		extra_syntax: bool,
		parse_comments: bool,
		lsp_mode: bool,
	) -> Self::ParseOptions;

	fn owned_module_from_module(m: Self::Module<'static>) -> Self::OwnedModule;

	/// For `for in` and `for of loops`
	fn declare_and_assign_to_fields<'a, T: crate::ReadFromFS>(
		field: &'a Self::VariableField<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
		arguments: VariableRegisterArguments,
	);
}

/// Contains all the modules and mappings for import statements
///
/// TODO could files and `synthesised_modules` be merged? (with a change to the source map crate)
pub struct ModuleData<'a, FileReader, AST: ASTImplementation> {
	pub(crate) file_reader: &'a FileReader,
	pub(crate) parser_requirements: AST::ParserRequirements,
	pub(crate) current_working_directory: PathBuf,
	/// Contains the text content of files (for source maps and diagnostics)
	pub(crate) files: MapFileStore<WithPathMap>,
	/// To catch cyclic imports
	pub(crate) _currently_checking_modules: HashSet<PathBuf>,
	/// The result of checking. Includes exported variables and info
	pub(crate) synthesised_modules: HashMap<SourceId, SynthesisedModule<AST::OwnedModule>>,
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
			current_working_directory,
			parser_requirements,
		}
	}

	pub(crate) fn get_file(
		&mut self,
		path: &Path,
		chronometer: Option<&mut Chronometer>,
	) -> Option<File> {
		// TODO only internal code should be able to do this
		if let Some("bin") = path.extension().and_then(|s| s.to_str()) {
			return self.file_reader.read_file(path).map(|s| File::Binary(s.clone()));
		}

		let get_source_at_path = self.files.get_source_at_path(path);

		// eprintln!(
		// 	"Found {:?} {:?} {:?}",
		// 	get_source_at_path,
		// 	path.display(),
		// 	self.files.get_paths()
		// );

		if let Some(source) = get_source_at_path {
			Some(File::Source(source, self.files.get_file_content(source)))
		} else {
			// Load into system
			let current = chronometer.is_some().then(std::time::Instant::now);
			let content = self.file_reader.read_file(path)?;
			if let Ok(content) = String::from_utf8(content) {
				if let Some(current) = current {
					chronometer.unwrap().fs += current.elapsed();
				}
				let source_id = self.files.new_source_id(path.to_path_buf(), content);
				Some(File::Source(source_id, self.files.get_file_content(source_id)))
			} else {
				eprintln!("{} is not valid Utf-8", path.display());
				None
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

#[derive(Default, Debug)]
pub struct Chronometer {
	/// In binary .d.ts files
	pub cached: Duration,
	/// read actions
	pub fs: Duration,
	/// parsing. (TODO only of first file)
	pub parse: Duration,
	/// type checking (inc binding). TODO this includes parsing of imports
	pub check: Duration,
	/// parsed and type checked lines
	pub lines: usize,
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
	pub local_type_mappings: TypeMappings,
	/// All module information
	pub(crate) modules: ModuleData<'a, FSResolver, ModuleAST>,
	/// Options for checking
	pub(crate) options: TypeCheckOptions,

	// pub(crate) events: EventsStore,
	pub types: TypeStore,

	pub(crate) chronometer: Chronometer,

	/// Do not repeat emitting unimplemented parts
	unimplemented_items: HashSet<&'static str>,
}

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
			modules,
			diagnostics_container: Default::default(),
			local_type_mappings: Default::default(),
			types: Default::default(),
			unimplemented_items: Default::default(),
			chronometer: Default::default(),
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
	pub fn raise_unimplemented_error(&mut self, item: &'static str, position: SpanWithSource) {
		if self.unimplemented_items.insert(item) {
			self.diagnostics_container
				.add_warning(TypeCheckWarning::Unimplemented { item, position });
		}
	}

	pub fn add_expression_mapping(&mut self, span: SpanWithSource, instance: Instance) {
		self.local_type_mappings.expressions_to_instances.push(span, instance);
	}
}

/// Used for transformers and other things after checking!!!!
pub struct CheckOutput<A: crate::ASTImplementation> {
	pub types: crate::types::TypeStore,
	pub module_contents: MapFileStore<WithPathMap>,
	pub modules: HashMap<SourceId, SynthesisedModule<A::OwnedModule>>,
	pub diagnostics: crate::DiagnosticsContainer,
	pub top_level_information: LocalInformation,
	pub chronometer: crate::Chronometer,
}

impl<A: crate::ASTImplementation> CheckOutput<A> {
	#[must_use]
	pub fn get_type_at_position(&self, path: &str, pos: u32, debug: bool) -> Option<String> {
		let source = self.module_contents.get_source_at_path(path.as_ref())?;
		let module = &self.modules.get(&source)?;

		module.get_instance_at_position(pos).map(|instance| {
			crate::types::printing::print_type(
				instance.get_value_on_ref(),
				&self.types,
				&ModuleInformation { top: &self.top_level_information, module: &module.info },
				debug,
			)
		})
	}

	#[must_use]
	pub fn get_type_at_position_with_span(
		&self,
		path: &str,
		pos: u32,
		debug: bool,
	) -> Option<(String, SpanWithSource)> {
		let source = self.module_contents.get_source_at_path(path.as_ref())?;
		if let Some(module) = self.modules.get(&source) {
			module.get_instance_at_position_with_span(pos).map(|(instance, range)| {
				(
					crate::types::printing::print_type(
						instance.get_value_on_ref(),
						&self.types,
						&ModuleInformation {
							top: &self.top_level_information,
							module: &module.info,
						},
						debug,
					),
					SpanWithSource { start: range.start, end: range.end, source },
				)
			})
		} else {
			eprintln!("no module here???");
			None
		}
	}

	#[must_use]
	pub fn get_module(&self, path: &str) -> Option<&A::OwnedModule> {
		let source_id = self.module_contents.get_source_at_path(path.as_ref())?;
		Some(&self.modules.get(&source_id).expect("no module").content)
	}

	#[must_use]
	pub fn empty() -> Self {
		Self {
			types: Default::default(),
			module_contents: Default::default(),
			modules: Default::default(),
			diagnostics: Default::default(),
			top_level_information: Default::default(),
			chronometer: Default::default(),
		}
	}
}

#[allow(clippy::needless_pass_by_value)]
pub fn check_project<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	entry_points: Vec<PathBuf>,
	type_definition_files: Vec<PathBuf>,
	resolver: &T,
	options: TypeCheckOptions,
	parser_requirements: A::ParserRequirements,
	existing_files: Option<MapFileStore<WithPathMap>>,
) -> CheckOutput<A> {
	let mut checking_data =
		CheckingData::<T, A>::new(options, resolver, existing_files, parser_requirements);

	let mut root = crate::context::RootContext::new_with_primitive_references();

	crate::utilities::notify!("--- Reading definition files from {:?} ---", type_definition_files);

	// Hide any debug messages from here
	if !checking_data.options.debug_dts {
		crate::utilities::pause_debug_mode();
	}
	add_definition_files_to_root(type_definition_files, &mut root, &mut checking_data);
	crate::utilities::unpause_debug_mode();

	if checking_data.diagnostics_container.contains_error() {
		return CheckOutput {
			types: checking_data.types,
			module_contents: checking_data.modules.files,
			modules: Default::default(),
			diagnostics: checking_data.diagnostics_container,
			top_level_information: Default::default(),
			chronometer: checking_data.chronometer,
		};
	}

	crate::utilities::notify!("--- Finished definition file ---");

	for point in &entry_points {
		// eprintln!("Trying to get {point} from {:?}", checking_data.modules.files.get_paths());
		let current = checking_data.options.measure_time.then(std::time::Instant::now);

		let entry_content = if let Some(source) =
			checking_data.modules.files.get_source_at_path(point)
		{
			Some((source, checking_data.modules.files.get_file_content(source)))
		} else if let Some(content) = checking_data.modules.file_reader.read_file(point) {
			let content = String::from_utf8(content).expect("invalid entry point encoding");
			let source = checking_data.modules.files.new_source_id(point.clone(), content.clone());
			Some((source, content))
		} else {
			None
		};
		if let Some(current) = current {
			checking_data.chronometer.fs += current.elapsed();
		}

		if let Some((source, content)) = entry_content {
			let module = parse_source(point, source, content, &mut checking_data);

			let current = checking_data.options.measure_time.then(std::time::Instant::now);
			match module {
				Ok(module) => {
					let _module = root.new_module_context(source, module, &mut checking_data);
					if let Some(current) = current {
						checking_data.chronometer.check += current.elapsed();
					}
				}
				Err(err) => {
					checking_data.diagnostics_container.add_error(err);
				}
			}
		} else {
			checking_data.diagnostics_container.add_error(TypeCheckError::CannotOpenFile {
				file: CouldNotOpenFile(point.clone()),
				import_position: None,
				possibles: checking_data
					.modules
					.files
					.get_paths()
					.keys()
					.filter_map(|path| path.to_str())
					.collect(),
				partial_import_path: point.to_str().unwrap_or(""),
			});
		}
	}

	let CheckingData {
		diagnostics_container,
		local_type_mappings: _,
		modules,
		options: _,
		types,
		unimplemented_items: _,
		chronometer,
	} = checking_data;

	CheckOutput {
		types,
		module_contents: modules.files,
		modules: modules.synthesised_modules,
		diagnostics: diagnostics_container,
		top_level_information: root.info,
		chronometer,
	}
}

fn parse_source<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	path: &Path,
	source: SourceId,
	content: String,
	checking_data: &mut CheckingData<T, A>,
) -> Result<<A as ASTImplementation>::Module<'static>, <A as ASTImplementation>::ParseError> {
	if checking_data.options.measure_time {
		let code_lines =
			content.lines().filter(|c| !(c.is_empty() || c.trim_start().starts_with('/'))).count();
		checking_data.chronometer.lines += code_lines;
	}

	// TODO pause check timing
	let current = checking_data.options.measure_time.then(std::time::Instant::now);

	// TODO abstract using similar to import logic
	let is_js = path.extension().and_then(|s| s.to_str()).is_some_and(|s| s.ends_with("js"));

	let parse_options = A::parse_options(
		is_js,
		checking_data.options.extra_syntax,
		checking_data.options.parse_comments,
		checking_data.options.lsp_mode,
	);

	let result = A::module_from_string(
		source,
		content,
		parse_options,
		&mut checking_data.modules.parser_requirements,
	);

	if let Some(current) = current {
		checking_data.chronometer.parse += current.elapsed();
	}

	result
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
	type_definition_files: Vec<PathBuf>,
	root: &mut RootContext,
	checking_data: &mut CheckingData<T, A>,
) {
	let length = type_definition_files.len();
	for path in type_definition_files {
		let chronometer =
			checking_data.options.measure_time.then_some(&mut checking_data.chronometer);

		let file = if path == PathBuf::from(crate::INTERNAL_DEFINITION_FILE_PATH) {
			File::Binary(crate::INTERNAL_DEFINITION_FILE.to_owned())
		} else if let Some(file) = checking_data.modules.get_file(&path, chronometer) {
			file
		} else {
			checking_data.diagnostics_container.add_error(Diagnostic::Global {
				reason: format!("could not find {}", path.display()),
				kind: crate::DiagnosticKind::Error,
			});
			continue;
		};

		match file {
			File::Binary(data) => {
				let current = checking_data.options.measure_time.then(std::time::Instant::now);
				deserialize_cache(length, data, checking_data, root);
				if let Some(current) = current {
					checking_data.chronometer.cached += current.elapsed();
				}
			}
			File::Source(source_id, source) => {
				if checking_data.options.measure_time {
					let code_lines = source
						.lines()
						.filter(|c| !(c.is_empty() || c.trim_start().starts_with('/')))
						.count();
					checking_data.chronometer.lines += code_lines;
				}

				let current = checking_data.options.measure_time.then(std::time::Instant::now);
				let result = A::definition_module_from_string(
					source_id,
					source,
					&mut checking_data.modules.parser_requirements,
				);
				if let Some(current) = current {
					checking_data.chronometer.parse += current.elapsed();
				}

				match result {
					Ok(tdm) => {
						let current =
							checking_data.options.measure_time.then(std::time::Instant::now);

						let (names, info) =
							A::synthesise_definition_module(&tdm, source_id, root, checking_data);

						// TODO bad. should be per file
						if let Some(current) = current {
							checking_data.chronometer.check += current.elapsed();
						}

						root.variables.extend(names.variables);
						root.named_types.extend(names.named_types);
						root.variable_names.extend(names.variable_names);
						root.info.extend(info, None);
					}
					Err(err) => {
						checking_data.diagnostics_container.add_error(err);
					}
				}
			}
		}
	}
}

fn deserialize_cache<T: ReadFromFS, A: ASTImplementation>(
	length: usize,
	mut content: Vec<u8>,
	checking_data: &mut CheckingData<T, A>,
	root: &mut RootContext,
) {
	crate::utilities::notify!("Using cache :)");
	assert_eq!(length, 1, "only a single cache is current supported");

	let end_content =
		content[CACHE_MARKER.len()..(CACHE_MARKER.len() + U32_BYTES as usize)].to_owned();

	let at_end =
		<u32 as BinarySerializable>::deserialize(&mut end_content.into_iter(), SourceId::NULL);

	let source_id = {
		// Get source and content which is at the end.
		let mut drain =
			content.drain((CACHE_MARKER.len() + U32_BYTES as usize + at_end as usize)..);

		// Okay as end
		let (_source_id, path) =
			<(SourceId, String) as BinarySerializable>::deserialize(&mut drain, SourceId::NULL);

		let get_source_at_path = checking_data.modules.files.get_source_at_path(Path::new(&path));

		if let Some(source_id) = get_source_at_path {
			eprintln!("reusing source id {source_id:?}");
			source_id
		} else {
			// Collect from end
			let source_content = String::from_utf8(drain.collect::<Vec<_>>()).unwrap();
			checking_data.modules.files.new_source_id(path.into(), source_content)
		}
	};

	let mut bytes = content.drain((CACHE_MARKER.len() + U32_BYTES as usize)..);

	// TODO WIP
	let Cache { variables, named_types, info, types } = Cache::deserialize(&mut bytes, source_id);

	root.variables = variables;
	root.named_types = named_types;
	root.info = info;
	checking_data.types = types;
}

const U32_BYTES: u32 = u32::BITS / u8::BITS;

pub fn generate_cache<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	on: &Path,
	read: &T,
	parser_requirements: A::ParserRequirements,
) -> Vec<u8> {
	let mut checking_data =
		CheckingData::<T, A>::new(Default::default(), read, None, parser_requirements);

	let mut root = crate::context::RootContext::new_with_primitive_references();

	{
		add_definition_files_to_root(vec![on.to_path_buf()], &mut root, &mut checking_data);

		assert!(
			!checking_data.diagnostics_container.contains_error(),
			"found error in definition file {:#?}",
			checking_data.diagnostics_container.get_diagnostics()
		);
	}

	let mut buf = CACHE_MARKER.to_vec();

	// This reserves a u32 bytes which marks where the content lives
	buf.extend_from_slice(&[0u8; (u32::BITS / u8::BITS) as usize]);

	let cache = Cache {
		variables: root.variables,
		named_types: root.named_types,
		info: root.info,
		types: checking_data.types,
	};

	cache.serialize(&mut buf);

	// Add content
	{
		let cache_len: usize = buf.len() - CACHE_MARKER.len() - U32_BYTES as usize;
		// Set length
		buf[CACHE_MARKER.len()..(CACHE_MARKER.len() + U32_BYTES as usize)]
			.copy_from_slice(&(cache_len as u32).to_le_bytes());

		// TODO not great
		let Some(File::Source(source, content)) = checking_data.modules.get_file(on, None) else {
			panic!()
		};

		let path = on.to_str().unwrap().to_owned();
		(source, path).serialize(&mut buf);
		buf.extend_from_slice(content.as_bytes());
	}

	buf
}

pub fn get_closest<'a, 'b>(
	items: impl Iterator<Item = &'a str>,
	closest_one: &'b str,
) -> Option<Vec<&'a str>> {
	const MIN_DISTANCE: usize = 2;
	let candidates = items
		.filter(|item| levenshtein::levenshtein(closest_one, item) <= MIN_DISTANCE)
		.collect::<Vec<&str>>();
	match candidates.len() {
		0 => None,
		1.. => Some(candidates),
	}
}
