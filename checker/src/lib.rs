#![doc = include_str!("../README.md")]
#![allow(clippy::new_without_default, clippy::too_many_lines, clippy::result_unit_err)]
#![warn(clippy::must_use_candidate)]
#![allow(unused)]

pub mod caching;
pub mod context;
pub mod diagnostics;
pub mod events;
pub mod features;
pub mod files;
mod options;
mod type_mappings;
pub mod types;
pub mod utilities;

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

pub use files::{ASTImplementation, File, ModuleData};

use features::{modules::CouldNotOpenFile, modules::SynthesisedModule};

pub use context::{Environment, GeneralContext, RootContext, Scope, VariableRegisterArguments};
pub use diagnostics::{Diagnostic, DiagnosticKind};
pub use options::TypeCheckOptions;
pub use types::{
	calling::call_type_handle_errors, generics::GenericTypeParameters, properties::PropertyValue,
	subtyping, Constant, Type, TypeId, TypeStore,
};

pub use source_map::{self, FileSystem, MapFileStore, SourceId, Span, SpanWithSource, WithPathMap};
pub use type_mappings::*;

pub const INTERNAL_DEFINITION_FILE_PATH: &str = "internal.ts.d.bin";
pub const INTERNAL_DEFINITION_FILE: &[u8] = include_bytes!("../definitions/internal.ts.d.bin");

// TODO renmae
pub trait ReadFromFS {
	/// Returns `Vec<u8>` as this callback can return binary file
	/// TODO this shouldn't take `&self`. Should be just `T::read_file`, doesn't need any data
	fn read_file(&self, path: &std::path::Path) -> Option<Vec<u8>>;

	fn emit_diagnostic(&mut self, diagnostic: Diagnostic, files: &MapFileStore<WithPathMap>);
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

/// Records timings about the project
#[derive(Default)]
pub struct Chronometer {
	/// In binary .d.ts files
	pub cached: Duration,
	/// read actions
	pub fs: Duration,
	/// parsing. (TODO only of first file)
	pub parse: Duration,
	/// type checking (inc binding). TODO this includes parsing of imports
	pub check: Duration,
}

#[derive(Default)]
pub struct YardStick {
	/// parsed and type checked lines
	pub lines: usize,
	/// number of errors emitted
	pub errors: u16,
	/// number of warnings emitted
	pub warnings: u16,
	/// number of infos emitted
	pub infos: u16,
}

impl YardStick {
	pub fn diagnostics_count(&self) -> u16 {
		self.errors + self.warnings + self.infos
	}
}

/// Contains logic for **checking phase** (none of the later steps)
/// All data is global, non local to current scope
/// TODO some of these should be mutex / ref cell
pub struct CheckingData<FSResolver, ModuleAST: ASTImplementation> {
	// pub(crate) type_resolving_visitors: [Box<dyn TypeResolvingExpressionVisitor>],
	// pub(crate) pre_type_visitors: FirstPassVisitors,
	/// Type checking errors
	/// TODO temp pub
	pub local_type_mappings: TypeMappings,
	/// All module information
	pub(crate) modules: ModuleData<ModuleAST>,
	/// Options for checking
	pub(crate) options: TypeCheckOptions,

	// pub(crate) events: EventsStore,
	pub types: TypeStore,

	pub(crate) chronometer: Chronometer,
	pub(crate) yardstick: YardStick,

	pub(crate) resolver: FSResolver,

	/// Do not repeat emitting unimplemented parts
	unimplemented_items: HashSet<&'static str>,
}

impl<T, A> CheckingData<T, A>
where
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
{
	// TODO improve on this function
	pub fn new(
		options: TypeCheckOptions,
		resolver: T,
		existing_files: Option<MapFileStore<WithPathMap>>,
		parser_requirements: A::ParserRequirements,
	) -> Self {
		// let custom_file_resolvers = HashMap::default();
		let cwd = Default::default();
		let modules = ModuleData::new(cwd, existing_files, parser_requirements);

		Self {
			options,
			modules,
			resolver,
			local_type_mappings: Default::default(),
			types: Default::default(),
			unimplemented_items: Default::default(),
			chronometer: Default::default(),
			yardstick: Default::default(),
		}
	}

	/// TEMP needs better place
	pub fn raise_unimplemented_error(&mut self, item: &'static str, position: SpanWithSource) {
		let not_yet_been_warned = self.unimplemented_items.insert(item);
		if not_yet_been_warned {
			let diagnostic = crate::diagnostics::UnimplementedWarning { item, position };
			self.resolver.emit_diagnostic(diagnostic.into(), &self.modules.files);
		}
	}

	pub fn add_expression_mapping(&mut self, span: SpanWithSource, instance: Instance) {
		self.local_type_mappings.expressions_to_instances.push(span, instance);
	}

	/// TEMP Utility methods
	pub(crate) fn add_error<I: crate::context::InformationChain>(
		&mut self,
		error: TypeCheckError,
		information: &I,
	) {
		// TODO time and subtract from total
		self.yardstick.errors += 1;
		let information =
			crate::types::printing::PrintingTypeInformation { types: &self.types, information };
		let diagnostic = error.into_diagnostic(information);
		self.resolver.emit_diagnostic(diagnostic, &self.modules.files);
	}

	/// TEMP Utility methods
	pub(crate) fn add_warning<I: crate::context::InformationChain>(
		&mut self,
		warning: TypeCheckWarning,
		information: &I,
	) {
		// TODO time and subtract from total
		self.yardstick.warnings += 1;
		let information =
			crate::types::printing::PrintingTypeInformation { types: &self.types, information };
		let diagnostic = warning.into_diagnostic(information);
		self.resolver.emit_diagnostic(diagnostic, &self.modules.files);
	}

	// TODO
	pub fn error_occured(&self) -> bool {
		self.yardstick.errors > 0
	}
}

/// Used for transformers and other things after checking!!!!
pub struct CheckOutput<T, A: crate::ASTImplementation> {
	pub types: crate::types::TypeStore,
	pub module_contents: MapFileStore<WithPathMap>,
	pub modules: HashMap<SourceId, SynthesisedModule<A::OwnedModule>>,
	pub top_level_information: LocalInformation,
	pub chronometer: crate::Chronometer,
	pub yardstick: crate::YardStick,
	pub resolver: T,
}

impl<T: crate::ReadFromFS, A: crate::ASTImplementation> CheckOutput<T, A> {
	#[must_use]
	pub fn get_type_at_position(&self, path: &str, pos: u32, debug: bool) -> Option<String> {
		let source = self.module_contents.get_source_at_path(path.as_ref())?;
		let module = &self.modules.get(&source)?;

		module.get_instance_at_position(pos).map(|instance| {
			crate::types::printing::print_type(
				instance.get_value_on_ref(),
				crate::types::printing::PrintingTypeInformation {
					types: &self.types,
					information: &ModuleInformation {
						top: &self.top_level_information,
						module: &module.info,
					},
				},
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
						crate::types::printing::PrintingTypeInformation {
							types: &self.types,
							information: &ModuleInformation {
								top: &self.top_level_information,
								module: &module.info,
							},
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

	// Might have to be None
	// #[must_use]
	// pub fn empty() -> Self {
	// 	Self {
	// 		types: Default::default(),
	// 		module_contents: Default::default(),
	// 		modules: Default::default(),
	// 		diagnostics: Default::default(),
	// 		top_level_information: Default::default(),
	// 		chronometer: Default::default(),
	// 	}
	// }
}

#[allow(clippy::needless_pass_by_value)]
pub fn check_project<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	entry_points: Vec<PathBuf>,
	type_definition_files: Vec<PathBuf>,
	resolver: T,
	options: TypeCheckOptions,
	parser_requirements: A::ParserRequirements,
	existing_files: Option<MapFileStore<WithPathMap>>,
) -> CheckOutput<T, A> {
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

	if checking_data.error_occured() {
		return CheckOutput {
			types: checking_data.types,
			module_contents: checking_data.modules.files,
			modules: Default::default(),
			top_level_information: Default::default(),
			chronometer: checking_data.chronometer,
			yardstick: checking_data.yardstick,
			resolver: checking_data.resolver,
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
		} else if let Some(content) = checking_data.resolver.read_file(point) {
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
			let module = files::parse_source(point, source, content, &mut checking_data);

			let current = checking_data.options.measure_time.then(std::time::Instant::now);
			match module {
				Ok(module) => {
					let _module = root.new_module_context(source, module, &mut checking_data);
					if let Some(current) = current {
						checking_data.chronometer.check += current.elapsed();
					}
				}
				Err(err) => {
					checking_data
						.resolver
						.emit_diagnostic(err.into(), &checking_data.modules.files);
				}
			}
		} else {
			let possibles = checking_data
				.modules
				.files
				.get_paths()
				.keys()
				.filter_map(|path| path.to_str())
				.collect();
			let error: Diagnostic = crate::diagnostics::CannotOpenFile {
				file: CouldNotOpenFile(point.clone()),
				import_position: None,
				possibles,
				partial_import_path: point.to_str().unwrap_or(""),
			}
			.into();
			checking_data.resolver.emit_diagnostic(error, &checking_data.modules.files);
			continue;
		}
	}

	let CheckingData {
		local_type_mappings: _,
		modules,
		options: _,
		types,
		unimplemented_items: _,
		chronometer,
		yardstick,
		resolver,
	} = checking_data;

	CheckOutput {
		types,
		module_contents: modules.files,
		modules: modules.synthesised_modules,
		top_level_information: root.info,
		chronometer,
		yardstick,
		resolver,
	}
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
		} else if let Some(file) =
			checking_data.modules.get_file(&mut checking_data.resolver, &path, chronometer)
		{
			file
		} else {
			checking_data.resolver.emit_diagnostic(
				crate::diagnostics::EntryPointNotFound(path.into()).into(),
				&checking_data.modules.files,
			);
			continue;
		};

		match file {
			File::Binary(data) => {
				let current = checking_data.options.measure_time.then(std::time::Instant::now);
				caching::deserialize_cache(length, data, checking_data, root);
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
					checking_data.yardstick.lines += code_lines;
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
						checking_data
							.resolver
							.emit_diagnostic(err.into(), &checking_data.modules.files);
						continue;
					}
				}
			}
		}
	}
}
