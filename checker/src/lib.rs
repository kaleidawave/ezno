#![doc = include_str!("../README.md")]
#![allow(unreachable_code, unused_variables, unused_mut, dead_code, irrefutable_let_patterns)]

mod behavior;
pub mod context;
pub mod errors;
pub mod events;
mod serialization;
mod settings;
pub mod structures;
pub mod temp;
mod type_mappings;
mod types;
mod utils;

// TODO temp pub
#[cfg(feature = "ezno-parser")]
pub mod synthesis;

use errors::TypeCheckWarning;
pub(crate) use serialization::BinarySerializable;

use context::store::ExistingContextStore;

use indexmap::IndexMap;
use std::{
	collections::{HashMap, HashSet},
	path::PathBuf,
};
use structures::functions::AutoConstructorId;

use types::TypeStore;

pub use behavior::{functions::SynthesizableFunction, variables::check_variable_initialization};
pub use context::{GeneralEnvironment, Root};
pub use errors::{Diagnostic, DiagnosticsContainer, ErrorWarningInfo};
pub use settings::TypeCheckSettings;
pub use structures::{
	functions::{FunctionPointer, InternalFunctionId},
	jsx::*,
	modules::SynthesizedModule,
	variables::Variable,
};
pub use types::{
	calling::call_type_handle_errors, operations::*, poly_types::GenericFunctionTypeParameters,
	subtyping,
};

pub use type_mappings::*;
pub use types::{Constant, Type, TypeId};

pub use context::{Environment, Scope};
pub(crate) use structures::functions;
pub(crate) use structures::modules::ModuleFromPathError;

pub trait FSResolver: Fn(&std::path::Path) -> Option<String> {}

impl<T> FSResolver for T where T: Fn(&std::path::Path) -> Option<String> {}

// TODO
pub use source_map::{SourceId, Span};

/// Contains all the modules and mappings for import statements
pub struct ModuleData<'a, T> {
	pub(crate) currently_checking_modules: HashSet<PathBuf>,
	/// TODO this also covers checked modules
	pub(crate) synthesized_modules: IndexMap<PathBuf, SynthesizedModule>,
	// pub(crate) custom_module_resolvers: HashMap<String, Box<dyn CustomModuleResolver>>,
	pub(crate) fs_resolver: &'a T,
	pub(crate) current_working_directory: PathBuf,
}

impl<'a, T: crate::FSResolver> ModuleData<'a, T> {
	pub(crate) fn new_with_custom_module_resolvers(
		// custom_module_resolvers: HashMap<String, Box<dyn CustomModuleResolver>>,
		fs_resolver: &'a T,
		current_working_directory: PathBuf,
	) -> Self {
		Self {
			synthesized_modules: Default::default(),
			currently_checking_modules: Default::default(),
			// custom_module_resolvers,
			fs_resolver,
			current_working_directory,
		}
	}
}

pub enum TruthyFalsy {
	Decidable(bool),
	/// Poly types
	Unknown,
}

/// Contains logic for **checking phase** (none of the later steps)
/// All data is global, non local to current scope
pub struct CheckingData<'a, T> {
	// pub(crate) type_resolving_visitors: [Box<dyn TypeResolvingExpressionVisitor>],
	// pub(crate) pre_type_visitors: FirstPassVisitors,
	/// Type checking errors
	pub diagnostics_container: DiagnosticsContainer,
	/// TODO these should be mutex / ref cell
	pub(crate) type_mappings: TypeMappings,
	/// All module information
	pub(crate) modules: ModuleData<'a, T>,
	/// Settings for checking
	pub(crate) settings: TypeCheckSettings,
	// pub(crate) parse_settings: parser::ParseSettings,
	/// For LSP, and more TODO
	pub(crate) existing_contexts: ExistingContextStore,

	// pub(crate) events: EventsStore,
	pub types: TypeStore,
}

impl<'a, T: crate::FSResolver> CheckingData<'a, T> {
	// TODO improve on this function
	pub fn new(settings: TypeCheckSettings, resolver: &'a T) -> Self {
		// let custom_file_resolvers = HashMap::default();
		let cwd = Default::default();
		let modules = ModuleData::new_with_custom_module_resolvers(resolver, cwd);

		Self {
			settings,
			type_mappings: Default::default(),
			diagnostics_container: Default::default(),
			modules,
			existing_contexts: Default::default(),
			types: Default::default(),
			// events: Default::default(),
		}
	}

	/// TODO method is a bit junky
	/// Returns the exports of the module
	pub(crate) fn load_and_check_module(
		&mut self,
		path: PathBuf,
		base_environment: &mut Root,
	) -> Result<
		(&HashMap<String, Variable>, &mut DiagnosticsContainer, &mut TypeMappings),
		ModuleFromPathError,
	> {
		todo!()
		// if let Some(ref _synthesized_modules) = self.modules.synthesized_modules.get(&path) {
		// 	todo!()
		// }

		// // TODO check module is not already loaded or checked.
		// // TODO paths maybe different to what is in map here here .... :(
		// let (module_result, _) = Module::from_path(
		// 	path.clone(),
		// 	&self.modules.custom_module_resolvers,
		// 	&self.modules.fs_resolver,
		// 	todo!("parse settings"),
		// )?;

		// self.modules.currently_checking_modules.insert(path.clone());
		// let synthesized_module = module_result.synthesize(base_environment, self);
		// self.modules.currently_checking_modules.remove(&path);

		// self.modules
		// 	.synthesized_modules
		// 	.insert(synthesized_module.module.path.clone(), synthesized_module)
		// 	.unwrap();

		// // module.get_exports()
		// Ok((todo!(), &mut self.error_warning_info_handler, &mut self.type_mappings))
	}

	pub fn raise_decidable_result_error(&mut self, span: Span, value: bool) {
		self.diagnostics_container.add_error(TypeCheckWarning::DeadBranch {
			expression_span: span,
			expression_value: value,
		})
	}
}

/// TODO this is a bad name
#[derive(Debug)]
pub(crate) struct FunctionDoesNotMeetConstraint {
	constraint: String,
	function: String,
}
