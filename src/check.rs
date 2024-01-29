use checker::{synthesis::EznoParser, CheckOutput};
use std::{
	collections::{HashMap, HashSet},
	path::{Path, PathBuf},
};

pub struct CheckingOutputWithoutDiagnostics {
	pub type_mappings: checker::TypeMappings,
	pub types: checker::types::TypeStore,
	pub module_contents: parser::source_map::MapFileStore<parser::source_map::WithPathMap>,
	pub modules: HashMap<
		parser::SourceId,
		checker::features::modules::SynthesisedModule<
			<EznoParser as checker::ASTImplementation>::OwnedModule,
		>,
	>,
}

impl CheckingOutputWithoutDiagnostics {
	#[must_use]
	pub fn is_function_called(&self, function_id: checker::FunctionId) -> bool {
		self.types.called_functions.contains(&function_id)
	}

	pub fn get_type_at_position(&self, _path: &str, _pos: u32) -> Option<String> {
		todo!("print_type needs context, should take facts chain")
		// let source =
		// 	self.module_contents.get_source_at_path(path.into()).expect("no source at path");
		// self.type_mappings.expressions_to_instances.get(pos).map(|instance| {
		// 	let value = instance.get_value();
		// 	checker::types::printing::print_type(value, types, ctx, debug)
		// })
	}
}

pub fn check<T: crate::ReadFromFS>(
	entry_points: Vec<PathBuf>,
	read_from_filesystem: &T,
	type_definition_module: Option<&Path>,
	type_check_options: Option<checker::TypeCheckOptions>,
) -> CheckOutput<checker::synthesis::EznoParser> {
	let definitions = if let Some(tdm) = type_definition_module {
		HashSet::from_iter(std::iter::once(tdm.into()))
	} else {
		HashSet::from_iter(std::iter::once(checker::INTERNAL_DEFINITION_FILE_PATH.into()))
	};

	let read_from_fs = |path: &Path| {
		if path == Path::new(checker::INTERNAL_DEFINITION_FILE_PATH) {
			Some(checker::INTERNAL_DEFINITION_FILE.to_owned())
		} else {
			read_from_filesystem.get_content_at_path(path)
		}
	};

	checker::check_project(entry_points, definitions, read_from_fs, type_check_options, ())
}
