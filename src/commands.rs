use parser::{source_map::MapFileStore, ASTNode, ParseOptions, SourceId, ToStringOptions};
use std::{
	collections::HashSet,
	path::{Path, PathBuf},
};

pub fn check<T: crate::FSResolver>(
	fs_resolver: &T,
	input: &Path,
	type_definition_module: Option<&Path>,
) -> (
	MapFileStore,
	checker::DiagnosticsContainer,
	Result<checker::synthesis::module::PostCheckData, ()>,
) {
	// let _cwd = env::current_dir().unwrap();

	// TODO temp
	let mut fs = parser::source_map::MapFileStore::default();
	let content = fs_resolver.get_content_at_path(&input).expect("No file");
	let source = parser::source_map::FileSystem::new_source_id(
		&mut fs,
		input.to_path_buf(),
		content.clone(),
	);
	let module = parser::Module::from_string(
		content,
		parser::ParseOptions::default(),
		source,
		None,
		Vec::new(),
	);

	let module = match module {
		Ok(module) => module,
		Err(error) => {
			let mut diagnostics = checker::DiagnosticsContainer::new();
			diagnostics.add_error(error);
			return (fs, diagnostics, Err(()));
		}
	};

	let definitions = if let Some(tdm) = type_definition_module {
		HashSet::from_iter(std::iter::once(tdm.into()))
	} else {
		HashSet::from_iter(std::iter::once(checker::INTERNAL_DEFINITION_FILE_PATH.into()))
	};

	let result = checker::synthesis::module::synthesize_module_root(&module, definitions, |path| {
		if path == Path::new(checker::INTERNAL_DEFINITION_FILE_PATH) {
			Some(checker::INTERNAL_DEFINITION_FILE.to_owned())
		} else {
			fs_resolver.get_content_at_path(path)
		}
	});

	(fs, result.0, result.1)
}

#[cfg_attr(target_family = "wasm", derive(serde::Serialize))]
pub struct Output {
	pub output_path: PathBuf,
	pub content: String,
	pub mappings: String,
}

#[cfg_attr(target_family = "wasm", derive(serde::Serialize))]
pub struct BuildOutput {
	pub outputs: Vec<Output>,
	pub temp_diagnostics: Vec<checker::Diagnostic>,
}

pub fn build<T: crate::FSResolver>(
	fs_resolver: T,
	input_path: &Path,
	output_path: &Path,
) -> (MapFileStore, Result<BuildOutput, Vec<checker::Diagnostic>>) {
	let mut fs = MapFileStore::default();

	let content = fs_resolver.get_content_at_path(input_path).expect("Could not find/get file");
	let source_id = SourceId::new(&mut fs, PathBuf::from(input_path), content.clone());

	let module_result = parser::Module::from_string(
		content,
		ParseOptions::default(),
		source_id,
		None,
		Default::default(),
	);

	let output = match module_result {
		Ok(t) => t,
		Err(parse_err) => {
			return (fs, Err(vec![parse_err.into()]));
		}
	};

	let temp_diagnostics = Vec::new();

	let (content, source_map) = output.to_string_with_source_map(&ToStringOptions::minified(), &fs);

	let output =
		Output { output_path: output_path.to_path_buf(), content, mappings: source_map.mappings };

	(fs, Ok(BuildOutput { outputs: vec![output], temp_diagnostics }))
}
