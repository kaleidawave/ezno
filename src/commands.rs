use checker::DiagnosticsContainer;
use parser::{
	source_map::{MapFileStore, NoPathMap},
	ASTNode, ToStringOptions,
};
use std::{
	collections::HashSet,
	path::{Path, PathBuf},
};

pub fn check<T: crate::FSResolver>(
	fs_resolver: &T,
	input: &Path,
	type_definition_module: Option<&Path>,
) -> (
	MapFileStore<NoPathMap>,
	checker::DiagnosticsContainer,
	Result<(checker::synthesis::module::PostCheckData, parser::Module), ()>,
) {
	// let _cwd = env::current_dir().unwrap();

	// TODO temp
	let mut fs = parser::source_map::MapFileStore::default();
	let content = fs_resolver.get_content_at_path(input).expect("No file");
	let source = parser::source_map::FileSystem::new_source_id(
		&mut fs,
		input.to_path_buf(),
		content.clone(),
	);
	let module =
		parser::Module::from_string(content, parser::ParseOptions::default(), source, None);

	let module = match module {
		Ok(module) => module,
		Err(error) => {
			let mut diagnostics = checker::DiagnosticsContainer::new();
			diagnostics.add_error((error, source));
			return (fs, diagnostics, Err(()));
		}
	};

	let definitions = if let Some(tdm) = type_definition_module {
		HashSet::from_iter(std::iter::once(tdm.into()))
	} else {
		HashSet::from_iter(std::iter::once(checker::INTERNAL_DEFINITION_FILE_PATH.into()))
	};

	let (diagnostics, data) =
		checker::synthesis::module::synthesize_module_root(&module, definitions, |path| {
			if path == Path::new(checker::INTERNAL_DEFINITION_FILE_PATH) {
				Some(checker::INTERNAL_DEFINITION_FILE.to_owned())
			} else {
				fs_resolver.get_content_at_path(path)
			}
		});

	(fs, diagnostics, data.map(|data| (data, module)))
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
	pub diagnostics: DiagnosticsContainer,
	/// For diagnostics
	/// TODO serde
	#[cfg_attr(target_family = "wasm", serde(skip))]
	pub fs: MapFileStore<NoPathMap>,
}

#[cfg_attr(target_family = "wasm", derive(serde::Serialize))]
pub struct FailedBuildOutput {
	pub diagnostics: DiagnosticsContainer,
	/// For diagnostics
	/// TODO serde
	#[cfg_attr(target_family = "wasm", serde(skip))]
	pub fs: MapFileStore<NoPathMap>,
}

pub fn build<T: crate::FSResolver>(
	fs_resolver: &T,
	input_path: &Path,
	type_definition_module: Option<&Path>,
	output_path: &Path,
	minify_output: bool,
) -> Result<BuildOutput, FailedBuildOutput> {
	let (fs, diagnostics, data_and_module) = check(fs_resolver, input_path, type_definition_module);

	if let (false, Ok((_data, module))) = (diagnostics.has_error(), data_and_module) {
		// TODO for all modules
		// TODO pass through transformers
		let to_string_options =
			if minify_output { ToStringOptions::minified() } else { ToStringOptions::default() };
		let content = module.to_string(&to_string_options);
		let main_output = Output {
			output_path: output_path.to_path_buf(),
			content,
			// TODO temp
			mappings: String::new(),
		};
		Ok(BuildOutput { outputs: vec![main_output], diagnostics, fs })
	} else {
		Err(FailedBuildOutput { fs, diagnostics })
	}
}
