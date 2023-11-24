use std::path::{Path, PathBuf};

use checker::DiagnosticsContainer;
use parser::{
	source_map::{MapFileStore, WithPathMap},
	ToStringOptions,
};
use serde::Deserialize;

use crate::check::EznoCheckerData;

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
	pub fs: MapFileStore<WithPathMap>,
}

#[cfg_attr(target_family = "wasm", derive(serde::Serialize))]
pub struct FailedBuildOutput {
	pub diagnostics: DiagnosticsContainer,
	/// For diagnostics
	/// TODO serde
	#[cfg_attr(target_family = "wasm", serde(skip))]
	pub fs: MapFileStore<WithPathMap>,
}

#[derive(Deserialize)]
pub struct BuildConfig {
	#[serde(default)]
	pub strip_whitespace: bool,
}

pub type EznoParsePostCheckVisitors = parser::visiting::VisitorsMut<EznoCheckerData>;

pub fn build<T: crate::ReadFromFS>(
	fs_resolver: &T,
	input_path: &Path,
	type_definition_module: Option<&Path>,
	output_path: &Path,
	config: BuildConfig,
	transformers: Option<EznoParsePostCheckVisitors>,
) -> Result<BuildOutput, FailedBuildOutput> {
	// TODO parse options + non_standard_library & non_standard_syntax
	let (diagnostics, data_and_module) =
		crate::check(fs_resolver, input_path, type_definition_module);

	match data_and_module {
		Ok(mut data) => {
			// TODO For all modules
			let main_module = data.modules.get_mut(&data.entry_source).unwrap();

			// TODO bundle using main_module.imports

			// TODO !!! DON'T CLONE !!! THE MODULE !!! BUT DON'T WANT TO REMOVE FROM ABOVE !!!
			let mut module = main_module.content.clone();

			let mut transformers = transformers.unwrap_or_default();

			module.visit_mut::<EznoCheckerData>(
				&mut transformers,
				&mut data,
				&parser::visiting::VisitOptions::default(),
			);

			let to_string_options = if config.strip_whitespace {
				ToStringOptions::minified()
			} else {
				ToStringOptions::default()
			};

			let content = parser::ASTNode::to_string(&module, &to_string_options);
			let main_output = Output {
				output_path: output_path.to_path_buf(),
				content,
				// TODO module.to_string_with_map
				mappings: String::new(),
			};

			Ok(BuildOutput { outputs: vec![main_output], diagnostics, fs: data.module_contents })
		}
		Err(err) => Err(FailedBuildOutput { diagnostics, fs: err }),
	}
}
