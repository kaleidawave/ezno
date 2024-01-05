use std::{
	mem,
	path::{Path, PathBuf},
};

use checker::{DiagnosticsContainer, TypeCheckOptions};
use parser::{
	source_map::{MapFileStore, WithPathMap},
	SourceId, Span, ToStringOptions,
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
	input_paths: Vec<PathBuf>,
	fs_resolver: &T,
	type_definition_module: Option<&Path>,
	output_path: &Path,
	config: &BuildConfig,
	transformers: Option<EznoParsePostCheckVisitors>,
) -> Result<BuildOutput, FailedBuildOutput> {
	// TODO parse options + non_standard_library & non_standard_syntax
	let type_check_options =
		TypeCheckOptions { store_expression_type_mappings: true, ..Default::default() };
	let (diagnostics, data_and_module) =
		crate::check(input_paths, fs_resolver, type_definition_module, Some(type_check_options));

	match data_and_module {
		Ok(mut data) => {
			// TODO For all modules
			let keys = data.modules.keys().cloned().collect::<Vec<_>>();

			let null_module = parser::Module {
				items: Default::default(),
				source: SourceId::NULL,
				span: Span::NULL_SPAN,
			};

			let mut outputs = Vec::new();

			let mut transformers = transformers.unwrap_or_default();

			for source in keys {
				// Remove the module
				let mut module = mem::replace(
					&mut data.modules.get_mut(&source).unwrap().content,
					null_module.clone(),
				);

				// TODO bundle using main_module.imports

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

				outputs.push(Output {
					output_path: output_path.to_path_buf(),
					content,
					// TODO module.to_string_with_map
					mappings: String::new(),
				})
			}

			Ok(BuildOutput { outputs, diagnostics, fs: data.module_contents })
		}
		Err(err) => Err(FailedBuildOutput { diagnostics, fs: err }),
	}
}
