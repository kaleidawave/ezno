use std::{
	mem,
	path::{Path, PathBuf},
};

use checker::{DiagnosticsContainer, TypeCheckOptions};
use parser::{
	source_map::{MapFileStore, WithPathMap},
	ToStringOptions,
};

use crate::check::CheckingOutputWithoutDiagnostics;

#[cfg_attr(target_family = "wasm", derive(serde::Serialize, tsify::Tsify))]
pub struct Output {
	pub output_path: PathBuf,
	pub content: String,
	pub mappings: String,
}

#[cfg_attr(target_family = "wasm", derive(serde::Serialize, tsify::Tsify))]
pub struct BuildOutput {
	pub outputs: Vec<Output>,
	pub diagnostics: DiagnosticsContainer,
	/// For diagnostics
	/// TODO serde
	#[cfg_attr(target_family = "wasm", serde(skip))]
	pub fs: MapFileStore<WithPathMap>,
}

#[cfg_attr(target_family = "wasm", derive(serde::Serialize, tsify::Tsify))]
pub struct FailedBuildOutput {
	pub diagnostics: DiagnosticsContainer,
	/// For diagnostics
	/// TODO serde
	#[cfg_attr(target_family = "wasm", serde(skip))]
	pub fs: MapFileStore<WithPathMap>,
}

#[cfg_attr(target_family = "wasm", derive(serde::Deserialize))]
pub struct BuildConfig {
	#[cfg_attr(target_family = "wasm", serde(default))]
	pub strip_whitespace: bool,
}

pub type EznoParsePostCheckVisitors =
	parser::visiting::VisitorsMut<CheckingOutputWithoutDiagnostics>;

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

	let result =
		crate::check(input_paths, fs_resolver, type_definition_module, Some(type_check_options));

	let mut data = crate::check::CheckingOutputWithoutDiagnostics {
		module_contents: result.module_contents,
		modules: result.modules,
		type_mappings: result.type_mappings,
		types: result.types,
	};

	if !result.diagnostics.has_error() {
		// TODO For all modules
		let keys = data.modules.keys().cloned().collect::<Vec<_>>();

		let null_module =
			parser::Module { items: Default::default(), span: parser::source_map::Nullable::NULL };

		let mut outputs = Vec::new();

		let mut transformers = transformers.unwrap_or_default();

		for source in keys {
			// Remove the module
			let mut module = mem::replace(
				&mut data.modules.get_mut(&source).unwrap().content,
				null_module.clone(),
			);

			// TODO bundle using main_module.imports

			module.visit_mut::<CheckingOutputWithoutDiagnostics>(
				&mut transformers,
				&mut data,
				&parser::visiting::VisitOptions::default(),
				source,
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

		Ok(BuildOutput { outputs, diagnostics: result.diagnostics, fs: data.module_contents })
	} else {
		Err(FailedBuildOutput { diagnostics: result.diagnostics, fs: data.module_contents })
	}
}
