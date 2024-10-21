use std::{
	mem,
	path::{Path, PathBuf},
};

use checker::{DiagnosticsContainer, TypeCheckOptions};
use parser::{
	source_map::{MapFileStore, SourceMap, WithPathMap},
	ToStringOptions,
};

#[cfg_attr(target_family = "wasm", derive(serde::Serialize, tsify::Tsify))]
pub struct Output {
	pub output_path: PathBuf,
	pub content: String,
	#[cfg_attr(target_family = "wasm", serde(skip_serializing))]
	pub mappings: SourceMap,
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
	#[cfg_attr(target_family = "wasm", serde(default))]
	pub source_maps: bool,
	#[cfg_attr(target_family = "wasm", serde(default))]
	pub optimise: bool,
}

pub type EznoParsePostCheckVisitors =
	parser::visiting::VisitorsMut<CheckingOutputWithoutDiagnostics>;

pub struct CheckingOutputWithoutDiagnostics {
	pub types: checker::types::TypeStore,
	pub module_contents: parser::source_map::MapFileStore<parser::source_map::WithPathMap>,
	pub modules: std::collections::HashMap<
		parser::SourceId,
		checker::features::modules::SynthesisedModule<
			<checker::synthesis::EznoParser as checker::ASTImplementation>::OwnedModule,
		>,
	>,
}

impl CheckingOutputWithoutDiagnostics {
	#[must_use]
	pub fn is_function_called(&self, function_id: checker::FunctionId) -> bool {
		self.types.called_functions.contains(&function_id)
	}
}

pub fn build<T: crate::ReadFromFS>(
	entry_points: Vec<PathBuf>,
	fs_resolver: &T,
	type_definition_module: Option<&Path>,
	output_path: &Path,
	config: &BuildConfig,
) -> Result<BuildOutput, FailedBuildOutput> {
	// TODO parse options + non_standard_library & non_standard_syntax
	let type_check_options = TypeCheckOptions { store_type_mappings: true, ..Default::default() };

	let result =
		crate::check(entry_points, fs_resolver, type_definition_module, type_check_options);

	let mut data = CheckingOutputWithoutDiagnostics {
		module_contents: result.module_contents,
		modules: result.modules,
		types: result.types,
	};

	if !result.diagnostics.has_error() {
		// TODO For all modules
		let keys = data.modules.keys().cloned().collect::<Vec<_>>();

		let null_module = parser::Module {
			hashbang_comment: None,
			items: Default::default(),
			span: parser::source_map::Nullable::NULL,
		};

		let mut outputs = Vec::new();

		let mut transformers = EznoParsePostCheckVisitors::default();

		if config.optimise {
			transformers
				.expression_visitors_mut
				.push(Box::new(crate::transformers::optimisations::ExpressionOptimiser));

			transformers
				.statement_visitors_mut
				.push(Box::new(crate::transformers::optimisations::StatementOptimiser));
		}

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

			// TODO under cfg

			let (content, mappings) =
				module.to_string_with_source_map(&to_string_options, source, &data.module_contents);

			outputs.push(Output {
				output_path: output_path.to_path_buf(),
				content,
				mappings: mappings.unwrap(),
			})
		}

		Ok(BuildOutput { outputs, diagnostics: result.diagnostics, fs: data.module_contents })
	} else {
		Err(FailedBuildOutput { diagnostics: result.diagnostics, fs: data.module_contents })
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn tree_shaking() {
		let source = r#"
	function make_observable(obj) {
		return new Proxy(obj, {
			get(on, prop: string, _rec) {
				return on[prop]
			},
		})
	}

	function get_a() {
		return 1
	}

	function get_b() {
		return 1
	}

	const obj = {
		a() { return get_a() },
		b() { return get_b() },
		c: 2
	}

	const value = make_observable(obj);
	const a_value = value.a();
	const c_value = value.c;
		"#;

		let cfg = BuildConfig { optimise: true, ..Default::default() };

		let output =
			build(vec!["index.tsx"], |_| Some(source.to_owned()), None, "out.tsx").unwrap();

		let first_source = output.outputs.content;

		// TODO assert equal
		panic!("{first_source:?}");
	}
}
