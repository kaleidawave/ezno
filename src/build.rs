use std::{collections::HashMap, mem, path::PathBuf};

use checker::TypeCheckOptions;
use parser::{
	source_map::{SourceId, SourceMap, WithPathMap},
	ToStringOptions,
};

#[cfg_attr(target_family = "wasm", derive(serde::Serialize, tsify::Tsify))]
pub struct Output {
	pub output_path: PathBuf,
	pub content: String,
	#[cfg_attr(target_family = "wasm", serde(skip_serializing))]
	pub mappings: SourceMap,
}

pub struct BuildOutput {
	pub artifacts: Vec<Output>,
	pub check_output: checker::CheckOutput<checker::synthesis::EznoParser>,
}

pub struct FailedBuildOutput(pub checker::CheckOutput<checker::synthesis::EznoParser>);

#[cfg_attr(target_family = "wasm", derive(serde::Deserialize, tsify::Tsify), serde(default))]
pub struct BuildConfig {
	pub tree_shake: bool,
	pub strip_whitespace: bool,
	pub source_maps: bool,
	/// Run checker with partial syntax support
	pub lsp_mode: bool,
	pub output_path: PathBuf,
	pub type_definition_module: Option<PathBuf>,
	#[cfg_attr(target_family = "wasm", serde(skip))]
	pub other_transformers: Option<EznoParsePostCheckVisitors>,
}

impl Default for BuildConfig {
	fn default() -> BuildConfig {
		BuildConfig {
			tree_shake: false,
			strip_whitespace: true,
			source_maps: false,
			lsp_mode: false,
			type_definition_module: None,
			// TODO not sure
			output_path: PathBuf::from("out.js"),
			other_transformers: None,
		}
	}
}

pub type EznoParsePostCheckVisitors =
	parser::visiting::VisitorsMut<CheckingOutputWithoutDiagnostics>;

pub type OwnedEznoModule =
	<checker::synthesis::EznoParser as checker::ASTImplementation>::OwnedModule;
pub type SynthesisedEznoModule = checker::features::modules::SynthesisedModule<OwnedEznoModule>;

/// Subset of check output which is nicer for transformers to inferface on
pub struct CheckingOutputWithoutDiagnostics {
	pub types: checker::types::TypeStore,
	pub module_contents: parser::source_map::MapFileStore<WithPathMap>,
	pub modules: HashMap<SourceId, SynthesisedEznoModule>,
}

impl CheckingOutputWithoutDiagnostics {
	#[must_use]
	pub fn is_function_called(&self, function_id: checker::FunctionId) -> bool {
		self.types.called_functions.contains(&function_id)
	}
}

#[allow(clippy::result_large_err)]
pub fn build<T: crate::ReadFromFS>(
	entry_points: Vec<PathBuf>,
	fs_resolver: &T,
	config: BuildConfig,
) -> Result<BuildOutput, FailedBuildOutput> {
	// TODO parse options + non_standard_library & non_standard_syntax
	let type_check_options = TypeCheckOptions {
		store_type_mappings: true,
		lsp_mode: config.lsp_mode,
		..Default::default()
	};

	let result = crate::check(
		entry_points,
		fs_resolver,
		config.type_definition_module.as_deref(),
		type_check_options,
	);

	if !result.diagnostics.contains_error() {
		let checker::CheckOutput {
			diagnostics,
			module_contents,
			chronometer,
			types,
			modules,
			top_level_information,
		} = result;
		let mut data = CheckingOutputWithoutDiagnostics { module_contents, modules, types };

		// TODO For all modules
		let keys = data.modules.keys().cloned().collect::<Vec<_>>();

		let null_module = parser::Module {
			hashbang_comment: None,
			items: Default::default(),
			span: parser::source_map::Nullable::NULL,
		};

		let mut artifacts = Vec::new();
		let mut transformers = config.other_transformers.unwrap_or_default();

		if config.tree_shake {
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

			let mut to_string_options = if config.strip_whitespace {
				ToStringOptions::minified()
			} else {
				ToStringOptions::default()
			};

			// TODO temp fix
			if config.lsp_mode {
				to_string_options.expect_markers = true;
			}

			// TODO source map creation not neccessary

			let (content, mappings) =
				module.to_string_with_source_map(&to_string_options, source, &data.module_contents);

			artifacts.push(Output {
				output_path: config.output_path.to_path_buf(),
				content,
				mappings: mappings.unwrap(),
			});
		}

		// Reconstruct
		let check_output = checker::CheckOutput {
			module_contents: data.module_contents,
			modules: data.modules,
			types: data.types,
			diagnostics,
			chronometer,
			top_level_information,
		};

		Ok(BuildOutput { artifacts, check_output })
	} else {
		Err(FailedBuildOutput(result))
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

		let config = BuildConfig { tree_shake: true, ..Default::default() };

		let build_result = build(
			vec!["index.tsx".into()],
			&|_path: &std::path::Path| Some(source.to_owned()),
			config,
		);

		match build_result {
			Ok(output) => {
				let first_source = &output.artifacts[0].content;
				let expectation = "function make_observable(obj){return new Proxy(obj,{get(on,prop,_rec){return on[prop]}})}function get_a(){return 1};const obj={a(){return get_a()},b:null,c:2};const value=make_observable(obj);const a_value=value.a();const c_value=value.c";

				pretty_assertions::assert_eq!(first_source, expectation);
			}
			Err(output) => {
				panic!(
					"build failed:\n{diagnostics:#?}",
					diagnostics = output.0.diagnostics.get_diagnostics()
				);
			}
		}
	}
}
