use checker::DiagnosticsContainer;
use parser::{
	source_map::{MapFileStore, WithPathMap},
	ASTNode, ToStringOptions,
};
use std::{
	collections::HashSet,
	path::{Path, PathBuf},
};

type EznoCheckerData = checker::PostCheckData<parser::Module>;

pub fn check<T: crate::FSResolver>(
	fs_resolver: &T,
	input: &Path,
	type_definition_module: Option<&Path>,
) -> (checker::DiagnosticsContainer, Result<EznoCheckerData, MapFileStore<WithPathMap>>) {
	let definitions = if let Some(tdm) = type_definition_module {
		HashSet::from_iter(std::iter::once(tdm.into()))
	} else {
		HashSet::from_iter(std::iter::once(checker::INTERNAL_DEFINITION_FILE_PATH.into()))
	};

	let (diagnostics, data) = checker::check_project(input.to_path_buf(), definitions, |path| {
		if path == Path::new(checker::INTERNAL_DEFINITION_FILE_PATH) {
			Some(checker::INTERNAL_DEFINITION_FILE.to_owned())
		} else {
			fs_resolver.get_content_at_path(path)
		}
	});

	(diagnostics, data)
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

pub fn build<T: crate::FSResolver>(
	fs_resolver: &T,
	input_path: &Path,
	type_definition_module: Option<&Path>,
	output_path: &Path,
	minify_output: bool,
) -> Result<BuildOutput, FailedBuildOutput> {
	let (diagnostics, data_and_module) = check(fs_resolver, input_path, type_definition_module);

	match data_and_module {
		Ok(mut data) if !diagnostics.has_error() => {
			// TODO For all modules

			let main_module = data.modules.remove(&data.entry_source).unwrap();
			let to_string_options = if minify_output {
				ToStringOptions::minified()
			} else {
				ToStringOptions::default()
			};

			// TODO bundle using main_module.imports

			let mut module = main_module.content;

			// pass through transformers
			let expression_visitors_mut: Vec<
				Box<(dyn parser::visiting::VisitorMut<parser::Expression, _> + 'static)>,
			> = Default::default();

			//  = vec![Box::new(crate::transformers::type_to_js::RuntimeTypeCompiler)];

			module.visit_mut::<EznoCheckerData>(
				&mut parser::visiting::VisitorsMut {
					expression_visitors_mut,
					..Default::default()
				},
				&mut data,
				&parser::visiting::VisitSettings::default(),
			);

			let content = module.to_string(&to_string_options);
			let main_output = Output {
				output_path: output_path.to_path_buf(),
				content,
				// TODO module.to_string_with_map
				mappings: String::new(),
			};

			Ok(BuildOutput { outputs: vec![main_output], diagnostics, fs: data.module_contents })
		}
		Ok(data) => Err(FailedBuildOutput { diagnostics, fs: data.module_contents }),
		Err(err) => Err(FailedBuildOutput { diagnostics, fs: err }),
	}
}
