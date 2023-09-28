use std::{collections::HashSet, path::PathBuf};

use parser::ParseOptions;
use source_map::SourceId;

use crate::{CheckingData, Diagnostic};

use super::block::synthesise_block;

pub struct PostCheckData {
	pub events: Vec<crate::events::Event>,
	pub root: crate::Root,
	pub type_mappings: crate::TypeMappings,
	pub types: crate::types::TypeStore,
}

/// TODO temp
pub fn synthesise_module_root<T: crate::FSResolver>(
	module: &parser::Module,
	type_definition_files: HashSet<PathBuf>,
	resolver: T,
) -> (crate::DiagnosticsContainer, Result<PostCheckData, ()>) {
	let default_settings = Default::default();
	let mut checking_data = CheckingData::new(default_settings, &resolver);

	let mut root = if type_definition_files.is_empty() {
		crate::context::Root::new_with_primitive_references()
	} else {
		// TODO concat and then combine
		let path = type_definition_files.iter().next().unwrap();
		let result = match resolver(path) {
			Some(result) => result,
			None => {
				let mut diagnostics = crate::DiagnosticsContainer::new();
				diagnostics.add_error(Diagnostic::Global {
					reason: format!("could not find {}", path.display()),
					kind: crate::DiagnosticKind::Error,
				});
				return (diagnostics, Err(()));
			}
		};
		// TODO temp
		let s = SourceId::NULL;
		let from_string =
			parser::TypeDefinitionModule::from_string(result, ParseOptions::default(), s);
		let tdm = match from_string {
			Ok(tdm) => tdm,
			Err(err) => {
				let mut diagnostics = crate::DiagnosticsContainer::new();
				diagnostics.add_error((err, s));
				return (diagnostics, Err(()));
			}
		};
		super::definitions::type_definition_file(tdm, &mut checking_data)
	};

	let (_, stuff, _) = root.new_lexical_environment_fold_into_parent(
		crate::context::Scope::Module { source: module.source },
		&mut checking_data,
		|environment, checking_data| synthesise_block(&module.items, environment, checking_data),
	);

	let CheckingData {
		diagnostics_container,
		type_mappings,
		modules,
		settings,
		existing_contexts,
		types,
		unimplemented_items,
	} = checking_data;

	(
		diagnostics_container,
		Ok(PostCheckData { events: stuff.expect("no events").0, root, type_mappings, types }),
	)
}
