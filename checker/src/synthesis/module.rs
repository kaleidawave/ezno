use std::{collections::HashSet, path::PathBuf};

use parser::ParseOptions;
use source_map::SourceId;

use crate::{CheckingData, Root, TypeMappings, TypeStore};

use super::block::synthesize_block;

/// TODO temp
pub fn synthesize_module_root<T: crate::FSResolver>(
	module: &parser::Module,
	type_definition_files: HashSet<PathBuf>,
	resolver: T,
) -> (crate::DiagnosticsContainer, Vec<crate::events::Event>, Root, TypeMappings, TypeStore) {
	let default_settings = Default::default();
	let mut checking_data = CheckingData::new(default_settings, &resolver);

	let mut root = if type_definition_files.is_empty() {
		crate::context::Root::new_with_primitive_references()
	} else {
		// TODO concat and then combine
		let path = type_definition_files.iter().next().unwrap();
		let result = resolver(path).unwrap();
		// TODO temp
		let (tdm, _) = parser::TypeDefinitionModule::from_string(
			result,
			ParseOptions::default(),
			SourceId::NULL,
			Vec::new(),
		)
		.unwrap();
		super::definitions::type_definition_file(tdm, &mut checking_data)
	};

	let (_, stuff, _) = root.new_lexical_environment_fold_into_parent(
		crate::context::Scope::Block {},
		&mut checking_data,
		|environment, checking_data| synthesize_block(&module.items, environment, checking_data),
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

	(diagnostics_container, stuff.unwrap().0, root, type_mappings, types)
}
