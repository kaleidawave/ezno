#[cfg(feature = "ezno-parser")]
#[test]
fn type_mappings() {
	use ezno_checker::{check_project, synthesis, TypeCheckOptions};
	use std::collections::HashSet;

	// Below source has several issues
	let text = "let x: 2 = 5 + ;
	const y = 
	const z = 2;
	function func(a: ) { }
	
	func(b)";

	let definition_file = ezno_checker::INTERNAL_DEFINITION_FILE_PATH.into();
	let type_definition_files = HashSet::from_iter([definition_file]);

	// `lsp_mode` <=> partial syntax
	let options = TypeCheckOptions { lsp_mode: true, ..Default::default() };

	let root = "index.ts";

	let result = check_project::<_, synthesis::EznoParser>(
		vec![root.into()],
		type_definition_files,
		|_path: &std::path::Path| Some(text.to_owned()),
		options,
		(),
		None,
	);

	let diagnostics: Vec<_> = result.diagnostics.into_iter().collect();
	assert_eq!(diagnostics.len(), 1);
	assert_eq!(diagnostics.first().unwrap().reason(), "Could not find variable 'b' in scope. Did you mean x, y or z");
}
