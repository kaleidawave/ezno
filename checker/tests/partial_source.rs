#[cfg(feature = "ezno-parser")]
#[test]
fn partial_checking() {
	use ezno_checker::{check_project, synthesis, TypeCheckOptions};

	// Below source has several issues
	let root = "index.ts";
	let text = "let x: 2 = 5 + ;
	const y = 
	const z = 2;
	function func(a: ) { }
	
	func(b)";

	let definition_file = ezno_checker::INTERNAL_DEFINITION_FILE_PATH.into();
	let type_definition_files = vec![definition_file];

	// `lsp_mode` <=> partial syntax
	let options = TypeCheckOptions { lsp_mode: true, ..Default::default() };

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
	assert_eq!(diagnostics.first().unwrap().reason(), "Could not find variable 'b' in scope");
}
