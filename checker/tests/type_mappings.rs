#[cfg(feature = "ezno-parser")]
#[test]
fn type_mappings() {
	use ezno_checker::{check_project, synthesis, TypeCheckOptions};

	let root = "index.ts";
	let text = "const x: number = 2;
function y() { return x }
y()";

	let definition_file = ezno_checker::INTERNAL_DEFINITION_FILE_PATH.into();
	let type_definition_files = vec![definition_file];

	// `store_expression_type_mappings` important
	let options = TypeCheckOptions { store_type_mappings: true, ..Default::default() };

	let result = check_project::<_, synthesis::EznoParser>(
		vec![root.into()],
		type_definition_files,
		&|_path: &std::path::Path| Some(text.to_owned()),
		options,
		(),
		None,
	);

	let types_at_positions = IntoIterator::into_iter([43, 47, 48])
		.map(|idx| result.get_type_at_position(root, idx, false))
		.collect::<Vec<_>>();

	assert_eq!(
		types_at_positions,
		vec![Some("number".to_owned()), Some("() => number".to_owned()), Some("2".to_owned())]
	);
}
