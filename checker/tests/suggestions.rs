const SIMPLE_DTS: Option<&str> = Some(include_str!("../definitions/simple.d.ts"));

#[cfg(feature = "ezno-parser")]
#[test]
fn suggestions() {
	use ezno_checker::{check_project, Diagnostic, TypeCheckOptions};
	use std::path::{Path, PathBuf};

	let root = "index.ts";
	let text = "
const something = 2, anything = 4; 
console.log(sothing);

const invalidType: Aray = [];

const obj = { property: 2, else: 2 };
console.log(obj.proberly);
const obj2 = { };
console.log(obj2.proberly);
";

	let definition_file_name: PathBuf = if SIMPLE_DTS.is_some() {
		"./checker/definitions/simple.d.ts".into()
	} else {
		ezno_checker::INTERNAL_DEFINITION_FILE_PATH.into()
	};
	let type_definition_files = vec![definition_file_name.clone()];

	let resolver = |path: &Path| -> Option<Vec<u8>> {
		if path == definition_file_name.as_path() {
			Some(SIMPLE_DTS.unwrap().to_owned().into_bytes())
		} else {
			Some(text.into())
		}
	};

	// `store_expression_type_mappings` important
	let options = TypeCheckOptions { store_type_mappings: true, ..Default::default() };

	let result = check_project::<_, ezno_checker::synthesis::EznoParser>(
		vec![root.into()],
		type_definition_files,
		resolver,
		options,
		(),
		None,
	);

	let diagnostics = result.diagnostics.into_iter().collect::<Vec<_>>();

	// Diagnostics doesn't implement PartialEq, so this is fine. Just want to check the [] + labels, not positions
	match_deref::match_deref! {
		match &diagnostics {
			Deref @ [
				Diagnostic::PositionWithAdditionalLabels {
					reason: Deref @ "Could not find type 'Aray'",
					labels: Deref @ [(Deref @ "Did you mean 'Array'?", _)],
					..
				},
				Diagnostic::PositionWithAdditionalLabels {
					reason: Deref @ "Could not find variable 'sothing' in scope",
					labels: Deref @ [(Deref @ "Did you mean 'something'?", _)],
					..
				},
				Diagnostic::PositionWithAdditionalLabels {
					reason: Deref @ "No property 'proberly' on { property: 2, else: 2 }",
					labels: Deref @ [(Deref @ "Did you mean 'property'?", _)],
					..
				},
				Diagnostic::PositionWithAdditionalLabels {
					reason: Deref @ "No property 'proberly' on {}",
					labels: Deref @ [],
					..
				},
			] => {},
			_ => panic!("{diagnostics:#?} did not match diagnostics"),
		}
	};
}
