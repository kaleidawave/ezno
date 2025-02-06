use checker::CheckOutput;
use std::path::{Path, PathBuf};

pub fn check(
	entry_points: Vec<PathBuf>,
	read_from_filesystem: crate::FSFunction,
	type_definition_module: Option<&Path>,
	type_check_options: checker::TypeCheckOptions,
) -> CheckOutput<crate::FSFunction, checker::synthesis::EznoParser> {
	let definitions = if let Some(tdm) = type_definition_module {
		vec![tdm.into()]
	} else {
		vec![checker::INTERNAL_DEFINITION_FILE_PATH.into()]
	};

	checker::check_project(
		entry_points,
		definitions,
		read_from_filesystem,
		type_check_options,
		(),
		None,
	)
}
