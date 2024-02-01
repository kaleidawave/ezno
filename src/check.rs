use checker::CheckOutput;
use std::{
	collections::HashSet,
	path::{Path, PathBuf},
};

pub fn check<T: crate::ReadFromFS>(
	entry_points: Vec<PathBuf>,
	read_from_filesystem: &T,
	type_definition_module: Option<&Path>,
	type_check_options: Option<checker::TypeCheckOptions>,
) -> CheckOutput<checker::synthesis::EznoParser> {
	let definitions = if let Some(tdm) = type_definition_module {
		HashSet::from_iter(std::iter::once(tdm.into()))
	} else {
		HashSet::from_iter(std::iter::once(checker::INTERNAL_DEFINITION_FILE_PATH.into()))
	};

	let read_from_fs = |path: &Path| {
		if path == Path::new(checker::INTERNAL_DEFINITION_FILE_PATH) {
			Some(checker::INTERNAL_DEFINITION_FILE.to_owned())
		} else {
			read_from_filesystem.get_content_at_path(path).map(String::into_bytes)
		}
	};

	checker::check_project(entry_points, definitions, read_from_fs, type_check_options, ())
}
