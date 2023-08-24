use parser::ASTNode;
use std::{collections::HashSet, env, path::PathBuf};

use crate::error_handling::emit_ezno_diagnostic;

pub(crate) fn check<T: crate::FSResolver>(
	fs_resolver: T,
	input: PathBuf,
	type_definition_module: Option<PathBuf>,
	watch: bool,
) {
	let _cwd = env::current_dir().unwrap();

	// TODO temp
	let mut fs = parser::source_map::MapFileStore::default();
	let content = fs_resolver(&input).expect("No file");
	let source = parser::source_map::FileSystem::new_source_id(&mut fs, input, content.clone());
	let module = parser::Module::from_string(
		content,
		parser::ParseOptions::default(),
		source,
		None,
		Vec::new(),
	);

	let module = match module {
		Ok(module) => module,
		Err(error) => {
			let source_id = error.position.source;
			emit_ezno_diagnostic(error.into(), &fs, source_id).unwrap();
			return;
		}
	};

	let (diagnostics, _events, _root, _mappings, _type_store) =
		checker::synthesis::module::synthesize_module_root(
			&module,
			HashSet::from_iter(type_definition_module.into_iter()),
			fs_resolver,
		);

	for diagnostic in diagnostics.into_iter() {
		emit_ezno_diagnostic(diagnostic, &fs, module.source).unwrap();
	}

	// let check_project = || {
	// if let Err(diagnostics_container) = diagnostics {
	// 	print_diagnostics_container(diagnostics_container);
	// } else {
	// 	print_diagnostics_container(project.pull_diagnostics_container());
	// 	println!("Project checked ✅, No errors 🎉");
	// }
	// };

	if watch {
		todo!()
		// watch_command(cwd, |_path_change| {
		// 	// TODO use _path_change info from change
		// 	let diagnostics_container = project.check(entry_point.clone());
		// 	if let Err(diagnostics_container) = diagnostics_container {
		// 		print_diagnostics_container(diagnostics_container);
		// 	} else {
		// 		print_diagnostics_container(project.pull_diagnostics_container());
		// 		println!("No errors 🎉");
		// 	}
		// });
	}
}
