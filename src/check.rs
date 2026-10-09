use crate::reporting::{checker_diagnostic_to_codespan_diagnostic, report_diagnostics_to_cli};
use crate::utilities::{print_to_cli, MaxDiagnostics};
pub use checker::{CheckOutput, TypeCheckOptions};
use std::path::{Path, PathBuf};

pub fn check<T: crate::ReadFromFS>(
	entry_points: Vec<PathBuf>,
	read_from_filesystem: &T,
	type_definition_module: Option<&Path>,
	type_check_options: checker::TypeCheckOptions,
) -> CheckOutput<checker::synthesis::EznoParser> {
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
		None,
	)
}

/// CLI entrypoint
pub fn check_and_report<T: crate::ReadFromFS>(
	entry_points: Vec<PathBuf>,
	read_file: &T,
	timings: bool,
	definition_file: Option<PathBuf>,
	max_diagnostics: MaxDiagnostics,
	type_check_options: checker::TypeCheckOptions,
	compact_diagnostics: bool,
) -> std::process::ExitCode {
	let result = check(entry_points, read_file, definition_file.as_deref(), type_check_options);

	let CheckOutput { diagnostics, module_contents, chronometer, types, .. } = result;

	let diagnostics_count = diagnostics.count();
	let found_error = diagnostics.contains_error();
	let current = timings.then(std::time::Instant::now);

	let diagnostics = diagnostics.into_iter().map(|diagnostic| {
		checker_diagnostic_to_codespan_diagnostic(diagnostic, compact_diagnostics)
	});

	report_diagnostics_to_cli(diagnostics, &module_contents, compact_diagnostics, max_diagnostics)
		.unwrap();

	let result = if found_error {
		std::process::ExitCode::FAILURE
	} else {
		print_to_cli(format_args!("No type errors found 🎉"));
		std::process::ExitCode::SUCCESS
	};

	#[cfg(not(target_family = "wasm"))]
	if timings {
		let checker::Chronometer { lines, cached, fs, parse, check, narrowing } = chronometer;
		let reporting = current.unwrap().elapsed();
		let type_count = types.count_of_types();

		eprintln!("---");
		eprintln!("Diagnostics: {diagnostics_count}",);
		eprintln!("Types:       {type_count}");
		eprintln!("Lines:       {lines}");
		eprintln!("Cache read:  {cached:?}");
		eprintln!("FS read:     {fs:?}");
		eprintln!("Parsed in:   {parse:?}");
		eprintln!("Checked in:  {check:?}");
		eprintln!("Narrowing:   {narrowing:?}");
		eprintln!("Reporting:   {reporting:?}");
	}

	result
}
