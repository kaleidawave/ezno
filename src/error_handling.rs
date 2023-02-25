use std::{collections::HashMap, env, iter};

use checker::Diagnostic as CheckerDiagnostic;
use codespan_reporting::{
	diagnostic::{Diagnostic, Label},
	files::SimpleFiles,
	term::{
		termcolor::{ColorChoice, StandardStream},
		Config,
	},
};
use parser::SourceId;

const ADDITIONAL_LINES_IN_DIAGNOSTIC: usize = 0;

pub(crate) fn print_error_warning_info_handler(error_handler: checker::ErrorWarningInfoHandler) {
	let mut files = SimpleFiles::new();
	let mut file_id_to_source_id = HashMap::<SourceId, usize>::new();

	// Handling adding filename-file id mappings
	for source_id in error_handler.sources() {
		let (filename, file_content) = source_id.get_file().unwrap();
		let name =
			filename.strip_prefix(env::current_dir().unwrap()).unwrap_or(&filename).to_owned();
		let file_id = files.add(name.display().to_string(), file_content);
		file_id_to_source_id.insert(source_id, file_id);
	}

	for item in error_handler.into_iter().rev() {
		// TODO tidy this up:
		let (diagnostic, info) = match item {
			checker::ErrorWarningInfo::Error(error) => (Diagnostic::error(), error),
			checker::ErrorWarningInfo::Warning(warning) => (Diagnostic::warning(), warning),
			checker::ErrorWarningInfo::Info(info) => (Diagnostic::note(), info),
			checker::ErrorWarningInfo::Data(_) => {
				continue;
			}
		};

		let diagnostic =
			checker_diagnostic_to_code_span_diagnostic(diagnostic, info, &file_id_to_source_id);

		emit(&files, &diagnostic);

		#[cfg(target_arch = "wasm")]
		fn emit<'a, F: codespan_reporting::files::Files<'a>>(
			files: &F,
			diagnostic: &Diagnostic<F::FileId>,
		) {
			todo!("buffer then print")
		}

		#[cfg(not(target_arch = "wasm"))]
		fn emit<'a, F: codespan_reporting::files::Files<'a>>(
			files: &'a F,
			diagnostic: &Diagnostic<F::FileId>,
		) {
			let writer = StandardStream::stderr(ColorChoice::Always);

			// TODO lines in diagnostic could be different
			codespan_reporting::term::emit(
				&mut writer.lock(),
				&Config {
					before_label_lines: ADDITIONAL_LINES_IN_DIAGNOSTIC,
					after_label_lines: ADDITIONAL_LINES_IN_DIAGNOSTIC,
					..Default::default()
				},
				files,
				diagnostic,
			)
			.unwrap();
		}
	}
}

fn checker_diagnostic_to_code_span_diagnostic(
	diagnostic: Diagnostic<usize>,
	information: CheckerDiagnostic,
	source_map: &HashMap<SourceId, usize>,
) -> Diagnostic<usize> {
	match information {
		CheckerDiagnostic::Global(message) => diagnostic.with_message(message),
		CheckerDiagnostic::Position { reason: message, pos } => {
			diagnostic.with_labels(vec![Label::primary(
				*source_map.get(&pos.source_id).unwrap(),
				pos,
			)
			.with_message(message)])
		}
		CheckerDiagnostic::PositionWithAdditionLabels { reason, pos, labels } => {
			let (labels, notes) =
				labels.into_iter().partition::<Vec<_>, _>(|(_, value)| value.is_some());

			diagnostic
				.with_labels(
					iter::once(
						Label::primary(*source_map.get(&pos.source_id).unwrap(), pos)
							.with_message(reason),
					)
					.chain(labels.into_iter().map(|(message, pos)| {
						let pos = pos.unwrap();
						Label::secondary(*source_map.get(&pos.source_id).unwrap(), pos)
							.with_message(message)
					}))
					.collect(),
				)
				.with_notes(notes.into_iter().map(|(message, _)| message).collect())
		}
	}
}
