use std::iter;

use codespan_reporting::{
	diagnostic::{Diagnostic, Label, Severity},
	files::Files,
	term::{emit, Config},
};
use parser::source_map::FileSystem;

pub(crate) fn emit_ezno_diagnostic(
	diagnostic: checker::Diagnostic,
	fs: &impl FileSystem,
) -> Result<(), codespan_reporting::files::Error> {
	let diagnostic = match diagnostic {
		checker::Diagnostic::Global { reason, kind } => Diagnostic {
			severity: ezno_diagnostic_to_severity(kind),
			code: None,
			message: reason,
			labels: Vec::new(),
			notes: Vec::default(),
		},
		checker::Diagnostic::Position { reason, position, kind } => Diagnostic {
			severity: ezno_diagnostic_to_severity(kind),
			code: None,
			message: Default::default(),
			labels: vec![Label::primary(position.source, position).with_message(reason)],
			notes: Vec::default(),
		},
		checker::Diagnostic::PositionWithAdditionLabels { reason, position, labels, kind } => {
			let (labels, notes) =
				labels.into_iter().partition::<Vec<_>, _>(|(_, value)| value.is_some());

			Diagnostic {
				severity: ezno_diagnostic_to_severity(kind),
				code: None,
				message: Default::default(),
				labels: iter::once(Label::primary(position.source, position).with_message(reason))
					.chain(labels.into_iter().map(|(message, position)| {
						let position = position.unwrap();
						Label::secondary(position.source, position).with_message(message)
					}))
					.collect(),
				notes: notes.into_iter().map(|(message, _)| message).collect(),
			}
		}
	};

	emit_diagnostic(&diagnostic, &fs.into_code_span_store())
}

fn ezno_diagnostic_to_severity(kind: checker::DiagnosticKind) -> Severity {
	match kind {
		checker::DiagnosticKind::Error => Severity::Error,
		checker::DiagnosticKind::Warning => Severity::Warning,
		checker::DiagnosticKind::Info => Severity::Note,
	}
}

#[cfg(target_family = "wasm")]
fn emit_diagnostic<'files, F: Files<'files>>(
	diagnostic: &Diagnostic<<F as Files<'files>>::FileId>,
	files: &'files F,
) -> Result<(), codespan_reporting::files::Error> {
	use crate::utilities::print_to_cli;
	use codespan_reporting::term::termcolor::Buffer;

	let config = Config::default();

	let mut buffer = Buffer::ansi();
	emit(&mut buffer, &config, files, diagnostic).unwrap();
	let output = String::from_utf8(buffer.into_inner()).expect("invalid string from diagnostic");
	print_to_cli(format_args!("{output}"));
	Ok(())
}

#[cfg(not(target_family = "wasm"))]
fn emit_diagnostic<'files, F: Files<'files>>(
	diagnostic: &Diagnostic<<F as Files<'files>>::FileId>,
	files: &'files F,
) -> Result<(), codespan_reporting::files::Error> {
	use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

	let config = Config::default();

	let writer = StandardStream::stderr(ColorChoice::Always);
	let mut lock = writer.lock();
	emit(&mut lock, &config, files, diagnostic)
}
