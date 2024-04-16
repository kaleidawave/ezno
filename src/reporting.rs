use std::iter;

use codespan_reporting::{
	diagnostic::{Diagnostic, Label, Severity},
	term::{emit, Config},
};
use parser::{
	source_map::{MapFileStore, PathMap},
	SourceId,
};

fn ezno_diagnostic_to_severity(kind: &checker::DiagnosticKind) -> Severity {
	match kind {
		checker::DiagnosticKind::Error => Severity::Error,
		checker::DiagnosticKind::Warning => Severity::Warning,
		checker::DiagnosticKind::Info => Severity::Note,
	}
}

fn checker_diagnostic_to_codespan_diagnostic(
	diagnostic: checker::Diagnostic,
) -> Diagnostic<SourceId> {
	match diagnostic {
		checker::Diagnostic::Global { reason, kind } => Diagnostic {
			severity: ezno_diagnostic_to_severity(&kind),
			code: None,
			message: reason,
			labels: Vec::new(),
			notes: Vec::default(),
		},
		checker::Diagnostic::Position { reason, position, kind } => Diagnostic {
			severity: ezno_diagnostic_to_severity(&kind),
			code: None,
			message: Default::default(),
			labels: vec![Label::primary(position.source, position).with_message(reason)],
			notes: Vec::default(),
		},
		checker::Diagnostic::PositionWithAdditionalLabels { reason, position, labels, kind } => {
			let (labels, notes) =
				labels.into_iter().partition::<Vec<_>, _>(|(_, value)| value.is_some());

			Diagnostic {
				severity: ezno_diagnostic_to_severity(&kind),
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
	}
}

pub(crate) fn emit_diagnostics<T: PathMap>(
	diagnostics: impl IntoIterator<Item = checker::Diagnostic>,
	fs: &MapFileStore<T>,
) -> Result<(), codespan_reporting::files::Error> {
	use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

	// TODO custom here
	let config = Config::default();

	#[cfg(target_family = "wasm")]
	{}

	#[cfg(not(target_family = "wasm"))]
	let mut writer = StandardStream::stderr(ColorChoice::Auto);

	let files = fs.into_code_span_store();
	// parser::source_map::CodeSpanStore(fs);

	for diagnostic in diagnostics {
		let diagnostic = checker_diagnostic_to_codespan_diagnostic(diagnostic);

		#[cfg(target_family = "wasm")]
		{
			let mut buffer = codespan_reporting::term::termcolor::Buffer::ansi();
			emit(&mut buffer, &config, &files, &diagnostic)?;
			let output =
				String::from_utf8(buffer.into_inner()).expect("invalid string from diagnostic");
			crate::utilities::print_to_cli(format_args!("{output}"));
		}

		#[cfg(not(target_family = "wasm"))]
		emit(&mut writer, &config, &files, &diagnostic)?;
	}

	Ok(())
}
