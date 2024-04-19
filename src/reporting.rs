use codespan_reporting::{
	diagnostic::{Diagnostic, Label, Severity},
	term::{
		emit,
		termcolor::{ColorChoice, StandardStream},
		Config, DisplayStyle,
	},
};
use parser::{
	source_map::{FileSystem, MapFileStore, PathMap},
	SourceId,
};

fn ezno_diagnostic_to_severity(kind: &checker::DiagnosticKind) -> Severity {
	match kind {
		checker::DiagnosticKind::Error => Severity::Error,
		checker::DiagnosticKind::Warning => Severity::Warning,
		checker::DiagnosticKind::Info => Severity::Note,
	}
}

/// If pretty printing, it looks nice to include the message under the label, rather than as a heading. However under
/// compact mode the label isn't printed, so instead do the opposite in the compact case
fn checker_diagnostic_to_codespan_diagnostic(
	diagnostic: checker::Diagnostic,
	compact: bool,
) -> Diagnostic<SourceId> {
	match diagnostic {
		checker::Diagnostic::Global { reason, kind } => Diagnostic {
			severity: ezno_diagnostic_to_severity(&kind),
			code: None,
			message: reason,
			labels: Vec::new(),
			notes: Vec::default(),
		},
		checker::Diagnostic::Position { reason, position, kind } => {
			let (message, labels) = if compact {
				(reason, Vec::new())
			} else {
				(
					String::new(),
					vec![Label::primary(position.source, position).with_message(reason)],
				)
			};

			Diagnostic {
				severity: ezno_diagnostic_to_severity(&kind),
				code: None,
				message,
				labels,
				notes: Vec::default(),
			}
		}
		checker::Diagnostic::PositionWithAdditionalLabels { reason, position, labels, kind } => {
			let mut diagnostic = Diagnostic {
				severity: ezno_diagnostic_to_severity(&kind),
				code: None,
				message: String::new(),
				labels: Vec::new(),
				notes: Vec::new(),
			};

			if compact {
				diagnostic.message = reason;
			} else {
				let main_label = Label::primary(position.source, position).with_message(reason);
				diagnostic.labels.push(main_label);

				for (message, position) in labels {
					if let Some(position) = position {
						diagnostic.labels.push(
							Label::secondary(position.source, position).with_message(message),
						);
					} else {
						diagnostic.notes.push(message)
					}
				}
			}

			diagnostic
		}
	}
}

pub(crate) fn emit_diagnostics<T: PathMap>(
	diagnostics: impl IntoIterator<Item = checker::Diagnostic>,
	fs: &MapFileStore<T>,
	compact: bool,
) -> Result<(), codespan_reporting::files::Error> {
	// TODO custom here
	let config = Config {
		display_style: if compact { DisplayStyle::Short } else { DisplayStyle::Rich },
		..Config::default()
	};

	#[cfg(target_family = "wasm")]
	{}

	#[cfg(not(target_family = "wasm"))]
	let mut writer = StandardStream::stderr(ColorChoice::Auto);

	let files = fs.into_code_span_store();

	for diagnostic in diagnostics {
		let diagnostic = checker_diagnostic_to_codespan_diagnostic(diagnostic, compact);

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
