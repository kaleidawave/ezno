use codespan_reporting::{
	diagnostic::{Diagnostic, Label, Severity},
	term::{emit, Config, DisplayStyle},
};

use checker::source_map::{MapFileStore, SourceId, WithPathMap};

#[derive(Debug, Copy, Clone)]
pub(crate) enum MaxDiagnostics {
	All,
	FixedTo(u16),
}

impl argh::FromArgValue for MaxDiagnostics {
	fn from_arg_value(value: &str) -> Result<Self, String> {
		if value == "all" {
			Ok(Self::All)
		} else {
			match std::str::FromStr::from_str(value) {
				Ok(value) => Ok(Self::FixedTo(value)),
				Err(reason) => Err(reason.to_string()),
			}
		}
	}
}

impl Default for MaxDiagnostics {
	fn default() -> Self {
		Self::FixedTo(30)
	}
}

fn checker_kind_to_severity(kind: &checker::DiagnosticKind) -> Severity {
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
			severity: checker_kind_to_severity(&kind),
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
				severity: checker_kind_to_severity(&kind),
				code: None,
				message,
				labels,
				notes: Vec::default(),
			}
		}
		checker::Diagnostic::PositionWithAdditionalLabels { reason, position, labels, kind } => {
			let mut diagnostic = Diagnostic {
				severity: checker_kind_to_severity(&kind),
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
					diagnostic
						.labels
						.push(Label::secondary(position.source, position).with_message(message));
				}
			}

			diagnostic
		}
	}
}

pub struct DiagnosticEmitter {
	#[cfg(not(target_family = "wasm"))]
	writer: codespan_reporting::term::termcolor::BufferedStandardStream,
}

impl DiagnosticEmitter {
	pub fn new() -> Self {
		Self {
			// maximum,
			// configuration: Config {
			// 	display_style: if compact { DisplayStyle::Short } else { DisplayStyle::Rich },
			// 	..Config::default()
			// },
			#[cfg(not(target_family = "wasm"))]
			writer: codespan_reporting::term::termcolor::BufferedStandardStream::stderr(
				codespan_reporting::term::termcolor::ColorChoice::Auto,
			),
		}
	}

	// pub fn should_emit(&self) -> bool {
	// 	match self.maximum {
	// 		MaxDiagnostics::All => true,
	// 		MaxDiagnostics::FixedTo(n) => todo!(),
	// 	}
	// }

	pub fn emit(&mut self, diagnostic: checker::Diagnostic, fs: &MapFileStore<WithPathMap>) {
		let files = fs.into_code_span_store();

		// TODO temp
		let compact = false;
		let config = Config {
			display_style: if compact { DisplayStyle::Short } else { DisplayStyle::Rich },
			..Config::default()
		};

		// let compact = matches!(self.configuration.display_style, DisplayStyle::Short);
		let diagnostic = checker_diagnostic_to_codespan_diagnostic(diagnostic, compact);

		#[cfg(target_family = "wasm")]
		{
			let mut buffer = codespan_reporting::term::termcolor::Buffer::ansi();
			emit(&mut buffer, &config, &files, &diagnostic).unwrap();
			let output =
				String::from_utf8(buffer.into_inner()).expect("invalid string from diagnostic");
			crate::utilities::print_to_cli(format_args!("{output}"));
		}

		#[cfg(not(target_family = "wasm"))]
		{
			emit(&mut self.writer, &config, &files, &diagnostic).unwrap();
		}
	}

	pub fn _finish(&mut self, emitted: u16) {
		#[cfg(not(target_family = "wasm"))]
		std::io::Write::flush(&mut self.writer).unwrap();

		// TODO print that there was no erros?
		// if let MaxDiagnostics::FixedTo(maximum) = self.maximum {
		// 	if emitted > maximum {
		// 		crate::utilities::print_to_cli(format_args!(
		// 			"... and {difference} other errors and warnings",
		// 			difference = emitted - maximum
		// 		));
		// 	}
		// }

		// Ok(())
	}
}
