// use checker::{Diagnostic as CheckerDiagnostic, ErrorWarningInfoHandler};
use codespan_reporting::{
	diagnostic::{Diagnostic, Label, Severity},
	files::Files,
	term::{emit, Config},
};
use parser::{source_map::FileSystem, Span};

/// This actually exists in the checker
#[cfg_attr(target_family = "wasm", derive(serde::Serialize))]
pub struct TempDiagnostic {
	pub label: String,
	pub position: Span,
	pub kind: ErrorWarningInfo,
}

#[allow(unused)]
#[cfg_attr(target_family = "wasm", derive(serde::Serialize))]
pub enum ErrorWarningInfo {
	Error,
	Warning,
	Info,
}

pub(crate) fn emit_ezno_diagnostic(
	fs: &impl FileSystem,
	error: crate::error_handling::TempDiagnostic,
) -> Result<(), codespan_reporting::files::Error> {
	let reason = error.label;
	let id = error.position.source_id;
	let range = std::ops::Range::from(error.position);

	let diagnostic = Diagnostic {
		severity: match error.kind {
			ErrorWarningInfo::Error => Severity::Error,
			ErrorWarningInfo::Warning => Severity::Warning,
			ErrorWarningInfo::Info => Severity::Note,
		},
		code: None,
		message: Default::default(),
		labels: vec![Label::primary(id, range).with_message(reason)],
		notes: Vec::default(),
	};

	emit_diagnostic(&fs.into_code_span_store(), &diagnostic)
}

pub(crate) fn emit_parser_error(
	source: String,
	error: parser::ParseError,
) -> Result<(), codespan_reporting::files::Error> {
	use codespan_reporting::files::SimpleFile;

	let simple_file = SimpleFile::new("INPUT", source);
	let label =
		Label::primary((), std::ops::Range::from(error.position)).with_message(error.reason);
	emit_diagnostic(
		&simple_file,
		&Diagnostic {
			severity: Severity::Error,
			code: None,
			message: Default::default(),
			labels: vec![label],
			notes: Vec::default(),
		},
	)
}

#[cfg(target_family = "wasm")]
fn emit_diagnostic<'files, F: Files<'files>>(
	files: &'files F,
	diagnostic: &Diagnostic<<F as Files<'files>>::FileId>,
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
	files: &'files F,
	diagnostic: &Diagnostic<<F as Files<'files>>::FileId>,
) -> Result<(), codespan_reporting::files::Error> {
	use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

	let config = Config::default();

	let writer = StandardStream::stderr(ColorChoice::Always);
	let mut lock = writer.lock();
	emit(&mut lock, &config, files, diagnostic)
}
