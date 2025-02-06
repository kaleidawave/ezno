#![cfg_attr(target_family = "wasm", allow(unused))]

mod ast_explorer;
mod build;
mod check;
mod repl;
mod reporting;

pub mod utilities;

pub mod cli;
pub mod transformers;

pub use build::build;
pub use check::check;

#[cfg(target_family = "wasm")]
mod wasm_bindings;

#[cfg(target_family = "wasm")]
pub struct FSFunction {
	pub read: js_sys::Function,
	pub read: reporting::DiagnosticEmitter,
}

use parser::source_map::{MapFileStore, WithPathMap};

#[cfg(target_family = "wasm")]
impl checker::ReadFromFS for FSFunction {
	fn read_file(&self, path: &std::path::Path) -> Option<Vec<u8>> {
		self.0
			.call1(
				&wasm_bindgen::JsValue::null(),
				&wasm_bindgen::JsValue::from(path.display().to_string()),
			)
			.ok()
			.and_then(|s| s.as_string())
			.map(|s| s.into_bytes())
	}

	fn emit_diagnostic(diagnostic: checker::Diagnostic, files: &MapFileStore<WithPathMap>) {
		todo!()
		// report_diagnostics_to_cli()
	}
}

#[cfg(not(target_family = "wasm"))]
pub struct FSFunction {
	pub reporter: reporting::DiagnosticEmitter,
}

impl FSFunction {
	#[cfg(not(target_family = "wasm"))]
	pub fn new() -> Self {
		Self { reporter: crate::reporting::DiagnosticEmitter::new() }
	}
}

#[cfg(not(target_family = "wasm"))]
impl checker::ReadFromFS for FSFunction {
	fn read_file(&self, path: &std::path::Path) -> Option<Vec<u8>> {
		std::fs::read(path).ok()
	}

	fn emit_diagnostic(
		&mut self,
		diagnostic: checker::Diagnostic,
		files: &MapFileStore<WithPathMap>,
	) {
		todo!()
	}
}

// #[cfg(target_family = "wasm")]
// pub use wasm_bindings::experimental_build_wasm;

// #[cfg(target_family = "wasm")]
// pub use wasm_bindings::run_cli_wasm;

pub use cli::run_cli;
