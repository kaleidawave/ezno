#![cfg_attr(target_family = "wasm", allow(unused))]

mod ast_explorer;
mod build;
mod check;
mod repl;
mod reporting;

pub(crate) mod utilities;

pub mod cli;
pub mod transformers;

pub use build::build;
pub use check::check;
pub use checker::{Diagnostic, DiagnosticKind};

pub use parser::{source_map, ASTNode, ToStringOptions};
use parser::{Module, ParseError};

pub fn prettifier(input: String) -> Result<String, ParseError> {
	let module = Module::from_string(input, Default::default())?;
	Ok(module.to_string(&ToStringOptions::default()))
}

pub trait WriteToFS: Fn(&std::path::Path, String) {}

impl<T> WriteToFS for T where T: Fn(&std::path::Path, String) {}

#[cfg(target_family = "wasm")]
mod wasm_bindings;

#[cfg(target_family = "wasm")]
pub use wasm_bindings::experimental_build_wasm;

#[cfg(target_family = "wasm")]
pub use wasm_bindings::run_cli_wasm;

pub use cli::run_cli;
