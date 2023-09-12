mod ast_explorer;
mod commands;
mod error_handling;

// mod repl;

pub(crate) mod utilities;

pub mod cli;

pub use checker::{Diagnostic, DiagnosticKind};
pub use commands::{build, check};
pub use parser::{source_map, ASTNode, ToStringOptions};
use parser::{Module, ParseError};

pub fn prettifier(input: String) -> Result<String, ParseError> {
	use parser::source_map::FileSystem;

	let mut fs = source_map::MapFileStore::default();
	let source_id = fs.new_source_id("".into(), input.clone());
	let module = Module::from_string(input, Default::default(), source_id, None, Vec::new())?;
	Ok(module.to_string(&ToStringOptions::default()))
}

pub trait FSResolver {
	fn get_content_at_path(&self, path: &std::path::Path) -> Option<String>;
}

impl<T> FSResolver for T
where
	T: Fn(&std::path::Path) -> Option<String>,
{
	fn get_content_at_path(&self, path: &std::path::Path) -> Option<String> {
		(self)(path)
	}
}

/// prompt -> response
pub trait CLIInputResolver: Fn(&str) -> Option<String> {}

impl<T> CLIInputResolver for T where T: Fn(&str) -> Option<String> {}

#[cfg(target_family = "wasm")]
mod wasm_bindings;

#[cfg(target_family = "wasm")]
pub use wasm_bindings::build_wasm;

#[cfg(target_family = "wasm")]
pub use wasm_bindings::run_cli_wasm;

pub use cli::run_cli;
