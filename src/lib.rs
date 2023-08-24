mod ast_explorer;
mod commands;
mod error_handling;

// mod repl;

pub(crate) mod utilities;

pub mod cli;

pub trait FSResolver: Fn(&std::path::Path) -> Option<String> {}

impl<T> FSResolver for T where T: Fn(&std::path::Path) -> Option<String> {}

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
