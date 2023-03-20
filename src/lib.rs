mod ast_explorer;
mod error_handling;
// mod repl;
mod temp;
pub(crate) mod utilities;

pub mod cli;

#[cfg(target_family = "wasm")]
mod wasm_bindings;

#[cfg(target_family = "wasm")]
pub use wasm_bindings::build_wasm as build;

pub use cli::run_cli;
