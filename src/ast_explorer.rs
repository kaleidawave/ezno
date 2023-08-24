#![allow(dead_code)]

use std::{fs, path::PathBuf};

use argh::FromArgs;
use enum_variants_strings::EnumVariantsStrings;
use parser::{ASTNode, Expression, Module, SourceId, ToStringOptions};

use crate::{error_handling::emit_ezno_diagnostic, utilities::print_to_cli};

/// Repl for testing out AST
#[derive(FromArgs, Debug)]
#[argh(subcommand, name = "ast-explorer")]
pub(crate) struct ExplorerArguments {
	#[argh(subcommand)]
	nested: ExplorerSubCommand,
	/// path to input file
	#[argh(option)]
	file: Option<PathBuf>,
}

impl ExplorerArguments {
	pub(crate) fn run<T: crate::FSResolver, U: crate::CLIInputResolver>(
		&mut self,
		fs_resolver: T,
		cli_input_resolver: U,
	) {
		if let Some(ref file) = self.file {
			let file = fs_resolver(file).unwrap();
			self.nested.run(file);
		} else {
			print_to_cli(format_args!("ezno ast-explorer\nUse #exit to leave. Also #switch-mode *mode name* and #load-file *path*"));
			loop {
				let mut input = cli_input_resolver(self.nested.to_str()).unwrap_or_default();
				if input.len() == 0 {
					continue;
				} else if input.trim() == "#exit" {
					break;
				} else if let Some(new_mode) = input.strip_prefix("#switch-mode ") {
					self.nested = match ExplorerSubCommand::from_str(new_mode.trim()) {
						Ok(mode) => mode,
						Err(expected) => {
							print_to_cli(format_args!(
								"Unexpected mode, options are {:?}",
								expected
							));
							continue;
						}
					};
					continue;
				} else if let Some(path) = input.strip_prefix("#load-file ") {
					input = match fs::read_to_string(path.trim()) {
						Ok(string) => string,
						Err(err) => {
							print_to_cli(format_args!("{:?}", err));
							continue;
						}
					};
				};
				self.nested.run(input);
			}
		}
	}
}

#[derive(FromArgs, Debug, EnumVariantsStrings)]
#[argh(subcommand)]
#[enum_variants_strings_transform(transform = "kebab_case")]
pub(crate) enum ExplorerSubCommand {
	AST(ASTArgs),
	FullAST(FullASTArgs),
	Prettifier(PrettyArgs),
	Uglifier(UglifierArgs),
}

/// Prints AST for given expression
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "ast")]
pub(crate) struct ASTArgs {}

/// Prints AST for given module
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "full-ast")]
pub(crate) struct FullASTArgs {}

/// Prettifies source code (full whitespace)
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "prettifier")]
pub(crate) struct PrettyArgs {}

/// Removes whitespace from input
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "uglifier")]
pub(crate) struct UglifierArgs {}

impl ExplorerSubCommand {
	pub fn run(&self, input: String) {
		match self {
			ExplorerSubCommand::AST(_) => {
				let res = <Expression as parser::ASTNode>::from_string(
					input.clone(),
					Default::default(),
					SourceId::NULL,
					None,
					Vec::new(),
				);
				match res {
					Ok(res) => {
						print_to_cli(format_args!("{:#?}", res));
					}
					// TODO temp
					Err(err) => emit_ezno_diagnostic(
						err.into(),
						&parser::source_map::MapFileStore::default(),
						SourceId::NULL,
					)
					.unwrap(),
				}
			}
			ExplorerSubCommand::FullAST(_) => {
				let res = Module::from_string(
					input.clone(),
					Default::default(),
					SourceId::NULL,
					None,
					Vec::new(),
				);
				match res {
					Ok(res) => print_to_cli(format_args!("{:#?}", res)),
					// TODO temp
					Err(err) => emit_ezno_diagnostic(
						err.into(),
						&parser::source_map::MapFileStore::default(),
						SourceId::NULL,
					)
					.unwrap(),
				}
			}
			ExplorerSubCommand::Prettifier(_) | ExplorerSubCommand::Uglifier(_) => {
				let res = Module::from_string(
					input.clone(),
					Default::default(),
					SourceId::NULL,
					None,
					Vec::new(),
				);
				match res {
					Ok(module) => {
						let settings = if matches!(self, ExplorerSubCommand::Prettifier(_)) {
							ToStringOptions::default()
						} else {
							ToStringOptions::minified()
						};
						print_to_cli(format_args!("{}", module.to_string(&settings)));
					}
					Err(err) => emit_ezno_diagnostic(
						err.into(),
						&parser::source_map::MapFileStore::default(),
						SourceId::NULL,
					)
					.unwrap(),
				}
			}
		}
	}
}
