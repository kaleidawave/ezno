#![allow(dead_code)]

use std::{fs, path::PathBuf};

use argh::FromArgs;
use enum_variants_strings::EnumVariantsStrings;
use parser::{source_map::FileSystem, ASTNode, Expression, Module, ToStringOptions};

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
			let content = fs_resolver.get_content_at_path(file).unwrap();
			self.nested.run(content, Some(file.to_owned()));
		} else {
			print_to_cli(format_args!("ezno ast-explorer\nUse #exit to leave. Also #switch-mode *mode name* and #load-file *path*"));
			loop {
				let input = cli_input_resolver(self.nested.to_str()).unwrap_or_default();
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
				} else if let Some(path) = input.strip_prefix("#load-file ") {
					let input = match fs::read_to_string(path.trim()) {
						Ok(string) => string,
						Err(err) => {
							print_to_cli(format_args!("{:?}", err));
							continue;
						}
					};
					self.nested.run(input, Some(PathBuf::from(path)));
				} else {
					self.nested.run(input, None);
				}
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
	pub fn run(&self, input: String, path: Option<PathBuf>) {
		match self {
			ExplorerSubCommand::AST(_) => {
				let mut fs = parser::source_map::MapFileStore::default();
				let source_id = fs.new_source_id(path.unwrap_or_default(), input.clone());
				let res =
					Expression::from_string(input, Default::default(), source_id, None, Vec::new());
				match res {
					Ok(res) => {
						print_to_cli(format_args!("{:#?}", res));
					}
					// TODO temp
					Err(err) => emit_ezno_diagnostic(err.into(), &fs, source_id).unwrap(),
				}
			}
			ExplorerSubCommand::FullAST(_) => {
				let mut fs = parser::source_map::MapFileStore::default();
				let source_id = fs.new_source_id(path.unwrap_or_default(), input.clone());
				let res =
					Module::from_string(input, Default::default(), source_id, None, Vec::new());
				match res {
					Ok(res) => print_to_cli(format_args!("{:#?}", res)),
					// TODO temp
					Err(err) => emit_ezno_diagnostic(err.into(), &fs, source_id).unwrap(),
				}
			}
			ExplorerSubCommand::Prettifier(_) | ExplorerSubCommand::Uglifier(_) => {
				let mut fs = parser::source_map::MapFileStore::default();
				let source_id = fs.new_source_id(path.unwrap_or_default(), input.clone());
				let res =
					Module::from_string(input, Default::default(), source_id, None, Vec::new());
				match res {
					Ok(module) => {
						let settings = if matches!(self, ExplorerSubCommand::Prettifier(_)) {
							ToStringOptions::default()
						} else {
							ToStringOptions::minified()
						};
						print_to_cli(format_args!("{}", module.to_string(&settings)));
					}
					Err(err) => emit_ezno_diagnostic(err.into(), &fs, source_id).unwrap(),
				}
			}
		}
	}
}
