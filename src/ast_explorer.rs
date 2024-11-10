#![allow(dead_code)]

use std::path::PathBuf;

use argh::FromArgs;
use console::style;
use enum_variants_strings::EnumVariantsStrings;
use parser::{source_map::FileSystem, ASTNode, Expression, Module, ToStringOptions};

use crate::{
	reporting::report_diagnostics_to_cli,
	utilities::{print_to_cli, print_to_cli_without_newline},
};

/// REPL for printing out AST from user input
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
	#[cfg(target_family = "wasm")]
	pub(crate) fn run<T: crate::ReadFromFS>(&mut self, _fs_resolver: &T) {
		panic!("Cannot run ast-explorer in WASM because of input callback. Consider reimplementing using library");
	}

	#[allow(clippy::needless_continue)]
	#[cfg(not(target_family = "wasm"))]
	pub(crate) fn run<T: crate::ReadFromFS>(&mut self, fs_resolver: &T) {
		if let Some(ref file) = self.file {
			let content = fs_resolver.read_file(file);
			if let Some(content) = content {
				self.nested.run(String::from_utf8(content).unwrap(), Some(file.to_owned()));
			} else {
				eprintln!("Could not find file at {}", file.display());
			}
		} else {
			print_to_cli(format_args!("ezno ast-explorer\nUse #exit to leave. Also #switch-mode *mode name* and #load-file *path*"));
			loop {
				let input = crate::utilities::cli_input_resolver(self.nested.to_str());

				if input.is_empty() {
					continue;
				} else if input.trim() == "#exit" {
					break;
				} else if let Some(new_mode) = input.strip_prefix("#switch-mode ") {
					self.nested = match ExplorerSubCommand::from_str(new_mode.trim()) {
						Ok(mode) => mode,
						Err(expected) => {
							print_to_cli(format_args!("Unexpected mode, options are {expected:?}"));
							continue;
						}
					};
				} else if let Some(path) = input.strip_prefix("#load-file ") {
					let input = match std::fs::read_to_string(path.trim()) {
						Ok(string) => string,
						Err(err) => {
							print_to_cli(format_args!("{err:?}"));
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
#[allow(clippy::upper_case_acronyms)]
pub(crate) enum ExplorerSubCommand {
	AST(ASTArgs),
	FullAST(FullASTArgs),
	Prettifier(PrettyArgs),
	Uglifier(UglifierArgs),
	Lexer(LexerArgs),
}

/// Prints AST for a given expression
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "ast")]
pub(crate) struct ASTArgs {
	/// print results as json
	#[argh(switch)]
	json: bool,
}

/// Prints AST for a given module/block
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "full-ast")]
pub(crate) struct FullASTArgs {
	/// print results as json
	#[argh(switch)]
	json: bool,
	/// just print whether parse was successful
	#[argh(switch)]
	check: bool,
}

/// Prettifies source code (full whitespace)
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "prettifier")]
pub(crate) struct PrettyArgs {}

/// Removes whitespace from input
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "uglifier")]
pub(crate) struct UglifierArgs {}

/// Prints sources with tokens over
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "lexer")]
pub(crate) struct LexerArgs {}

impl ExplorerSubCommand {
	pub fn run(&self, input: String, path: Option<PathBuf>) {
		match self {
			ExplorerSubCommand::AST(cfg) => {
				let mut fs =
					parser::source_map::MapFileStore::<parser::source_map::NoPathMap>::default();
				let source_id = fs.new_source_id(path.unwrap_or_default(), input.clone());
				let res = Expression::from_string(input, parser::ParseOptions::all_features());
				match res {
					Ok(res) => {
						if cfg.json {
							print_to_cli(format_args!(
								"{}",
								serde_json::to_string_pretty(&res).unwrap()
							));
						} else {
							print_to_cli(format_args!("{res:#?}"));
						}
					}
					// TODO temp
					Err(err) => report_diagnostics_to_cli(
						std::iter::once((err, source_id).into()),
						&fs,
						false,
						crate::utilities::MaxDiagnostics::All,
					)
					.unwrap(),
				}
			}
			ExplorerSubCommand::FullAST(cfg) => {
				let mut fs =
					parser::source_map::MapFileStore::<parser::source_map::NoPathMap>::default();
				let source_id = fs.new_source_id(path.clone().unwrap_or_default(), input.clone());
				let res = Module::from_string(input, parser::ParseOptions::all_features());
				match res {
					Ok(res) => {
						if cfg.check {
							if let Some(ref path) = path {
								print_to_cli(format_args!(
									"{path} parsed successfully",
									path = path.display()
								));
							} else {
								print_to_cli(format_args!("Parsed successfully",));
							}
						} else if cfg.json {
							print_to_cli(format_args!(
								"{}",
								serde_json::to_string_pretty(&res).unwrap()
							));
						} else {
							print_to_cli(format_args!("{res:#?}"));
						}
					}
					// TODO temp
					Err(err) => report_diagnostics_to_cli(
						std::iter::once((err, source_id).into()),
						&fs,
						false,
						crate::utilities::MaxDiagnostics::All,
					)
					.unwrap(),
				}
			}
			ExplorerSubCommand::Prettifier(_) | ExplorerSubCommand::Uglifier(_) => {
				let mut fs =
					parser::source_map::MapFileStore::<parser::source_map::NoPathMap>::default();
				let source_id = fs.new_source_id(path.unwrap_or_default(), input.clone());
				let res = Module::from_string(input, Default::default());
				match res {
					Ok(module) => {
						let options = if matches!(self, ExplorerSubCommand::Prettifier(_)) {
							ToStringOptions { trailing_semicolon: true, ..Default::default() }
						} else {
							ToStringOptions::minified()
						};
						print_to_cli(format_args!("{}", module.to_string(&options)));
					}
					Err(err) => report_diagnostics_to_cli(
						std::iter::once((err, source_id).into()),
						&fs,
						false,
						crate::utilities::MaxDiagnostics::All,
					)
					.unwrap(),
				}
			}
			ExplorerSubCommand::Lexer(_) => {
				let mut color = console::Color::Red;
				for (section, with) in parser::script_to_tokens(input) {
					if with {
						let value = style(section).bg(color);
						// Cycle through colors
						color = match color {
							console::Color::Red => console::Color::Green,
							console::Color::Green => console::Color::Yellow,
							console::Color::Yellow => console::Color::Blue,
							console::Color::Blue => console::Color::Magenta,
							console::Color::Magenta => console::Color::Cyan,
							console::Color::Cyan => console::Color::Red,
							_ => unreachable!(),
						};
						print_to_cli_without_newline(format_args!("{value}"));
					} else {
						print_to_cli_without_newline(format_args!("{section}"));
					}
				}
				print_to_cli(format_args!(""));
			}
		}
	}
}
