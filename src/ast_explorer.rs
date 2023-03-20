#![allow(dead_code)]

use std::{fs, io, path::PathBuf};

use argh::FromArgs;
use enum_variants_strings::EnumVariantsStrings;
use parser::{
	ASTNode, Expression, Module, ParseOutput, SourceId, ToStringSettings, ToStringSettingsAndData,
};

use crate::error_handling::emit_parser_error;

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
	pub(crate) fn run(&mut self) {
		if let Some(ref file) = self.file {
			let file = fs::read_to_string(file).unwrap();
			self.nested.run(file);
		} else {
			println!("ezno ast-explorer\nUse #exit to leave. Also #switch-mode *mode name* and #load-file *path*");
			loop {
				print!("{}> ", self.nested.to_str());
				io::Write::flush(&mut io::stdout()).unwrap();
				let (n, mut input) = crate::utilities::read_from_cli(&mut io::stdin());
				if n == 0 {
					continue;
				} else if input.trim() == "#exit" {
					break;
				} else if let Some(new_mode) = input.strip_prefix("#switch-mode ") {
					self.nested = match ExplorerSubCommand::from_str(new_mode.trim()) {
						Ok(mode) => mode,
						Err(expected) => {
							println!("Unexpected mode, options are {:?}", expected);
							continue;
						}
					};
					continue;
				} else if let Some(path) = input.strip_prefix("#load-file ") {
					input = match fs::read_to_string(path.trim()) {
						Ok(string) => string,
						Err(err) => {
							println!("{:?}", err);
							continue;
						}
					};
				};
				self.nested.run(input);
				io::Write::flush(&mut io::stdout()).unwrap();
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
						if res.1.function_extractor.is_empty() {
							println!("{:#?}", res.0);
						} else {
							println!("{:#?}", res);
						}
					}
					Err(err) => emit_parser_error(input, err).unwrap(),
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
					Ok(res) => println!("{:#?}", res),
					Err(err) => emit_parser_error(input, err).unwrap(),
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
					Ok(ParseOutput(value, state)) => {
						let settings = if matches!(self, ExplorerSubCommand::Prettifier(_)) {
							ToStringSettings::default()
						} else {
							ToStringSettings::minified()
						};
						let d = ToStringSettingsAndData(settings, state.function_extractor);
						println!("{}", value.to_string(&d));
					}
					Err(err) => emit_parser_error(input, err).unwrap(),
				}
			}
		}
	}
}
