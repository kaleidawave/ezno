#![allow(dead_code)]

use std::{fs, io, path::PathBuf};

use argh::FromArgs;
use codespan_reporting::term::Config;
use enum_variants_strings::EnumVariantsStrings;
use parser::{
	ASTNode, Expression, Module, ParseOutput, SourceId, ToStringSettings, ToStringSettingsAndData,
};

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
			match self.nested.run(file) {
				_ => {}
			}
		} else {
			// println!("EZNO REPL\nUse #exit to leave and #switch_mode to um... switch mode");
			loop {
				print!("{}> ", self.nested.to_str());
				io::Write::flush(&mut io::stdout()).unwrap();
				let (n, mut input) = crate::utilities::read_string_from_cli(&mut io::stdin());
				if n == 0 {
					continue;
				} else if input.trim() == "#exit" {
					break;
				} else if let Some(new_mode) = input.strip_prefix("#switch_mode ") {
					self.nested = match ExplorerSubCommand::from_str(new_mode.trim()) {
						Ok(mode) => mode,
						Err(expected) => {
							println!("Unexpected mode, options are {:?}", expected);
							continue;
						}
					};
					continue;
				} else if let Some(path) = input.strip_prefix("#load_file ") {
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
pub(crate) enum ExplorerSubCommand {
	AST(ASTArgs),
	FullAST(FullASTArgs),
	Prettifier(PrettyArgs),
	Uglifier(UglifierArgs),
	// Lex(LexerArgs),
}

/// Prints AST for given expression
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "ast")]
pub(crate) struct ASTArgs {}

/// Prints AST for given module
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "full-ast")]
pub(crate) struct FullASTArgs {}

// /// Returns the tokens from lexing
// #[derive(FromArgs, Debug, Default)]
// #[argh(subcommand, name = "lexer")]
// pub(crate) struct LexerArgs {}

/// Prettifies source code (full whitespace)
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "prettifier")]
pub(crate) struct PrettyArgs {}

/// Removes whitespace from input
#[derive(FromArgs, Debug, Default)]
#[argh(subcommand, name = "uglifier")]
pub(crate) struct UglifierArgs {}

fn parse_error_to_codespan_error(input: String, error: parser::ParseError) {
	use codespan_reporting::{
		diagnostic::{Diagnostic, Label},
		files::SimpleFile,
	};

	let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
		codespan_reporting::term::termcolor::ColorChoice::Always,
	);
	let reason = error.get_reason().to_owned();
	codespan_reporting::term::emit(
		&mut writer.lock(),
		&Config { start_context_lines: 3, end_context_lines: 3, ..Default::default() },
		&SimpleFile::new("input", input),
		&Diagnostic::error().with_labels(vec![Label::primary(
			(),
			std::ops::Range::from(error.position),
		)
		.with_message(reason)]),
	)
	.unwrap();
}

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
					Err(err) => parse_error_to_codespan_error(input.clone(), err),
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
					Err(err) => parse_error_to_codespan_error(input, err),
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
					Err(err) => parse_error_to_codespan_error(input, err),
				}
			}
		}
	}
}
