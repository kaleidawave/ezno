use std::fs;
use std::path::{Path, PathBuf};

use argh::FromArgs;
use parser::{visiting::VisitorsMut, ASTNode};
use parser::{Expression, Module, Statement};
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

use crate::error_handling::emit_ezno_diagnostic;

/// Run project repl using deno. (`deno` command must be in path)
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "repl")]
pub(crate) struct ReplArguments {
	/// whether to print type of last expression, rather than executing
	#[argh(switch)]
	type_output: bool,
	/// use mutable variables everywhere
	#[argh(switch)]
	const_as_let: bool,
	/// a definition file to check with
	#[argh(option, short = 'd')]
	type_definition_module: Option<PathBuf>,
}

#[allow(unused)]
fn file_system_resolver(path: &Path) -> Option<String> {
	// Cheaty
	if path.to_str() == Some("BLANK") {
		Some(String::new())
	} else if path == Path::new(checker::INTERNAL_DEFINITION_FILE_PATH) {
		Some(checker::INTERNAL_DEFINITION_FILE.to_owned())
	} else {
		match fs::read_to_string(path) {
			Ok(source) => Some(source),
			Err(_) => None,
		}
	}
}

pub(crate) fn run_repl<U: crate::CLIInputResolver>(
	cli_input_resolver: U,
	ReplArguments { type_output, const_as_let, type_definition_module }: ReplArguments,
) {
	let mut items = if type_output {
		None
	} else {
		let mut deno = Command::new("deno");
		let command = deno.arg("repl").arg("-q").stdout(Stdio::piped()).stdin(Stdio::piped());
		let mut process = command.spawn().unwrap();
		let stdin = process.stdin.take().unwrap();
		let child_buf = BufReader::new(process.stdout.take().unwrap());
		Some((process, stdin, child_buf))
	};

	let definitions = if let Some(tdm) = type_definition_module {
		std::iter::once(tdm).collect()
	} else {
		std::iter::once(checker::INTERNAL_DEFINITION_FILE_PATH.into()).collect()
	};

	let state = checker::synthesis::interactive::State::new(&file_system_resolver, definitions);

	let mut state = match state {
		Ok(state) => state,
		Err((diagnostics, fs)) => {
			for diagnostic in diagnostics {
				emit_ezno_diagnostic(diagnostic, &fs).unwrap();
			}
			return;
		}
	};

	let source = state.get_source_id();

	loop {
		let input = cli_input_resolver("");
		let input = if let Some(input) = input {
			if input.is_empty() {
				continue;
			} else if input.trim() == "close()" {
				if let Some((_process, stdin, _child_buf)) = items.as_mut() {
					stdin.write_all("close()\n".as_bytes()).unwrap();
					stdin.flush().unwrap();
				}
				break;
			}

			input
		} else {
			continue;
		};

		let (from_index, _) = state.get_fs_mut().append_to_file(source, &input);

		let options = Default::default();
		let offset = Some(from_index as u32);
		let result = if input.trim_start().starts_with('{') {
			Expression::from_string_with_options(input, options, offset).map(|(expression, _)| {
				Module {
					span: *expression.get_position(),
					items: vec![Statement::Expression(expression.into()).into()],
				}
			})
		} else {
			Module::from_string(input, options)
		};

		let mut item = match result {
			Ok(item) => item,
			Err(err) => {
				emit_ezno_diagnostic((err, source).into(), state.get_fs_ref()).unwrap();
				continue;
			}
		};

		if const_as_let {
			item.visit_mut(
				&mut VisitorsMut {
					statement_visitors_mut: vec![Box::new(crate::transformers::ConstToLet)],
					..Default::default()
				},
				&mut (),
				&Default::default(),
				source,
			);
		}

		let result = state.check_item(&item);

		match result {
			Ok((last_ty, diagnostics)) => {
				for diagnostic in diagnostics {
					emit_ezno_diagnostic(diagnostic, state.get_fs_ref()).unwrap();
				}
				if let Some((_process, stdin, child_buf)) = items.as_mut() {
					let output = item.to_string(&Default::default());
					stdin.write_all(output.as_bytes()).unwrap();
					// Enter command for repl
					stdin.write_all(b"\n\"REPL_END\"\n").unwrap();
					stdin.flush().unwrap();

					loop {
						let mut buf = String::new();
						if let Ok(_output) = child_buf.read_line(&mut buf) {
							if buf.contains("REPL_END") {
								break;
							}
							// deno already emits new line so just print here
							print!("{buf}");
						} else {
							println!("Error");
							break;
						}
					}
				} else if let Some(last_ty) = last_ty {
					println!("{last_ty}");
				}
			}
			Err(diagnostics) => {
				for diagnostic in diagnostics {
					emit_ezno_diagnostic(diagnostic, state.get_fs_ref()).unwrap();
				}
			}
		}
	}

	if let Some((mut process, _, _)) = items {
		let _status = process.wait().unwrap();
	}
}
