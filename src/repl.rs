use std::fs;
use std::path::{Path, PathBuf};

use argh::FromArgs;
use parser::{
	source_map::{FileSystem, MapFileStore},
	visiting::VisitorsMut,
	ASTNode,
};
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
}

#[allow(unused)]
fn file_system_resolver(path: &Path) -> Option<String> {
	// Cheaty
	if path.to_str() == Some("BLANK") {
		return Some(String::new());
	}
	match fs::read_to_string(path) {
		Ok(source) => Some(source),
		Err(_) => None,
	}
}

pub(crate) fn run_deno_repl<U: crate::CLIInputResolver>(
	cli_input_resolver: U,
	ReplArguments { type_output, const_as_let }: ReplArguments,
) {
	let mut items = if !type_output {
		let mut deno = Command::new("deno");
		let command = deno.arg("repl").arg("-q").stdout(Stdio::piped()).stdin(Stdio::piped());
		let mut process = command.spawn().unwrap();
		let stdin = process.stdin.take().unwrap();
		let child_buf = BufReader::new(process.stdout.take().unwrap());
		Some((process, stdin, child_buf))
	} else {
		None
	};

	let mut fs = MapFileStore::<parser::source_map::NoPathMap>::default();
	let source_id = fs.new_source_id(PathBuf::from("CLI"), Default::default());

	let mut state = checker::synthesis::interactive::State::new(&file_system_resolver, source_id);

	loop {
		let input = cli_input_resolver("");
		let input = if let Some(input) = input {
			if input.len() == 0 {
				continue;
			} else if input.trim() == "close()" {
				if let Some((_process, stdin, _child_buf)) = items.as_mut() {
					stdin.write_all("close()\n".as_bytes()).unwrap();
					stdin.flush().unwrap();
				}
				break;
			} else {
				input
			}
		} else {
			continue;
		};

		let (from_index, _) = fs.append_to_file(source_id, &input);

		let mut item = match parser::Module::from_string(
			input,
			Default::default(),
			source_id,
			Some(from_index as u32),
		) {
			Ok(item) => item,
			Err(err) => {
				emit_ezno_diagnostic((err, source_id).into(), &fs).unwrap();
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
			);
		}

		let result = state.check_item(&item);

		match result {
			Ok((last_ty, diagnostics)) => {
				for diagnostic in diagnostics.into_iter() {
					emit_ezno_diagnostic(diagnostic, &fs).unwrap();
				}
				if let Some((_process, stdin, child_buf)) = items.as_mut() {
					let output = item.to_string(&Default::default());
					stdin.write_all(output.as_bytes()).unwrap();
					// Enter command for repl
					stdin.write_all(b"\n\"REPL_END\"\n").unwrap();
					stdin.flush().unwrap();

					loop {
						let mut buf = String::new();
						match child_buf.read_line(&mut buf) {
							Ok(_output) => {
								if buf.contains("REPL_END") {
									break;
								}
								// deno already emits new line so just print here
								print!("{}", buf);
							}
							Err(_) => {
								println!("Error");
								break;
							}
						}
					}
				} else {
					if let Some(last_ty) = last_ty {
						println!("{last_ty}");
					}
				}
			}
			Err(diagnostics) => {
				for diagnostic in diagnostics.into_iter() {
					emit_ezno_diagnostic(diagnostic, &fs).unwrap();
				}
			}
		}
	}

	if let Some((mut process, _, _)) = items {
		let _status = process.wait().unwrap();
	}
}
