use std::{collections::HashSet, io};

use argh::FromArgs;
use checker::{Project, TypeDefinitionModulePath};
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

use crate::{
	error_handling::print_error_warning_info_handler, file_system_resolver,
	utilities::read_string_from_cli,
};

/// Run project repl using deno
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

pub(crate) fn run_deno_repl(ReplArguments { type_output, const_as_let }: ReplArguments) {
	let project = Project::new(
		&file_system_resolver,
		Vec::new(),
		Default::default(),
		HashSet::from_iter([TypeDefinitionModulePath::SimpleBin]),
		std::env::current_dir().unwrap(),
		checker::SecondPassVisitors::default(),
	);

	let mut project = match project {
		Ok(project) => project,
		Err(error_handler) => {
			print_error_warning_info_handler(error_handler);
			return;
		}
	};

	let mut deno = Command::new("deno");
	let command = deno.arg("repl").arg("-q").stdout(Stdio::piped()).stdin(Stdio::piped());
	let mut process = command.spawn().unwrap();
	let mut stdin = process.stdin.take().unwrap();

	let mut child_buf = BufReader::new(process.stdout.take().unwrap());
	let mut state = None;

	println!("Entering repl");
	loop {
		print!("> ");
		io::Write::flush(&mut io::stdout()).unwrap();

		let (n, input) = read_string_from_cli(&mut io::stdin());
		if n == 0 {
			continue;
		} else if input.trim() == "close()" {
			stdin.write_all("close()\n".as_bytes()).unwrap();
			stdin.flush().unwrap();
			break;
		}

		let result = project.transform_statement(input, &mut state, const_as_let);

		match result {
			Ok((output, last_ty, error_handler)) => {
				print_error_warning_info_handler(error_handler);
				if type_output {
					let pretty = crate::highlighting::es_colorizer(last_ty);
					println!("{pretty}");
				} else {
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
				}
			}
			Err(error_handler) => {
				print_error_warning_info_handler(error_handler);
			}
		}
	}

	let _status = process.wait().unwrap();
}
