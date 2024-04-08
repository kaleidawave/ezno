use std::fs;
use std::path::{Path, PathBuf};

use argh::FromArgs;
use parser::{visiting::VisitorsMut, ASTNode};
use parser::{Expression, Module, Statement};

use crate::error_handling::emit_ezno_diagnostic;
use crate::utilities::print_to_cli;

/// Run project repl using deno. (`deno` command must be in path)
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "repl")]
pub(crate) struct ReplArguments {
	/// use mutable variables everywhere
	#[argh(switch)]
	const_as_let: bool,
	/// a definition file to check with
	#[argh(option, short = 'd')]
	type_definition_module: Option<PathBuf>,
}

#[allow(unused)]
fn file_system_resolver(path: &Path) -> Option<Vec<u8>> {
	// Cheaty
	if path.to_str() == Some("BLANK") {
		Some(Vec::new())
	} else {
		match fs::read_to_string(path) {
			Ok(source) => Some(source.into()),
			Err(_) => None,
		}
	}
}

pub(crate) fn run_repl<U: crate::CLIInputResolver>(
	cli_input_resolver: U,
	ReplArguments { const_as_let, type_definition_module }: ReplArguments,
) {
	print_to_cli(format_args!("Entering REPL. Exit with `close()`"));

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
					span: expression.get_position(),
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
				if let Some(last_ty) = last_ty {
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
}
