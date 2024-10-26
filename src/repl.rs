use std::fs;
use std::path::{Path, PathBuf};

use argh::FromArgs;
use parser::{visiting::VisitorsMut, ASTNode};
use parser::{Expression, Module, Statement};

use crate::reporting::report_diagnostics_to_cli;
use crate::utilities::print_to_cli;

/// Interactive type checking
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

/// Wraps `checker::synthesis::interactive::State`
pub struct ReplSystem {
	arguments: ReplArguments,
	source: SourceId,
	state: checker::synthesis::interactive::State
}

impl ReplSystem {
	pub fn new(arguments: ReplArguments) -> Result<Self, (DiagnosticsContainer, MapFileStore<WithPathMap>)> {
		let definitions = if let Some(tdm) = type_definition_module {
			std::iter::once(tdm).collect()
		} else {
			std::iter::once(checker::INTERNAL_DEFINITION_FILE_PATH.into()).collect()
		};
	
		let state = checker::synthesis::interactive::State::new(&file_system_resolver, definitions)?;
		let source = state.get_source_id();

		ReplSystem {
			arguments,
			source,
			state
		}
	}

	pub fn execute_statement(&mut self, input: String) {
		let (from_index, _) = self.state.get_fs_mut().append_to_file(source, &input);

		let options = Default::default();
		let offset = Some(from_index as u32);
		let result = if input.trim_start().starts_with('{') {
			Expression::from_string_with_options(input, options, offset).map(|(expression, _)| {
				Module {
					hashbang_comment: None,
					span: expression.get_position(),
					items: vec![Statement::Expression(expression.into()).into()],
				}
			})
		} else {
			Module::from_string(input, options)
		};

		match result {
			Ok(item) => item,
			Err(err) => {
				report_diagnostics_to_cli(
					std::iter::once((err, source).into()),
					state.get_fs_ref(),
					false,
					crate::utilities::MaxDiagnostics::All,
				)
				.unwrap();
			}
		};

		if self.arguments.const_as_let {
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
				report_diagnostics_to_cli(
					diagnostics,
					state.get_fs_ref(),
					false,
					crate::utilities::MaxDiagnostics::All,
				)
				.unwrap();

				if let Some(last_ty) = last_ty {
					println!("{last_ty}");
				}
			}
			Err(diagnostics) => {
				report_diagnostics_to_cli(
					diagnostics,
					state.get_fs_ref(),
					false,
					crate::utilities::MaxDiagnostics::All,
				)
				.unwrap();
			}
		}
	}
}

pub(crate) fn run_repl<U: crate::CLIInputResolver>( arguments: ReplArguments) {
	if cfg!(target_family = "wasm") {
		panic!("Cannot run repl in WASM because of input callback. Consider reimplementing using library");
	}

	print_to_cli(format_args!("Entering REPL. Exit with `close()`"));

	let mut system = match ReplSystem::new(arguments) {
		Ok(system) => system,
		Err((diagnostics, fs)) => {
			report_diagnostics_to_cli(
				diagnostics,
				&fs,
				false,
				crate::utilities::MaxDiagnostics::All,
			)
			.unwrap();
			return;
		}
	};

	loop {
		let input = cli_input_resolver("");
		if input.is_empty() {
			continue;
		} else if input.trim() == "close()" {
			break;
		}
		system.execute_statement(input)
	}
}
