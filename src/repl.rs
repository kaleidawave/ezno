use std::path::PathBuf;

use argh::FromArgs;
use parser::{
	visiting::VisitorsMut, ASTNode, Expression, Module, SourceId, StatementOrDeclaration,
};

use crate::reporting::report_diagnostics_to_cli;

#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::wasm_bindgen;

/// Run type checking REPL
#[derive(FromArgs, PartialEq, Debug, Default)]
#[argh(subcommand, name = "repl")]
#[cfg_attr(target_family = "wasm", derive(serde::Deserialize), serde(default))]
pub struct ReplArguments {
	/// use mutable variables everywhere
	#[argh(switch)]
	const_as_let: bool,
	/// a definition file to check with
	#[argh(option, short = 'd')]
	type_definition_module: Option<PathBuf>,
}

/// Wraps `checker::synthesis::interactive::State`
#[cfg_attr(target_family = "wasm", wasm_bindgen)]
pub struct ReplSystem {
	arguments: ReplArguments,
	source: SourceId,
	state: checker::synthesis::interactive::State<'static, crate::utilities::FSFunction>,
}

pub type ReplSystemErr = (
	checker::DiagnosticsContainer,
	crate::source_map::MapFileStore<crate::source_map::WithPathMap>,
);

impl ReplSystem {
	pub fn new(
		arguments: ReplArguments,
		file_system_resolver: crate::utilities::FSFunction,
	) -> Result<ReplSystem, ReplSystemErr> {
		let definitions = if let Some(tdm) = arguments.type_definition_module.clone() {
			std::iter::once(tdm).collect()
		} else {
			std::iter::once(checker::INTERNAL_DEFINITION_FILE_PATH.into()).collect()
		};

		// TOOD
		let static_file_system_resolver = Box::leak(Box::new(file_system_resolver));

		let state =
			checker::synthesis::interactive::State::new(static_file_system_resolver, definitions)?;
		let source = state.get_source_id();

		Ok(ReplSystem { arguments, source, state })
	}
}

#[cfg_attr(target_family = "wasm", wasm_bindgen)]
impl ReplSystem {
	/// Not a constructor because returns result (can fail if can't find `d.ts` file)
	/// Also `repl_arguments` rather than `arguments` otherwise breaks JS emit
	#[cfg(target_family = "wasm")]
	#[wasm_bindgen(js_name = "new_system")]
	pub fn new_js(
		repl_arguments: wasm_bindgen::JsValue,
		cb: &js_sys::Function,
	) -> Option<ReplSystem> {
		let repl_arguments: ReplArguments =
			serde_wasm_bindgen::from_value(repl_arguments).expect("invalid ReplArguments");
		let cb = crate::utilities::FSFunction(cb.clone());
		Self::new(repl_arguments, cb).ok()
	}

	#[cfg_attr(target_family = "wasm", wasm_bindgen)]
	pub fn execute_statement(&mut self, mut input: String) {
		// New line fixes #210
		input.truncate(input.trim_end().len());
		input.push('\n');
		let (start, _) = self.state.get_fs_mut().append_to_file(self.source, &input);

		let options = Default::default();
		let offset = Some(start as u32);

		// self.offset += input.len() as u32 + 1;

		// Fix to remain consistent with other JS REPLs
		let starts_with_brace = input.trim_start().starts_with('{');
		let result = if starts_with_brace {
			Expression::from_string_with_options(input, options, offset).map(|(expression, _)| {
				Module {
					hashbang_comment: None,
					span: expression.get_position(),
					items: vec![StatementOrDeclaration::Expression(expression.into())],
				}
			})
		} else {
			Module::from_string_with_options(input, options, offset).map(|(module, _state)| module)
		};

		match result {
			Ok(mut item) => {
				// crate::utilities::print_to_cli(format_args!("item={item:?}"));

				if self.arguments.const_as_let {
					item.visit_mut(
						&mut VisitorsMut {
							statement_visitors_mut: vec![Box::new(crate::transformers::ConstToLet)],
							..Default::default()
						},
						&mut (),
						&Default::default(),
						self.source,
					);
				}

				let result = self.state.check_item(&item);

				match result {
					Ok((last_ty, diagnostics)) => {
						report_diagnostics_to_cli(
							diagnostics,
							self.state.get_fs_ref(),
							false,
							crate::utilities::MaxDiagnostics::All,
						)
						.unwrap();

						if let Some(last_ty) = last_ty {
							crate::utilities::print_to_cli(format_args!("{last_ty}"));
						}
					}
					Err(diagnostics) => {
						report_diagnostics_to_cli(
							diagnostics,
							self.state.get_fs_ref(),
							false,
							crate::utilities::MaxDiagnostics::All,
						)
						.unwrap();
					}
				}
			}
			Err(err) => {
				report_diagnostics_to_cli(
					std::iter::once((err, self.source).into()),
					self.state.get_fs_ref(),
					false,
					crate::utilities::MaxDiagnostics::All,
				)
				.unwrap();
			}
		}
	}
}

#[cfg(target_family = "wasm")]
pub(crate) fn run_repl(_arguments: ReplArguments) {
	panic!(
		"Cannot run repl in WASM because of input callback. Consider reimplementing using library"
	);
}

#[cfg(not(target_family = "wasm"))]
pub(crate) fn run_repl(arguments: ReplArguments) {
	use crate::utilities::print_to_cli;

	print_to_cli(format_args!("Entering REPL\n.Use #exist, .exit or close() to leave"));

	let mut system = match ReplSystem::new(arguments, crate::utilities::FSFunction) {
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
		let input = crate::utilities::cli_input_resolver("");
		if input.is_empty() {
			continue;
		} else if let "#exit" | ".exit" | "close()" = input.trim() {
			break;
		}
		system.execute_statement(input)
	}
}
