use std::path::Path;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
	#[wasm_bindgen(js_namespace = console)]
	pub(crate) fn log(s: &str);
}

/// Wrapper the abstracts some of the properties
#[wasm_bindgen(typescript_custom_section)]
const TYPES_WASM_CHECK_OUTPUT: &str = r###"
interface WASMCheckOutput {
	readonly diagnostics: DiagnosticsContainer,
	get_type_at_position(path: string, pos: number): string;
	get_type_at_position_debug(path: string, pos: number): string;
}
"###;
#[wasm_bindgen]
pub struct WASMCheckOutput(checker::CheckOutput<checker::synthesis::EznoParser>);

#[wasm_bindgen]
impl WASMCheckOutput {
	#[wasm_bindgen(js_name = diagnostics, getter, skip_typescript)]
	pub fn get_diagnostics(&self) -> JsValue {
		serde_wasm_bindgen::to_value(&self.0.diagnostics).unwrap()
	}

	pub fn get_type_at_position(&self, path: &str, pos: u32) -> Option<String> {
		self.0.get_type_at_position(path, pos, false)
	}

	pub fn get_type_at_position_debug(&self, path: &str, pos: u32) -> Option<String> {
		self.0.get_type_at_position(path, pos, true)
	}

	pub fn get_module_ast(&self, path: &str) -> JsValue {
		self.0.get_module(path).map_or(JsValue::NULL, |m| {
			serde_wasm_bindgen::to_value(m).expect("cannot turn Module into `JsValue`")
		})
	}
}

#[wasm_bindgen(typescript_custom_section)]
const TYPES_CHECK_WITH_OPTIONS: &str = r#"
export function check(
	entry_path: string, 
	fs_resolver_js: (path: string) => string | undefined, 
	options: TypeCheckOptions | undefined
): WASMCheckOutput"#;
#[wasm_bindgen(js_name = check, skip_typescript)]
pub fn check_wasm(
	entry_path: String,
	fs_resolver_js: &js_sys::Function,
	options: JsValue,
) -> WASMCheckOutput {
	std::panic::set_hook(Box::new(console_error_panic_hook::hook));

	let options: checker::TypeCheckOptions = if options.is_undefined() || options.is_null() {
		Default::default()
	} else {
		serde_wasm_bindgen::from_value(options).expect("invalid TypeCheckOptions")
	};

	let fs_resolver = |path: &std::path::Path| {
		let res =
			fs_resolver_js.call1(&JsValue::null(), &JsValue::from(path.display().to_string()));

		res.ok().and_then(|res| res.as_string())
	};

	WASMCheckOutput(crate::check::check(vec![entry_path.into()], &fs_resolver, None, options))
}

/// Wrapper the abstracts some of the properties
#[wasm_bindgen(typescript_custom_section)]
const TYPES_WASM_CHECK_OUTPUT: &str = r###"
interface WASMBuildOutput {
	readonly artifacts: Vec<Output>,
	readonly check_output: WASMCheckOutput
}
"###;
#[wasm_bindgen]
pub struct WASMBuildOutput {
	artifacts: Vec<crate::build::Output>,
	check_output: WASMCheckOutput,
}

#[wasm_bindgen]
impl WASMBuildOutput {
	#[wasm_bindgen(js_name = artifacts, getter, skip_typescript)]
	pub fn get_artifacts(&self) -> JsValue {
		serde_wasm_bindgen::to_value(&self.artifacts).unwrap()
	}

	#[wasm_bindgen(js_name = diagnostics, getter, skip_typescript)]
	pub fn get_diagnostics(&self) -> JsValue {
		self.check_output.get_diagnostics()
	}

	pub fn get_type_at_position(&self, path: &str, pos: u32) -> Option<String> {
		self.check_output.get_type_at_position(path, pos)
	}

	pub fn get_type_at_position_debug(&self, path: &str, pos: u32) -> Option<String> {
		self.check_output.get_type_at_position_debug(path, pos)
	}
}

#[wasm_bindgen(typescript_custom_section)]
const TYPES_EXPERIMENTAL_BUILD: &str = r###"
export function experimental_build(
	entry_path: string, 
	fs_resolve_js: (path: string) => string | undefined, 
	minify: boolean
): WASMBuildOutput
"###;
#[wasm_bindgen(js_name = experimental_build, skip_typescript)]
pub fn experimental_build_wasm(
	entry_path: String,
	fs_resolver_js: &js_sys::Function,
	minify: bool,
) -> WASMBuildOutput {
	std::panic::set_hook(Box::new(console_error_panic_hook::hook));

	let fs_resolver = |path: &std::path::Path| {
		let res =
			fs_resolver_js.call1(&JsValue::null(), &JsValue::from(path.display().to_string()));
		res.ok().and_then(|res| res.as_string())
	};

	let result = crate::build::build(
		vec![entry_path.into()],
		&fs_resolver,
		None,
		Path::new("out.js"),
		&crate::build::BuildConfig {
			tree_shake: minify,
			strip_whitespace: minify,
			source_maps: false,
		},
		None,
	);

	match result {
		Ok(crate::build::BuildOutput { artifacts, check_output }) => {
			WASMBuildOutput { artifacts, check_output: WASMCheckOutput(check_output) }
		}
		Err(crate::build::FailedBuildOutput(check_output)) => WASMBuildOutput {
			artifacts: Default::default(),
			check_output: WASMCheckOutput(check_output),
		},
	}
}

#[wasm_bindgen(typescript_custom_section)]
const TYPES_RUN_CLI: &str = r#"
export function run_cli(
	cli_arguments: string[],
	read_from_file: (path: string) => string | undefined,
	write_to_file: (path: string, content: string) => void,
): void
"#;
#[wasm_bindgen(js_name = run_cli, skip_typescript)]
pub fn run_cli_wasm(
	cli_arguments: Box<[JsValue]>,
	read_from_file: &js_sys::Function,
	write_to_file: &js_sys::Function,
) {
	std::panic::set_hook(Box::new(console_error_panic_hook::hook));

	let arguments = cli_arguments.iter().flat_map(JsValue::as_string).collect::<Vec<_>>();
	let arguments = arguments.iter().map(String::as_str).collect::<Vec<_>>();

	let read_from_file = |path: &std::path::Path| {
		let res =
			read_from_file.call1(&JsValue::null(), &JsValue::from(path.display().to_string()));
		res.ok().and_then(|res| res.as_string())
	};

	let write_to_file = |path: &std::path::Path, content: String| {
		write_to_file
			.call2(
				&JsValue::null(),
				&JsValue::from(path.display().to_string()),
				&JsValue::from(content),
			)
			.unwrap();
	};

	crate::run_cli(&arguments, &read_from_file, write_to_file);
}

#[wasm_bindgen(typescript_custom_section)]
const TYPES_PARSE_EXPRESSION: &str =
	"export function parse_expression(input: string): Expression | [string, Span]";
#[wasm_bindgen(js_name = parse_expression, skip_typescript)]
pub fn parse_expression_to_json(input: String) -> JsValue {
	use parser::{ASTNode, Expression};

	std::panic::set_hook(Box::new(console_error_panic_hook::hook));
	let item = Expression::from_string(input, Default::default());
	match item {
		Ok(item) => serde_wasm_bindgen::to_value(&Ok::<_, ()>(item)).unwrap(),
		Err(parse_error) => {
			serde_wasm_bindgen::to_value(&(parse_error.reason, parse_error.position)).unwrap()
		}
	}
}

#[wasm_bindgen(typescript_custom_section)]
const TYPES_PARSE_MODULE: &str =
	"export function parse_module(input: string): Module | [string, Span]";
#[wasm_bindgen(js_name = parse_module, skip_typescript)]
pub fn parse_module_to_json(input: String) -> JsValue {
	use parser::{ASTNode, Module};

	std::panic::set_hook(Box::new(console_error_panic_hook::hook));
	let item = Module::from_string(input, Default::default());
	match item {
		Ok(item) => serde_wasm_bindgen::to_value(&Ok::<_, ()>(item)).unwrap(),
		Err(parse_error) => {
			serde_wasm_bindgen::to_value(&(parse_error.reason, parse_error.position)).unwrap()
		}
	}
}
#[wasm_bindgen(typescript_custom_section)]
const TYPES_PARSE_MODULE_AND_INTO_STRING: &str = r#"
export function parse_module_and_into_string(
	input: string,
	parse_options: ParseOptions,
	to_string_options: ToStringOptions
): string | [string, Span]
"#;
#[wasm_bindgen(js_name = parse_module_and_into_string, skip_typescript)]
pub fn parse_module_and_into_string(
	input: String,
	parse_options: JsValue,
	to_string_options: JsValue,
) -> JsValue {
	use parser::{ASTNode, Module};

	std::panic::set_hook(Box::new(console_error_panic_hook::hook));
	let item = Module::from_string(
		input,
		serde_wasm_bindgen::from_value(parse_options).expect("invalid ParseOptions"),
	);
	match item {
		Ok(item) => serde_wasm_bindgen::to_value(&item.to_string(
			&serde_wasm_bindgen::from_value(to_string_options).expect("invalid ToStringOptions"),
		))
		.unwrap(),
		Err(parse_error) => {
			serde_wasm_bindgen::to_value(&(parse_error.reason, parse_error.position)).unwrap()
		}
	}
}
#[wasm_bindgen(typescript_custom_section)]
const TYPES_JUST_IMPORTS: &str =
	"export function just_imports(input: string): string | [string, Span]";
#[wasm_bindgen(skip_typescript)]
pub fn just_imports(input: String) -> JsValue {
	use parser::{ASTNode, Module};

	std::panic::set_hook(Box::new(console_error_panic_hook::hook));
	let item = Module::from_string(input, Default::default());
	match item {
		Ok(mut item) => {
			crate::transformers::filter_imports(&mut item);
			serde_wasm_bindgen::to_value(&item.to_string(&parser::ToStringOptions::minified()))
				.unwrap()
		}
		Err(parse_error) => {
			serde_wasm_bindgen::to_value(&(parse_error.reason, parse_error.position)).unwrap()
		}
	}
}

#[wasm_bindgen]
pub fn get_version() -> JsValue {
	serde_wasm_bindgen::to_value(&env!("CARGO_PKG_VERSION")).unwrap()
}
