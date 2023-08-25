use std::path::Path;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
	#[wasm_bindgen(js_namespace = console)]
	pub(crate) fn log(s: &str);
}

#[wasm_bindgen(js_name = build)]
pub fn build_wasm(fs_resolver_js: &js_sys::Function, entry_path: String) -> JsValue {
	let fs_resolver = |path: &std::path::Path| {
		let res =
			fs_resolver_js.call1(&JsValue::null(), &JsValue::from(path.display().to_string()));
		res.ok().and_then(|res| res.as_string())
	};
	let (_fs, result) =
		crate::commands::build(fs_resolver, Path::new(&entry_path), Path::new("out.js"));
	serde_wasm_bindgen::to_value(&result).unwrap()
}

#[wasm_bindgen(js_name = check)]
pub fn check_wasm(fs_resolver_js: &js_sys::Function, entry_path: String) -> JsValue {
	let fs_resolver = |path: &std::path::Path| {
		let res =
			fs_resolver_js.call1(&JsValue::null(), &JsValue::from(path.display().to_string()));
		res.ok().and_then(|res| res.as_string())
	};
	let (_fs, diagnostics, _) = crate::commands::check(fs_resolver, Path::new(&entry_path), None);

	serde_wasm_bindgen::to_value(&diagnostics).unwrap()
}

#[wasm_bindgen(js_name = run_cli)]
pub fn run_cli_wasm(
	cli_arguments: Box<[JsValue]>,
	fs_resolver_js: &js_sys::Function,
	cli_input_resolver_js: &js_sys::Function,
) {
	let arguments = cli_arguments.into_iter().flat_map(JsValue::as_string).collect::<Vec<_>>();
	let arguments = arguments.iter().map(String::as_str).collect::<Vec<_>>();

	let fs_resolver = |path: &std::path::Path| {
		let res =
			fs_resolver_js.call1(&JsValue::null(), &JsValue::from(path.display().to_string()));
		res.ok().and_then(|res| res.as_string())
	};

	let cli_input_resolver = |prompt: &str| {
		cli_input_resolver_js
			.call1(&JsValue::null(), &JsValue::from(prompt.to_owned()))
			.ok()
			.as_ref()
			.and_then(JsValue::as_string)
	};

	crate::run_cli(&arguments, fs_resolver, cli_input_resolver)
}
