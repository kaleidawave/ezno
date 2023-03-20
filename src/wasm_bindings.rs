use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
	#[wasm_bindgen(js_namespace = console)]
	pub(crate) fn log(s: &str);
}

#[wasm_bindgen(raw_module = "./bindings.js")]
extern "C" {
	#[wasm_bindgen]
	pub(crate) fn read_from_cli() -> String;

	// TODO error
	#[wasm_bindgen]
	pub(crate) fn read_from_path(path: &str) -> String;

	#[wasm_bindgen(raw_module = "./bindings")]
	pub(crate) fn get_cli_args() -> String;
}

#[wasm_bindgen]
pub fn build_wasm(content: String, path: String) -> JsValue {
	let (_fs, result) = crate::temp::build(content, path, "OUTPUT".to_owned());
	serde_wasm_bindgen::to_value(&result).unwrap()
}
