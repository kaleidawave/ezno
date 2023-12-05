import { initSync, experimental_build, check, parse_expression, parse_module, just_imports, minify_module, get_version } from "./index.mjs";
import { readFileSync } from "node:fs";

const wasmPath = new URL("./shared/ezno_lib_bg.wasm", import.meta.url);
if (wasmPath.protocol === "https:") {
    initSync(await fetch(wasmPath).then(response => response.arrayBuffer()))
} else {
    initSync(readFileSync(wasmPath));
}

export { experimental_build, check, parse_expression, parse_module, just_imports, minify_module, get_version }