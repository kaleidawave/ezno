import { initSync, build, check, parse_expression, parse_module } from "./index.mjs";
import { readFileSync } from "node:fs";

const wasmPath = new URL("./shared/ezno_lib_bg.wasm", import.meta.url);
if (wasmPath.protocol === "https:") {
    throw Exception("Cannot fetch remote module. Use /remote")
    // initSync(await fetch(wasmPath).then(response => response.arrayBuffer()))
} else {
    initSync(readFileSync(wasmPath));
}

export { build, check, parse_expression, parse_module }