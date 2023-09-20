import { initSync, build, check, parse_expression } from "./index.mjs";
import { readFileSync } from "node:fs";

const wasmPath = new URL("./shared/ezno_lib_bg.wasm", import.meta.url);
initSync(readFileSync(wasmPath));

export { build, check, parse_expression }