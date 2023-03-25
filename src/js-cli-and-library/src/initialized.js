import { initSync, build } from "./index.mjs";
import { readFileSync } from "node:fs";

const wasmPath = new URL("./shared/ezno_lib_bg.wasm", import.meta.url);
initSync(readFileSync(wasmPath));

export { build }