export * from "./index.mjs";
import { init_sync } from "./index.mjs";
import { readFileSync } from "node:fs";

const wasmPath = new URL("./shared/ezno_lib_bg.wasm", import.meta.url);

if (wasmPath.protocol === "https:") {
    init_sync(await fetch(wasmPath).then(response => response.arrayBuffer()))
} else {
    init_sync(readFileSync(wasmPath));
}