#!/usr/bin/env node

import { initSync, run_cli } from "../build/ezno_lib.js";
import { readFileSync } from "node:fs";

const wasmPath = new URL("./shared/ezno_lib_bg.wasm", import.meta.url);
if (wasmPath.protocol === "https:") {
    initSync(await fetch(wasmPath).then(response => response.arrayBuffer()))
} else {
    initSync(readFileSync(wasmPath));
}

const onDeno = typeof Deno !== "undefined";
const cliArguments = onDeno ? Deno.args : process.argv.slice(2);

run_cli(cliArguments, (path) => {
    return readFileSync(path).toString()
}, (prompt_msg) => {
    if (typeof Deno !== "undefined") {
        return prompt(`${prompt_msg}>`)
    } else {
        console.error("Prompt not supported in NodeJS (sync issue)");
        throw new Error("Prompt not supported in NodeJS")
    }
});