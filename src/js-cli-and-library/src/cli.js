#!/usr/bin/env node
import { initSync, run_cli } from "../build/ezno_lib.js";
import { readFileSync } from "node:fs";

const wasmPath = new URL("./shared/ezno_lib_bg.wasm", import.meta.url);
initSync(readFileSync(wasmPath));

const cli_arguments = typeof Deno !== "undefined" ? Deno.args : process.argv.slice(2);

run_cli(cli_arguments, (path) => {
    console.info(`Reading '${path}'`);
    return readFileSync(path).toString()
}, (prompt_msg) => {
    if (typeof Deno !== "undefined") {
        return prompt(`${prompt_msg}> `)
    } else {
        console.error("Prompt not supported in NodeJS (sync issue)");
        throw new Error("Prompt not supported in NodeJS")
    }
});