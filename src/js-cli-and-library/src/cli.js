#!/usr/bin/env node

import { initSync, run_cli } from "../build/ezno_lib.js";
import { readFileSync, writeFileSync } from "node:fs";

const wasmPath = new URL("./shared/ezno_lib_bg.wasm", import.meta.url);
if (wasmPath.protocol === "https:") {
    initSync(await fetch(wasmPath).then(response => response.arrayBuffer()))
} else {
    initSync(readFileSync(wasmPath));
}

const onDeno = typeof Deno !== "undefined";
const cliArguments = onDeno ? Deno.args : process.argv.slice(2);

function readFile(path) {
    return readFileSync(path).toString();
}

function writeFile(path, content) {
    writeFileSync(path, content)
}

// Fix because REPL requires syncronous stdin input which isn't 
// TODO also ast-explorer
if (cliArguments.length === 1 && (cliArguments[0] === "repl" || cliArguments[0] === "ast-explorer")) {
    console.error("TODO")
} else {
    run_cli(cliArguments, readFile, writeFile);
}
