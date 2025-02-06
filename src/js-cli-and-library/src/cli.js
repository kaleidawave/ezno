#!/usr/bin/env node

import { initSync, run_cli, parse_module, ReplSystem } from "../build/ezno_lib.js";
import { readFileSync, writeFileSync } from "node:fs";
import { stdin as input, stdout as output } from 'node:process';
import * as readline from 'node:readline/promises';

const wasmPath = new URL("./shared/ezno_lib_bg.wasm", import.meta.url);
if (wasmPath.protocol === "https:") {
    initSync({ module: await fetch(wasmPath).then(response => response.arrayBuffer()) });
} else {
    initSync({ module: readFileSync(wasmPath) });
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
    const kind = cliArguments[0];
    const rl = readline.createInterface({ input, output });

    if (kind === "repl") {
        console.log("Entering REPL");
        const system = ReplSystem.new_system({}, (path) => { console.trace(path); return "" });
        while (true) {
            const answer = await rl.question('> ');
            if (answer.length === 0 || answer === ".exit" || answer === "close()") {
                break
            }
            system.execute_statement(answer);
        }
    } else if (kind === "ast-explorer") {
        console.log("Entering ast-explorer");
        while (true) {
            const answer = await rl.question('> ');
            if (answer.length === 0 || answer === ".exit" || answer === "close()") {
                break
            }
            console.dir(parse_module(answer), { depth: Number.POSITIVE_INFINITY });
        }
    }
    rl.close();
} else {
    run_cli(cliArguments, readFile, writeFile);
}
