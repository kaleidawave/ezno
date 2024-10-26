import { equal } from "node:assert";
import { test } from "node:test";
import { spawn } from "node:child_process";

const encoder = new TextEncoder();
const decoder = new TextDecoder();

function read(child) {
	return new Promise((res, rej) => {
		child.stdout.on("data", (d) => res(decoder.decode(d)));
		child.stderr.on("data", (d) => res(decoder.decode(d)));
	})
}

function write(child, command) {
	const promise = new Promise((res, rej) => { child.stdin.addListener("finish", res) });
	child.stdin.write(command + "\n");
	return promise
}

// Use Deno as nodejs repl doesn't work at the moment
const child = spawn("deno", ["run", "--allow-read", "./dist/cli.mjs", "repl"]);

console.dir(await read(child));
console.dir(await write(child, "print_type(4);"));
console.dir(await read(child));
console.dir(await write(child, "close()"));

// test("Parse from CLI", (t) => {
// 	t.test("temp", async () => {

// 	})
// })