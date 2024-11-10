import { equal } from "node:assert";
import { test } from "node:test";
import { spawn } from "node:child_process";
import stripAnsi from "strip-ansi";

test("ast-explorer", async (t) => {
    await t.test("works", async () => {
        const decoder = new TextDecoder();

        function write(child, command) {
            child.stdin.write(command + "\r\n");
        }

        const child = spawn("node", ["./dist/cli.mjs", "ast-explorer"], { stdio: "pipe" });

        child.stdout.on("data", (d) => out.push(decoder.decode(d)));
        child.stderr.on("data", (d) => out.push(decoder.decode(d)));

        const out = [];

        const wait = (timeout = 500) => new Promise((res, _rej) => setTimeout(res, timeout));

        await wait();
        write(child, "'Hello World'");
        await wait();
        write(child, "close()")
        await wait();

        const expected = `Entering ast-explorer\n> {\n  Ok: {\n    hashbang_comment: undefined,\n    items: [\n      {\n        Statement: {\n          Expression: {\n            Single: {\n              StringLiteral: [ 'Hello World', 'Single', { start: 0, end: 13 } ]\n            }\n          }\n        }\n      }\n    ],\n    span: { start: 0, end: 13 }\n  }\n}\n> `;

        // console.log(stripAnsi(out.join("")).replaceAll("\n", "\\n"));

        equal(stripAnsi(out.join("")), expected);
    });
});

test("type checking repl", async (t) => {
    await t.test("works", async () => {
        const decoder = new TextDecoder();

        function write(child, command) {
            child.stdin.write(command + "\r\n");
        }

        const child = spawn("node", ["./dist/cli.mjs", "repl"], { stdio: "pipe" });

        child.stdout.on("data", (d) => out.push(decoder.decode(d)));
        child.stderr.on("data", (d) => out.push(decoder.decode(d)));

        const out = [];

        const wait = (timeout = 500) => new Promise((res, _rej) => setTimeout(res, timeout));

        await wait();
        write(child, "const var1: string = 5 + 6;");
        await wait();
        write(child, "close()")
        await wait();

        const expected = `Entering REPL\n> error: \n  ┌─ CLI.tsx:1:22\n  │\n1 │ const var1: string = 5 + 6;\n  │             ------   ^^^^^ Type 11 is not assignable to type string\n  │             │         \n  │             Variable declared with type string\n\n\n> `;

        // console.log(stripAnsi(out.join("")).replaceAll("\n", "\\n"));

        equal(stripAnsi(out.join("")), expected);
    });
});