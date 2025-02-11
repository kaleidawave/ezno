import { rollup } from "rollup";
import { nodeResolve } from "@rollup/plugin-node-resolve";
import { cpSync, mkdirSync, writeFileSync, readFileSync } from "node:fs";
import { generate as generate_comparsion } from "./comparison/generator.mjs";
import { join } from "node:path";

// TODO watch mode under flag
async function build(input) {
    mkdirSync("./dist", { recursive: true });
    for (const key in input) {
        const request = input[key];
        if ("copy_from" in request) {
            cpSync(request.copy_from, request.to, { recursive: true });
        } else if ("generate" in request) {
            const out = await request.generate();
            if (Array.isArray(out)) {
                for (const out of out) {
                    writeFileSync(out.path, out.output, 'utf-8');
                }
            } else {
                writeFileSync(out.path, out.output, 'utf-8');
            }
        }
    }
}

await build({
    index_html: {
        copy_from: "./index.html",
        to: "./dist/index.html"
    },
    playground_html: {
        copy_from: "./playground/index.html",
        to: "./dist/playground.html"
    },
    ezno_wasm_fix: {
        copy_from: "./node_modules/ezno/dist/shared/ezno_lib_bg.wasm",
        to: "./dist/ezno_lib_bg.wasm"
    },
    assets: {
        copy_from: "./assets",
        to: "./dist/assets"
    },
    playground_js: {
        using: ["./playground/index.js"],
        async generate() {
            const bundle = await rollup({
                input: "./playground/index.js",
                plugins: [nodeResolve()],
                treeshake: true
            });
            const { output } = await bundle.generate({
                format: 'es',
            });
            return {
                path: "./dist/out.js",
                output: output[0].code
            }
        }
    },
    comparison: {
        using: [],
        async generate() {
            const content = readFileSync(join(import.meta.dirname, "../../checker/specification/specification.md"));
            const output = await generate_comparsion(content.toString());
            return {
                path: "./dist/comparison.html",
                output
            }
        }
    }
});

console.log("Site built!")