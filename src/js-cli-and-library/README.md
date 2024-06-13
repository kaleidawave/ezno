# Ezno

This contains a JavaScript edition of [Ezno](https://github.com/kaleidawave/ezno)

## CLI

This package includes a JS & WASM based version of the CLI. You can use as follows

```shell
node dist/cli.cjs info
# or
deno run -A dist/cli.mjs info
```

## Library example

For a environment that supports `node:fs`, you can use a version that initializes the WASM for you

```js
import { check } from 'ezno/initialised';
const fs_handler = (_path) => "const x: string = t ? 4 : 5;";
const output = check("input.ts", fs_handler);
console.dir(output.diagnostics, { depth: 5 });
```

For the web, `init()` is needed to load the WASM before calling any functions.

```js
import { check, init } from 'ezno';

await init();

const fs_handler = (_path) => "const x: string = t ? 4 : 5;";
const output = check("input.ts", fs_handler);
console.dir(output.diagnostics, { depth: 5 });
```

See more usage in the [./test.mjs](./test.mjs) and in the [playground](../playground/main.js).

You can see the Rust definitions of these exports under [../../src/wasm_bindings.rs](../../src/wasm_bindings.rs). Thanks to [#114](https://github.com/kaleidawave/ezno/pull/114) all these exports have associated type definitions.

There also exists more functions such as `check_with_options` which takes an additional `TypeCheckOptions`. For parsing string to AST there is `parse_expression` & `parse_module`. If you find something implemented in Rust, but not exposed to the JS or WASM context: [feel to make a request issue for it to be exposed via the JS API](https://github.com/kaleidawave/ezno/issues/new).

## Commands for building this package

Run this **from this folder, not the root**:

```shell
npm run clean
npm run build
```

See `package.json` for the other building commands.

- It first builds the WASM binary with rustc, `cargo build --lib --target wasm32-unknown-unknown`
- It then binds (builds associate JS library) into the `build` folder with `wasm-bindgen --out-dir build --target web`
- It then bundles (and generates CJS & EJS formats) with some associate JS using `unbuild` into the `dist` folder
