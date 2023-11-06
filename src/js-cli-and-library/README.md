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
import { build } from "ezno/initialised";

// Just use a local string. Could use readFileSync for FS access
const fs_handler = (_path) => "const x = !t ? 4 : 5;";
console.dir(build("input.js", fs_handler), { depth: 5 })
```

For the web, `init()` is needed to load the WASM before calling any functions.

```js
import { init, build } from "ezno";

await init();

const res = build("input.js", () => "const x = 2 + 5;");

document.querySelector('#app').innerHTML = `<pre>${JSON.stringify(res)}</pre>`;
```

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
