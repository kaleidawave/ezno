# Ezno

This contains a JavaScript edition of [Ezno](https://github.com/kaleidawave/ezno)

## Example

```js
import { build } from "ezno";

const content = "const x = !t ? 4 : 5;";
console.dir(build(content, "input.js"), { depth: 5 })
```

## Commands for building this package

Run from this folder, **not the root**

```shell
cargo build --lib --target wasm32-unknown-unknown
wasm-bindgen --out-dir . --target nodejs ../../target/wasm32-unknown-unknown/debug/ezno.wasm
```
