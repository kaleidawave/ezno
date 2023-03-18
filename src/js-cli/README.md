# JS CLI

This contains the JS based tool for ezno and library

## Building commands

```shell
cargo build --lib --target wasm32-unknown-unknown
wasm-bindgen --out-dir src/js-cli --target nodejs .\target\wasm32-unknown-unknown\debug\ezno.wasm
```
