# Ezno's Checker

[![crates.io badge](https://img.shields.io/crates/v/ezno-checker?style=flat-square)](https://crates.io/crates/ezno-checker)
[![docs.rs badge](https://img.shields.io/docsrs/ezno-checker?style=flat-square)](https://docs.rs/ezno-checker/latest)

Contains type checking logic for TypeScript and logic for running type checking checks over `ezno-parser`.

See [specification](./specification/specification.md) for all currently implemented checking features.

## Definitions

*At the moment* it comes packaged with `internal.ts.d.bin`. This can either be a syntactic or binary definition of methods, types and such in the runtime.

## Adding type checking outside of the Ezno toolchain

While the checker is indented for the Ezno toolchain and its parser, most (3/4) of the checker code is AST agnostic. The synthesis directory that contains the bindings with `ezno-parser` can disabled with `no-default-features`. You can build your own `AST <-> Type checking APIs` using it for adding Ezno's type checking features into other toolchains (without needing to parse or convert ASTs).

#TODO `ASTImplementation`, `synthesis` folder rule

## Testing

Set `EZNO_DEBUG` to any value to trace diagnostic information from the `crate::utils::notify!` macro (In powershell = `$Env:EZNO_DEBUG=1`)
