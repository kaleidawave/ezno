# Ezno's Checker

[![crates.io badge](https://img.shields.io/crates/v/ezno-checker?style=flat-square)](https://crates.io/crates/ezno-checker)
[![docs.rs badge](https://img.shields.io/docsrs/ezno-checker?style=flat-square)](https://docs.rs/ezno-checker/latest)

Contains type checking logic.

See [specification](./specification/specification.md) for what is currently implemented.

The `synthesis` folder contains logic for [Ezno's parser](https://github.com/kaleidawave/ezno/tree/main/parser). See [oxc_type_synthesis](https://github.com/web-infra-dev/oxc) for similar code that connects the logic with OXCs AST.

## Testing

Set `EZNO_DEBUG` to any value to trace diagnostic information from the `crate::utils::notify!` macro (In powershell = `$Env:EZNO_DEBUG=1`)
