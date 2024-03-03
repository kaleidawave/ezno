### Contributing to Ezno

Consider checking out [current standing issues](https://github.com/kaleidawave/ezno/issues) before contributing. In general, leave an issue before putting in a PR.

If any problems come up in the following steps, please leave an issue :)

#### Setting up

Want to contribute, here is how to set up the environment?

[First install Rust](https://www.rust-lang.org/tools/install)

then clone the repo with `git`:

```shell
git clone https://github.com/kaleidawave/ezno.git
# or using the GH CLI
gh repo clone kaleidawave/ezno
```

Now in the `ezno` directory, `cargo run` should show the CLI.

## Development

If you don't want to run the whole Ezno CLI. You can run just the checker with

```shell
cargo run -p ezno-checker -F ezno-parser --example check path/to/file.ts
```

If you want to check all the checker tests

```shell
cargo test -p ezno-checker-specification
# To including staging.md (which is really useful for keeping new fixes/additions separate)
cargo test -p ezno-checker-specification -F staging
# and for all the tests (which includes all of to_implement.md)
cargo test -p ezno-checker-specification -F all
```

If you want to regenerate the binary definition file

```shell
cargo run -p ezno-checker -F ezno-parser --example cache ./checker/definitions/full.d.ts ./checker/definitions/internal.ts.d.bin
```

If you want to test the lexing and parsing in Ezno's parser

```shell
# Parsing, prints parse errors or the debug view of the AST
cargo run -p ezno-parser --example parse path/to/file.ts
# Lexing, prints lex errors or the tokens
cargo run -p ezno-parser --example lex path/to/file.ts
```

### Useful commands

- Check source is valid with `cargo check --workspace`
- Check that code is formatted in accordance with the configuration with `cargo fmt --all --check`
- Run all tests `cargo test --workspace --verbose`
- Use `cargo clippy -- -A warnings` to find blocking lints

### The notify! macro

The checker crate has the `crate::utils::notify!` macro, which can be used to trace information when the `EZNO_DEBUG` environment variable is set.

## *Rules* for contributions

- Code **must** be formatted with `cargo format` inline with the current format configuration
- It **must** pass `cargo clippy -- --allow warnings`. In many cases adding `allow` to items is fine

<!-- ## Oxc

If working on [oxc_type_synthesis](https://github.com/web-infra-dev/oxc/tree/main/crates/oxc_type_synthesis) **and Ezno simultaneously**. You can (git) clone [oxc](https://github.com/web-infra-dev/oxc) alongside Ezno and then use path dependencies to work on them simultaneously. -->
