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

You can run just the checker with

```shell
cargo run -p ezno-checker --example run-checker path/to/file.ts
```

> [!TIP]
> This can be faster as doesn't have recompile the CLI options and things. (although the errors don't contain the nice source annotations)
> Note you can skip the cache with the additional `--no-cache` option (re-reads base `.d.ts` file)

If you want to check all the checker tests

```shell
cargo test -p ezno-checker-specification
# To include the Staging file (which is really useful for keeping new fixes/additions separate)
cargo test -p ezno-checker-specification -F staging
# and for all the tests (which includes all of to_implement.md)
cargo test -p ezno-checker-specification -F all
```

> [!IMPORTANT]
> `cache` might need to be regenerated between specification runs if working on internal methods (e.g. `Array.map` etc). See below

If you want to regenerate the binary definition file

```shell
cargo run -p ezno-checker -F ezno-parser --example generate_cache ./checker/definitions/overrides.d.ts ./checker/definitions/internal.ts.d.bin
```

If you want to test the lexing and parsing in Ezno's parser

```shell
# Parsing, prints parse errors or the debug view of the AST
cargo run -p ezno-parser --example parse path/to/file.ts
# Lexing, prints lex errors or the tokens
cargo run -p ezno-parser --example lex path/to/file.ts
```

### Bacon (script runner)

The [Bacon script runner](https://dystroy.org/bacon/) is configured for this repo. This can watch your files and re-run things like checks or tests on file change.
The configuration is managed in the [`bacon.toml`](./bacon.toml) file. The configuration has dedicated jobs for the checker specification tests mentioned above.

#### Installing Bacon

To install bacon, simply run `cargo install --locked bacon`, and you're ready to go.

#### Using Bacon

To use bacon, you can start it by running `bacon check`. This will spin up a job that listens to the source and runs checks on file changes!
There are also hotkeys you can use to switch jobs, or manually trigger a re-run of a job (for jobs where watch functionality is disabled).
We have some custom jobs defined as well:

| Job Name      | Description      | Hotkey    |
| ------------- | ------------- | ------------- |
| test-spec-no-watch | this job runs `cargo test -p ezno-checker-specification` and does *not* watch for file changes | 1 |
| test-staging-no-watch | this job runs `cargo test -p ezno-checker-specification -F staging` and does *not* watch for file changes | 2 |
| test-all-no-watch | this job runs `cargo test -p ezno-checker-specification -F all` and does *not* watch for file changes | 3 |
| test-spec | this job runs `cargo test -p ezno-checker-specification` and watches for file changes | 4 |
| test-staging | this job runs `cargo test -p ezno-checker-specification -F staging` and watches for file changes | 5 |
| test-all | this job runs `cargo test -p ezno-checker-specification -F all` and watches for file changes | 6 |

At any point, you can press `?` to see a list of all available hotkeys.

#### Adding new jobs to our Bacon config

New jobs can easily be added to our `bacon.toml` config if we find there are repetitive actions we're doing frequently.
[The Bacon documentation](https://dystroy.org/bacon/config/#jobs) does a good job of explaining how to do so.

### Useful commands

- Check source is valid with `cargo check --workspace`
- Check that code is formatted in accordance with the configuration with `cargo fmt --all --check`
- Run all tests `cargo test --workspace --verbose`
- Use `cargo clippy -- -A warnings` to find blocking lints

### The `notify!` macro

The checker crate has the `crate::utilities::notify!` macro, which can be used to trace information when the `EZNO_DEBUG` environment variable is set.

## *Rules* for contributions

- Code **must** be formatted with `cargo format` inline with the current format configuration
- It **must** pass `cargo clippy -- --allow warnings`. In many cases adding `allow` to items is fine

<!-- ## Oxc

If working on [oxc_type_synthesis](https://github.com/web-infra-dev/oxc/tree/main/crates/oxc_type_synthesis) **and Ezno simultaneously**. You can (git) clone [oxc](https://github.com/web-infra-dev/oxc) alongside Ezno and then use path dependencies to work on them simultaneously. -->
