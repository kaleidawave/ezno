### Contributing to Ezno

Consider checking out [current standing issues](https://github.com/kaleidawave/ezno/issues) before contributing. In general, leave an issue before putting in a PR.

*If any problems come up following the steps. Leave an issue*

#### Setting up

Want to contribute, here is how to set up the environment?

[First install Rust](https://www.rust-lang.org/tools/install)

then clone the repo with `git`:

```shell
git clone https://github.com/kaleidawave/ezno.git
```

or using the GH CLI

```shell
gh repo clone kaleidawave/ezno
```

Now in the `ezno` directory, `cargo run` should show the CLI

### Rules

- Won't merge PRs that introduce new errors. However will merge PRs which pick up or find existing issues
- Code **must** be formatted with `cargo format` inline with the current format configuration
- Use `cargo clippy` as guidance but lints are not a blocker

### Useful commands

- Check source is valid with `cargo check --workspace`
- Check that code is formatted in accordance with the specification with `cargo fmt --all --check`
- Run all tests `cargo test --workspace --verbose`

### The notify! macro

The checker crate has the `crate::utils::notify!` macro, which can be used to trace information when the `EZNO_DEBUG` environment variable is set.

### Oxc

If working on [oxc_type_synthesis](https://github.com/web-infra-dev/oxc/tree/main/crates/oxc_type_synthesis). You can (git) clone [oxc](https://github.com/web-infra-dev/oxc) alongside Ezno and then use path dependencies to work on them simultaneously.
