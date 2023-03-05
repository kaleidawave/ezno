### Contributing to Ezno

Consider checking out [current standing issues](https://github.com/kaleidawave/ezno/issues) before contributing. In general leave an issue before putting in a PR.

#### Setting up

Want to contribute, here is how to set up the environment?

[Installing Rust](https://www.rust-lang.org/tools/install)

Clone the repo with `git`:
```
git clone https://github.com/kaleidawave/ezno.git
```
or with the GH cli
```
gh repo clone kaleidawave/ezno
```

Running `cargo run` should now run the CLI 

### General rules

- Won't merge PRs that introduce new errors. However will merge PRs which pick up or find existing issues
- Code **must** be formatted with rustfmt inline with the current format configuration
- Use `cargo clippy` as guidance but lints are not a blocker

### Useful commands

- Check source is valid with `cargo check --workspace`
- Check that code is formatted in accordance with the specification with `cargo fmt --all --check`
- Run all tests `cargo test --workspace --verbose`
