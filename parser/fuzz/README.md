Fuzzing tests for [Ezno's parser](https://github.com/kaleidawave/ezno/tree/main/parser).

This reuses [boa](https://github.com/boa-dev/boa)s fuzzing setup.

If developing in vscode and in top level folder, add the following to your `.vscode/settings.json` file to get Rust Analyzer to work in the tests folder.

```json
{
  "rust-analyzer.linkedProjects": ["./Cargo.toml", "./parser/fuzz/Cargo.toml"]
}
```

To run fuzz tests, `cargo install cargo-fuzz`, make sure you're set to use nightly rust, and run `cargo fuzz run <target>`, for example `cargo fuzz run module_roundtrip_naive`
