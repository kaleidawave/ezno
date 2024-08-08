[The specification](./specification.md) is a markdown document that defines what the behavior of Ezno's checker. It is meant to server both as documentation and what is currently supported as well as a test suite to ensure there isn't a regression in the checker.

[specification.md](./specification.md) defines current behavior which works on the main branch. [staging.md](./staging.md) can be used as a well to introduce behavior during the implementation and remains separated from the rest. It can be enabled with the `staging` features and exclusively with `staging-only`. [to_implement.md](./to_implement.md) contains future tests that do not currently passed. They can be viewed as a target for the checker and copied to staging when ready to implement.

These can all be ran as tests using the markdown to Rust test code transpiler in [build.rs](./build.rs).

- The cases should be brief and only test a specific aspect of the language
- Each block contains errors, the list afterwards is the expected errors
- Comments can be in block quotes to explain additional details in the tests
- Sections are at level three headings (`###`), tests are at level four headings (`####`), the tested code goes a code block with the language tag `ts` and errors in a bullet list after in order
- Blocks can be split into files with a `// in file.ts` comment, below which all code is in the `file.ts` file. Default is `main.tsx`
- **Code in blocks is indented with tabs not spaces**

---
Other tests unrelated to specific checking features can be found in `checker/tests`

> [!IMPORTANT]
> Any tests that throw or produce unreachables must be contained as it breaks the total concatenation.
