A JavaScript compiler and TypeScript checker written in Rust with a focus on static analysis and runtime performance.

You can *try* what it current [supports today](./checker/specification/specification.md) with `npx`

```shell
npx ezno check file.ts
```

Or download the binary with `npm install ezno`, `cargo install ezno` or on [GitHub releases](https://github.com/kaleidawave/ezno/releases). [Or try it in Oxc](https://gist.github.com/kaleidawave/5dcb9ec03deef1161ebf0c9d6e4b88d8)!

![project lines of code](https://projects.kaleidawave.workers.dev/project/ezno/badge)

What Ezno is
- A type checker for JavaScript, usable as a library or with *optional* compiler infrastructure (parser, CLI, LSP, etc)
- Fully typed programs, guaranteeing type safety (as long as definitions are sound)
- Types aimed at soundness and tracing for better static analysis
- A *imperative* type system, with event tracing and evaluating function side effects. Equivalent to an interpreter, but with types rather than values
- A collection of experiments of types. Some features work well, others are in the prototype stage. Some are specific to JavaScript, others could be applied to other dynamic languages
- A challenge to the status quo of DCE, type checking and compilation through deeper static analysis beyond syntax analysis
- Written in Rust

What Ezno is not
- **eNZo, Z goes before the N** (pronounce as 'Fresno' without the 'fr') 😀
- 1:1 / parity with TSC, it has different behaviors **but** should work in existing projects using TSC (check out [stc](https://github.com/dudykr/stc) that is aimed at 1:1)
- A binary executable generator. It takes in JavaScript (or TS/Ezno superset) and does similar methods to traditional compilers, but at the end emits JavaScript. However the event intermediate representation could be used in the future to generate a lower level format
- Usable, [still a way to go until it can check actual programs](https://github.com/kaleidawave/ezno/milestone/1). See the [specification](./checker/specification/specification.md) for what is currently implemented in the checker

Read some more detailed posts
- [Introducing Ezno](https://kaleidawave.github.io/posts/introducing-ezno/)
- [Ezno '23](https://kaleidawave.github.io/posts/ezno-23/)
- [A preview of the checker](https://kaleidawave.github.io/posts/a-preview-of-the-checker/)

---

This project is a workspace consisting of a few crates:

| Crate | Lines Of Code | Contains |
|---|---|---|
| checker | ![checker lines of code](https://projects.kaleidawave.workers.dev/project/ezno-checker/badge) | Stores for types and contexts, type checking logic and optional synthesis over the parser AST |
| parser | ![parser lines of code](https://projects.kaleidawave.workers.dev/project/ezno-parser/badge) | AST definitions, logic for parsing, AST to string and visiting |
<!-- | ezno-web-framework | ![](https://projects.kaleidawave.workers.dev/project/framework/badge) | Visitors and code generation for JSX and reactive expression transformations. | -->
<!-- | ezno-lsp | ![](https://projects.kaleidawave.workers.dev/project/framework/badge) | Visitors and code generation for JSX and reactive expression transformations. | -->

Also checkout [oxc_type_synthesis](https://github.com/web-infra-dev/oxc/tree/main/crates/oxc_type_synthesis), a crate which allows using the type checker through [oxc](https://github.com/web-infra-dev/oxc/tree/main)!

## Help contribute

Check out issues and comment on discussions! Read [CONTRIBUTING.md](https://github.com/kaleidawave/ezno/blob/main/CONTRIBUTING.md) for more information.
