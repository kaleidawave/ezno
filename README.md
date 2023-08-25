A JavaScript compiler and TypeScript checker written in Rust with a focus on static analysis and runtime performance.

[Try the checker in Oxc today](https://gist.github.com/kaleidawave/5dcb9ec03deef1161ebf0c9d6e4b88d8)!

![project lines of code](https://projects.kaleidawave.workers.dev/project/ezno/badge)

What Ezno is
- A type checker for JavaScript, usable as a library or with *optional* compiler infrastructure (parser, CLI, LSP, etc)
- Fully typed programs, guaranteeing type safety (as long as definitions are sound)
- Types aimed at soundness and tracing for better static analysis
- A *imperative* type system, with event tracing and evaluating function side effects. Equivalent to an interpreter, but with types rather than values.
- A collection of experiments of types
- Written in Rust

What Ezno is not
- **eNZo, Z goes before the N** (pronounce as 'Fresno' without the 'fr') 😀
- 1:1 / parity with TSC, it has different behaviors **but** should work in existing projects using TSC (check out [stc](https://github.com/dudykr/stc) that is aimed at 1:1)
- A binary executable generator. It compiles JavaScript (or TS/Ezno superset) to JavaScript. The process is compiler rather than transpiler. Although you could use its event IR to generate a lower level format
- Usable, [still a long way to go to usability](https://github.com/kaleidawave/ezno/milestone/1)

Read some more detailed posts
- [Introducing Ezno](https://kaleidawave.github.io/posts/introducing-ezno/)
- [Ezno '23](https://kaleidawave.github.io/posts/ezno-23/)

---

This project is a workspace consisting of a few crates:

| Crate | Lines Of Code | Contains |
|---|---|---|
| checker | ![checker lines of code](https://projects.kaleidawave.workers.dev/project/ezno-checker/badge) | Stores for types and contexts, type checking logic and optional synthesis over the parser AST |
| parser | ![parser lines of code](https://projects.kaleidawave.workers.dev/project/ezno-parser/badge) | AST definitions, logic for parsing, AST to string and visiting |
<!-- | ezno-web-framework | ![](https://projects.kaleidawave.workers.dev/project/framework/badge) | Visitors and code generation for JSX and reactive expression transformations. | -->
<!-- | ezno-lsp | ![](https://projects.kaleidawave.workers.dev/project/framework/badge) | Visitors and code generation for JSX and reactive expression transformations. | -->

Also checkout [oxc_type_synthesis](https://github.com/web-infra-dev/oxc/tree/main/crates/oxc_type_synthesis), a crate which allows using the type checker through [oxc](https://github.com/web-infra-dev/oxc/tree/main)!

## Experimental

This is an experimental compiler. If you are looking for a stable compiler, Ezno is not the right choice **at the moment**.

## Help contribute

Check out issues and comment on discussions! Read [CONTRIBUTING.md](https://github.com/kaleidawave/ezno/blob/main/CONTRIBUTING.md) for more information.
