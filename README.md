A JavaScript compiler and TypeScript checker written in Rust with a focus on static analysis and runtime performance.

> [!IMPORTANT]
> Ezno is in active development and **currently does not support enough features to check existing projects**. Check out the [getting started guide](./checker/docs/getting-started.md) for experimenting with what it current [supports today](./checker/specification/specification.md).

Read about Ezno
- [Introducing Ezno](https://kaleidawave.github.io/posts/introducing-ezno/)
- [Ezno in '23](https://kaleidawave.github.io/posts/ezno-23/)
- [A preview of the checker](https://kaleidawave.github.io/posts/a-preview-of-the-checker/)

<!-- Currently out ![project lines of code](https://projects.kaleidawave.workers.dev/project/ezno/badge) -->

What Ezno is
- A type checker for JavaScript, usable as a library or with *optional* compiler infrastructure (parser, CLI, LSP, etc)
- Fully typed programs with guaranteed type safety (as long as definitions are sound)
- Types aimed at soundness and tracing for better static analysis
- A *imperative* type system that tracks and evaluates the side effects of functions and control flow structures. It is similar to an interpreter, but acts with types instead of *values*
- A collection of experiments of types. Many are being worked out and are in the prototype stage. Some of the new behaviors benefit JavaScript specifically and others could be applied to other languages
- A challenge to the status quo of type checking, optimisations and compilation through deeper static analysis beyond syntax analysis
- [A high level library](https://docs.rs/ezno-checker/latest/ezno_checker/) that allows [type checking to be added to other tools!](https://github.com/web-infra-dev/oxc/tree/main/crates/oxc_type_synthesis)
- Written in Rust
- [**Efficient**](https://kaleidawave.github.io/posts/a-preview-of-the-checker/#checking-performance)

What Ezno is not
- **eNZo, the Z is in front of the N** (pronounce as 'Fresno' without the 'fr') 😀
- Be on parity with TSC or 1:1, it has some different behaviors **but** should work in existing projects using TSC (check out [stc](https://github.com/dudykr/stc) that is written in Rust and is aimed at replicating TSC)
- A binary executable generator. It takes in JavaScript (or a TypeScript or Ezno superset) and does similar processes to traditional compilers, but at the end emits JavaScript. However in the future it could generate a lower level format using its event representation

---

This project is a workspace consisting of a few crates:

| Crate | Lines Of Code | Contains |
|---|---|---|
| checker | ![checker lines of code](https://projects.kaleidawave.workers.dev/project/ezno-checker/badge) | Stores for types and contexts, type checking logic and optional synthesis over the parser AST |
| parser | ![parser lines of code](https://projects.kaleidawave.workers.dev/project/ezno-parser/badge) | AST definitions, logic for parsing, AST to string and visiting |
<!-- | ezno-web-framework | ![](https://projects.kaleidawave.workers.dev/project/framework/badge) | Visitors and code generation for JSX and reactive expression transformations. | -->
<!-- | ezno-lsp | ![](https://projects.kaleidawave.workers.dev/project/framework/badge) | Visitors and code generation for JSX and reactive expression transformations. | -->

Also checkout [oxc_type_synthesis](https://github.com/web-infra-dev/oxc/tree/main/crates/oxc_type_synthesis), a crate which allows using the type checker inside [oxc](https://github.com/web-infra-dev/oxc/tree/main)!

## Help contribute

Check out issues and comment on discussions! Read [CONTRIBUTING.md](https://github.com/kaleidawave/ezno/blob/main/CONTRIBUTING.md) for more information.
