A fast, rich and correct TypeScript type checker and compiler

> [!IMPORTANT]
> Ezno is in active development and **does not currently support enough features to check existing projects** (see [blocking issues](https://github.com/kaleidawave/ezno/labels/blocking)). Check out the [getting started guide](./checker/documentation/getting-started.md) for experimenting with what it [currently supports](./checker/specification/specification.md).

<!-- ![project lines of code](https://projects.kaleidawave.workers.dev/project/ezno/badge) -->

What Ezno is:
- A type checker for JavaScript usable through a CLI ([with a LSP also in the works](https://github.com/kaleidawave/ezno/issues/22))
- Checks programs with guaranteed type safety (no runtime [`TypeError`s](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypeError)) (**as long as definitions are sound**)
- Types aimed at soundness and tracing for better static analysis
- A *imperative* type system that tracks and evaluates the side effects of functions and control flow structures. It is similar to an interpreter, but acts with types instead of *values* and does not run IO, side effects, etc.
- A collection of experiments of types. Many are being worked out and are in the prototype stage. Some of the new behaviors benefit JavaScript specifically and others could be applied to other languages
- Written in Rust
- [Fast](https://github.com/kaleidawave/ezno/actions/workflows/performance-and-size.yml) and [Small](https://github.com/kaleidawave/ezno/actions/workflows/lines-of-code.yml)
- Open source! [You can help build Ezno!](https://github.com/kaleidawave/ezno/issues?q=is%3Aopen+label%3Agood-first-issue%2Cfeedback-needed)
- A challenge to the status quo of type checking, optimisations and compilation through deeper static analysis beyond syntax analysis!

What Ezno is not
- **eNZo, the Z is in front of the N** (pronounce as 'Fresno' without the 'fr') 😀
- Be on parity with TSC or 1:1, it has some different behaviors **but** should work in existing projects using TSC
- Faster as a means to serve large codebases. Cut out bloat and complex code first!
- Smarter as a means to allow more *dynamic patterns*. Keep things simple!
- A binary executable compiler. It takes in JavaScript (or a TypeScript or Ezno superset) and does similar processes to traditional compilers, but at the end emits JavaScript. However, in the future, it *could* generate a lower level format using its event (side-effect) representation.

Read more about Ezno (in chronological order)
- [Introducing Ezno](https://kaleidawave.github.io/posts/introducing-ezno/)
- [Ezno in '23](https://kaleidawave.github.io/posts/ezno-23/)
- [A preview of the checker](https://kaleidawave.github.io/posts/a-preview-of-the-checker/)
- [The quest continues](https://kaleidawave.github.io/posts/the-quest-continues/)

---

This project is a workspace consisting of a few crates:

| Crate | Lines Of Code | Contains |
|---|---|---|
| checker | ![checker lines of code](https://projects.kaleidawave.workers.dev/project/ezno-checker/badge) | Stores for types and contexts, type checking logic and optional synthesis over the parser AST |
| parser | ![parser lines of code](https://projects.kaleidawave.workers.dev/project/ezno-parser/badge) | AST definitions, logic for parsing, AST to string and visiting |
<!-- | ezno-web-framework | ![](https://projects.kaleidawave.workers.dev/project/framework/badge) | Visitors and code generation for JSX and reactive expression transformations. | -->
<!-- | ezno-lsp | ![](https://projects.kaleidawave.workers.dev/project/framework/badge) | Visitors and code generation for JSX and reactive expression transformations. | -->

## Help contribute

Check out [good first issues]((https://github.com/kaleidawave/ezno/issues?q=is%3Aopen+label%3Agood-first-issue%2Cfeedback-needed)) and comment on discussions! Feel free to ask questions on parts of the code of the checking implementation.

Read [CONTRIBUTING.md](https://github.com/kaleidawave/ezno/blob/main/CONTRIBUTING.md) for information about building and testing.
