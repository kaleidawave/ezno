What Ezno is:

- A type checker for JavaScript usable through a CLI ([with a LSP also in the works](https://github.com/kaleidawave/ezno/issues/22))
- Checks programs with guaranteed type safety (no runtime [`TypeError`s](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypeError)) (**as long as definitions are sound**)
- Types aimed at soundness and tracing for better static analysis
- A *imperative* type system that tracks and evaluates the side effects of functions and control flow structures. It is similar to an interpreter, but acts with types instead of *values* and does not run IO, side effects, etc.
- A collection of experiments of types. Many are being worked out and are in the prototype stage. Some of the new behaviors benefit JavaScript specifically and others could be applied to other languages
- Written in Rust
- [Fast](https://kaleidawave-getlatestgithubrun.web.val.run/kaleidawave/ezno/main?workflow=performance) and [Small](https://kaleidawave-getlatestgithubrun.web.val.run/kaleidawave/ezno/main?workflow=lines-of-code)
- Open source! [You can help build Ezno!](https://github.com/kaleidawave/ezno/issues?q=is%3Aopen+label%3Agood-first-issue%2Cfeedback-needed)
- A challenge to the status quo of type checking, optimisations and compilation through deeper static analysis beyond syntax analysis!

What Ezno is not

- **eNZo, the Z is in front of the N** (pronounce as 'Fresno' without the 'fr') 😀
- Be on parity with TSC or 1:1, it has some different behaviors **but** should work in existing projects using TSC. [You can see a full comparison of emitted errors and warnings compared with TSC here](https://kaleidawave.github.io/ezno/comparison)
- Faster as a means to serve large codebases. Cut out bloat and complex code first!
- Smarter as a means to allow more *dynamic patterns*. Keep things simple!
- A binary executable compiler. It takes in JavaScript (or a TypeScript or Ezno superset) and does similar processes to traditional compilers, but at the end emits JavaScript. However, in the future, it *could* generate a lower level format using its event (side-effect) representation.

### Motivation

> Here are some reasons why a project with these goals should exist. Most of this is finding errors and performance
> opportunities earlier

- Reliability: we want frontend code served to users to not break
- Output/artifact performance (compiler): we want to serve the leanest modules
- Usability (syntax): we want syntax to be simple and readable
- Usability (runtime): we want compilation to be fast
- Usability (tooling): we want the tool to be configurable and allow components to be reused
- Compatability (typescript): we want to not diverge from TypeScript *the obiquitous type checker*
- Compatability (javascript): we want this to be a tool for JavaScript and work with those constraints and not break too much of its existing behaviors
- Innovation and experimentation (types, checking, syntax, compilation): we want to build new things. Some may be proof-of-concepts to explore

### Using the tool

> #TODO CLI, playground, NPM, crates
