A JavaScript compiler and TypeScript checker written in Rust with a focus on static analysis and runtime performance.

![](https://projects.kaleidawave.workers.dev/project/ezno/badge)


This project is a workspace consisting of a few crates:

| Crate | Lines Of Code <div style="min-width: 150px"><div> | Description  |
|---|---|---|
| parser | ![](https://projects.kaleidawave.workers.dev/project/ezno-parser/badge) | Contains AST definitions, logic for parsing and serializing, and visiting |
<!-- | checker | ![](https://projects.kaleidawave.workers.dev/project/ezno-parser/badge) | Contains logic for validating types in AST, generating in memory type representation, a 7 phase visiting step for extensions and low level interpolation and graphing of code | 
| ezno-web-framework | ![](https://projects.kaleidawave.workers.dev/project/framework/badge) | Visitors and code generation for JSX and reactive expression transformations. | -->

Read some more detailed posts
- [Introducing Ezno](https://kaleidawave.github.io/posts/introducing-ezno/)
- [Ezno '23](https://kaleidawave.github.io/posts/ezno-23/)

## Experimental

This is an experimental compiler. If you are looking for a stable compiler, Ezno is not the right choice **at the moment**.

## Type checking

Ezno is a type checker based on TypeScript type annotations.

### Features

- Declare interfaces and other type definitions
- Usage checking: property access, function parameters etc
- Effects that track mutations across functions
- Parameter constraint inference
- Powerful dependent type system

### Differences to checking TSC

- The `any` type has no properties on it
- Type annotations on variables are the *reassignment constraint*, not it's current value. Current value is inferred and is mutable
- ...

*ultimately the differences shouldn't break existing code and in any case pick up more errors than TSC*

## Help contribute

Check out issues. Comment on discussions.
