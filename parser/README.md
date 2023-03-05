# Ezno's Parser

Contains "string to AST" parser, AST definitions, AST back to text/string form methods and hooks for traversing/visiting AST. Used in the Ezno checker.

![](https://projects.kaleidawave.workers.dev/project/ezno-parser/badge)
[![](https://img.shields.io/crates/v/ezno-parser?style=flat-square)](https://crates.io/crates/ezno-parser)
[![](https://img.shields.io/docsrs/ezno-parser?style=flat-square)](https://docs.rs/ezno-parser/latest)

This is more of an exercise project in getting better at writing Rust and doesn't offer too much over other great Rust based JS parsers such as [swc](https://github.com/swc-project/swc), [rome](https://github.com/rome/tools), [oxc](https://github.com/Boshen/oxc) and [boa](https://github.com/boa-dev/boa).

## Goals:

- Keep under 15k lines of code (excluding `/tests` and `/examples` folders)
- Easy to use (see `/tests` and `/examples` folders)
- Keep readable and maintainable
- **Designed for analysis and transformations**
    - See expression identifiers can be used to bind information to
    - Retain source positions for throwing errors
    - All AST should be visitable. Immutably to collect facts or mutable to transform/remove
- Optionally via configuration extend the ECMAscript language definition
    - TypeScript type annotations
        - Interfaces, enums and type alias statements
        - Parameter, return type and variable annotations
        - `satisfies` and `as` 😑
    - JSX support
        - Includes HTML comments, special handing of self closing tags from the specification
    - Others under `feature = "extras"` 👀

## Non-goals

- CSTs, close to source operations etc
    - Source with unbalanced parenthesis/brackets
- Increase code size or decrease readability for speed improvements
- Allow adding new syntax at runtime, that would require modifying the lexer at runtime adding new tokens 

## Features

### Positions

All syntax has reference to where it was in the source using a [Span](https://docs.rs/ezno-parser/0.0.2/ezno_parser/struct.Span.html). This uses the [source-map](https://github.com/kaleidawave/source-map) crate, so it can generate source maps.

### Identifiers

Most expressions, functions, blocks and some other AST has a unique identifier. This can be used to associate information with the node. For example Ezno's checker associates type information with variable reference AST for later visitors or for hover information in the LSP.

### "Cursors"

Allows holes in AST where a cursor exists. This allows for LSP to provide suggestions here while the whole source might not be valid.

### Function extraction

All functions end up in a map rather than in the AST, which can make some static analysis and transformation operations easier.

### Visiting

[See example](https://github.com/kaleidawave/ezno/blob/main/parser/tests/visiting.rs)

### Generator

Easily generate AST nodes with data interpolation using the constant compiled quasi-quoted macro. [See example](https://github.com/kaleidawave/ezno/blob/main/parser/generator/examples/example.rs).

## Notable structures

- ASTNode, a trait that all AST should implement
- FunctionBase, a abstraction for functions
- Expression
- Statements
- Operators
- TSXToken
- TSXKeyword