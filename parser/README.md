# Ezno's Parser

Contains "string to AST" parser, AST definitions, AST back to text/string form methods and hooks for traversing/visiting AST. Used in the Ezno checker.

![](https://projects.kaleidawave.workers.dev/project/ezno-parser/badge)

This is more of an exercise project and doesn't offer too much over other great Rust based JS parsers such as [swc](https://github.com/swc-project/swc), [rome](https://github.com/rome/tools) and [boa](https://github.com/boa-dev/boa).

### Goals:

- Keep under 15k lines of code (excluding `/tests` and `/examples` folders)
- Easy to use (see examples folder)
- Easy to read and is maintainable
- **Useful for analysis and transformations**
    - See expression identifiers & source positions
    - Contains visiting implementation
- *Optionally* extend the ecmascript language definition
    - TypeScript type annotations, interfaces and enums
    - JSX support
    - Others under `feature = "extras"`

### Non-goals

- CSTs, close to source operations etc
- Increase code size or decrease readability for speed improvements
- Allow new syntax at that would require modifications the lexer at runtime

### "Cursors"

- Allows holes in AST where a cursor exists, allows for LSP to provide suggestions here.

### Notable structures

- ASTNode, a trait that all AST should implement
- FunctionBase, a abstraction for functions
- Expression
- Statements
- Operators
- TSXToken
- TSXKeyword