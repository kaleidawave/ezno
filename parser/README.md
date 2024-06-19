# Ezno's Parser

Contains "string to AST" parser, AST definitions, AST back to text/string form methods and hooks for traversing/visiting AST. Used in `ezno-checker` and the Ezno toolchain.

![parser lines of code](https://projects.kaleidawave.workers.dev/project/ezno-parser/badge)
[![crates.io badge](https://img.shields.io/crates/v/ezno-parser?style=flat-square)](https://crates.io/crates/ezno-parser)
[![docs.rs badge](https://img.shields.io/docsrs/ezno-parser?style=flat-square)](https://docs.rs/ezno-parser/latest)

```rs
use ezno_parser::{ASTNode, Expression};

fn main() {
	let expressions = [
		"4 + 2 * 5",
		"4 * 2 + 5",
		"4 * 2 * 5",
		"console.log(4 * 2, t ? true : `Hi`) == 2 && 4 == 2",
	];
	for expression in expressions {
		let expression = Expression::from_string(expression.to_owned(), Default::default());
		println!("{expression:#?}");
	}
}
```

> Also checkout other parsers such as [boa](https://github.com/boa-dev/boa), [biome](https://github.com/biomejs/biome), [oxc](https://github.com/oxc-project/oxc) and [swc](https://github.com/swc-project/swc).

## Goals

- Keep under 15k lines of code (excluding `/tests` and `/examples` folders)
- Easy to use API (see `/tests` and `/examples` folders for example usage)
- Run in WASM
- Keep readable and maintainable
- **Designed for analysis and transformations**
   	- See expression identifiers can be used to bind information to
   	- Retain source positions for use in analysis diagnostics and generating source maps
   	- All AST should be visitable. Immutably to collect facts or mutable to transform/remove
- Optionally via configuration extend the *ECMAScript language definition*
   	- TypeScript type annotations
      		- Interfaces, enums and type alias statements
      		- Parameter, return type and variable annotations
      		- `satisfies` and `as` 😑
   	- JSX support
      		- Includes HTML comments, special handing of self closing tags from the specification
   	- Others under `feature = "extras"` 👀
- Transformation and visiting
   	- [See example](https://github.com/kaleidawave/ezno/blob/main/parser/tests/visiting.rs)
   	- `ezno-parser-visitable-derive` is a macro that automates/generates the visiting implementation
   	- The generator macro also makes creating AST easy. [See example](https://github.com/kaleidawave/ezno/blob/main/parser/generator/examples/example.rs)
- Positions in source
   	- All syntax has reference to where it was in the source using a [Span](https://docs.rs/ezno-parser/latest/ezno_parser/type.Span.html). This uses the [source-map crate](https://github.com/kaleidawave/source-map) crate which makes it trivial to build source maps.
- Partial AST
   	- Most of the parser requires valid input. However under a option you can enable a option which can add marked nodes for cases where a expression might be missing. This allows tools that require an AST to work with a source that is still being edited (such as in a LSP).
   	- It checks two cases, if either of these cases is encountered, then it adds a marker node: (1) Whether the expression ends in `)` etc (useful in `if` etc) or (2) Whether the next token is `const` etc and on the next line
- Output
   	- Stripping type annotations can be stripped from output using `ToStringOptions { include_types: false, ..Default::default() }`
   	- Adding indentation under `pretty: true`, not adding whitespace for production builds
   	- Setting `max_line_length` to a size to wrap certain structures
   	- Support for source map mapping generation

## Non-goals

- CSTs, close to source operations etc
   	- Source with unbalanced parenthesis/brackets
- Increase code size or decrease readability for minor speed improvements

## Testing

> If in main root rather than this folder, add `-p ezno-parser` after `cargo run` to the following commands.

For testing whether the parser can lex a file

```shell
cargo run --example lex path/to/file.js
```

and parse

```shell
cargo run --example parse path/to/file.js
```

> Note the Ezno CLI includes the `ast-playground` subcommand: a more user oriented version of these commands
