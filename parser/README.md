# Ezno's Parser

Contains "string to AST" parser, AST definitions, AST back to text/string form methods and hooks for traversing/visiting AST. Used in the Ezno checker.

![lines of code badge](https://projects.kaleidawave.workers.dev/project/ezno-parser/badge)
[![crates.io badge](https://img.shields.io/crates/v/ezno-parser?style=flat-square)](https://crates.io/crates/ezno-parser)
[![docs.rs badge](https://img.shields.io/docsrs/ezno-parser?style=flat-square)](https://docs.rs/ezno-parser/latest)

> [This parser was started before the checker in early 2021](https://twitter.com/kaleidawave/status/1347297111579226120). While usable as a project it is oriented towards the checker.
> This is more of an exercise project in getting better at writing Rust and doesn't offer too much over other great Rust based JS parsers such as [swc](https://github.com/swc-project/swc), [rome](https://github.com/rome/tools), [oxc](https://github.com/Boshen/oxc) and [boa](https://github.com/boa-dev/boa).

## Goals

- Keep under 15k lines of code (excluding the `/tests` and `/examples` folders)
- Easy to use API (see `/tests` and `/examples` folders)
- Keep readable and maintainable
- **Designed for analysis and transformations**
    - All expressions have identifiers that information (such as type information) can be associated to
    - Retain source positions for diagnostics to reveal locations
    - All AST should be visitable. Either immutably to collect facts or mutable to transform or remove nodes
- Optionally via configuration extend the ECMAscript language definition
    - TypeScript type annotations
        - Interfaces, enums and type alias statements
        - Parameter, return type and variable annotations
        - `satisfies` and `as` 😑
    - JSX support
        - Includes HTML comments, special handing of self closing tags from the specification
    - Others under `feature = "extras"` 👀

## Non-goals

- CSTs, close to source operations
    - e.g. parsing sources with unbalanced parenthesis/brackets
- Increase code size or decrease readability for speed improvements
- Allow adding new syntax at runtime

## Notable structures

- [ASTNode](https://docs.rs/ezno-parser/latest/ezno_parser/trait.ASTNode.html), a trait that all AST should implement
- [FunctionBase](https://docs.rs/ezno-parser/latest/ezno_parser/functions/struct.FunctionBase.html), a abstraction for functions
- [Expression](https://docs.rs/ezno-parser/latest/ezno_parser/expressions/enum.Expression.html), [Statement](https://docs.rs/ezno-parser/latest/ezno_parser/statements/enum.Statement.html) and [Declaration](https://docs.rs/ezno-parser/latest/ezno_parser/declarations/enum.Declaration.html)

## Details

### Lexing

You can view the lexing output with `cargo run -p ezno-parser --example lex ./path/to/file.js`

Some sections of syntax is combined like the triple equal operator `===` etc. These tokens are produced by the lexer. and they are combined by `derive-finite-automaton`. Derive finite automaton forms a "compile time trie" from a [declarative format](https://github.com/kaleidawave/ezno/blob/75d31ddb60eee495915fcf805a56221d2e79ce7d/parser/src/tokens.rs#L16).

The handler for the tokens is defined in [Tokenizer lib](https://crates.io/crates/tokenizer-lib). The crate contains three tokenizer implementations. Currently native platforms use a [std::sync::mpsc::channel](https://doc.rust-lang.org/std/sync/mpsc/fn.channel.html) token reader. In this case the lexer is run in another thread to the parser. As WASM doesn't support threads at the moment there is also a buffered approach, running the lexer first then passing the tokens to the parser.

#### The `>>` problem

TS generic syntax clashes with JavaScript bitwise operators. This is normally fine for `<` as the parser can just interpret this *less than* token as a chevron in places where the parser expects less than tokens. However for the `>>` token it is encoded as bitwise shift right by derive finite automaton. This creates problems as there is a token split over the boundary over a section of responsibility for the parser.

The implementation that fixes it is not great, but at the moment the head token on the token reader can be mutated. When this token is encountered it splits it up. [See the implementation](https://github.com/kaleidawave/ezno/blob/75d31ddb60eee495915fcf805a56221d2e79ce7d/parser/src/types/type_references.rs#L728-L760).

#### Regex and JSX literals

Another clash of syntax is JSX literals and prefix type casts. I think it might be possible if the lexer had context. Unfortunately Ezno's is parser context free. Instead JSX is only lexed when the [config is enabled](https://github.com/kaleidawave/ezno/blob/75d31ddb60eee495915fcf805a56221d2e79ce7d/parser/src/lexer.rs#L21).

The chevron syntax is a bit overloaded
1. `x < 2`
2. `x<4>(t)`
3. `<x>2</x>`

To distinguished between cases 1 and 2, generic type arguments are only read when there is no gap between the tokens and that looking ahead after the chevrons it is immediately followed by an open parenthesis. [See `is_generic_arguments`](https://github.com/kaleidawave/ezno/blob/75d31ddb60eee495915fcf805a56221d2e79ce7d/parser/src/expressions/mod.rs#L1363).

To distinguished between 3 from 1 and 2 requires handing JSX in the lexer. This is because the elements text needs to be treated as literal. Therefore the lexer needs to find expression boundaries. This is done by a bool in the lexing loop that is flipped when the previous token was something like `=`, `=>`, `return`.

This feature is also used for the Regex parsing, similar to JSX it needs to be treated as literal. [See the lexer](https://github.com/kaleidawave/ezno/blob/75d31ddb60eee495915fcf805a56221d2e79ce7d/parser/src/lexer.rs#L162-L167).

## The AST

All AST implements the [`ASTNode` trait](https://docs.rs/ezno-parser/0.0.3/ezno_parser/trait.ASTNode.html). This trait has three things:
1. Getting a source position (and associate file) under the `Span` type of an `ASTNode`
2. Parsing from a token stream `ASTNode::from_string` including the lexing
3. Turning the AST back into the string form. This takes settings so the AST can be printed without whitespace etc. It also can include with or without a source map, or without the buffer at all to get the output length.

### The [source-map crate](https://crates.io/crates/source-map)

The position information is based of one of my own crates [source-map](https://crates.io/crates/source-map).

The crate contains lots of abstract string position information
- Contains `Span` and `Position` information
- Contains ways to convert to from literal slices to line-column items
- Contains the `ToString` definition which is used to make to allow to stringing to generate source maps.

#### Generating source maps from the AST

From the `ASTNode::to_string`  definition it works over multiple `ToString`s
- Buffer into standard `std::string::String`
- Buffer source map, which is a string bundled with a source map
    - Keeps track of how much is in the buffer, when to add
- Count length of strings being added to get a size of the module without creating a string.

### The visitable trait and its macro

Every AST has handlers for calling a set of a set of callbacks on the item. This recurses through the AST. While strictly following the visitor design. The design for this
is work in progress. The checker has its own method for traversing AST.

Built using another one of my libraries [syn-helpers](https://crates.io/crates/syn-helpers)

#### Chains

Chains are built into the visitors. This enables a reference of where the current AST is. This was a feature from a while ago which doesn't have much usage now.

### Strictly typed AST

Cursors are special places where syntax is not complete. Otherwise things like expressions (rhs of assignments and declarations, function parameters) have declarations that doesn't allow invalid AST. Unfortunately there are a few exceptions which are pretty much impossible to strongly type in Rust (aka parameters in strict mode).

## AST Generator

Another experiment as I was tired of writing long and deep expressions. It is inspired by `parse_quote` on the Rust side.

This actually does the parsing at compile time, not just runtime.
Constant compilation in Rust is a bit difficult (Zig has a better implementation). The biggest thing is allocating in Rust const contexts is not possible. Running the macro you can get around this.

```text
use ezno_ast_generator::stmt;
use parser::ASTNode;

fn main() {
	let content = "World!";
	let my_stmt = stmt!(let my_element = <h1>Hello {#content}</h1>);
	println!("{}", my_stmt.to_string(&Default::default()));
}
```

This started by turning the a tokenization one. Created special tokens in the lexer which were formed abstract syntax trees, however it complicated lexing.

You can try the results out:

```shell
> gh clone kaleidawave/ezno
> cd ezno/parser/generator
# If don't already have it
> cargo install cargo-expand
> cargo expand --example example1
...
```

### Self tokenization

Every AST node can be turned into Rust tokens. Run the parser in a macro. Take the result and produce its token representations. This is why all AST structs contain the following line:

```text
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
```

#### Interpolations

Interpolation is done using the cursors.

This does mean that interpolations can only happen at certain points compared to Rust `quote!` however I think that is a reasonable compromise and ensures better

Syntax errors are also caught at compile time.
