<!-- ![visiting-proc-macro lines of code](https://kaleidawave-projectinformation.web.val.run/project/ezno-parser-visitable-derive/badge) -->

A package which generates mutable and immutable *visiting* implementations for AST nodes.

A visitor is a implementor a trait of which the a function is called of a each node.

This module is specific for the *parser* module and contains the ability to extend with data.
## Usage

```rust
use visitable_derive::Visitable;

#[derive(Visitable)]
struct MyAstNode {
	...
}
```

## Attributes
### `visit_self`
Will visit all fields **first then** self

```rust
#[derive(Visitable)]
#[visit_self]
struct MyAstNode {
	...
}
```

Options:
- `also_visit_first_if`
Will visit self additionally before if predicate is true

```rust
#[derive(Visitable)]
#[visit_self(also_visit_first_if="self.predicate()"]
struct MyAstNode {
	...
}

impl MyAstNode {
	fn predicate(&self) -> bool {
		...
	}
}
```

### `visit_skip_field`
Skips visiting the field. Used if type does not implement `Visitable`

```rust
#[derive(Visitable)]
struct MyAstNode {
	#[visit_skip_field]
	a: String
}
```
