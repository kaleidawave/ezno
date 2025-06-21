> #TODO some/all of this will be copied from or in the README

The parser contains: AST definitions, string -> AST functions, AST -> string functions (including with *pretty* mode and generating source maps), methods for traversing (visiting) and manipulating AST. A macro for generating AST. Functions for turning AST to JSON in WASM (> #TODO-issue is up in the air). It contains AST source positions. It is tested against test-262 > #TODO link.

### (supported) Syntax

> #TODO a program that covers most of JS syntax (skip some operators and stuff)

```javascript
import { something, x as b }, n from "./file.js";

let x = 2;
class Y {
	constructor() {
		x += 6 * x;
	}

	get a() {
		for (let i = 0; i < 5; i++) {

		}
	}

	set b() {
		while (false) {

		}

		switch (false) {
			false: console.log(false);
			default: {
				x
			}
		}

		if (false) {

		} else if (false) {

		}

		do {

		} while (false)
	}

	static "something" = class {
		v = {
			method() {

			}
		}
	}
}

function b() {

}
console?.log(message)
```

#### TypeScript

Supports *all* of TypeScript syntax

```typescript
...
```

> #TODO However some is under flags

```
satisfies, as, 'const generics', enum
```

#### Partial syntax

We can have parse (and analyse) partially formed AST nodes

```
const x =
const y = ;
```

> The reason for this is LSPs

#### Extra syntax

> Under flags (right?).
> #TODO examples for each
- `is` expression and `is` binary operator

- Generator keyword
- Server & worker function header
- Reverse import syntax (`from "x" import { a }`)
- Enum datatype + enum methods
- Class shorthand
- Function type annotation shorthand
- Pipe operator
- Destructuring with type annotations (> #TODO see TS issue, breaks things)
- > #TODO things tracked in ecmascript proposals (import etc)

### The parser as a library

#### Rust and WASM (API)

> #TODO

#### Configuration (parsing)

> #TODO

#### Configuration (output)

> #TODO

#### Visiting (immutable and mutable)

> #TODO holding state etc

#### Syntax errors

> #TODO parser errors

#### Generator macro

> #TODO code here

```rs
```

#### Comments

> #TODO
