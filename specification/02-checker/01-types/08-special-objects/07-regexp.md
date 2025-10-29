> Thanks to https://github.com/kaleidawave/ezno/pull/184 and https://crates.io/crates/regress library

### Implementation

Uses the [`regress`](https://crates.io/crates/regress) crate!

#### Literals and constructors

Both syntax and `new RegExp` works

#### Evaluation

> #TODO test flags

#### Groups

> #TODO

### `RegExp` constructor

> RegExp = Regular expression
> In the future, their definition could be considered and evaluated at runtime

```ts
const regexp = /hi/ satisfies string;
```

- Expected string, found /hi/

### Invalid regular expressions

```ts
const regexp1 = /(?a2)/;
const regexp2 = new RegExp("?a2");
```

- Invalid regular expression: Invalid token at named capture group identifier
- Invalid regular expression: Invalid atom character

### Constant `RegExp.exec`

```ts
const regexp = /hi/;
const match = regexp.exec("hi");
match satisfies number;
match.index satisfies string;
match.input satisfies boolean;
```

- Expected number, found ["hi"]
- Expected string, found 0
- Expected boolean, found "hi"

### Constant `RegExp.exec` groups

```ts
const regexp = /Hi (?<name>.*)/;
const match = regexp.exec("Hi Ben");
match.input satisfies number;
match.groups satisfies string;
```

- Expected number, found "Hi Ben"
- Expected string, found { name: "Ben" }

### Constant `RegExp.exec` groups greedy

```ts
const regexp = /.*(?<x>[a-z]+)(?<y>[0-9]+)/;
const match = regexp.exec("ez as abc123");
match.input satisfies number;
match.groups.x satisfies "c";
match.groups.y satisfies boolean;
```

- Expected number, found "ez as abc123"
- Expected boolean, found "123"
