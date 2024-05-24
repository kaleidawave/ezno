Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Collections

#### `Array.fill`

```ts
const array1 = [1, 2, 3, 4];

array1.fill(0, 2, 4) satisfies [1, 2, 0, 0];

array1.fill(5, 1) satisfies [1, 5, 5, 5];

array1.fill(6) satisfies [1, 1, 1, 1];
```

- Expected [1, 1, 1, 1] found [6, 6, 6, 6]

### Classes

#### `super` call

```ts
class X extends Y {
  constructor() {
    super()
  }
}
```

- uhh

### Statements

> TODO Await position.

#### `instanceof` operator

> TODO dependent version

```ts
([] instanceof Array) satisfies true;
({} instanceof Map) satisfies 4;
```

- Expected 4, found false

#### No generics

```ts
function id(a) { return a }

id<5>(4)
```

- Cannot pass generic arguments to function without generic arguments

> Or at least explicit generic arguments

### Readonly and `as const`

> TODO constrained inference
>

#### Readonly parameter

```ts
function x(p: readonly { a: string }) {
    p.a = 5;
}
```

- Cannot assign to const

### Properties

> These require fixes to free variables & key-of custom type

#### `Object.keys`, `Object.values`, `Object.entries`

```ts
Object.keys({}) satisfies string;
```

- TODO

#### `Object.fromEntries`

```ts
Object.fromEntries([["hello", "world"]]) satisfies string;
```

- TODO

#### Spread condition

```ts
declare let condition: boolean;

const obj = {
  foo: 1,
  ...(condition ? {
    bar: 2,
    non_existent: 3,
  } : {}),
};

print_type(obj.bar);
```

- TODO

#### Getter and setter through function

> TODO subtyping

```ts
let t = 0;
function x(a: { b: string }) {
    // TODO what happens here
    const b = a.b;

    a.b = 4;
}

x({ set b(v) { t = v } })
print_type(t)

x({ get b() { return 2 } })
```

- Expected string, found 5

### Exceptions and `try-catch-finally`

#### Conditional throw

> This emits a warning if a throw was created in a conditional branch

```ts
// no complex numbers :(
function checkedLn(x: number) {
    if (x > 0) {
        return Math.log(x)
    } else {
        throw new Exception("Cannot log long string")
    }
}

// Fine
try { checkedLn(Math.E ** 3) satisfies 3 } catch {}
// Will throw
try { checkedLn(-5) } catch {}
```

- Conditional 'Exception' was thrown in function

#### Function effect

```ts
function exceptionToResult(cb: () => number) {
    try {
        return cb()
    } catch (e) {
        return e
    }
}

exceptionToResult(() => 6) satisfies 6;
exceptionToResult(() => { throw 12 }) satisfies 8;
```

- Expected 8, found 12

#### Checked thrown type from callback

```ts
function exceptionToResult(cb: () => number) {
    try {
        cb()
    } catch (e: number) {
        return e
    }
}

exceptionToResult(() => { throw "not a number" })
```

- TODO

#### Internal function effect

```ts
function exceptionToResult(s: string) {
    try {
        return JSON.parse(s)
    } catch (e: number) {
        return e
    }
}
```

- Throw is SyntaxError | never

### Narrowing

#### Conditional operator

```ts
function optionalNumber(n: number | undefined): string {
    return n ?? 2
}
```

- Cannot return string, found number | 2

#### Has property

> TODO maybe need to constrain side effects here

```ts
function func(parameter: { property: string }) {
    if (parameter.property === "hello") {
        parameter.property satisfies 4;
    }
}
```

- Expected 4, found "hello"

> TODO `typeof`, `instanceof`, conditional, across a function

#### Equality

```ts
declare let a: string;
if (a === "hi") {
	a satisfies "hello"
}
```

- Expected "hello", found "hi"

### Mapped types

#### Specialisation

```ts
type Pick<T, K extends keyof T> = {
    [P in K]: T[P];
};

interface X { a: number, b: string, c: string }

const x: Pick<X, "a"> = { a: 5 };

({ b: "string" }) satisfies Pick<X, "a">;
```

- TODO

#### Optional

```ts
type Partial<T> = {
    [P in keyof T]?: T[P];
};

const x: Partial<{ a: number, b: string }> = { a: 3 },
      y: Partial<{ a: number, b: string }> = { a: "hi" }
```

- Cannot assign { a: "hi" }

#### Negated

```ts
type Required<T> = {
    [P in keyof T]-?: T[P];
};

const x: Required<{ a?: number }> = { a: 3 },
      y: Required<{ a?: number }> = { };
```

- Cannot assign { } to required

### Types

#### Infer, extends + distribution

```ts
type ElementOf<T> = T extends Array<infer U> ? U : never;

declare let mk_x: ElementOf<number>;
declare let mk_y: ElementOf<Array<number>>;
declare let mk_z: ElementOf<Array<string> | string>;

print_type(mk_x, mk_y, mk_z);
```

- TODO
