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
let b: number = 0;
class Y {
    constructor(a) {
        this.a = a;
        b++;
    }
}

class X extends Y {
    constructor(a) {
        super(a);
    }
}

const x = new X("hi");
x.a satisfies "hello";
b satisfies 1;
```

- Expected "hello", found "hi"

### Statements

> TODO Await position.

#### `new` on function prototype

```ts
function MyClass(value) {
	this.value = value
}

MyClass.prototype.other = 2;

const object = new MyClass("hi");
object.value satisfies "hi";
object.other satisfies "hello";
```

- Expected "hello", found 2

#### Checking with function prototype

```ts
function MyClass(this: { other: string }, value) {
	this.value = value
}

MyClass.prototype.other = 2;

const object = new MyClass("hi");
```

- Cannot call with { other: 2 }, required { other: string } (or smth like that)

#### `instanceof` operator

> TODO dependent version

```ts
([] instanceof Array) satisfies true;
({} instanceof Map) satisfies 4;

class X {}

(new X instanceof X) satisfies true;
([] instanceof X) satisfies false;
```

- Expected 4, found false

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

#### Condition as a function

```ts
declare let a: string;

const equalsHi = (p: string) => p === "hi";

if (equalsHi(a)) {
	a satisfies "hello"
}
```

- Expected "hello", found "hi"

#### Passed around

```ts
declare let a: string;

const b = a;
if (b === "hi") {
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

#### Union with never

```ts
declare function func<T>(): T | string;

func<number>() satisfies string | number;
func<never>() satisfies boolean;
```

- Expected boolean, found string

#### Infer and extends distribution

```ts
type ElementOf<T> = T extends Array<infer U> ? U : never;

declare let y: ElementOf<Array<number>>;
declare let z: ElementOf<Array<number> | string>;

y satisfies number;
z satisfies string;

declare let n: never;
n satisfies ElementOf<number>;
```

- Expected string, found number

### Functions

#### Function hoisting

> `getString` can be used and has a type before it has been synthesised
> TODO actual calling before defined (this currently only works bc of free-variables)

```ts
function x() {
    getString(3)
}

function y() {
    getString("something") satisfies string;
}

function getString(param: string): string {
    return "hi"
}
```

- Argument of type 3 is not assignable to parameter of type string

### Others

#### Unconditional throw

```ts
function safeDivide(num: number, denom: number) {
	if (denom === 0) {
		throw Error("Cannot divide by zero");
	}
	return num / denom
}

safeDivide(8, 4) satisfies 2;

safeDivide(10, 0);
```

- thrown!
