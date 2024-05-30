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

#### Nominal-ness

```ts
class X { a: number = 2 }
class Y { a: number = 2}

function doThingWithX(x: X) {}

doThingWithX(new X());
doThingWithX(new Y())
```

- Argument of type [Y] { a: 2 } is not assignable to parameter of type X

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
function MyClass(this: { other: string }) { }

MyClass.prototype.other = 2;

const m = new MyClass();
```

- The 'this' context of the function is expected to be { other: string }, found { other: 2 }

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

#### Readonly parameter

```ts
function x(p: readonly { a: string }) {
    p.a = 5;
}
```

- Cannot assign to immutable property

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

> TODO technically `.bar` should be 2 | undefined

```ts
declare let condition: boolean;

const obj = {
  foo: 1,
  ...(condition ? {
    bar: 2,
    non_existent: 3,
  } : {}),
};

obj.foo satisfies number;
obj.bar satisfies string;
```

- Expected string, found 2 | undefined

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

- Cannot catch type number because the try block throws SyntaxError

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

#### `keyof` type annotation

```ts
interface X {
    a: string,
    b: string
}

"a" satisfies keyof X;
"b" satisfies keyof X;
"c" satisfies keyof X;
```

- Expected keyof X, found "c"

#### Template literal types

```ts
type Introduction = `Hello ${string}`;

const first: Introduction = "Hello Ben";
const second: Introduction = "Hi Ben";
```

- Type "Hi Ben" is not assignable to type Introduction

### Function calling

#### Canceled generics

> aka remove generic arguments if they don't match the structure

```ts
declare function func<T>(prop: { a: number, b: T, c: string } | { a: number, b: string, c: T }): T;

func({ a: 3, b: "hi", c: false }) satisfies string;
```

- Expected string, found false

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

#### Use type annotation in the presence of error

> Note x and y are still string and the function still returns string

```ts
const x: string = 5;
const y: string = h;

function getString(a: number): string {
    return a
}

x satisfies string;
y satisfies string;

getString(2) satisfies number;
```

- Expected number, found string
- Cannot return number because the function is expected to return string
- Type 5 is not assignable to type string
- Could not find variable 'h' in scope

#### Unconditional throw

```ts
function safeDivide(num: number, denom: number) {
	if (denom === 0) {
		throw new Error("Cannot divide by zero");
	}
	return num / denom
}

safeDivide(8, 4) satisfies 2;

safeDivide(10, 0);
```

- Conditional '[Error] { message: \"Cannot divide by zero\" }' was thrown in function

### Closures

#### TDZ

```ts
function func() {
    return function () { return closedOverVariable }
    let closedOverVariable = 2;
}
```

- Unreachable statement
- Function contains unreachable closed over variable 'closedOverVariable'

### Object constraints

#### Mutation by a function with unknown effects

> This is where the object loses its constant-ness
> Effectively raises it to the parameter type

```ts
function doThingWithCallback(callback: (obj: { prop: number }) => any) {
	const obj = { prop: 8 };
	callback(obj);
	(obj.prop satisfies 8);
	return obj;
}

const object = doThingWithCallback((obj: { prop: number }) => obj.prop = 2);
object.prop satisfies string;
```

- Expected 8, found number
- Expected string, found 2

#### Mutation negated via `readonly`

> This is where the object loses its constant-ness

```ts
function doThingWithCallback(callback: (obj: readonly { prop: number }) => any) {
	const obj = { prop: 8 };
	callback(obj);
	(obj.prop satisfies 6);
}
```

- Expected 6, found 8

#### Possible mutation breaks object constraint

> This unfortunately can flag up valid code, but handling those is too difficult atm

```ts
function doThingWithCallback(callback: (obj: { prop: number | string }) => any) {
	const obj: { prop: number } = { prop: 8 };
	callback(obj);
}
```

- Cannot raise TODO. If possible avoid the constraints or mark parameter as readonly

#### Possible mutation via anytime function

```ts
const x = { a: 2 }
setTimeout(() => { Math.sin(x.a) })
x.a = "hi"
```

- Cannot assign. Restricted to number
