Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Broken

> Was working, now not

#### *Inconclusive* conditional update

```ts
declare var value: string;
let a: string | number = 0;

function conditional(v: string) {
	if (v === "value") {
		a = "hi"
	}
}
conditional(value);
a satisfies string;
```

- Expected string, found "hi" | 0

#### Break with label

```ts
let a: number = 0;
let result;

top: while (a++ < 10) {
	let b: number = 0;
	while (b++ < 10) {
		if (a === 3 && b === 2) {
			result = a * b;
			break top
		}
	}
}

a satisfies string;
result satisfies boolean;
```

- Expected string, found 3
- Expected boolean, found 6

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

### Readonly and `as const`

> TODO constrained inference

#### Readonly parameter

```ts
function x(p: readonly { a: string }) {
    p.a = 5;
}
```

- Property not writeable

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

### Exceptions and `try-catch-finally`

#### Conditional throw

> This emits a warning if a throw was created in a conditional branch

```ts
// no complex numbers :(
function checkedLn(x: number) {
    if (x > 0) {
        return Math.log(x)
    } else {
        throw new Error("Cannot log")
    }
}

// Fine
try { checkedLn(Math.E ** 3) satisfies 3 } catch {}
// Will throw
try { checkedLn(-5) } catch {}
```

- Conditional '[Error] { message: \"Cannot log\" }' was thrown in function

### Variables

#### `var` can be reregistered

```ts
{
    // Fine
    var x = 2;
    x satisfies 2;
    var x = 3;
    x satisfies 3;
}

{
    let b = 2;
    var b = 2;
}
```

- Cannot redeclare variable 'b'

### Effects

#### `throw` short-circuit

```ts
let x: number = 2;

function func(cb: () => void) {
	try {
		cb();
		x = 10;
		return "not-thrown"
	} catch {
		return "thrown"
	}
}

func(() => { throw "error" }) satisfies "thrown";
x satisfies string;
```

- Expected string, found 2

#### `delete` as an effect

```ts
function dewete(param: { prop: string }) {
	const { prop } = param;
	delete param.prop;
	return prop
}

const obj = { prop: "hi" };
dewete(obj);
obj.prop;
```

- No property 'prop' on {}

### Calling type

#### Generics on dependent type

> Weird annotation here is to work around [#165](https://github.com/kaleidawave/ezno/issues/165)

```ts
function createNew(cb: { f<T>(t: T) => { a: T }}["f"]) {
	return cb(4)
}

createNew satisfies string;
```

- Expected string, found { a: 4 }

#### Builder pattern

> Testing for `this` returning

```ts
class StringBuilder {
    s: string = ""

    append(s: string) {
        this.s += s;
        return this
    }

    finish() {
        return this.s
    }
}

(new StringBuilder).append("Hello ").append("Ben").finish() satisfies number
```

- Expected number, found "Hello Ben"

#### Calling or

> *Calling* is distributive `(A | B)()` -> `A() | B()`

```ts
function a(p: string) { return 2 }
function b(p: string) { return 4 }

function c(c: boolean) {
	const func = c ? a : b;
	const result = func()
	result satisfies string;
	return result
}
```

- Expected string, found 2 | 4

#### Setter or

```ts
TODO
```

- Expected string, found 2 | 4

### To sort

#### Conditional break

```ts
function getNumber(a: number) {
	for (let i = 0; i < 10; i++) {
		if (i === a) {
			return "found"
		}
	}
	return "not-found"
}

getNumber(4) satisfies "found";
getNumber(100) satisfies boolean;
```

- Expected boolean, found "not-found

#### Dependent operations

```ts
function isFive(a: number): boolean {
	return a === 5
}

isFive(5) satisfies true;
isFive(6) satisfies string;

function hasPropertyX(obj: object): boolean {
	return "x" in obj;
}

hasPropertyX({ a: 2 }) satisfies false;
hasPropertyX({ x: 5 }) satisfies number;
```

- Expected string, found false
- Expected number, found true

#### Regexp patterns

```ts
new RegExp("<string>x").group.string
```

- ?

#### Array slice matching pattern

```ts
type Head<T> = T extends [infer H, ...Array<any>] ? H : null;

type Tail<T> = T extends [any, ...infer Tail] ? Tail : [];
```

- ?

#### String slice matching pattern

```ts
type GetPrefix<P, S> = S extends `${P}${infer T}` ? T : S;
```

- ?

#### `infer ... extends ...`

```ts
type X<T> = T extends { a: infer I extends string } ? I : string;

declare let a: X<{ a: 4 }>;
declare let b: X<{ a: "hello" }>;

a satisfies number;
b satisfies "hello";
```

- Expected number, found string

#### Properties on big or

> TODO this creates a fat or type

```ts
const array = [1, 2, 3, 4, 5, 6, 7, 8]
array[Math.random()] satisfies 4;
```

- ?

#### Array destructuring iterator

```ts
TODO
```

- ?

#### Out of order generics

```ts
```

- ?

#### Known symbol inference & checking

> #TODO extra

```ts
class X {
	[Symbol.iterator]() {
		return {}
	}
}
```

- ?

#### Un-delete-able property

```ts
const x: { a?: number } = { a: 4 };
// Fine
delete x.a;

const y: { a: number } = { a: 4 };
// Bad
delete y.a;
```

- Cannot delete 'a' from { a: number }

### Collections

#### Mutation

> This is part of [assignment mismatch](#assignment-mismatch)

```ts
function fakeRead(a: Array<string | number>) {
	a.push(2)
}

const array1: Array<string> = []
fakeRead(array1)
```

- Invalid assignment to parameter

#### Array filter

```ts

```

- ?

#### Array slice

```ts

```

- ?

#### Array splice

```ts

```

- ?

#### Array shift and unshift

```ts

```

- ?

#### Array copying methods

```ts
toReversed
with
```

- ?

#### Array find & index of

```ts
indexOf
lastIndexOf
find
findIndexOf
```

- ?

#### Array constructor

```ts
new Array({})
```

- ?

#### Array concat and spread push

```ts
concat()
push(...x)
```

- ?

#### Array values and entries

> Iterators

```ts
```

- ?

#### Array flat

```ts
flatten
flatMap
```

- ?

#### Array reducers

> May be hard bc reversed generics order

```ts
reduce
reduceRight
```

- ?

#### Map `set` and `get`

```ts
const x = new Map();
x.set(4, 2);
x.set(4, 3);
x.get(4) satisfies 2;
x.get(2) satisfies string;
```

- Expected 2, found 3
- Expected string, found undefined

#### Map `items`

```ts
const x = new Map();
x.items()
```

- ?

#### Map generics

```ts
const x: Map<number, string> = new Map();
x.set(4, false);

const y = new Map();
y.set(6, 2);
y.set(4, "hi");
y satisfies string;
```

- Expected string, found Map<6 | 4, 2 | "hi">

### Inferred return types

#### Simple

```ts
function id(a: number) {
	return a
}

function simple() {
	return "hello world"
}

id satisfies (n: number) => number;
simple satisfies () => number;
```

- Expected () => number, found () => "hello world"

#### Conditional

```ts
function t<T extends boolean>(condition: T) {
	if (condition) {
		return 4
	} else {
		return 4
	}
}
```

- ?

#### Early return

```ts
function func(value: number) {
	if (value === 3) {
		return "is three"
	}
	console.log("hi")
	return "another"
}

loop satisfies (a: number) => "is three" | "another";

function loop(value: number) {
	for (let i = 0; i < 10; i++) {
		if (value === i) {
			return "something"
		}
	}
	return "another"
}

loop satisfies (a: number) => "something" | "another";

function sometimes(a: boolean) {
	if (a) {
		return "sometimes"
	}
}

sometimes satisfies (a: boolean) => string;
```

- Expected (a: boolean) => string, found (a: boolean) => "sometimes" | undefined

#### `throw` in body

```ts
function throwSomething() {
	throw "to implement!"
}

throwSomething satisfies string;
```

- Expected string, found () => never

> #TODO try-catch, Promise
