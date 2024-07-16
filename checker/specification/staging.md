Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Mapped types

#### Readonly and optionality carries through

```ts
type Mapped<T> = {
	[P in keyof T]: string
}

interface Y { readonly a: string, b?: number }
declare let x: Mapped<Y>;

x.a = "hi";
x.b satisfies number;
```

- Cannot write to property 'a'
- Expected number, found number | undefined

#### Specialisation

```ts
type Pick<T, K extends keyof T> = {
    [P in K]: T[P];
};

interface X { a: number, b: string, c: string }

const x: Pick<X, "a"> = { a: 5 };

declare let y: Pick<X, "b" | "c">;
y.b satisfies string;
y.a;
```

- No property 'a' on { ["b" | "c"]: X["b" | "c"] }

#### Optional

```ts
type Partial<T> = {
    [P in keyof T]?: T[P];
};

const x: Partial<{ a: number, b: string }> = { a: 3 },
      y: Partial<{ a: number, b: string }> = { a: "hi" }
```

- Cannot assign { a: "hi" }

#### Negated optionality

```ts
type Required<T> = {
    [P in keyof T]-?: T[P];
};

const x: Required<{ a?: number }> = { a: 3 }, y: Required<{ a?: number }> = { };
```

- Cannot assign { } to required

#### Readonly

```ts
type Mutable<T> = {
    readonly [P in keyof T]: T[P];
};

interface Y { a: string }
declare let x: Mutable<Y>;
x.a = "hi";
```

- Cannot write to property 'a'

#### Negated readonly

```ts
type Mutable<T> = {
    -readonly [P in keyof T]-?: T[P];
};

interface Y { readonly a: string }
declare let x: Mutable<Y>;
x.a = "hi";
x.a = 4
```

- Cannot assign 4, to type of string

#### `as` rewrite

```ts
type PrefixKeys<T> = {
	[P in ((keyof T) & string) as `property_${P}`]: T[P];
};

interface X { a: number };

declare let x: PrefixKeys<X>;
x.property_a satisfies number;
x.property_b
```

- No property 'property_b' on { [string]: X[keyof X & string] }

### Readonly and `as const`

> TODO constrained inference

#### Readonly parameter

```ts
function x(p: readonly { a: string }) {
    p.a = "hi";
}
```

- Cannot write to property 'a'

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

#### Throw through internal callback

```ts
try {
	[1, 2, 3].map((x: number) => {
		if (x === 2) {
			throw "error"
		}
	});
	console.log("unreachable")
} catch (e) {
	e satisfies number;
}
```

- Unreachable statement
- Expected number, found "error"

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
function dewete(param: { prop?: string }) {
	const { prop } = param;
	delete param.prop;
	return prop
}

const obj = { prop: "hi" };
dewete(obj);

obj.prop;
```

> This error *should* be from the last statement

- No property 'prop' on {}

### Calling type

#### Generics on dependent type

> Weird annotation here is to work around [#165](https://github.com/kaleidawave/ezno/issues/165)

```ts
function createNew(cb: { f<T>(t: T): { a: T }}["f"]) {
	return cb(4)
}

createNew satisfies string;
```

- Expected string, found (cb: \<T\>(t: T) => { a: T }) => { a: 4 }

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
type GetPrefix<S, End> = S extends `${infer T} ${End}` ? T : false;

4 satisfies GetPrefix<"Hello Ben", "Ben">;
```

- Expected "Hello", found 4

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

> TODO in a function as well

```ts
const x: { a?: number } = { a: 4 };
// Fine
delete x.a;

const y: { a: number } = { a: 4 };
// Bad
delete y.a;
```

- Cannot delete from object constrained to { a: number }

#### TSC string intrinsics

```ts
const a: Uppercase<"something" |"hi"> = "HI";
const b: Uppercase<string> = "hi"
```

- Type \"hi\" is not assignable to type Uppercase\<string\>

#### Ezno intrinsics

```ts
5 satisfies MultipleOf<2>;
4 satisfies MultipleOf<2>;
```

- Expected MultipleOf\<2\>, found 5

#### Order of numerical properties

> TODO test could be better using `for in` or `Object.keys` etc

```ts
let x = {}; x.something = null; x[4] = null; x["eight"] = null; x["2"] = null;
x satisfies string;
```

- Expected string, found { 2: null, 4: null, something: null, eight: null }

#### `NoInfer`

```ts
declare function func<T>(a: T, b: NoInfer<T>): T;

func("hi", "hello") satisfies number;
```

> but not `| "hello"` !!!

- Expected number, found "hi"

#### Function and class name

> TODO should also check that it is readonly

```ts
function x() { }
class X { }

x.name satisfies "x"
X.name satisfies "Y"
```

- Expected "Y", found "X"

#### Optional effect key

```ts
let i: number = 0;
({ a: true})?.[i++, "a"] satisfies true;
i satisfies 1;

null?.[i++, "a"];
i satisfies string;
```

- Expression is always false
- Expression is always true
- Expected string, found 1

#### Effects across functions

```ts
let value: number = 2;
function a() { value = 8; }
function b() { a() }

let func = () => {};

function c() { b() }
function d(newCb: () => void, then: () => void) { func = newCb; then() }

value satisfies 2;
d(a, c);
value satisfies boolean;
```

- Expected boolean, found 8

### Collections

#### Mutation

> This is part of [assignment mismatch](https://github.com/kaleidawave/ezno/issues/18)

```ts
function fakeRead(a: Array<string | number>) {
	a.push(2)
}

const array1: Array<string> = []
fakeRead(array1)
```

- Invalid assignment to parameter

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
function func<T extends boolean>(condition: T) {
	if (condition) {
		return 4
	} else {
		return 3
	}
}

func satisfies string;
```

> There are some issues around printing here, when to include the generic etc

- Expected string, found \<T\>(condition: T) => T ? 4 : 3

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

### Properties

#### `enumerable` in for in

```ts
const obj = { n: 1, b: 2 };
Object.defineProperty(obj, "c", { value: 3, enumerable: false });
Object.defineProperty(obj, "d", { value: 4, enumerable: true });

let keys: string = "";
for (const key in obj) {
	keys += key;
}
keys satisfies boolean
```

- Expected boolean, found "nbd"

#### `Object.freeze`

> TODO seal & preventExtensions

```ts
const obj = {}
let result = Object.freeze(obj);
(obj === result) satisfies true;
obj.property = 2;
Object.isFrozen(obj) satisfies true;
```

> TODO maybe error should say that whole object is frozen

- Cannot write to property 'property'

#### `Object.defineProperty`

> TODO defineProperties

```ts
const obj = {};
Object.defineProperty(obj, 'property', {
  value: 42,
  writable: false,
});
obj.property satisfies string;
obj.property = 70;
```

- Expected string, found 42
- Cannot write to property 'property'

#### `Object.getOwnPropertyDescriptor`

> TODO getOwnPropertyDescriptors

```ts
const obj = { a: true };
Object.defineProperty(obj, 'b', { value: 42, writable: false });

Object.getOwnPropertyDescriptor(obj, 'a') satisfies string;
Object.getOwnPropertyDescriptor(obj, 'b').writable satisfies false;
```

> Order is also important

- Expected string, found { value: true, writable: true, enumerable: true, configurable: true }
