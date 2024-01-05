These tests do not pass ATM. But hopefully will in the future 🤞

### Types

#### Resolving value by property on dependent

```ts
function getProperty(property: "a" | "b" | "c") {
	return { a: 1, b: 2, c: 3 }[property]
}

getProperty("d")
getProperty("c") satisfies 2
```

- Expected "a" | "b" | "c" found "d"
- Expected 2 found 3

#### Generic type argument restriction

```ts
function map<T, U>(a: T, b: (t: T) => U) {
	return b(a)
}

map(2, Math.sin)
map("string", Math.sin)
```

- Argument of type "string" is not assignable to parameter of type number

> Because `Math.sin` set T to number

#### Calling on or type

```ts
type Func1 = () => 3;
type Func2 = () => 2;
function callFunc<T, U>(func: (() => T) | (() => U)): 3 | 2 {
	return func()
}

print_type(callFunc)
```

- Expected "a" | "b" | "c" found "d"

#### Symmetric or

```ts
function or<T, U>(obj: T | U): U | T {
	return obj
}

print_type(or)
```

- Expected "a" | "b" | "c" found "d"

#### Symmetric and

```ts
function and<T, U>(obj: T & U): U & T {
	return obj
}

print_type(and)
```

- Expected "a" | "b" | "c" found "d"

#### Distributivity

```ts
function distribute<T, U, V>(obj: (T | U) & V): (T & V) | (U & V) {
	return obj
}

print_type(distribute)
```

- Expected "a" | "b" | "c" found "d"

#### Or missing

```ts
function get(obj: {a: 2} | { b: 3 }) {
	return obj.a
}
```

- Expected "a" | "b" | "c" found "d"

#### Calling or type

```ts
function callFunc<T, U>(func: (() => T) | (() => U)): T | U {
	return func()
}

print_type(callFunc)
```

- Expected string, found (obj: { prop: 3 } | { prop: 2 }) => 3 | 2

#### Generics pass down

> Too many generics here, doesn't get caught for some reason?

```ts
let c: Array<number> = []

function add() {
    c.push("hi")
}
```

- Type "hi" is not assignable to argument of type number

### Narrowing

> TODO `typeof`, `instanceof`, conditional, across a function

#### Equality

```ts
declare let a: string;
if (a === "hi") {
	a satisfies "hello"
}
```

- Expected "hello", found "hi"

### This

#### Bind function

```ts
function ChangeThis(this: { value: any }) {
	return this.value
}

const x = ChangeThis.bind({ value: 2 })
x() satisfies 3
```

- Expected 3, found 2

### Iteration

#### For-in fixed object

```ts
let properties: string = "";
for (const property in { a: 1, b: 2, c: 3 }) {
    properties += property;
}
properties satisfies boolean;
```

- Expected boolean, found "abc"

#### For-in non fixed object

> TypeScript anonymous object annotations do not guarantee ordering and the subtyping rules allow for the RHS to have more
> properties than defined

```ts
declare const myObject: { a: 1, b: 2, c: 3 };

let properties: string = "";
for (const property in myObject) {
    properties += property;
}
properties satisfies boolean;
```

- Expected boolean, found string

> TODO for in and generators

#### For loops

```ts
function func(array: Array<string>) {
	for (const item of array) {
		item satisfies number
	}
}
```

- Expected number found string

#### Constant for loop

```ts
function join(array: Array<string>) {
	let buf = ""
	for (let item of array) {
		buf += item
	}
	return buf
}

join(["a", "b", "c"]) satisfies "cba"
```

- Expected "cba" found "abc"

### Inference

#### Parameter property

> Although this is valid JS, it is currently assumed that this is beneficial behavior

```ts
function x(p) {
    return p.b
}

x satisfies string;
```

- Expected string, found (p: { b: any }) => any

#### this

```ts
function ChangeThis() {
	return this.value
}
```

- TODO

#### Free variable restriction

```ts
let x = "hi"
function call() {
	return Math.cos(x)
}

call()
x = 0
call() satisfies 2
```

- Cannot call TODO
- Expected 2 found 1

#### Property restriction

```ts
function call(prop) {
	return { a: 1, b: 2, c: 3 }[prop]
}
print_type(call)
```

- (prop: "a" | "b" | "c") => number

### Asynchronous functions and promises

> TODO Promise properties

#### Async function

```ts
async function x() {
    return 2
}

x satisfies string;
```

- Expected string, found Promise { 2 }

#### Await promise

```ts
async function x() {
    return 2
}

(await x) satisfies string;
```

- Expected string, found 2

#### External promise

```ts
declare let a: Promise<string>;

(await a) satisfies number
```

- Expected number, found string

### Generators

```ts
function x*() {
    return 2
}

(await x) satisfies string;
```

- TODO

### `Proxy` and `Object`

> TODO effects, different traps and `Object.defineProperty`

#### Proxy object with default callback

```ts
const a = new Proxy({ prop: 2 })

a.prop satisfies 3
```

- Expected 3, found 2

#### Proxy getters

```ts
const a = new Proxy({ }, { get(prop) { return prop } })

a.prop1 satisfies "prop1"
a.prop3 satisfies "prop2"
```

- Expected "prop2", found "prop3"

### Collections

> TODO filter, every and all, find, etc

#### Simple array map

```ts
function mapper(a: Array<string>) {
	return a.map(item => item + "hi")
}

print_type(mapper)
```

- TODO

#### Generic array map

```ts
function mapper<T, U>(a: Array<T>, func: T => U) {
	return a.map(func)
}

print_type(mapper)
```

- TODO

### Expressions

#### Bad arithmetic operator

> This is allowed under non strict casts option (and will return NaN) but the tests run with strict casts on

> This would need to support [Symbol.toPrimitive] + a bunch of error handling

```ts
console + 2
```

- Expected number, found Console

#### Array spread

```ts
const array1 = [1, 2, 3];
const array2 = [...array1, 4, 5, 6];

array2.length satisfies 6;
array2[2] satisfies string;
```

- Expected string, found 3

#### Destructuring assign

> TODO include an object destructure here

```ts
let array1 = [1, 2, 3];
let a = 0, b = 0;
[a, b] = array1;

a satisfies 1;
b satisfies "hello world";
```

- Expected "hello world", found 2

#### Optional interface property

> TODO needs `Logical`-ish `PropertyValue`

```ts
declare const global: { a?: string };

("a" in global) satisfies string;
(global.a) satisfies 2;
```

- Expected string, found boolean
- Expected 2, found string | undefined

#### Delete from required propertied

```ts
declare let global: { a?: string, b: string };

// Fine
delete global.a;
// Bad
delete global.b;
```

- Cannot delete property "b" off { a?: string, b: string }

#### Try-catch variable restriction

```ts
try {
	throw 2;
} catch (e: string) {
	// ...
}
```

- Thrown type 2, not assignable to catch variable of string

#### `typeof` expression

> TODO better test

```ts
(typeof "hello") satisfies "string";
(typeof 5) satisfies "number";
(typeof {}) satisfies "Number";
```

- Expected "Number", found "object"

#### `instanceof` expression

```ts
class X {}
class Y {}

(new X instanceof X) satisfies number;
(new X instanceof Y) satisfies false;
```

- Expected number, found true

### Runtime

```ts
let x: number = 0;

document.addEventListener("click", () => {
	x++;
});

document.addEventListener("scroll", () => {
	x satisfies 0;
	x++;
})
```

- Expected 0, found number

### Classes

#### Extends

```ts
class BaseClass extends class { x: 2 } {
	y: 3
}

const b = new BaseClass;
print_type(b.x);
```

- TODO

#### Privacy

```ts
class MyClass {
	#a = 2;

	getA(this: { #a: any }) {
		return this.#a
	}
}

(new MyClass).#a;
((new MyClass).getA() satisfies 3);
```

- Cannot get private property "#a"
- Expected 3, found 2

### Recursion

#### Application

```ts
function x(a: number) {
	if (a > 10 || a < 0) {
		return a
	}
	return a--
}

print_type(x(4))
print_type(x(90))
```

- TODO

#### No loop

```ts
function call(cb: () => void) {
	return cb()
}

call(call)
```

- TODO hopefully doesn't blow up

### Function checking

#### Default parameter side effect on parameter

> I don't think this works because of fact combining

```ts
function doThing(a, b = (a += 2)) {
    return a
}

doThing(3) satisfies 2;
doThing(6, 1) satisfies 6;
```

- Expected 2, found 5

#### Default parameter type check

```ts
function doThing(b: number = "hello") {
    return a
}
```

- Default value "hello" is not assignable to parameter of type number
