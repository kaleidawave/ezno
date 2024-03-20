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

#### Simple array map

```ts
function mapper(a: Array<string>) {
	return a.map((item: string) => item + "hi")
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

> TODO for in and generators

#### For of loops

```ts
function func(array: Array<string>) {
	for (const item of array) {
		item satisfies number
	}
}
```

- Expected number found string

#### Order of properties

> TODO this is because setting properties are simply appended. There are two straightforward fixes, but I am unsure which one is better...

```ts
const obj = { a: 1, b: 2 };
obj.a = 2; obj.c = 6; obj.b = 4;
let properties: string = "";
for (const property in obj) {
	properties += property;
}
properties satisfies boolean;
```

- Expected boolean, found "abc"

### Inference

#### Parameter property

> Although this is valid JS, it is currently assumed that this is beneficial behavior

```ts
function getB(p) {
	return p.b
}

getB satisfies string;
```

- Expected string, found (p: { b: any }) => any

#### Parameter type

```ts
function Sinc(x) {
	return Math.sin(x) / x
}

Sinc satisfies string;
```

- Expected string, found (x: number) => number

#### Parameter type callable

```ts
function CallSomething(p, n: number) {
	return p("hi", 5, c)
}

CallSomething satisfies string;
```

- Expected string, found (p: ("hi", 5, number) => any, n: number) => any

#### Parameter type nested

```ts
function ComplexFunction(p) {
	const { a, b } = p;
	b("test")
}

ComplexFunction satisfies string;
```

- Expected string, found (p: { a: any, b: ("test") => any }) => any

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

#### Async function side effects

```ts
let fv: number = 2;
function doFetch() {
	await fetch("/thing");
	fv += 1;
}

const x = doFetch();
fv satisfies string;
```

- Expected string, found 2

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

#### `some` and `every`

```ts
declare let aNumber: number;

[1, 2, 3].some(x => x > 0) satisfies true;
[-5].some(x => x > 0) satisfies false;

[1, aNumber, 3].every(x => x > 0) satisfies string;
```

- Expected string, found boolean

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

#### Class type

```ts
class X {
	a: number
}

function doThingWithX(x: X) {
	x.a satisfies string;
}
```

- Expected string, found number

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

#### Implements

```ts
interface Draw {
	draw(c: CanvasRenderingContext2D): void;
}

class MyNumber implements Draw { }

class Rectangle implements Draw {
	draw(c) {
		c satisfies string;
	}
}
```

- Class "MyNumber", does not implement draw
- Expected string, found CanvasRenderingContext2D

#### Nominal-ness

```ts
class X { a: number }
class Y { a: number }

function doThingWithX(x: X) {}

doThingWithX(new X())
doThingWithX(new Y())
```

- Cannot Y with X

### Recursion

#### Application

```ts
function x(a: number) {
	if (a > 10 || a < 0) {
		return a
	}
	return x(a--)
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

#### Default parameter type check

```ts
function doThing(b: number = "hello") {
	return a
}
```

- Default value "hello" is not assignable to parameter of type number

### Statements

#### Try catch variable

```ts
try {
	throw 5
} catch (exception: string) {

}
```

- Catch variable cannot be string as 5 thrown in try block

#### Array destructuring

> TODO this currently cheats as the LHS looks at numeric properties, not at the iterator of the RHS

```ts
const array = [1, 2, 3]
const [a, b] = array
a satisfies 1; b satisfies string;
```

- Expected string, found 2

#### Destructuring assignment

```ts
let a = 2, b = 3;
[a, b] = [b, a];
a satisfies 3; b satisfies string;
```

- Expected string, found 2

#### Tuple push and pop

> Should flag up length constraint
> This is caught, but the errors aren't great

```ts
const x: [1, 2, 3] = [1, 2, 3];
x.push(4);

const y: [1, 2, 3] = [1, 2, 3];
y.pop();
```

- TODO cannot push
- TODO cannot pop

### Breaking

> Were working, but a temporary change broke them

#### Try-catch and throw

```ts
try {
	throw 2
} catch (err) {
	err satisfies string
}
```

- Expected string, found 2

#### Throw effects carry through

```ts
function throwType(a) {
	throw a
}

try {
	throwType(3)
} catch (err) {
	err satisfies string
}
```

- Expected string, found 3

#### Generic condition

```ts
declare function isNumber<T>(t: T): T extends number ? true : false;

isNumber(5) satisfies true;
isNumber("5") satisfies number;
```

- Expected number, found false

#### More accurate generic

```ts
declare function unwrap<T>(a: T | { item: T }): T;

unwrap({ item: 5 }) satisfies string;
```

- Expected string, found 5

#### Across alias

```ts
type WithLabel<T> = { label: string, item: T };

declare function getItem<T>(a: WithLabel<T>): T;

getItem({ label: "item 1", item: 5 }) satisfies string;
```

- Expected string, found 5

#### Double generics

> Really want to only have one covariant and one contravariant but want to keep TSC semantics

```ts
declare function what<T>(a: T, b: T): T;

what(2, 3) satisfies string;
```

- Expected string, found 2 | 3
