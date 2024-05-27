> [!IMPORTANT]
> These tests do not pass on the current build. But hopefully will in the future 🤞

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

### Imports

#### Import package with definition file

> Does have to synthesis both files. The definition file doesn't include additional information and
> the normal shipped file doesn't include type definitions and restrictions

```ts
import { spin } from "earth";

spin("10");
spin(720) satisfies 1;

// in node_modules/earth/package.json
{
    "main": "index.ts",
    "types": "index.d.ts"
}

// in node_modules/earth/index.ts
export function spin(degrees) {
    degrees satisfies boolean;
    // ..
    return degrees / 360;
}

// in node_modules/earth/index.d.ts
export function spin(degrees: number): number;
```

- Expected boolean, found number
- Argument "10" not assignable to number
- Expected 1, found 2

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

### Forward inference

#### Returning a function

> Yes, returns another function

```ts
type ExpectedFunction = () => ((a: string) => string)

const x: ExpectedFunction = function () {
    return function (b) {
        b satisfies number;
        return b
    }
}
```

- Expected number, found string

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

> TODO `Promise.then` etc

#### Async function

> Tests return type, usage and awaiting on runtime `Promise`

```ts
async function get2(): 2 {
    return 2
}

function nonAsync() {
    await get2()
}

const four: 4 = await get2();
```

- Expected to return 2, found Promise<2>
- Cannot use await in non-async context
- Type 2, is not assignable to 4

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

### `Proxy` and `Object`

> TODO effects, different traps and `Object.defineProperty`

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

#### Array destructuring assign

```ts
const a = [4, 5];
[a[1], a[0]] = [6, 7];
a satisfies string;
```

- Expected string, found [7, 6]

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

#### Optional property access

```ts
interface X {
    possibly?: string
}

declare let x: X;

x?.possibly satisfies number;
```

- Expected string, found string | undefined

### Runtime

#### Free variable + anytime calls

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

### Functions and classes

#### New can return an object

> TODO test for class constructors as well

```ts
function MyClass(value) {
	this.value = value
    return { v: this }
}

const object = new MyClass("hi").v.value satisfies number;
```

- Expected number, found "hi"

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

#### Array destructuring

> TODO this currently cheats as the LHS looks at numeric properties, not at the iterator of the RHS

```ts
const array = [1, 2, 3]
const [a, b] = array
a satisfies 1; b satisfies string;
```

- Expected string, found 2

#### Array destructuring assignment

```ts
let a = 2, b = 3;
[a, b] = [b, a];
a satisfies 3; b satisfies string;
```

- Expected string, found 2

### Functions

#### No generics

```ts
function id(a) { return a }

id<5>(4)
```

- Cannot pass generic arguments to function without generic arguments

> Or at least explicit generic arguments

#### Method overloading

```ts
interface X {
    overload(a: number): string;
    overload(a: string): number;
}

declare let x: X;
x.overload(5) satisfies string;
x.overload("hi") satisfies boolean;

declare function f(param: string): string;
declare function f(param: number): number;

f("hi") satisfies string;
f(3) satisfies boolean;
f(false)
```

- Expected boolean, found string
