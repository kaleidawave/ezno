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

#### Set property on dependent observed

```ts
function add_property(obj: { prop: number }) {
    obj.prop = 2;
    (obj.prop satisfies 4);
}
```

> Not number

- Expected 4, found 2

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

#### This as generic argument

> Was working, now broken for some reason :(

```ts
function callToUpperCase(s: string) {
	return s.toUpperCase()
}

(callToUpperCase("hi") satisfies "HEY")
```

- Expected "HEY", found "HI"

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

### Looping

#### For loops

```ts
function func(array: Array<string>) {
	for (const item of array) {
		item satisfies number
	}
}
```

- Expected number found string

#### Constant loops

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

> TODO for in

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

#### Statements, declarations and expressions

```ts
function myTag(static_parts: Array<string>, first_name: string) {
	return first_name + static_parts[0]
}

const name = "Ben";
myTag`Hello ${name}` satisfies "Hi Ben"
```

- Expected "Hi Ben", found "Hello Ben"

#### Bad arithmetic operator

> This is allowed under non strict casts option (and will return NaN) but the tests run with strict casts on

> This would need to support [Symbol.toPrimitive] + a bunch of error handling

```ts
console + 2
```

- Expected number, found Console

#### Expected argument

> Requires synthesising arguments later ...?

```ts
function map(a: (a: number) => number) {}

map(a => a.t)
```

- No property t on string

#### Spread arguments

```ts
function spread(main, ...others) {
	return {
		main,
		others,
	}
}

spread(1, 2, 3) satisfies string
```

- TODO

#### Array spread

```ts
const array1 = [1, 2, 3];
const array2 = [...array1, 4, 5, 6];

array2.length satisfies 6;
array2[2] satisfies string;
```

- Expected string, found 3

### Classes

> TODO extends

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
