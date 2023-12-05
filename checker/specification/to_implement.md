### Function calling

#### TDZ (temporary dead zone)

> `b` is a free variable and hoisted, so `getB` can only be used once it is defined

```ts
function getB() {
    return b
}

getB();
const b = 2;
getB();
```

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

#### Calling new on a function

```ts
function MyClass(value) {
	this.value = value
}

new MyClass("hi").value satisfies "hello"
```

- Expected "hello", found "hi"

#### Bind function

```ts
function ChangeThis(this: { value: any }) {
	return this.value
}

const x = ChangeThis.bind({ value: 2 })
x() satisfies 3
```

- Expected 3, found 2

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

- Cannot call ...
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

#TODO promise

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

- #TODO

### Proxy and Object

#TODO effects
#TODO traps
#TODO Object.defineProperty

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

#### Push to array

> Currently panics on set property diagnostic not being able to be register in apply event :(

```ts
const myArray: Array<number> = [];
myArray.push("hi")
```

- Type "hi" is not assignable to type number

#### Simple array map

```ts
function mapper(a: Array<string>) {
	return a.map(item => item + "hi")
}

print_type(mapper)
```

#### Generic array map

```ts
function mapper<T, U>(a: Array<T>, func: T => U) {
	return a.map(func)
}

print_type(mapper)
```

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

> No property t on string

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

- TODO...

#### Array spread

```ts
const array1 = [1, 2, 3];
const array2 = [...array1, 4, 5, 6];

array2.length satisfies 6;
array2[2] satisfies string;
```

- Expected string, found 3

#### Index into array

```ts
function getFirst(a: Array<string>) {
	return a[3]
}

print_type(getFirst)
```

- TODO
