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

### Classes

#### Constructor

```ts
class X {
	constructor(value) {
		this.value = value
	}
}

const x = new X(4)
x.value satisfies string
```

- Expected string found 4

#### Class methods

```ts
class X {
	constructor(value) {
		this.value = value
	}

	getObject(b) {
		return { a: this.value, b }
	}
}

const x = new X(4)
x.getObject(2) satisfies string
```

- Expected string found {"a": 4, "b": 2, }

#### Static class property

```ts
class X {
	static a = 2
}

X.a satisfies 3
```

- Expected 3 found 2

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

#### Spread

```ts
function spread(main, ...others) {
	return {
		main,
		others,
	}
}
spread(1, 2, 3) satisfies string
```

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

### This

#### Calling new on a function

```ts
function MyClass(value) {
	this.value = value
}

new MyClass("hi").value satisfies "hello"
```

- Expected "hello" found "hi"

#### Call function with this

```ts
function ChangeThis(this: { value: any }) {
	return this.value
}

ChangeThis.call({ value: "hi" }) satisfies "hello"
```

- Expected "hello" found "hi"

#### Bind function

```ts
function ChangeThis(this: { value: any }) {
	return this.value
}

const x = ChangeThis.bind({ value: 2 })
x().value satisfies 3
```

- Expected 3 found 2

### Inference

#### Parameter property

> Although this is valid JS, it is currently assumed that this is beneficial behavior

```ts
function x(p) {
    return p.b
}

x satisfies string;
```

- Expected string, found (p: { b: any }, ) => any

#### this

```ts
function ChangeThis(this) {
	return this.value
}

ChangeThis.call({ valued: "hi" })
```

- Expected "hello" found "hi"

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

#### Proxy object

```ts
const a = new Proxy({ prop: 2 })

a.prop satisfies 3
```

- #TODO

### Properties

#### Delete properties

> Yah weird

```ts
const x = { a: 2, b: 3 }
delete x.b;
const b = x.b;
```

- No property "b" on {"a": 2, }

#### Interface merging

> Currently panics on double object restriction :(

```ts
interface X {
	a: string,
	b: boolean
}

interface X {
	c: number
}

const x: X = { a: "field", b: false, c: false }
const y: X = { a: "field", b: false, c: 2 }
```

- Type {"a": "field", "b": false, "c": false, } is not assignable to type X

#### Property updates object outside of function

> Currently panics on double object restriction :(

```ts
const obj: { a: number } = { a: 2 }
function func(value: number) {
	obj.a = value
}

const a: 2 = obj.a
func(4)
const b: 2 = obj.a
```

- Type 4 is not assignable to type 2

#### Push to array

> Currently panics on set property diagnostic not being able to be register in apply event :(

```ts
const myArray: Array<number> = [];
myArray.push("hi")
```

- Type "hi" is not assignable to type number
