Current unsupported features. Eventually moved to `specification.md`

## TODO

### Types

#### Array equality

```ts
const x: Array<string> = [1, "test"]
```

- TODO

### Control flow

#### For loop

```ts
function func(array: Array<string>) {
	for (const item of array) {
		item satisfies number
	}
}
```

- Expected number found string

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

#### For loop

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

- ?

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

#### this

```ts
function ChangeThis(this) {
	return this.value
}

ChangeThis.call({ valued: "hi" })
```

- Expected "hello" found "hi"

#### Closed over restriction

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
