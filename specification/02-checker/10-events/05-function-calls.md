A inner function being called is an event

#### Effects carry through dependent calls

```ts
let a: number = 2
function runFunctionTwice(func: () => void) {
	func()
	func()
}

a satisfies 2
runFunctionTwice(() => { a++ })
a satisfies string
```

- Expected string, found 4

#### Updates recognised inside of events

```ts
let a: number = 2
function runFunctionTwice(func: () => void): number {
	func()
	const b = a
	func()
	return b;
}

a satisfies 2
const out = runFunctionTwice(() => { a++ });
a satisfies 4
out satisfies string
```

- Expected string, found 3

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
