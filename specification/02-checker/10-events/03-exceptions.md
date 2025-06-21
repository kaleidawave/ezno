`throw` is a special event type. This tests things relating to it

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

#### Function effect

```ts
function exceptionToResult(cb: () => number) {
	try {
		return cb()
	} catch (e) {
		return e
	}
}

exceptionToResult(() => 6) satisfies 6;
exceptionToResult(() => { throw 12 }) satisfies 8;
console.log("Error caught!")
```

- Expected 8, found 12

#### Checked thrown type from callback

```ts
function exceptionToResult(cb: () => number) {
	try {
		cb()
	} catch (e: number) {
		return e
	}
}

exceptionToResult(() => { throw "not a number" });
console.log("Error caught!")
```

- Cannot throw "not a number" in block that expects number

#### Internal function effect

```ts
function exceptionToResult(s: string) {
	try {
		return JSON.parse(s)
	} catch (e: number) {
		return e
	}
}
console.log("Error caught!")
```

- Cannot catch type number because the try block throws SyntaxError

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

### Extras

#### Unconditional throw warning

```ts
function safeDivide(num: number, denom: number) {
	if (denom === 0) {
		throw new Error("Cannot divide by zero");
	}
	return num / denom
}

function func() {
	safeDivide(8, 4) satisfies 2;
	// ahh
	safeDivide(10, 0);
}
```

- Conditional '[Error] { message: \"Cannot divide by zero\" }' was thrown in function
