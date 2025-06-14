#### Calling higher order function

```ts
function addTwoToResult(func: (n: number) => number) {
	return func(4) + 2
}

addTwoToResult((a: number) => a * 4) satisfies 5
```

- Expected 5, found 18

#### Calling higher order function that is constant

```ts
function call(func: (n: number) => number) {
	return func(9)
}

call(Math.sqrt) satisfies 2
```

- Expected 2, found 3

#### Constant call and operation with a parameter

> An example of the generic constructor type (namely call and operation)

```ts
function floorPlusB(a: number, b: number) {
	return Math.floor(a) + b
}

floorPlusB(100.22, 5) satisfies 8
```

- Expected 8, found 105

#### Arguments in to rest parameter

```ts
function myRestFunction(...r: string[]) {
	return r
}

myRestFunction("hello ", "world") satisfies number;
```

- Expected number, found ["hello ", "world"]

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

#### Calling non-callable

```ts
const x = "hi"
x()
```

- Cannot call type "hi"
