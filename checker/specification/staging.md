Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Iteration

#### Destructuring with iterating

```ts
const [x, y, z] = {
    [Symbols.iterator]() {
        let i = 0;
        return {
            next() {
                if (i++ === 4) return { done: true, value: i };
                return { done: false, value: i };
            },
            return() {
                return { done: true };
            },
        };
    }
};

({ x, y, z }) satisfies string;
```

- Expected string, found { x: 1, y: 2, z: 3 }

### Collections

#### Map `get` and `set`

```ts
const numberMap = new Map();
numberMap.set(1, 2);
numberMap.get(1) satisfies 2;
numberMap.get(3) satisfies string;
```

- Expected string, found undefined

### Function calling

#### Recursion

> WIP

```ts
type List<T> = { item: T, parent: List<T> } | null;

function listToString(item: List<string>): string {
    if (item) {
        return item.item + listToString(item.parent);
    } else {
        return ""
    }
}

listToString({ item: "B", parent: { item: "e", parent: { item: "n", parent: null } } }) satisfies null;
```

- Expected null, found "Ben"

#### Recursion (cylic case)

> WIP, returns when call stack > 10 & encounters same function id call

```ts
type List<T> = { item: T, parent: List<T> } | null;

function listToString(item: List<string>): string {
    if (item) {
        return item.item + listToString(item.parent);
    } else {
        return ""
    }
}

const start = { item: "B", parent: { item: "e", parent: null } };
start.parent.parent = start;
listToString(start) satisfies null;
```

> TODO should catch lol

- Expected null, found string

### Control flow

#### Collecting

> Needs narrowing inside of event application

```ts
let global: string | null = null;

function one(param: string) {
    if (param === "a") {
        return 0
    }
    global = "Found";

    if (param === "b") {
        return 1
    }
    if (param === "c") {
        return 2
    }

    return 3
}

declare let argument: "a" | "b";
one(argument) satisfies null;

global satisfies number;
```

- Expected null, found 0 | 1
- Expected number, found null | "found";

#### Conditional break

```ts
function getNumber(a: number) {
	for (let i = 0; i < 10; i++) {
		if (i === a) {
			return "found"
		}
	}
	return "not-found"
}

getNumber(4) satisfies "found";
getNumber(100) satisfies boolean;
```

- Expected boolean, found "not-found"

#### *Inconclusive* conditional update

```ts
declare var value: string;
let a: string | number = 0;

function conditional(v: string) {
    if (v === "value") {
        a = "hi"
    }
}
conditional(value);
a satisfies string;
```

- Expected string, found "hi" | 0

#### Break with label

> Note the numbers here, if they are larger they break over the `max_inline` limit and get different results below

```ts
let a: number = 0;
let result;

top: while (a++ < 8) {
	let b: number = 0;
	while (b++ < 8) {
		if (a === 3 && b === 2) {
			result = a * b;
			break top
		}
	}
}

a satisfies string;
result satisfies boolean;
```

- Expected string, found 3
- Expected boolean, found 6

### Narrowing

#### From `contains`

> Uses accumulation in events

```ts
function func(item: string) {
    const condition = ["a", "b", "c"].contains(item);

    if (condition) {
        item satisfies number;
    }
}
```

- Expected number, found "a" | "b" | "c"

### Extras

#### TypeScript Enum

> TODO temp implementation

```ts
enum Color {
    Red,
    Blue
}

const color1: Color = Color.Red;
(color1 === Color.Red) satisfies string;
```

> Must be used with `extra_syntax: true`

- enum must be used with 'extras' option
- Expected string, found true

### Conditionality destructuring from poly

```ts
declare let x: { a?: 1 }; // also { a: 1 } | { b: 2 }
let { a = 2 } = x;
a satisfies 3;
```

- Expected 3, found 1 | 2

#### Creation in function

```ts
function newClass(property, value) {
	return class {
		[property] = value
	}
}

new (newClass("hello", 2)).hello satisfies 2;
new (newClass("hi", 6)).hi satisfies string;
```

- Expected string, found 6

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
