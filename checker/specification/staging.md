Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Broken

> Was working, now not

#### `find` and `includes`

> TODO other arguments (index and `this`). and poly

```ts
[1, 2, 3].find(x => x % 2 === 0) satisfies 4;

[1, 2, 3].includes(6) satisfies string;
```

- Expected 4, found 2
<!-- - Expected string, found false -->

#### Conditional return type inference

```ts
function func(a: boolean) {
	if (a) {
		return 2
	}
}

func satisfies (a: boolean) => 5;
```

- Expected (a: boolean) => 5, found (a: boolean) => 2 | undefined

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

```ts
let a: number = 0;
let result;

top: while (a++ < 10) {
	let b: number = 0;
	while (b++ < 10) {
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

### Mapped types

#### Specialisation

```ts
type Pick<T, K extends keyof T> = {
    [P in K]: T[P];
};

interface X { a: number, b: string, c: string }

const x: Pick<X, "a"> = { a: 5 };

({ b: "string" }) satisfies Pick<X, "a">;
```

- TODO

#### Optional

```ts
type Partial<T> = {
    [P in keyof T]?: T[P];
};

const x: Partial<{ a: number, b: string }> = { a: 3 },
      y: Partial<{ a: number, b: string }> = { a: "hi" }
```

- Cannot assign { a: "hi" }

#### Negated

```ts
type Required<T> = {
    [P in keyof T]-?: T[P];
};

const x: Required<{ a?: number }> = { a: 3 },
      y: Required<{ a?: number }> = { };
```

- Cannot assign { } to required

### Readonly and `as const`

> TODO constrained inference

#### Readonly parameter

```ts
function x(p: readonly { a: string }) {
    p.a = 5;
}
```

- Cannot assign to immutable property

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

### Exceptions and `try-catch-finally`

#### Conditional throw

> This emits a warning if a throw was created in a conditional branch

```ts
// no complex numbers :(
function checkedLn(x: number) {
    if (x > 0) {
        return Math.log(x)
    } else {
        throw new Exception("Cannot log long string")
    }
}

// Fine
try { checkedLn(Math.E ** 3) satisfies 3 } catch {}
// Will throw
try { checkedLn(-5) } catch {}
```

- Conditional 'Exception' was thrown in function
