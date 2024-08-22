Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Operators

#### Always known math

```ts
// True regardless of 
function func(a: number) { return a ** 0 }

func satisfies string;

declare let x: NotNotANumber;
(x ** 1 === x) satisfies true;
```

- Expected string, found (a: number) => 1

#### Less than checks

```ts
function func1(a: GreaterThan<4>) {
	(a > 3) satisfies true;
	(a < 3) satisfies false;
	(a < 10) satisfies string;
}

// thanks narrowing 🙏
function func2(a: number) {
	if (a > 2) {
		(a > 1) satisfies true;
		(a < 1) satisfies false;
		(a > 6) satisfies number;
	}
}
```

- Expected string, found boolean
- Expected number, found boolean

#### Arithmetic operand check

> This is allowed under non strict casts option (and will return NaN) but the tests run with strict casts on

> This would need to support [Symbol.toPrimitive] + a bunch of error handling

```ts
console + 2
```

> TODO temp diagnostic

- Cannot Console Add 2

#### Inequality operand check

```ts
function func(a: number) {
	a < console;
}
```

> TODO temp diagnostic

- Cannot number LessThan Console

#### Unary operand check

```ts
function func(a: number, b: boolean) {
	const x = !a;
	const y = ~b;
	(!b), (~a);
}
```

> TODO temp diagnostic

- Cannot LogicalNot number
- Cannot BitwiseNot boolean

#### Disjoint equality

```ts
function neverEqual(a: string, b: number) {
	(a === b) satisfies false;
}

function sometimes(a: string | number, b: number) {
	(a === b) satisfies string;
}
```

- This equality is always false as string and number have no overlap
- Expected string, found boolean

#### Identity equality

```ts
function func(a: string, b: number) {
	(a === a) satisfies string;
    (b === b) satisfies null;
}
```

- Expected string, found true
- Expected null, found boolean

### Statements

#### Interface and generic constraint checking

```ts
interface BoxString<T extends string> {
	inner: T
}

type BoxedFour = BoxString<"4">;
type BoxedFive = BoxString<5>;
```

- Generic argument 5 does not match string

### Narrowing

#### External predicate

```ts
function func(param: number | Array<string>) {
	if (Array.isArray(param)) {
		param satisfies null;
	}
}
```

- Expected number, found Array\<string>
