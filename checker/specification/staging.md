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

#### Inequality checks

```ts
function func1(a: GreaterThan<4>) {
	(a > 3) satisfies true;
	(a < 3) satisfies false;
	(a < 10) satisfies string;
}

// thanks narrowing 🙏
function func2(a: number) {
	if (a < 2) {
		(a > 6) satisfies false;
		(a < 7) satisfies true;
		(a > 0) satisfies null;
	}
}
```

- This equality is always false as GreaterThan<4> and 3 have no overlap
- Expected string, found boolean
- This equality is always false as LessThan<2> and 6 have no overlap
- Expected null, found boolean

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
function isLessThan(a: number) {
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

> Can only do it not NaN

```ts
function func(a: string, b: number) {
	(a === a) satisfies string;
    (b === b) satisfies null;
}
```

- Expected string, found true
- Expected null, found boolean

#### Ranges for interal types

```ts
function func(a: number) {
	(Math.sin(a) > -2) satisfies true;
	(Math.sin(a) > -1) satisfies string;
}
```

- Expected string, found boolean

### Statements

#### Interface generic constraint checking

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

- Expected null, found Array\<string>

#### Number `isNan`

```ts
function func(param: number) {
	if (param !== param) {
		param satisfies string;
	}

	// Derives from `!==`
	if (Number.isNaN(param)) {
		param satisfies null;
	}
}
```

- Expected string, found Not\<NaN>
- Expected null, found Not\<NaN>

#### Not disjoint

```ts
function func(param: number) {
	if (param !== 2) {
		return param === 2
	}
}
```

- This equality is always false as Not<2> and 2 have no overlap
