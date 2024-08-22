Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Operators

#### Always known math

```ts
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

#### Bad arithmetic operator

> This is allowed under non strict casts option (and will return NaN) but the tests run with strict casts on

> This would need to support [Symbol.toPrimitive] + a bunch of error handling

```ts
console + 2
```

> TODO bad diagnostic

- Cannot Console Add 2

#### Inequality checks

```ts
function func(a: number) {
	a < console;
}
```

- TODO

#### Disjoint equality

```ts
function neverEqual(a: string, b: number) {
	(a === b) satisfies false;
}

function sometime(a: string | number, b: number) {
	(a === b) satisfies string;
}
```

- Expected string, found boolean
