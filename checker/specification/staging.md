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
function x(a: GreaterThan<4>) {
	(a < 3) satisfies false;
	(a < 10) satisfies string;
}
```

- Expected string, found boolean

#### Bad arithmetic operator

> This is allowed under non strict casts option (and will return NaN) but the tests run with strict casts on

> This would need to support [Symbol.toPrimitive] + a bunch of error handling

```ts
console + 2
```

> TODO bad diagnostic

- Cannot Console Add 2
