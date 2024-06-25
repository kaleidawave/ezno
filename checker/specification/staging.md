Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Narrowing

#### Equality

```ts
declare let a: string;
if (a === "hi") {
	a satisfies "hello"
}
```

- Expected "hello", found "hi"

#### Condition as a function

```ts
declare let a: string;

const equalsHi = (p: string) => p === "hi";

if (equalsHi(a)) {
	a satisfies "hello"
}
```

- Expected "hello", found "hi"

#### Passed around

```ts
declare let a: string;

const b = a;
if (b === "hi") {
	a satisfies "hello"
}
```

- Expected "hello", found "hi"
