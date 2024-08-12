Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Narrowing

#### Equality

```ts
function eqNarrow(a: string) {
	if (a === "hi") {
		a satisfies "hello"
	}
}
```

- Expected "hello", found "hi"

#### Condition outside of `if`

```ts
function eqNarrow(a: string) {
	const a_equals_hi = a === "hi";
	if (a_equals_hi) {
		a satisfies "hello"
	}
}
```

- Expected "hello", found "hi"

#### Condition as a function

```ts
function eqNarrow(a: string) {
	function equalsHi(p: string): boolean { return p === "hi" }

	if (equalsHi(a)) {
		a satisfies "hello"
	}
}
```

- Expected "hello", found "hi"

#### Reference passed around

```ts
function eqNarrow(a: string) {
	const b = a;
	if (b === "hi") {
		a satisfies "hello"
	}
}
```

- Expected "hello", found "hi"

#### `typeof` operator

```ts
function typeOfNarrow(param: any) {
	if (typeof param === "string") {
		param satisfies number;
	}
}
```

- Expected number, found string

#### Boolean narrowing

```ts
function booleanNarrow(param: boolean) {
	if (param) {
		param satisfies string
	}
	if (!param) {
		param satisfies number
	}
}
```

- Expected string, found true
- Expected number, found false

#### Narrowing from operators

```ts
function operatorNarrows(thing: string | null) {
	(thing ?? "something") satisfies string;
	(thing || "something") satisfies number;
	
	const result = thing === "hi" && (thing satisfies boolean);
}
```

- Expected number, found string | "something"
- Expected boolean, found "hi"

#### Logic

```ts
function logicNarrow(thing: any, other: any) {
	if (typeof thing === "string" && other === 4) {
		({ thing, other }) satisfies string;
	}
	
	if (typeof thing === "string" || typeof thing === "number") {
		thing satisfies null;
	}
}
```

- Expected string, found { thing: string, other: 4 }
- Expected null, found string | number

#### Eating cases

> TODO This tests two things. Context negation through final event and negating effect of typeof

```ts
function func(param: boolean | string | number) {
    if (typeof param === "boolean") {
        return 5
    }
    param satisfies null;
}
```

- Expected null, found string | number

### Control flow

#### Early return

```ts
function func(param: boolean) {
	let a = 2;
    if (param) {
		a = 3;
        return a;
    } else {
		a = 7;
	}
    a satisfies string;
}
```

> Note not `3 | 7`

- Expected string, found 7
