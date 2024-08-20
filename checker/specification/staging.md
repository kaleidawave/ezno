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

#### Prototype narrowed

```ts
function func(param: Array<string> | string) {
    if (param instanceof Array) {
		param satisfies null;
    }
}
```

- Expected null, found Array\<string>

#### Type generated from prototype

```ts
function func(param: any) {
    if (param instanceof Array) {
		param satisfies null;
    }
}
```

- Expected null, found Array\<any>

#### Narrowing via property result

```ts
function narrowPropertyEquals(param: { tag: "a", a: string } | { tag: "b", b: number }) {
    if (param.tag === "a") {
        param.a satisfies string;
        param satisfies null;
    }
}
```

- Expected null, found { tag: "a", a: string }

#### Narrowing via `in`

```ts
function narrowFromTag(param: { tag: "a", a: string } | { tag: "b", b: number }) {
    if ("a" in param) {
        param.a satisfies string;
        param satisfies null;
    }
}
```

- Expected null, found { tag: "a", a: string }

#### Build object

> TODO `.prop === 2` doesn't work because of get on `ANY_TYPE`

```ts
function buildObject(param: any) {
    if ("a" in param) {
        param satisfies null;
    }
}
```

- Expected null, found { a: any }

#### Object equality

```ts
function conditional(param: boolean) {
	const obj1 = {}, obj2 = {};
	const sum = param ? obj1 : obj2;
	if (sum === obj1) {
		sum.a = 2;
	}
	[obj1, obj2] satisfies string;
}
```

- Expected string, found [{ a: 2 }, {}]

#### From condition equality

```ts
function conditional(param: boolean) {
	const obj1 = { a: 1 }, obj2 = {};
	const sum = param ? obj1 : obj2;
	if (param) {
		sum satisfies string;
	}
}
```

- Expected string, found { a: 1 }

#### Across free variable

```ts
function conditional(param: boolean) {
	let b;
	if (param) {
		b = () => param;
	} else {
		return;
	}

	b() satisfies string;
}
```

- Expected string, found true

#### Edge case

> De-Morgans laws for and

```ts
function func1(param: string | number) {
    if (typeof param === "number" && param > 0) {
        param satisfies number;
    } else {
        param satisfies null;
	}
}

function func2(param: string | number | boolean) {
    if (typeof param === "string" || !(typeof param === "number")) {
        param satisfies undefined;
    } else {
        param satisfies number;
    }
}
```

- Expected null, found string | number
- Expected undefined, found string | boolean

#### Mutation

> TODO test more

```ts
function func(param: boolean) {
	let a = param;
	const inner = (value: boolean) => a = value;
    if (a) {
		inner(false);
		a satisfies null;
    }
}
```

- Expected null, found false

#### Assertions annotation

```ts
function func1(param: any): asserts param is number {
    if (typeof param !== "string") {
        throw "bad"
    }
}

function func2(param: any): asserts param is boolean {
    if (typeof param !== "boolean") {
        throw "bad"
    }
}
```

> TODO `any` should be parameter name

- Cannot return asserts any is string because the function is expected to return asserts any is number

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

#### Dependent `instanceof`

> TODO combine with prevoous

```ts
function isArray(param: any): boolean {
	return param instanceof Array;
}

declare let myArray: Array<string>;

isArray([1, 2, 3]) satisfies true;
isArray(myArray) satisfies string;
isArray({ }) satisfies null;
```

- Expected string, found true
- Expected null, found false
