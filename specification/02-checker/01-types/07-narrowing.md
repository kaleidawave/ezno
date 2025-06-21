We recieve a type constructor and we try to deduce information about it

#### De Morgan laws

#### Number instrincs

> #TODO-link

#### Inferred guards - annotation byepass

> Using events and the constant compilation model #TODO-link

#### Inferred guards - annotation checking

> Not sure why TS does not do this
> #TODO-link to checking

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
	const obj1 = { b: 2 }, obj2 = { c: 6 };
	const sum = param ? obj1 : obj2;
	if (sum === obj1) {
		sum.a = 3;
	}
	[obj1, obj2] satisfies string;
}
```

- Expected string, found [{ b: 2, a: 3 }, { c: 6 }]

#### From condition equality

```ts
function conditional(param: boolean) {
	const obj1 = { a: 1 }, obj2 = { b: 2};
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
	const constant = param;
	if (constant) {
		const b = () => constant;
		b satisfies string;
	}
}
```

- Expected string, found () => true

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
declare function isNumber(param: any): asserts param is number;
declare const value: any;

if (isNumber(value)) {
	value satisfies string;
}
```

- Expected string, found number

#### Assertions annotation return type checked

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

- Cannot return asserts param is string because the function is expected to return asserts param is number

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

#### Narrowing falsy values

```ts
function getName(name?: string) {
	if (name) {
		name satisfies undefined;
		return name
	} else {
		return "default"
	}
}
```

- Expected undefined, found string

#### Implication from equality

```ts
function func(a: boolean) {
  const x = a ? 1 : 2;
  if (x === 1) {
    a satisfies "hi"
  }
}
```

- Expected "hi", found true

#### Narrowing in for loop

> Can't do modulo because of post mutation

```ts
for (let i = 0; i < 3; i++) {
  const x = i === 50;
}
```

With advanced_numbers

- This equality is always false as LessThan<3> and 50 have no overlap

#### Narrowing chains

```ts
export type User = { username: string, password: string };
export type AuthPredicate = (username: string, password: string) => boolean;
export type Auth = User | User[] | AuthPredicate;

function run(auth: Auth)  {
    if (Array.isArray(auth)) {
        auth satisfies number;
    } else if (typeof auth === "function") {
        auth("hi", 5) satisfies string;
    } else {
        auth satisfies boolean;
    }
}
```

- Expected number, found Array\<User>
- Argument of type 5 is not assignable to parameter of type string
- Expected string, found boolean
- Expected boolean, found { username: string, password: string }

#### Narrowing free variable

```ts
function func(value: any) {
  function isNumber() { return typeof value === "number" }

  if (isNumber()) {
    value satisfies string
  }
}
```

- Expected string, found number
