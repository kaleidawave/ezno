inferfaces, casts, satisfies, non-null assertions, generics, non-existant etc, generics extends, extends, cyclic type alias, template literal type, infer, edge cases, declare functiom, aliases

#### Interfaces

```ts
interface X {
	a: string,
	b: boolean
}

const x: X = { a: 2, b: false }
```

- Type { a: 2, b: false } is not assignable to type X

#### Type aliases

```ts
type MyNumber = number;
type MyObj = { a: string };

"hi" satisfies MyNumber;
4 satisfies MyNumber;

declare let obj: MyObj;
obj.a satisfies string;
```

- Expected MyNumber, found "hi"

#### Declare variable

```ts
declare const global_number: number
const my_number: string = global_number
```

- Type number is not assignable to type string

#### Interface merging

```ts
interface X {
	a: string,
	b: boolean
}

{
	interface X {
		c: number
	}

	const x: X = { a: "field", b: false, c: false }
	const y: X = { a: "field", b: false, c: 2 }
}
```

- Type { a: "field", b: false, c: false } is not assignable to type X

#### Interfaces do not merge with aliases

```ts
type X = { a: string }

{
	interface X {
		b: number
	}

	const x: X = { b: 3 } // Don't require 'a' here <-
	const y: X = { b: "NaN" }
}
```

- Type { b: "NaN" } is not assignable to type X

#### Optional interface member

```ts
interface Optional {
	a?: "hi"
}

const op1: Optional = {}
const op2: Optional = { a: "hello" }
```

- Type { a: "hello" } is not assignable to type Optional

#### Invalid intersection

> Based on disjoint

```ts
type X = 2 & "hi";
type Y = string & number;
```

- No intersection between types 2 and "hi"
- No intersection between types string and number

#### Cyclic type alias check

```ts
type X = Y;
type Y = X;

// test usage doesn't blow up subtyping
const x: X = 2;
```

- Circular type reference
