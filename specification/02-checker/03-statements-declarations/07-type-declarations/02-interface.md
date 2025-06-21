#### Interfaces

```ts
interface X {
	a: string,
	b: boolean
}

const x: X = { a: 2, b: false }
```

- Type { a: 2, b: false } is not assignable to type X

#### Interface extends

```ts
interface X {
	a: string
}

interface Y {
	b: string
}

interface Z extends X, Y {
	c: string
}

({ a: "", b: "", c: "hello" }) satisfies Z;
({ a: "", b: 4, c: "hello" }) satisfies Z;
({ c: "hi" }) satisfies Z;
```

- Expected Z, found { a: "", b: 4, c: "hello" }
- Expected Z, found { c: "hi" }

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