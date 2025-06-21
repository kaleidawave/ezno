- Subtyping
- Hoisting

#### Type checking basic function types

```ts
function func(a: string, b: number): boolean {
	return true
}
func satisfies (a: string, b: number) => boolean;
func satisfies (a: string, b: number) => string;
func satisfies (a: number, b: number) => boolean;
```

- Expected (a: string, b: number) => string, found (a: string, b: number) => boolean
- Expected (a: number, b: number) => boolean, found (a: string, b: number) => boolean

#### Simple

```ts
function id(a: number) {
	return a
}

function simple() {
	return "hello world"
}

id satisfies (n: number) => number;
simple satisfies () => number;
```

- Expected () => number, found () => "hello world"

#### Function parameter subtyping

```ts
// Perfectly fine
const x: (a: number) => string = (p: string | number) => "hi"
// Bad
const y: (a: number | string) => string = (p: number) => "hi"
```

- Type (p: number) => "hi" is not assignable to type (a: number | string) => string

> I think reasons contains more information

#### Function parameter excess allowed

```ts
// Perfectly fine
const x: (a: number, b: string) => string = (p: number) => "hi"
// Bad
const y: (a: string) => string = (p: number, q: string) => "hi"
```

- Type (p: number, q: string) => "hi" is not assignable to type (a: string) => string

> I think reasons contains more information
