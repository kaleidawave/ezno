#TODO-link to hoisting, events, other inference

This tests function types that simply are
- A list of parameter types
- A return type

### Implementation

A function type is subtype of a type `T` if

- `T` is the `Function`  object (or `any`)
- It is another function type such that
	- The parameters **left-hand-side** parameters are subtype-able to the **right-hand-side**. Yes, that is correct the subtyping of parameters switches the subtyping operands. **The right-hand-side function type is allowed to have more parameters than the left-hand-side**. Not an issue because JavaScript does not throw calling functions with more arguments than parameters #TODO-link .
	- The right-hand-side return type is a subtype of the left-hand-side (standard, non-reversed)

The following tests show some checking

### Type checking basic function types

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

### Function parameter subtyping

```ts
// Perfectly fine
const x: (a: number) => string = (p: string | number) => "hi"
// Bad
const y: (a: number | string) => string = (p: number) => "hi"
```

- Type (p: number) => "hi" is not assignable to type (a: number | string) => string

> I think reasons contains more information

### Function parameter excess allowed

```ts
// Perfectly fine
const x: (a: number, b: string) => string = (p: number) => "hi"
// Bad
const y: (a: string) => string = (p: number, q: string) => "hi"
```

- Type (p: number, q: string) => "hi" is not assignable to type (a: string) => string

> I think reasons contains more information

> #TODO subtyping with default, optional and spread results