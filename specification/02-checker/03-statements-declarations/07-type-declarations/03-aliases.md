### Implementation

They are represented on `Type` with their `name`, any generic parameters and a pointer to their `value`

> #TODO generics

### Type aliases

```ts
type MyNumber = number;
type MyObj = { a: string };

"hi" satisfies MyNumber;
4 satisfies MyNumber;

declare let obj: MyObj;
obj.a satisfies string;
```

- Expected MyNumber, found "hi"

### Cyclic type alias check

> This uses disjoint checking to work. The value of the alias must be disjoint to the value

```ts
type X = Y;
type Y = X;

// test usage doesn't blow up subtyping
const x: X = 2;
```

- Circular type reference
