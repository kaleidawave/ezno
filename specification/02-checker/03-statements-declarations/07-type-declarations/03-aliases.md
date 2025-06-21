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

#### Cyclic type alias check

```ts
type X = Y;
type Y = X;

// test usage doesn't blow up subtyping
const x: X = 2;
```

- Circular type reference
