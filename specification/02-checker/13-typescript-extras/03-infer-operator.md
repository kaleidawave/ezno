### Infer and extends distribution

```ts
type ElementOf<T> = T extends Array<infer U> ? U : never;

declare let elementOfNumber: ElementOf<Array<number>>;
declare let elementOfNumberOrString: ElementOf<"not array" | Array<number>>;

elementOfNumber satisfies number;
elementOfNumberOrString satisfies string;

declare let n: never;
n satisfies ElementOf<"not array">;
```

- Expected string, found number

### Infer with extends clause

```ts
type X<T> = T extends { a: infer I extends string } ? I : string;

declare let a: X<{ a: 4 }>;
declare let b: X<{ a: "hello" }>;

a satisfies number;
b satisfies "hello";
```

- Expected number, found string
