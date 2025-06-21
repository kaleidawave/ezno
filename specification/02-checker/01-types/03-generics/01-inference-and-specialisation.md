After scanning generics, we have to pick which ones should be used in the output

#### Canceled generics

> aka remove generic arguments if they don't match the structure

```ts
declare function func<T>(prop: { a: number, b: T, c: string } | { a: number, b: string, c: T }): T;

func({ a: 3, b: "hi", c: false }) satisfies string;
```

- Expected string, found false

#### More accurate generic

```ts
declare function unwrap<T>(a: T | { item: T }): T;

unwrap({ item: 5 }) satisfies string;
unwrap(16) satisfies 16;
```

- Expected string, found 5

#### Double generics

> Really want to only have one covariant and one contravariant but want to keep TSC semantics

```ts
declare function what<T>(a: T, b: T): T;

what(2, 3) satisfies string;
```

- Expected string, found 2 | 3

#### Specialisation of return for declare functions

```ts
declare function id<T>(a: T): T;
declare function box<T>(a: T): { item: T };
declare let someNumber: number;

id(someNumber) satisfies string;
box(someNumber) satisfies boolean;
```

- Expected string, found number
- Expected boolean, found { item: number }
