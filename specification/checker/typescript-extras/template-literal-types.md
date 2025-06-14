#### Template literal type specialisation

> Uses `+` logic behind the scenes

```ts
declare function Concat<T extends string, U extends string>(a: T, b: U): `${T}, ${U}`;

Concat("test", "something") satisfies boolean
```

- Expected boolean, found "test, something"

#### Template literal types

> Last one tests printing

```ts
type Introduction = `Hello ${string}`;

const first: Introduction = "Hello Ben";
const second: Introduction = "Hi Ben";
const third: `Hiya ${string}` = "Hello Ben";

// Edge cases
const invalidNum1: `${1}` = 1;
const invalidNum2: `${1}` = "1";
const invalidNum3: `${1}` = "2";
```

- Type "Hi Ben" is not assignable to type Introduction
- Type "Hello Ben" is not assignable to type `Hiya ${string}`
- Type 1 is not assignable to type "1"
- Type \"2\" is not assignable to type "1"

#### Disjoint template literals

```ts
function func(a: `a${string}`, b: `b${string}`, c: string) {
    const res1 = a === b;
    const res2 = (b === c) satisfies string;
}
```

- This equality is always false as `a${string}` and `b${string}` have no overlap
- Expected string, found boolean

#### String slice matching pattern

```ts
type GetPrefix<S, End> = S extends `${infer T} ${End}` ? T : false;

4 satisfies GetPrefix<"Hello Ben", "Ben">;
```

- Expected "Hello", found 4
