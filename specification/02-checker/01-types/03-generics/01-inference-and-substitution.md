After scanning generics, we have to pick which ones should be used in the output.

### Implementation

When matching against a generic structure we may not have type arguments for the parameters in the function. So we have to **infer** these arguments. The inferred values are known as **candidates**.

Substitution relates to taking these inferred values and substituting these into the return type.

> #TODO data-log reference? And specialisation vs substitution

We perform *candidate collection* during subtyping. When checking argument `U` against a parameter of type `T`, after we know that its constraint has passed ([[02-checking]]) we register a mapping as `T=U` as a pair `(T, U)` in a #TODO name? map.

There are some technicalities when picking inferred values when there are

- More than one inferred argument for a type parameter
- No inferred arguments for a type parameter

The implementation uses a vector of possible values and **tracks the depth of how many objects deep the match was**. 

The following tests go into more depth as to how this is tracked.

### Differences between Ezno and TypeScript

- In Ezno inference is done during subtyping #TODO-check 
- In Ezno literals are inferred as type arguments. This is fine as we have a more accurate type.
- In Ezno everything is inferred, so explicit wider arguments are ignored any only the narrowest forms are ignored.
- In Ezno inference and specialisation is done on the first and only program pass. While there may be multiple passes at the call site. #TODO-check 

### Specialisation of return for declare functions

```ts
declare function id<T>(a: T): T;
declare function box<T>(a: T): { item: T };
declare let someNumber: number;

id(someNumber) satisfies string;
box(someNumber) satisfies boolean;
```

- Expected string, found number
- Expected boolean, found { item: number }

### Cancelled generics

> Remove generic arguments if they don't match the structure

```ts
declare function func<T>(
	prop: { a: number, b: T, c: string } | { a: number, b: string, c: T }
): T;

func({ a: 3, b: "hi", c: false }) satisfies string;
```

> We see that `T` is `false` and not `"hi"`

- Expected string, found false

#### Implementation

During checking a *fallible* branch, we first record the length of the list. If the check passes on the branch we continue. **If the subtyping check failed, we truncate the branch to the original length, thus dropping any invalid candidates**. 

### More accurate generic

```ts
declare function unwrap<T>(a: T | { item: T }): T;

unwrap({ item: 5 }) satisfies string;
unwrap(16) satisfies 16;
```

- Expected string, found 5

#### Implementation

When subtyping properties we increment a depth field. Then we do an inference we additionally record the depth at each inferred value. Here we have `(T, { item: 5}, 0)` and `(T, 5, 1)`. At the end we used the *deepest* value, so we choose `5` here as it is the deeper of the two in the first example.

### Double generics

> Really want to only have one covariant and one contravariant but want to keep TSC semantics

```ts
declare function what<T>(a: T, b: T): T;

what(2, 3) satisfies string;
```

- Expected string, found 2 | 3

#### Implementation

When we have two candidates with the same depth (see above), we form a union type for the substituted value.

### Across alias

> This test ensures nothing goes odd with the nested generic structure during specialisation

```ts
type WithLabel<T> = { label: string, item: T };

declare function getItem<T>(a: WithLabel<T>): T;

getItem({ label: "item 1", item: 5 }) satisfies string;
```

- Expected string, found 5
