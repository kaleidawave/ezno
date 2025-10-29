This covers

- Access
- Order of properties and numeric
- Destructuring
- Object spread
- Conditional spread
- Set with key
- Delete property
- And + or on properties

### Property exists

```ts
let my_obj = { a: 3 }
const a = my_obj.a
const b = my_obj.b
```

- No property 'b' on { a: 3 }

### Reading property (via accessor)

```ts
const my_obj = { a: 2 }
const three: 3 = my_obj.a
```

- Type 2 is not assignable to type 3

### Property updates registered

```ts
let my_obj = { a: 3 }
my_obj.a = 4
let b: 3 = my_obj.a
```

- Type 4 is not assignable to type 3

### Objects checks

```ts
const my_obj: { b: 3 } = { b: 4 }
```

- Type { b: 4 } is not assignable to type { b: 3 }

### Properties matched against continuous type become conditional

> I think this is TSC behaviour under indexed access

```ts
declare let strings: { [a: string]: number };
declare let record: Record<string, number>;
declare let d: { [a: "a" | "b"]: number };

strings.a satisfies number | undefined;
record.a satisfies boolean;
d.a satisfies number;
```

- Expected boolean, found number | undefined

### Shorthand object literal

```ts
const x = 2
const y = { x }
y.x satisfies 3
```

- Expected 3, found 2

## Structural typing

### And on properties

> Note that it keeps it as a `and`. It does not join the properties into a single typ

```ts
declare type U = { a: 2 } & { b: 3 }
declare let x: U;
x.b satisfies 3;

({ a: 2, b: 3 } satisfies U);
({ b: 3 } satisfies U);
```

- Expected U, found { b: 3 }

## Ordering

### Order of numerical properties

> TODO test could be better using `for in` or `Object.keys` etc

```ts
let x = {}; x.something = null; x[4] = null; x["eight"] = null; x["2"] = null;
x satisfies string;
```

- Expected string, found { 2: null, 4: null, something: null, eight: null }

#### Implementation

#TODO-check I think it performs a sort after if there are any number like keys

### Order of properties after assignment

```ts
const obj = { a: 1, b: 2 };
obj.a = 2; obj.c = 6; obj.b = 4;
obj satisfies boolean;
```

- Expected boolean, found { a: 2, b: 4, c: 6 }

#### Implementation

#TODO-check I think it replaces values in order