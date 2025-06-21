Objects can have a prototype. This is recorded in the context and is the prototype is a `TypeId`.

#### Set prototype

```ts
const x = { a: 3 };
Object.setPrototypeOf(x, { a: 5, b: 2 });
x.a satisfies 3;
x.b satisfies string;

const obj = Object.setPrototypeOf(
  {},
  Math.random() ? { a: 2 } : { get a() { return 0 } }
);

const result = 'a' in obj;
result satisfies string;
```

- Expected string, found 2
- Expected string, found true

#### Get prototype

```ts
const x = { a: 3 };
const p = { b: 2 }
Object.setPrototypeOf(x, p);
const p_of_x = Object.getPrototypeOf(x);
// ('a' in p_of_x.a) satisfies false;
(p === p_of_x) satisfies string;
```

- Expected string, found true
