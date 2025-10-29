### Object destructuring

```ts
const object = { a: 1, b: 2 }
const { a, b } = object
a satisfies 1; b satisfies string;
```

- Expected string, found 2

### Nested object destructuring

```ts
const object = { a: { b: { c: 2 } } }
const { a: { b: { c: d } } } = object
d satisfies 1;
```

- Expected 1, found 2

### Object destructuring assignment

> Added in #127

```ts
const o = { a: 1, b: { c: 3 } };

let a, b, d;
({
	d = o.a++,
	b: { c: b = 7 },
	a,
} = o);

a satisfies string;
b satisfies boolean;
d satisfies 3;
```

- Expected string, found 2
- Expected boolean, found 3
- Expected 3, found 1
