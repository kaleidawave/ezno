### `in` operator

```ts
const obj = { a: 2 };
("a" in obj) satisfies string;
("b" in obj) satisfies true;
```

- Expected string, found true
- Expected true, found false
