#### `keyof` type annotation

```ts
interface X {
	a: string,
	b: string
}

"a" satisfies keyof X; "b" satisfies keyof X; "c" satisfies keyof X;
```

- Expected keyof X, found "c"

#### Subtyping edge cases

```ts
"hi" satisfies { length: 3 };
"hi" satisfies { length: 2 };
(() => {}) satisfies Function;
```

- Expected { length: 3 }, found "hi"
