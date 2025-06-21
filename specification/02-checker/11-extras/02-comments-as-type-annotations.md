#### Comments as type annotations

```ts
function x(a /** string */) {
	a satisfies number
}

const c /** number */ = "hello"
```

- Expected number, found string
- Type "hello" is not assignable to type number
