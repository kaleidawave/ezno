### Comments as type annotations

```ts
function x(a /** string */) {
	a satisfies number
}

const c /** number */ = "hello"
```

- Expected number, found string
- Type "hello" is not assignable to type number

#### Use cases

When want the tool to check types with annotations but are constrained to an environment that does not support the TypeScript type annotations syntax
