#### `void` return type

> This works similarly to undefined except that it accepts any function return type

```ts
function runWithCallback(cb: () => void): void {
	cb() satisfies string;

	return 5;
}

runWithCallback(() => 3)
```

> Here argument is fine. In the body the return type is `any` (inferred constraint, but doesn't matter)

- Expected string, found void
- Cannot return 5 because the function is expected to return void

#### `throw` in body

```ts
function throwSomething() {
	throw "to implement!"
}

throwSomething satisfies string;
```

- Expected string, found () => never
