#### `never` subtyping

```ts
function getSpecialNumber(): number {
	throw "to implement!"
}

getSpecialNumber satisfies string;
```

- Expected string, found () => number

#### Union with never

```ts
declare function func<T>(): T | string;

func<number>() satisfies string | number;
func<never>() satisfies boolean;
```

- Expected boolean, found string
