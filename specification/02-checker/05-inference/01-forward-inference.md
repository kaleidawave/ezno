This is where some existing known type inferes a constraint on something untyped.

### Expected parameter from variable declaration

> Technically works with inference but this method should be less overhead + produce better positioned errors

```ts
const x: (a: string) => number = a => a.to;
```

- No property 'to' on string

### Expected argument from parameter declaration

```ts
function map(a: (a: number) => number) {}

// No annotation on `a`. But error comes from body
// (rather than parameter assignment)
map(a => a.t)
```

- No property 't' on number

### Object function inference

```ts
interface MyObject {
	a(b: string): any;
}

const obj: MyObject = {
	a(b) {
		b satisfies number;
	}
}
```

- Expected number, found string

### Generic argument/constraint leads to inference

```ts
function callFunction<T>(fn: (p: T) => void) {
	// ...
}

callFunction<string>(a => {
	a satisfies number;
})
```

- Expected number, found string

### Computed generics from collection

```ts
const x = [1, 2, 3];
x.map(a => (a satisfies string, 2))
```

- Expected string, found 1 | 2 | 3
