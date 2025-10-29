- Spread
- Default (+ effects)
- Generics?

### Type of parameter

```ts
function func(a: number) {
	a satisfies string
}
```

- Expected string, found number

### Default parameter value type check

> Thanks to #132

```ts
function outer(a: number) {
	function inner(b: string = Math.floor(a)) {
	}
}
```

- Cannot use a default value of type number for parameter of type string

### Variadic function parameter type

> aka rest parameter type

```ts
function variadic(...r: string[]) {
	r satisfies boolean;
}
```

- Expected boolean, found Array\<string>

### Destructuring parameter

```ts
function myFunction({ a }: { a: number }) {
	a satisfies boolean;
	return a
}

myFunction({ a: 6 }) satisfies string;
```

- Expected boolean, found number
- Expected string, found 6

### Argument type against parameter

```ts
function func(a: number) {}
func("hello world")
```

- Argument of type "hello world" is not assignable to parameter of type number

### Parameters retain argument values

```ts
function id(a) {
	return a
}

const d: 3 = id(2)
```

- Type 2 is not assignable to type 3

### Get value of property on parameter

```ts
function getA(obj: { a: string }) {
	return obj.a
}

const d: 3 = getA({ a: "hi" })
```

- Type "hi" is not assignable to type 3

### Missing argument

```ts
function func(p1: number, p2: string) {}

func(4)
```

- Missing argument

### Excess argument

```ts
function func(p1: number) {}

func(4, "extra")
```

- Excess argument

## TO SORT

### Assignment to parameter

```ts
function alterParameter(a: number, b: { prop: string }) {
	a = 2;
	a = "hi";

	b.prop = 3;

	b.prop = "hello";
	// Observed. TODO disabled because of possible impure (getters etc)
	// b.prop satisfies "hello";
}
```

> Assigning straight to `a` might be disallowed by an option in the future. Right now it is allowed by JavaScript and so is allowed

- Type \"hi\" is not assignable to type number
- Type 3 does not meet property constraint string
