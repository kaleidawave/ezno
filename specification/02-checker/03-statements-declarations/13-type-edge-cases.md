### Non-existent types

```ts
type X = number;
const a: Y = 2;
```

- Could not find type 'Y'

### Type shadowing

> TODO maybe should test loops, functions, function parameters etc...

```ts
type X = string;
{
	type X = number;
	const a: X = "hello world";
}

function func<YEA>() {}

type B = YEA;
```

- Could not find type 'YEA'
- Type "hello world" is not assignable to type X

### Generics pass down

> Too many generics here, doesn't get caught for some reason?

```ts
let c: Array<number> = []

function add() {
	c.push("hi")
}
```

- Argument of type "hi" is not assignable to parameter of type number

### Declare variable

```ts
declare const global_number: number
const my_number: string = global_number
```

- Type number is not assignable to type string

### Invalid intersection

> Based on disjoint

```ts
type X = 2 & "hi";
type Y = string & number;
```

- No intersection between types 2 and "hi"
- No intersection between types string and number
