Currently implementing:

#### Spread arguments

```ts
function spread(main, ...others) {
	return {
		main,
		others,
	}
}

spread(1, 2, 3) satisfies string
```

- TODO...

#### Array spread

```ts
const array1 = [1, 2, 3];
const array2 = [...array1, 4, 5, 6];

array2.length satisfies 6;
array2[2] satisfies string;
```

- Expected string, found 3

#### Index into array

```ts
function getFirst(a: Array<string>) {
	return a[3]
}

print_type(getFirst)
```

- TODO

#### Expected parameter from variable declaration

> Technically works with inference but this method should be less overhead + produce better positioned errors

```ts
const x: (a: string) => number = a => a.to;
```

- No property 'to' on string

#### Indexing into (fixed) type

```ts
interface ThePrimitives {
	a: number,
	b: string,
	c: boolean
}

(2 satisfies ThePrimitives["b"]);
```

- Expected string, found 2

#### Indexing into (generic) type

```ts
function getProp<T extends { prop: string, other: string }>(t: T): T["prop"] {
	return t.other
}

function getOther<T extends { prop: string, other: string }>(t: T): T["other"] {
	return t.other
}
```

- Expected T["prop"], found T["other"]
