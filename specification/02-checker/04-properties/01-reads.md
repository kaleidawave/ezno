This file contains reading properties

### Index into string

```ts
("something"[2]) satisfies number;
```

- Expected number, found "m"

### Optional effect key

```ts
let i: number = 0;
({ a: true})?.[i++, "a"] satisfies true;
i satisfies 1;

null?.[i++, "a"];
i satisfies string;
```

- Expression is always false
- Expression is always true
- Expected string, found 1

### Optional property access

```ts
interface X {
	a: string
	b: string
}

function func(x: X | null) {
	x.a;
	x?.b satisfies number;
}
```

> TODO message should just be null

- No property 'a' on X | null
- Expected number, found undefined | string

### Property on an or type

```ts
function getProp(obj: { prop: 3, prop2: 6 } | { prop: 2 }) {
	obj.prop2;
	return obj.prop
}

getProp satisfies string
```

- No property 'prop2' on { prop: 3, prop2: 6 } | { prop: 2 }
- Expected string, found (obj: { prop: 3, prop2: 6 } | { prop: 2 }) => 3 | 2

### Indexing into (fixed) type

```ts
interface ThePrimitives {
	a: number,
	b: string,
	c: boolean
}

2 satisfies ThePrimitives["b"];
```

- Expected string, found 2

### Indexing into (generic) type

```ts
function getProp<T extends { prop: string, other: string }>(t: T): T["prop"] {
	return t.other
}

function getOther<T extends { prop: string, other: string }>(t: T): T["other"] {
	return t.other
}
```

> This is not TS behavior

- Cannot return T["other"] because the function is expected to return T["prop"]

### Index into dependent array

```ts
function getFirst(array: number[]) {
	return array[0]
}

getFirst satisfies boolean;
```

- Expected boolean, found (array: Array\<number>) => number | undefined

### Index into dependent string

```ts
function getSecondCharacter(s: string) {
	return s[1]
}

getSecondCharacter satisfies boolean;
getSecondCharacter("string") satisfies "b";
```

- Expected boolean, found (s: string) => string | undefined
- Expected "b", found "t"

### Or object missing property

```ts
function get(obj: {a: 2} | { b: 3 }) {
	return obj.a
}
```

> `Cannot read property "a" from { b: 3 }`

- No property 'a' on { a: 2 } | { b: 3 }

### Object literal (constant) computed key

```ts
const y = { ["EZNO".toLowerCase()]: 7 }
y.ezno satisfies 3
y.not_a_key
```

- Expected 3, found 7
- No property 'not_a_key' on { ezno: 7 }

### Properties on or

```ts
declare let key: "a" | "b";

const object = { a: "apple", b: "banana" };
object[key] satisfies boolean;
```

- Expected boolean, found "apple" | "banana"

### Properties on big or

> TODO this creates a fat or type

```ts
function func(idx: number) {
	const array = [1, 2, 3];
	array[idx] satisfies string;
}
```

- Expected string, found 1 | 2 | 3 | undefined
