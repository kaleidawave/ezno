Currently implementing:

### Iteration

#### For-in fixed object

```ts
let properties: string = "";
for (const property in { a: 1, b: 2, c: 3 }) {
	properties += property;
}
properties satisfies boolean;
```

- Expected boolean, found "abc"

#### For-in non fixed object

> TypeScript anonymous object annotations do not guarantee ordering and the subtyping rules allow for the RHS to have more
> properties than defined

```ts
declare const myObject: { a: 1, b: 2, c: 3 };

let properties: string = "";
for (const property in myObject) {
	properties += property;
}
properties satisfies boolean;
```

- Expected boolean, found string

### Control flow

#### Unknown condition assignment

```ts
let i = 0;
declare let b: boolean;
if (b) {
    i = 1
} else {
    i = 2
}

i satisfies string;
```

- Expected string, found 1 | 2

### Expressions

#### `typeof` operator

```ts
function func() {}

(typeof 5) satisfies "number";
(typeof "hi") satisfies "string";
(typeof func) satisfies "function";

declare let someNumber: number;
(typeof someNumber) satisfies "function";
```

- Expected "function", found "number"

#### `var`

```ts
s satisfies string;
var s = "hello"
s satisfies number;
```

- Expected string, found undefined
- Expected number, found "hello"

### Function calling

#### Default parameter side effect on parameter

> I don't think this works because of fact combining

```ts
function doThing(a, b = (a += 2)) {
	return a
}

doThing(3) satisfies 2;
doThing(6, 1) satisfies 6;
```

- Expected 2, found 5

#### Generic condition

```ts
declare function isNumber<T>(t: T): T extends number ? true : false;

isNumber(5) satisfies true;
isNumber("5") satisfies number;
```

- Expected number, found false

### Function checking

#### Object function inference

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

### Collection

> TODO other arguments

#### `map` and `filter`

```ts
[1, 2, 3].map(x => x + 1) satisfies [2, 3, 4];

[1, 2, 3].filter(x => x % 2) satisfies [6];
```

- Expected [6], found [2]

#### `some` and `every`

```ts
declare let aNumber: number;

[1, 2, 3].some(x => x > 0) satisfies true;
[-5].some(x => x > 0) satisfies false;

[1, aNumber, 3].every(x => x > 0) satisfies string;
```

- Expected string, found boolean

#### `find` and `includes`

```ts
[1, 2, 3].find(x => x % 2) satisfies 4
```

- Expected 4, found 2
