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

#### Return type annotation is used in constraint

> While could use the returned type (as done in the second example). Using the annotation prevents other code breaking if the body changes
> As shown later, this doesn't affect what is returned if called

```ts
function getNumber1(): number {
    return 4
}

function getNumber2() {
    return 6
}

getNumber1 satisfies () => 4;
getNumber2 satisfies () => 6;

getNumber1() satisfies 4;
getNumber2() satisfies 6;
```

- Expected () => 4, found () => number

### Collections

#### Array push restriction

```ts
const x: Array<number> = [1]
x.push("hi");
```

- Argument of type \"hi\" is not assignable to parameter of type number

### Types

#### And object constraint

```ts
const obj = { a: 2, b: 3 };
obj.c = obj;
const second: { a: number, c: { b: number } } = obj;

obj.a = "hi";
obj.b = "hello";
```

- Type "hi" does not meet property constraint number
- Type "hello" does not meet property constraint number

#### Double generics

> Really want to only have one covariant and one contravariant but want to keep TSC semantics

```ts
declare function what<T>(a: T, b: T): T;

what(2, 3) satisfies string;
```

- Expected string, found 2 | 3

#### More accurate generic

```ts
declare function unwrap<T>(a: T | { item: T }): T;

unwrap({ item: 5 }) satisfies string;
```

- Expected string, found 5

#### As casts

> Disabled normally, allowed for these tests. Provides TSC compatibility and because narrowing not implemented (including secret feature)

```ts
declare let global: any;

5 as boolean;
global satisfies boolean;
(global as string) satisfies number;
```

- Cannot cast 5 to boolean
- Expected boolean, found any
- Expected number, found string
