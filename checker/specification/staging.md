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

### Forward inference

> This is where usage a parameter gets a type via a type (on some variable or parameter somewhere). Aka from above or the usage of the function

> Constraint inference is where the parameter gets it from below. Usage of the parameter value

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

#### Generic argument/constraint leads to inference

```ts
function callFunction<T>(fn: (p: T) => void) {
    // ...
}

callFunction<string>(a => {
    a satisfies number;
})
```

- Expected number, found string

#### Return type

```ts
const x: () => ((a: string) => string) = function () {
    return function (b) {
        b satisfies number;
        return b
    }
}
```

- Expected number, found string

### Function checking

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

#### `map` and `filter`

> TODO other arguments (index and `this`)

```ts
[6, 8, 10].map(x => x + 1) satisfies [7, 8, 11];

[1, 2, 3].filter(x => x % 2 === 0) satisfies [2];
```

- Expected [7, 8, 11], found [7, 9, 11]

#### `find` and `includes`

> TODO other arguments (index and `this`). and poly

```ts
[1, 2, 3].find(x => x % 2 === 0) satisfies 4
```

- Expected 4, found 2

### Object constraint

> Any references to a annotated variable **must** be within its LHS type. These test that it carries down to objects.

#### Nested constraint

```ts
const obj1 = { a: 5 };
const obj2: { prop: { a: number } } = { prop: obj1 }

obj1.a = 6;
obj1.a = "hello";
```

- Type "hello" does not meet property constraint number

#### And object constraint

```ts
{
    const obj = { a: true, b: false };
    const x: { a: boolean } = obj, y: { b: boolean } = obj;

    obj.a = "yo";
    obj.b = "wassup";
}

{
    // and in the same assignment through a cycle
    const obj = { a: 2, b: 3 }; obj.c = obj;
    const something: { a: number, c: { b: number } } = obj;

    obj.a = "hi";
    obj.b = "hello";
}
```

- Type "yo" does not meet property constraint boolean
- Type "wassup" does not meet property constraint boolean
- Type "hi" does not meet property constraint number
- Type "hello" does not meet property constraint number

#### Through another variable

```ts
const obj1 = { a: 5 };
const obj2: { prop: { a: number } } = { prop: obj1 }

obj1.a = 6;
obj1.a = "hello";
```

- Type "hello" does not meet property constraint number

> As would violate any usage of `obj2`

#### Cyclic object check

```ts
interface X {
	a: number
	b: X
}

const myObject = { a: 2 };

myObject satisfies X;
myObject.b = myObject;
myObject satisfies X;
```

- Expected X, found { a: 2 }

### Types

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

#### Symmetric or

```ts
function or1<T, U>(obj: T | U): U | T { return obj }

function or2(obj: string | number): number | string { return obj }

// Lack of symmetry
function or3(obj: string | number): number { return obj }
```

- Cannot return string | number because the function is expected to return number

#### Symmetric and

```ts
function and1<T, U>(obj: T & U): U & T { return obj }

// Lack of symmetry
function and2<T, U>(obj: T): U & T { return obj }
```

- Cannot return T because the function is expected to return U & T

#### Distributivity

```ts
function distribute1<T, U, V>(obj: (T | U) & V): (T & V) | (U & V) { return obj }

function distribute2<T, U, V>(obj: V & (T | U)): (T & V) | (U & V) { return obj }

// bad!
function distribute3<T, U, V>(obj: (T | U) & V): (T & U) | (U & V) { return obj }
```

- Cannot return T & V | U & V because the function is expected to return T & U | U & V

#### Or object missing property

```ts
function get(obj: {a: 2} | { b: 3 }) {
	return obj.a
}
```

> `Cannot read property "a" from { b: 3 }`

- No property 'a' on { a: 2 } | { b: 3 }

#### Optional interface member

```ts
interface Optional {
    a?: "hi"
}

const op1: Optional = {}
const op2: Optional = { a: "hello" }
```

- Type { a: "hello" } is not assignable to type Optional

#### Invalid intersection

```ts
type X = 2 & "hi";
type Y = string & number;
```

- No intersection between types 2 and "hi"
- No intersection between types string and number

#### Generic type argument parameter

```ts
function func<T>(a: T) {}
func<number>("hello world")
```

- Argument of type "hello world" is not assignable to parameter of type number

#### Generics pass down

> Too many generics here, doesn't get caught for some reason?

```ts
let c: Array<number> = []

function add() {
	c.push("hi")
}
```

- Argument of type "hi" is not assignable to parameter of type number
