Currently implementing:

### Types

#### Interface extends

```ts
interface X {
    a: string
}

interface Y {
    b: string
}

interface Z extends X, Y {
    c: string
}

({ a: "", b: "", c: "hello" }) satisfies Z;
({ a: "", b: 4, c: "hello" }) satisfies Z;
({ c: "hi" }) satisfies Z;
```

- Expected Z, found { a: "", b: 4, c: "hello" }
- Expected Z, found { c: "hi" }

#### Specialisation of return for declare functions

```ts
declare function id<T>(a: T): T;
declare function box<T>(a: T): { item: T };
declare let someNumber: number;

id(someNumber) satisfies string;
box(someNumber) satisfies boolean;
```

- Expected string, found number
- Expected boolean, found { item: number }

### Mapped types

> Aka generic property keys

#### Assignment

```ts
type Record2<K extends string, T> = { [P in K]: T }

const x: Record2<"test", boolean> = { no: false },
      y: Record2<"test", boolean> = { test: 6 },
      z: Record2<"test", boolean> = { test: false };
```

- Type { no: false } is not assignable to type { [\"test\"]: boolean }
- Type { test: 6 } is not assignable to type { [\"test\"]: boolean }

#### Union and types

```ts
type Record2<K extends string, T> = { [P in K]: T }

declare let obj1: Record2<"hi" | "hello", boolean>;

obj1.hi satisfies boolean;
obj1.hello satisfies boolean;

obj1.bye;

declare let obj2: Record2<string, boolean>;
obj2.fine satisfies boolean;
obj2[2];
```

- No property 'bye' on { ["hi" | "hello"]: boolean }
- No property '2' on { [string]: boolean }

#### Specialisation

```ts
type Id<T> = { [P in K]: T[P] }

```

- TODO

#### Negated

```ts
type Omit<K extends string, T> = { [P in K]-?: T }

```

- TODO

### Imports

#### Import package

> Yay doesn't require type definition to be shipped!!

```ts
import { mean_gravity } from "earth";

mean_gravity satisfies 2;

// in node_modules/earth/package.json
{
    "main": "constants.js"
}

// in node_modules/earth/constants.js
export const mean_gravity = 9.806;
```

- Expected 2, found 9.806

#### Import package with definition file

> Does have to synthesis both files. The definition file doesn't include additional information and
> the normal shipped file doesn't include type definitions and restrictions

```ts
import { spin } from "earth";

spin("10");
spin(720) satisfies 1;

// in node_modules/earth/package.json
{
    "main": "index.ts",
    "types": "index.d.ts"
}

// in node_modules/earth/index.ts
export function spin(degrees) {
    degrees satisfies boolean;
    // ..
    return degrees / 360;
}

// in node_modules/earth/index.d.ts
export function spin(degrees: number): number;
```

- Expected boolean, found number
- Argument "10" not assignable to number
- Expected 1, found 2

### Expressions

#### Optional thing

```ts
interface X {
    possibly?: string
}

declare let x: X;

x?.possibly satisfies number;
```

- Expected string, found string | undefined

### Async and promises

```ts
const resp = await (fetch("/some-endpoint") satisfies string);

resp.ok satisfies number;
```

- Expected string, found Promise\<Response\>
- Expected number, found boolean

### Closures

#### TDZ

```ts
function func() {
    return function () { return closedOverVariable }
    let closedOverVariable = 2;
}
```

- Unreachable statement
- Function contains unreachable closed over variable 'closedOverVariable'

### Classes

#### Class type `extends`

```ts
class BaseClass {
    b: boolean = false
}

class Class extends BaseClass {
    a: number = 2
}

new Class().b satisfies 5
```

- Expected 5, found false

#### Hoisting of class type

```ts
function doThingWithClass(instance: Class) {
    instance.prop satisfies string;
    instance.parent_prop satisfies boolean;
    instance.method(4);
}

class BaseClass {
    parent_prop: number
}

class Class extends BaseClass {
    prop: number

    method(s: string) {}
}
```

- Expected string, found number
- Expected boolean, found number
- Argument of type 4 is not assignable to parameter of type string

#### Hoisting of class type with `extends`

```ts
function doThingWithClass(instance: Class) {
    instance.a satisfies number;
    instance.b satisfies string;
}

class BaseClass {
    b: boolean
}

class Class extends BaseClass {
    a: number
}
```

- Expected string, found boolean

### Overloads

#### Calling

```ts
interface X {
    overload(a: number): string;
    overload(a: string): number;
}

declare let x: X;
x.overload(5) satisfies string;
x.overload("hi") satisfies boolean;
```

- Expected boolean, found string

### Control flow

#### Conditional return

```ts
declare let string: string;

function stringIsHi(s: string) {
    if (s === "hi") {
        return true
    }
    return false
}

stringIsHi(string) satisfies number;
```

- Expected number, found boolean

### Forward inference

#### Returning a function

> Yes, returns another function

```ts
type ExpectedFunction = () => ((a: string) => string)

const x: ExpectedFunction = function () {
    return function (b) {
        b satisfies number;
        return b
    }
}
```

- Expected number, found string

#### Computed generics from collection

```ts
const x = [1, 2, 3];
x.map(a => (a satisfies string, 2))
```

- Expected string, found 1 | 2 | 3

### Object constraints

#### Mutation by a function with unknown effects

> This is where the object loses its constant-ness
> Effectively raises it to the parameter type

```ts
function doThingWithCallback(callback: (obj: { prop: number }) => any) {
	const obj = { prop: 8 };
	callback(obj);
	(obj.prop satisfies 8);
	return obj;
}

const object = doThingWithCallback((obj: { prop: number }) => obj.prop = 2);
object.prop satisfies string;
```

- Expected 8, found number
- Expected string, found 2

#### Mutation negated via `readonly`

> This is where the object loses its constant-ness

```ts
function doThingWithCallback(callback: (obj: readonly { prop: number }) => any) {
	const obj = { prop: 8 };
	callback(obj);
	(obj.prop satisfies 6);
}
```

- Expected 6, found 8

#### Possible mutation breaks object constraint

> This unfortunately can flag up valid code, but handling those is too difficult atm

```ts
function doThingWithCallback(callback: (obj: { prop: number | string }) => any) {
	const obj: { prop: number } = { prop: 8 };
	callback(obj);
}
```

- Cannot raise TODO. If possible avoid the constraints or mark parameter as readonly

#### Possible mutation via anytime function

```ts
const x = { a: 2 }
setTimeout(() => { Math.sin(x.a) })
x.a = "hi"
```

- Cannot assign. Restricted to number

### Asynchronous functions and promises

> TODO Promise properties

#### Async function

```ts
async function x() {
	return 2
}

x satisfies string;
```

- Expected string, found Promise { 2 }

#### Await promise

```ts
async function x() {
	return 2
}

(await x) satisfies string;
```

- Expected string, found 2

#### External promise

```ts
declare let a: Promise<string>;

(await a) satisfies number
```

- Expected number, found string

### Extras

This contains new features (which don't have)

#### Comments as type annotations

```ts
function x(a /** string */) {
    a satisfies number
}

const c /** number */ = "5"
```

- Expected number, found string
- Type "5" is not assignable to type number
