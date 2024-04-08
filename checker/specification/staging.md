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

#### Template literal type restriction

> TODO dynamic restriction

```ts
type Name = "Ben"
"test" satisfies `Hello ${Name}`;
```

- Expected "Hello Ben", found "test"

#### Template literal type specialisation

> Uses `+` logic behind the scenes

```ts
declare function Concat<T extends string, U extends string>(a: T, b: U): `${T}, ${U}`;

Concat("test", "something") satisfies boolean
```

- Expected boolean, found "test, something"

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

### Async and `Promise`s

> Position of await is not checked (here is fine because top level await)

#### `fetch`

> Uses external `Promise`

```ts
const resp = await (fetch("/some-endpoint") satisfies string);

resp.ok satisfies number;
```

- Expected string, found Promise\<Response\>
- Expected number, found boolean

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

#### Static blocks

```ts
class X {
    static x = 2;

    static {
        const property: 4 = ++this.x;
    }
}

X.x satisfies 3;
```

- Type 3 is not assignable to type 4

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

#### Computed generics from collection

```ts
const x = [1, 2, 3];
x.map(a => (a satisfies string, 2))
```

- Expected string, found 1 | 2 | 3

### Extras

> This contains new features. Most are WIP

#### Comments as type annotations

```ts
function x(a /** string */) {
    a satisfies number
}

const c /** number */ = "hello"
```

- Expected number, found string
- Type "hello" is not assignable to type number

#### Literal special type

```ts
function register(a: Literal<string>) {
    // ...
}

register("something")
// `document.title` is an unknown string, non-literal
register(document.title)
```

- Argument of type string is not assignable to parameter of type Literal\<string\>
