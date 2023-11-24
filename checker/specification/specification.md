## Specification

See [./README.md](README.md) for details

### Variables

#### Variable declarations

```ts
const x: number = 2
const y: string = 2
const z: object = 4
```

- Type 2 is not assignable to type string
- Type 4 is not assignable to type object

#### Variable assignment constraints

```ts
let x: number = 3
x = "not a number"
```

- Type "not a number" is not assignable to type number

#### Variable references

```ts
const a = 3
const b: string = a
```

- Type 3 is not assignable to type string

#### Variable updates registered

```ts
let a = 2
a = "not a number"
let b: number = a
```

- Type "not a number" is not assignable to type number

#### Variable references does not exist

```ts
const a = c
```

- Could not find variable c in scope

#### Variable declared twice

```ts
const a = 2
{
	const a = 3;
	a satisfies 3;
}
a satisfies 2;
const a = 3;
```

- Cannot redeclare variable a

#### Un-intialised variables are undefined

> Might be a usage warning at some point

```ts
let b;
b satisfies string;
```

- Expected string, found undefined

### Generic types

#### Generic interface

```ts
interface Wrapper<T> {
	internal: T
}

const my_wrapped: Wrapper<number> = { internal: "hi" }
```

- Type { internal: "hi" } is not assignable to type Wrapper\<number>

#### Array property checking

```ts
const numbers1: Array<number> = [1, 2, "3"]
const numbers2: Array<string> = [ "hi", "3"]
```

> Printing is a bit wack here

- Type [Array] { 0: 1, 1: 2, 2: "3", length: 3 } is not assignable to type Array\<number>

### Function checking

#### Type of parameter

```ts
function func(a: number) {
	a satisfies string
}
```

- Expected string, found number

#### (simple) return type checking

```ts
function func(): string {
	return 2
}
```

- Cannot return 2 because the function is expected to return string

#### *Inferred* return type

```ts
function func() {
	return 2
}
func satisfies () => string
```

- Expected () => string, found () => 2

#### Generic type argument restriction

```ts
function map<T, U>(a: T, b: (t: T) => U) {
	return b(a)
}

map(2, Math.sin)
map("string", Math.sin)
```

- Argument of type "string" is not assignable to parameter of type number

> Because `Math.sin` set T to number

#### Parameters are always considered generic

```ts
function id(a) {
	return a
}

const d: 3 = id(2)
```

- Type 2 is not assignable to type 3

#### Type checking basic function types

```ts
function func(a: string, b: number): boolean {
	return true
}
func satisfies (a: string, b: number) => boolean;
func satisfies (a: string, b: number) => string;
func satisfies (a: number, b: number) => boolean;
```

- Expected (a: string, b: number) => string, found (a: string, b: number) => true
- Expected (a: number, b: number) => boolean, found (a: string, b: number) => true

#### Function that throws returns never

```ts
function myThrow() {
	throw "err!"
}

myThrow satisfies string;
```

- Expected string, found () => never

#### Return generics mismatch

```ts
function getSecond1<T, U>(p1: T, p2: U): U {
    return p1
}

function getSecond2<T, U>(p1: T, p2: U): U {
    return p2
}
```

- Cannot return T because the function is expected to return U

#### Use of generics in function body

```ts
function setFirst1<T, U>(a: T, b: U) {
	const a2: T = a;
}

function setFirst2<T, U>(a: T, b: U) {
	const a2: U = a;
}
```

- Type T is not assignable to type U

#### Generics as property

```ts
function createObject1<T, U>(a: T, b: U): { a: T, b: U } {
	return { a, b }
}

function createObject2<T, U>(a: T, b: U): { a: U, b: U } {
	return { a, b }
}
```

- Cannot return { a: T, b: U } because the function is expected to return { a: U, b: U }

### Function calling

#### Argument type against parameter

```ts
function func(a: number) {}
func("not a number")
```

- Argument of type "not a number" is not assignable to parameter of type number

#### Generic type argument parameter

```ts
function func<T>(a: T) {}
func<number>("not a number")
```

- Argument of type "not a number" is not assignable to parameter of type number

#### Get value of property on parameter

```ts
function getA(obj: { a: string }) {
	return obj.a
}

const d: 3 = getA({ a: "hi" })
```

- Type "hi" is not assignable to type 3

#### Missing argument

```ts
function func(p1: number, p2: string) {}

func(4)
```

- Missing argument

#### Excess argument

```ts
function func(p1: number) {}

func(4, "extra")
```

- Excess argument

#### Calling non-callable

```ts
const x = "hi"
x()
```

- Cannot call type "hi"

#### Calling higher order function

```ts
function addTwoToResult(func: (n: number) => number) {
	return func(4) + 2
}

addTwoToResult((a: number) => a * 4) satisfies 5
```

- Expected 5, found 18

#### Calling higher order function that is constant

```ts
function call(func: (n: number) => number) {
	return func(9)
}

call(Math.sqrt) satisfies 2
```

- Expected 2, found 3

#### This in object literal

```ts
const obj = {
	a: 4,
	getA(this: { a: any }) {
		return this.a
	}
}

obj.a = 5;
obj.getA() satisfies 6;
```

- Expected 6, found 5

### Closures

#### Reading variable

```ts
function kestrel(a) {
    return function (_b) {
        return a
    }
}

kestrel(3)(2) satisfies 4
```

- Expected 4, found 3

#### Nesting

```ts
function kestrel2(a) {
    return _b => _c => a
}

kestrel2(3)(2)(6) satisfies 4
```

- Expected 4, found 3

#### Carry across objects

```ts
function magicNumber(a: number) {
    return {
		plusOne() { return a + 1 },
		doubled() { return 2 * a }
	}
}

const myNumber = magicNumber(4);
myNumber.plusOne() satisfies 5
myNumber.doubled() satisfies 6
```

- Expected 6, found 8

#### Stateful

```ts
function myClosure(a) {
    return {
		getValue() { return a },
		setValue(b) { a = b }
	}
}

const value = myClosure(4);
value.getValue() satisfies 4;
value.setValue(10);
value.getValue() satisfies 6
```

- Expected 6, found 10

### Effects

> Side effects of functions. Registered internally as `Event`s

#### Assignment to free variable

```ts
let a: number = 0
function func() {
	a = 4
}

func()
let b: 2 = a
```

- Type 4 is not assignable to type 2

#### Assignment from parameter

```ts
let a: number = 0
function func(c: number) {
	a = c
}

func(4)
let b: 2 = a
```

- Type 4 is not assignable to type 2

#### Property updates object inside function

```ts
const obj: { a: number } = { a: 2 }
function func(value: number) {
	obj.a = value
}

obj.a satisfies 2
func(4)
obj.a satisfies 3
```

- Expected 3, found 4

#### Constant call and operation with a parameter

> An example of the generic constructor type  (namely call and operation)

```ts
function sinPlusB(a: number, b: number) {
	return Math.floor(a) + b
}

sinPlusB(100.22, 5) satisfies 8
```

- Expected 8, found 105

#### Effects carry through dependent calls

```ts
let a: number = 2
function runFunctionTwice(func: () => void) {
	func()
	func()
}

a satisfies 2
runFunctionTwice(() => { a++ })
a satisfies string
```

- Expected string, found 4

#### Updates recognised inside of events

```ts
let a: number = 2
function runFunctionTwice(func: () => void): number {
	func()
	const b = a
	func()
	return b;
}

a satisfies 2
const out = runFunctionTwice(() => { a++ });
a satisfies 4
out satisfies string
```

- Expected string, found 3

### Constant evaluation

#### Arithmetic

```ts
const x: 4 = 2 + 3
const y: 6 = 2 * 3
const z: 8 = (2 * 3) - 2
```

- Type 5 is not assignable to type 4
- Type 4 is not assignable to type 8

#### Bitwise arithmetic

```ts
const x: 2 = 2 & 3
const y: 6 = 2 ^ 7
const z: 14 = 8 | 4
```

- Type 5 is not assignable to type 6
- Type 12 is not assignable to type 14

#### Logical operators

```ts
const x: 2 = 3 && 2
const y: 6 = 3 && false
const z: false = true || 4
```

- Type false is not assignable to type 6
- Type true is not assignable to type false

#### Equality

```ts
(4 === 2) satisfies true;
(4 !== 2) satisfies string;
```

- Expected true, found false
- Expected string, found true

#### Inequality

```ts
(Math.PI > 3) satisfies true;
(4 < 2) satisfies true;
(4 > 2) satisfies number;
(2 >= 2) satisfies string;
```

- Expected true, found false
- Expected number, found true
- Expected string, found true

#### String operations

```ts
"hi".toUpperCase() satisfies number
```

- Expected number, found "HI"

#### Math operations

```ts
Math.cos(0) satisfies 0;
Math.sqrt(16) satisfies 1;
Math.floor(723.22) satisfies 2
```

- Expected 0, found 1
- Expected 1, found 4
- Expected 2, found 723

#### Updating assignments

```ts
let a = 5, b = 6;
a++;
a satisfies 4;
b *= 4;
b satisfies 23;
```

- Expected 4, found 6
- Expected 23, found 24

### Objects

#### Property exists

```ts
let my_obj = { a: 3 }
const a = my_obj.a
const b = my_obj.b
```

- No property 'b' on { a: 3 }

#### Property updates registered

```ts
let my_obj = { a: 3 }
my_obj.a = 4
let b: 3 = my_obj.a
```

- Type 4 is not assignable to type 3

#### Property references

```ts
const my_obj = { a: 2 }
const three: 3 = my_obj.a
```

- Type 2 is not assignable to type 3

#### Object property constraints

```ts
const my_obj: { a: number } = { a: 2 }
my_obj.a = "not a number"
```

- Type "not a number" does not meet property constraint number

#### Objects checks

```ts
const my_obj: { b: 3 } = { a: 2 }
```

- Type { a: 2 } is not assignable to type { b: 3 }

#### Getters

```ts
const b = {
	get c() {
		return 2
	},
}
b.c satisfies string
```

- Expected string, found 2

#### Object spread

```ts
const obj1 = { a: 2, b: 3 };
const obj2 = { b: 4, ...obj1, a: 6 };

obj2.b satisfies 100;
obj2.a satisfies boolean;
```

- Expected 100, found 3
- Expected boolean, found 6

#### Array pushing and pop-ing

```ts
const x = [1]
x.push("hi")
x[1] satisfies 3
x.length satisfies 4;
x.pop() satisfies "hi";
x.length satisfies 1;
```

- Expected 3, found "hi"
- Expected 4, found 2

#### Functions create objects

```ts
function newObject() {
	return { prop: 2 }
}

const a = newObject(), b = newObject();
const c = a;
(a === c) satisfies false;
(a === b) satisfies string;
```

- Expected false, found true
- Expected string, found false

#### Set property with key

```ts
const obj = { a: 2 }

function setProperty(key: string, value) {
	obj[key] = value;
}

setProperty("b", 6)
obj satisfies string;
```

- Expected string, found { a: 2, b: 6 }

#### Delete properties

```ts
const x = { a: 2, b: 3 }
delete x.b;
const b = x.b;
```

- No property 'b' on { a: 2 }

### Control flow

#### Resolving conditional

```ts
function isNegative(x: number) {
	return x < 0 ? "negative" : "positive"
}
isNegative(-4) satisfies number
isNegative(4) satisfies boolean
```

- Expected number, found "negative"
- Expected boolean, found "positive"

#### *Conclusive* conditional update

```ts
let a: number = 0
function conditional(v: string) {
	if (v === "value") {
		a++
	}
}
conditional("x")
a satisfies 2
conditional("value")
a satisfies 3
```

- Expected 2, found 0
- Expected 3, found 1

#### *Inconclusive* conditional update

```ts
declare var value: string;
let a: string | number = 0;

function conditional(v: string) {
	if (v === "value") {
		a = "hi"
	}
}
conditional(value);
a satisfies string
```

- Expected string, found "hi" | 0

#### If else

```ts
function print_number(value: number) {
    if (value === 0) {
        return "zero"
    } else if (value === 1) {
        return "one"
    } else {
        return "some number"
    }
}

print_number(0) satisfies "zero"
print_number(0) satisfies "some number"
print_number(1) satisfies "ONE"
print_number(100) satisfies "100"
print_number(-1) satisfies "TWO"
```

- Expected "some number", found "zero"
- Expected "ONE", found "one"
- Expected "100", found "some number"
- Expected "TWO", found "some number"

#### Operator short circuiting

```ts
let a: number = 0
const func = condition => condition || ++a;

func(true);
a satisfies 0;
func(false) satisfies 1;
a satisfies 2;
```

- Expected 2, found 1

#### Conditional assignment

```ts
let a = false, b = 4;
a ||= b++;
a satisfies 3;
b ||= (b = 10);
b satisfies string;
```

- Expected 3, found 4
- Expected string, found 5

#### Conditional return type inference

```ts
function func(a: boolean) {
	if (a) {
		return 2
	}
}

func satisfies (a: boolean) => 5;
```

- Expected (a: boolean) => 5, found (a: boolean) => 2 | undefined

### Statements, declarations and expressions

> Some of these are part of synthesis, rather than checking

#### Interfaces

```ts
interface X {
	a: string,
	b: boolean
}

const x: X = { a: 2, b: false }
```

- Type { a: 2, b: false } is not assignable to type X

#### Type aliases

```ts
type MyNumber = number;
"hi" satisfies MyNumber;
4 satisfies MyNumber
```

- Expected MyNumber, found "hi"

#### Declare variable

```ts
declare var global_number: number
const my_number: string = global_number
```

- Type number is not assignable to type string

#### (untagged) Template literal

```ts
const name = "Ben";
`Hello ${name}` satisfies "Hi Ben"
```

- Expected "Hi Ben", found "Hello Ben"

#### In operator

```ts
const obj = { a: 2 };
("a" in obj) satisfies string;
("b" in obj) satisfies true;
```

- Expected string, found true
- Expected true, found false

#### Type of mathematical operator

```ts
declare var x: number;
(x * 2) satisfies string
```

- Expected string, found number

#### Type of equality operators

```ts
declare var x: number;
(x < 4) satisfies string;
(x === 4) satisfies Math;
```

- Expected string, found boolean
- Expected Math, found boolean

#### Type of logical operators

```ts
declare var x: number, y: boolean;
(x && y) satisfies string;
```

- Expected string, found boolean | number

#### Object literal (constant) computed key

```ts
const y = { ["EZNO".toLowerCase()]: 7 }
y.ezno satisfies 3
```

- Expected 3, found 7

#### Shorthand object literal

```ts
const x = 2
const y = { x }
y.x satisfies 3
```

- Expected 3, found 2

#### Array destructuring

```ts
const array = [1, 2, 3]
const [a, b] = array
a satisfies 1; b satisfies string;
```

- Expected string, found 2

#### Object destructuring

```ts
const object = { a: 1, b: 2 }
const { a, b } = object
a satisfies 1; b satisfies string;
```

- Expected string, found 2

#### Nested object destructuring

```ts
const object = { a: { b: { c: 2 } } }
const { a: { b: { c: d } } } = object
d satisfies 1;
```

- Expected 1, found 2

#### Try-catch and throw

```ts
try {
	throw 2
} catch (err) {
	err satisfies string
}
```

- Expected string, found 2

#### Throw effects carry through

```ts
function throwType(a) {
	throw a
}

try {
	throwType(3)
} catch (err) {
	err satisfies string
}
```

- Expected string, found 3

#### Interface merging

```ts
interface X {
	a: string,
	b: boolean
}

interface X {
	c: number
}

const x: X = { a: "field", b: false, c: false }
const y: X = { a: "field", b: false, c: 2 }
```

- Type { a: "field", b: false, c: false } is not assignable to type X

### Classes

> TODO privacy

#### Constructor

```ts
class X {
	constructor(value) {
		this.value = value
	}
}

const x = new X(4)
x.value satisfies string
```

- Expected string, found 4

#### Property keys

> Property keys are synthesised once and their effects are once (as opposed to their value)

```ts
let global: number = 0;
class X {
	[global++] = "b";
}
global satisfies 0;
(new X)[0] satisfies "a";
(new X, new X);
global satisfies string;
```

- Expected 0, found 1
- Expected "a", found "b"
- Expected string, found 1

#### Properties

> TODO check timing

```ts
let global: number = 0;

class X {
	property = ++global;
}

(new X()).property satisfies string;
(new X()).property satisfies 2;
(new X()).property satisfies boolean;
```

- Expected string, found 1
- Expected boolean, found 3

#### Class methods

```ts
class X {
	constructor(value) {
		this.value = value
	}

	getObject(this: { value: any }, b) {
		return { a: this.value, b }
	}
}

const x = new X(4)
x.getObject(2) satisfies string
```

- Expected string, found { a: 4, b: 2 }

#### Automatic class constructor

```ts
class X {
	a = 2
}

(new X).a satisfies 3
```

- Expected 3, found 2

#### Static class property

```ts
class X {
	static a = 2
}

X.a satisfies 3
```

- Expected 3, found 2

### Types

#### Non existent type

```ts
type X = number;
const a: Y = 2;
```

- Cannot find type Y

#### Type has no generics

```ts
type X = number;
const a: X<number> = 2;
```

- Type 'X' has no generic parameters

#### Type alias with type parameters

```ts
type X<T> = T;

(2 satisfies X<string>);
```

- Expected string, found 2

#### Type annotation missing type arguments

```ts
type X<T> = T;

(2 satisfies X);
```

- Type X requires type arguments

#### Property on an or type

```ts
function getProp(obj: { prop: 3 } | { prop: 2 }) {
	return obj.prop
}

getProp satisfies string
```

- Expected string, found (obj: { prop: 3 } | { prop: 2 }) => 3 | 2

#### Generic extends

```ts
function getA<T extends { a: string }>(p: T) {
    return p.a
}

getA({ p: 2 })
```

- Argument of type { p: 2 } is not assignable to parameter of type T

> I think reasons contains more information

#### Function subtyping

```ts
// Perfectly fine
const x: (a: number) => string = (a: 4) => "hi"
// Bad
const y: (a: 4) => string = (a: number) => "hi"
```

- Type (a: number) => "hi" is not assignable to type (a: 4) => string

> I think reasons contains more information

### Prototypes

#### Set prototype

```ts
const x = { a: 3 };
Object.setPrototypeOf(x, { a: 5, b: 2 });
x.a satisfies 3;
x.b satisfies string;
```

- Expected string, found 2

#### Get prototype

```ts
const x = { a: 3 };
const p = { b: 2 }
Object.setPrototypeOf(x, p);
const p_of_x = Object.getPrototypeOf(x);
// ('a' in p_of_x.a) satisfies false;
p === p_of_x satisfies string;
```

- Expected string, found true

### Imports and exports

#### Import and export named

also *imports work with and without extensions*

```ts
import { PI } from "./constants.ts";
import { PI as otherPI, "non identifier" as a } from "./other";

PI satisfies string;
otherPI satisfies boolean;
a satisfies 8;

// in constants.ts
export const PI = 4;

// in other.ts
export const PI = 22 / 7;
const private = 2;
export { private as "non identifier" }
```

- Expected string, found 4
- Expected boolean, found 3.142857142857143
- Expected 8, found 2

#### Imports are constant

```ts
import { PI } from "./constants";
PI += 2;

// in constants.ts
export let PI = 4;
```

- Cannot assign to constant

#### Import default

```ts
import PI from "./pi";
PI satisfies string;

// in pi.ts
export default 4;
```

- Expected string, found 4

#### Import type

```ts
import { MyNumber } from "./types";
2 satisfies MyNumber;

// in types.ts
export type MyNumber = string;
```

- Expected MyNumber, found 2

#### Export let

```ts
import { counter, incrementCounter } from "./mutable";

counter satisfies string;
incrementCounter();
counter satisfies 3;
incrementCounter();
counter satisfies string;

// in mutable.ts
export let counter = 2;
export function incrementCounter() {
	counter++
}
```

- Expected string, found 2
- Expected string, found 4

#### Import star

```ts
import * as the from "./many";

the satisfies string;

// in many.ts
export const a = 2, b = 3, c = 4;
```

- Expected string, found { a: 2, b: 3, c: 4 }

#### Import from non existent file

```ts
import { a } from "./two";

console.log(a.prop);

// in one.ts
export const a = 2;
```

- Cannot find file

#### Import where export does not exist

```ts
import { a } from "./export";

console.log(a.prop);

// in export.ts
export const b = 2;
```

- a not exported from ./export

#### Import from invalid file

```ts
import { a } from "./export";

console.log(a.prop);

// in export.ts
export default const x = 2;
```

- Expected SemiColon found x

#### Only synthesis module once

```ts
import { a } from "./export1";
import { b } from "./export2";

(a === b) satisfies string;

// in export1.ts
export { the as a } from "./base"

// in export2.ts
export { the as b } from "./base"

// in base.ts
export const the = ((4 satisfies 1),3);
```

- Expected 1, found 4
- Expected string, found true

> The fact the `Expected 1, found 4` only occurs once means that the module was only synthesised once

#### Use export in scope

```ts
export const x = 2;
x satisfies 3;
```

- Expected 3, found 2

#### Imports don't leak non exports

```ts
import { x } from "./exports"
console.log(y)

// in exports.ts
export const x = 2;
const y = "122LH"
```

- Could not find variable y in scope

#### Import side effect

> Don't take this as permission to do this

```ts
import { x } from "./export";
import "./side_effect";

x satisfies number;

// in side_effect.ts
import { x } from "./export";

x satisfies string;

x.b = x.a + 2;

// in export.ts
export const x = { a: 2 };
```

- Expected string, found { a: 2 }
- Expected number, found { a: 2, b: 4 }
