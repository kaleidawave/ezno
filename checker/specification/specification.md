## Specification

See [./README.md](README.md) for details about how `specification.md` works

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
x = "hello world"
```

- Type "hello world" is not assignable to type number

#### Variable references

```ts
const a = 3
const b: string = a
```

- Type 3 is not assignable to type string

#### Variable updates registered

```ts
let a = 2
a = "hello world"
let b: boolean = a
```

- Type "hello world" is not assignable to type boolean

#### Variable references does not exist

```ts
const a = c
```

- Could not find variable 'c' in scope

#### Assignment to non-existent variable

```ts
doesNotExist = 4;
```

- Cannot assign to unknown variable 'doesNotExist'

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

- Cannot redeclare variable 'a'

#### Unintialised variables are undefined

> Might be a usage warning at some point

```ts
let b;
b satisfies string;
```

- Expected string, found undefined

### Properties

#### Property exists

```ts
let my_obj = { a: 3 }
const a = my_obj.a
const b = my_obj.b
```

- No property 'b' on { a: 3 }

#### Reading property (via accessor)

```ts
const my_obj = { a: 2 }
const three: 3 = my_obj.a
```

- Type 2 is not assignable to type 3

#### Property updates registered

```ts
let my_obj = { a: 3 }
my_obj.a = 4
let b: 3 = my_obj.a
```

- Type 4 is not assignable to type 3

#### Object property constraints

```ts
const my_obj: { a: number } = { a: 2 }
my_obj.a = "hello world"
```

- Type "hello world" does not meet property constraint number

#### Objects checks

```ts
const my_obj: { b: 3 } = { a: 2 }
```

- Type { a: 2 } is not assignable to type { b: 3 }

#### Getters

```ts
let global = 0;
const object = {
	// This getter has an impure side effect
	get getValue() {
		return ++global
	},
}

object.getValue satisfies string
object.getValue satisfies boolean
```

- Expected string, found 1
- Expected boolean, found 2

#### Object spread

```ts
const obj1 = { a: 2, b: 3 };
const obj2 = { b: 4, ...obj1, a: 6 };

obj2.b satisfies 100;
obj2.a satisfies boolean;
```

- Expected 100, found 3
- Expected boolean, found 6

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

> Lot of "Expression is always true" here

- Expression is always true
- Expression is always true
- Type false is not assignable to type 6
- Expression is always true
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

#### String operations (constant functions can use `this`)

```ts
"hi".toUpperCase() satisfies number
```

- Expected number, found "HI"

#### Math operations

```ts
Math.cos(0) satisfies 0
Math.sqrt(16) satisfies 1
Math.floor(723.22) satisfies 2;
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

#### Index into string

```ts
("something"[2]) satisfies number;
```

- Expected number, found "m"

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

#### Set property on dependent observed

```ts
function add_property(obj: { prop: number }) {
	obj.prop = 2;
	(obj.prop satisfies 4);
}
```

> Not number

- Expected 4, found 2

#### Type checking basic function types

```ts
function func(a: string, b: number): boolean {
	return true
}
func satisfies (a: string, b: number) => boolean;
func satisfies (a: string, b: number) => string;
func satisfies (a: number, b: number) => boolean;
```

- Expected (a: string, b: number) => string, found (a: string, b: number) => boolean
- Expected (a: number, b: number) => boolean, found (a: string, b: number) => boolean

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

#### Expected parameter from variable declaration

> Technically works with inference but this method should be less overhead + produce better positioned errors

```ts
const x: (a: string) => number = a => a.to;
```

- No property 'to' on string

#### Expected argument from parameter declaration

```ts
function map(a: (a: number) => number) {}

// No annotation on `a`. But error comes from body
// (rather than parameter assignment)
map(a => a.t)
```

- No property 't' on number

#### Assignment to parameter

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

#### Type of rest parameter

```ts
function myRestFunction(...r: string[]) {
	r satisfies boolean;
}
```

- Expected boolean, found Array\<string\>

#### Destructuring parameter

```ts
function myFunction({ a }: { a: number }) {
	a satisfies boolean;
	return a
}

myFunction({ a: 6 }) satisfies string;
```

- Expected boolean, found number
- Expected string, found 6

### Function calling

#### Argument type against parameter

```ts
function func(a: number) {}
func("hello world")
```

- Argument of type "hello world" is not assignable to parameter of type number

#### Parameters retain argument values

```ts
function id(a) {
	return a
}

const d: 3 = id(2)
```

- Type 2 is not assignable to type 3

#### Generic type argument parameter

```ts
function func<T>(a: T) {}
func<number>("hello world")
```

- Argument of type "hello world" is not assignable to parameter of type T

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

#### Constant call and operation with a parameter

> An example of the generic constructor type (namely call and operation)

```ts
function floorPlusB(a: number, b: number) {
	return Math.floor(a) + b
}

floorPlusB(100.22, 5) satisfies 8
```

- Expected 8, found 105

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

#### This passed around

```ts
function getToUpperCase(s: string) {
	return s.toUpperCase
}

getToUpperCase("hi")() satisfies "HEY";
```

- Expected "HEY", found "HI"

#### This as generic argument

```ts
function callToUpperCase(s: string) {
	return s.toUpperCase()
}

callToUpperCase("hi") satisfies "HEY";
```

- Expected "HEY", found "HI"

#### Calling new on a function

```ts
function MyClass(value) {
	this.value = value
}

new MyClass("hi").value satisfies "hello"
```

- Expected "hello", found "hi"

#### Arguments in to rest parameter

```ts
function myRestFunction(...r: string[]) {
	return r[0] + r[1]
}

myRestFunction("hello ", "world") satisfies number;
```

- Expected number, found "hello world"

#### Default parameter

```ts
function withDefault(x: number = 1) {
	return x
}

withDefault() satisfies 2;
withDefault(3) satisfies 3;
```

- Expected 2, found 1

#### Default parameter side effect

```ts
let b: number = 0
function doThing(a = (b += 2)) {
	return a
}

doThing("hello");
b satisfies 0;
doThing();
b satisfies 1;
```

- Expected 1, found 2

#### Tagged template literal

```ts
function myTag(static_parts: Array<string>, name: string) {
	return static_parts[0] + name
}

const name = "Ben";
myTag`${name}Hello ` satisfies "Hi Ben"
```

- Expected "Hi Ben", found "Hello Ben"

### Effects

> Side effects of functions. Registered internally as `Event`s

#### Assignment to free variable

```ts
let a: number = 0
function func() {
	a = 4;
	// Important that subsequent reads use the 
	// new value, not the same free variable
	a satisfies 4;
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

#### Free variable property update object inside function

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

#### TDZ from free variable (across function)

```ts
function getX() {
	return x
}

getX satisfies () => number;

getX();

let x: number = 5;
```

- Variable 'x' used before declaration

> Not shown in the example but thanks to [#69](https://github.com/kaleidawave/ezno/pull/69) for adding the position of the error

#### Assignment to union

> Solves the common subtyping issue between read and write properties

```ts
let myObject: { a: number } = { a: 4 }

function readA(someObject: { a: number | string }) {
	return someObject.a;
}

function setAtoString(someObject: { a: number | string }) {
	someObject.a = "hi";
}

// Allowed
readA(myObject);
setAtoString({ a: 6 });
setAtoString(myObject);
```

- Assignment mismatch

> Error message could be better. Full diagnostic contains labels with more information
> `readA` is allowed, which is disallowed in Hegel, but here is allowed to preserve TSC compatibility (and because how structural subtyping is implemented)
> Not shown in the example but thanks to [#69](https://github.com/kaleidawave/ezno/pull/69) for adding the position of the error

#### Property assignment from conditional

```ts
function getObject(condition: boolean) {
	const mainObject = { a: 2 };
	const object = condition ? mainObject : { b: 3 };
	object.c = 4;
	mainObject.c satisfies string;
	return mainObject
}
```

- Expected string, found 4

#### Mutating an object by a function

> This is where the object loses its constant-ness

```ts
function doThingWithCallback(callback: (obj: { x: number }) => any) {
	const obj: { x: number } = { x: 8 };
	callback(obj);
	(obj.x satisfies 8);
	return obj;
}

const object = doThingWithCallback((obj: { x: number }) => obj.x = 2);
object.x satisfies string;
```

- Expected 8, found number
- Expected string, found 2

#### Assigning to parameter observed via effect

```ts
function add_property(obj: { prop: number }) {
	obj.prop += 2;
}

const obj = { prop: 4 };
add_property(obj);
obj.prop satisfies 8;
```

- Expected 8, found 6

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

// Create a one in between to test they don't have a global state
magicNumber(8).doubled() satisfies 16;

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

### Collection types

#### Array push

```ts
const x = [1]
x.push("hi")
x[1] satisfies 3
x.length satisfies 4;
```

- Expected 3, found "hi"
- Expected 4, found 2

#### Array pop

```ts
const myArray = [6, "hi"]
myArray.pop() satisfies 3;
myArray.length satisfies 1;
```

- Expected 3, found "hi"

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

#### If and else (across function)

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

> a is always assigned ('Expression is always false') and b is always true (so the RHS never runs)

- Expression is always false
- Expected 3, found 4
- Expression is always true
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

### Iteration

#### While loop unrolling

```ts
let a = 1;
let i = 0;
while (i < 5) {
	a *= 2;
	i++;
}

a satisfies 8;
```

- Expected 8, found 32

#### While loop event in the condition

```ts
let a = 1;
let i = 0;
while (i++ < 5) {
	a *= 2;
}

a satisfies 8;
```

- Expected 8, found 32

#### Do while loop

```ts
let a = 0;
do {
	a++
} while (a < 3)

a satisfies 8;
```

- Expected 8, found 3

#### For loop with initialiser and condition

```ts
let a: string = "";
for (let i: number = 0; i < 10; i++) {
	a = a + i;
}

a satisfies number;
```

- Expected number, found "0123456789"

#### While loop with unknown number of iterations

```ts
declare let i: number;
let a: number = 0;
while (a < i) {
	a++;
}

a satisfies string;
```

- Expected string, found number

> Important that type is widened to 'number' (think it is an open poly in this case)

#### Limit to iterations

```ts
let a: number = 0;
while (a++ < 1_000_000) {}

a satisfies string;
```

> The important part is that it doesn't run the loop. Eventually this might be run in a way that is not calling the assign to variable
> function that evaluates `a = a + 1` a million times. There also should be per project, per module, per loop configuration

- Expected string, found number

#### While loop unrolling as a side-effect

```ts
function loop(n: number, c: string) {
	let a: string = c;
	let i: number = 0;
	while (i++ < n) {
		a += c
	}
	return a
}

loop(10, "!") satisfies number;
```

- Expected number, found "!!!!!!!!!!"

#### Break in a while loop

```ts
let a = 2;
let i = 0;
while (i++ < 10) {
	a *= 2;
	if (a > 5) {
		break;
	}
}

a satisfies 2;
```

- Expected 2, found 8

#### Break with label

```ts
let a: number = 0;
let result;

top: while (a++ < 10) {
	let b: number = 0;
	while (b++ < 10) {
		if (a === 3 && b === 2) {
			result = a * b;
			break top
		}
	}
}

a satisfies string;
result satisfies boolean;
```

- Expected string, found 3
- Expected boolean, found 6

#### Continue in a while loop

> With the continue the update to `a` only happens on even runs (5 times)

```ts
let a = 2;
let i = 0;
while (i++ < 10) {
	if (i % 2) {
		continue;
	}
	a *= 2;
}

a satisfies 2;
```

- Expected 2, found 64

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
4 satisfies MyNumber;
```

- Expected MyNumber, found "hi"

#### Declare variable

```ts
declare const global_number: number
const my_number: string = global_number
```

- Type number is not assignable to type string

#### Function (and interface) hoisting

> Using functions and interface **before** their position of declaration in the source

```ts
getFive() satisfies 4;

function getFive() {
	return 5
}

let x: X = { a: 3 }

interface X {
	a: 2
}
```

- Expected 4, found 5
- Type { a: 3 } is not assignable to type X

#### RegExp

> RegExp = Regular expression
> In the future, their definition could be considered and evaluated at runtime

```ts
/hi/ satisfies string;
```

- Expected string, found /hi/

#### Null and undefined

```ts
undefined satisfies null;
null satisfies undefined;
```

- Expected null, found undefined
- Expected undefined, found null

#### void operator

```ts
(void 2) satisfies string;
```

- Expected string, found undefined

#### (untagged) Template literal

```ts
const name = "Ben";
`Hello ${name}` satisfies "Hi Ben"
```

- Expected "Hi Ben", found "Hello Ben"

#### `in` operator

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

#### Type of relation operators

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
y.not_a_key
```

- Expected 3, found 7
- No property 'not_a_key' on { ezno: 7 }

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

{
	interface X {
		c: number
	}
	
	const x: X = { a: "field", b: false, c: false }
	const y: X = { a: "field", b: false, c: 2 }
}
```

- Type { a: "field", b: false, c: false } is not assignable to type X

#### Interfaces do not merge with aliases

```ts
type X = { a: string }

{
	interface X {
		b: number
	}

	const x: X = { b: 3 } // Don't require 'a' here <-
	const y: X = { b: "NaN" }
}
```

- Type { b: "NaN" } is not assignable to type X

#### TDZ in statements

```ts
let first = second;

let second = 2;
```

- Variable 'second' used before declaration

### Classes

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

2 satisfies X<string>;
```

- Expected string, found 2

#### Type annotation missing type arguments

```ts
type X<T> = T;

2 satisfies X;
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

#### Function parameter subtyping

```ts
// Perfectly fine
const x: (a: number) => string = (p: string | number) => "hi"
// Bad
const y: (a: number | string) => string = (p: number) => "hi"
```

- Type (p: number) => "hi" is not assignable to type (a: number | string) => string

> I think reasons contains more information

#### Function return type subtyping

```ts
const x: (a: number) => number = p => 4
const y: (a: number) => number = p => "a number"
```

- Type (p: number) => "a number" is not assignable to type (a: number) => number

#### `void` return type

> This works similarly to undefined except that it accepts any function return type

```ts
function runWithCallback(cb: () => void): void {
	cb() satisfies string;

	return 5;
}

runWithCallback(() => 3)
```

> Here argument is fine. In the body the return type is `any` (inferred constraint, but doesn't matter)

- Expected string, found any
- Cannot return 5 because the function is expected to return void

#### Indexing into (fixed) type

```ts
interface ThePrimitives {
	a: number,
	b: string,
	c: boolean
}

2 satisfies ThePrimitives["b"];
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

- Cannot return T["other"] because the function is expected to return T["prop"]

#### Index into dependent array

```ts
function getFirst(array: number[]) {
	return array[0]
}

getFirst satisfies boolean;
```

- Expected boolean, found (array: Array\<number>) => number | undefined

#### Index into dependent string

```ts
function getSecondCharacter(s: string) {
	return s[1]
}

getSecondCharacter satisfies boolean;
getSecondCharacter("string") satisfies "b";
```

- Expected boolean, found (s: string) => string | undefined
- Expected "b", found "t"

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
const numbers2: Array<string> = ["hi", "3"]
```

- Type [1, 2, "3"] is not assignable to type Array\<number>

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
(p === p_of_x) satisfies string;
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

#### Import type and variable

```ts
import { MyNumber } from "./types";
2 satisfies MyNumber;
MyNumber satisfies boolean;

// in types.ts
export type MyNumber = string;
export const MyNumber = 6;
```

- Expected MyNumber, found 2
- Expected boolean, found 6

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

- Expected SemiColon found Identifier(\"x\")

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

- Could not find variable 'y' in scope

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
