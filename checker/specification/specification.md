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

#### Assigning before declaration

```ts
a = 3;
let a = 2;
```

- Cannot assign to 'a' before declaration

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

#### Variable shadowing

> TODO maybe should test loops, functions, function parameters etc...

```ts
const a = 2
{
	const a = 3;
	a satisfies 2;
}
```

- Expected 2, found 3

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

#### Objects checks

```ts
const my_obj: { b: 3 } = { b: 4 }
```

- Type { b: 4 } is not assignable to type { b: 3 }

#### Getters

```ts
let global = 0;
const object = {
	// This getter has an impure side effect
	get value() {
		return ++global
	},
}

object.value satisfies string
object.value satisfies boolean
```

- Expected string, found 1
- Expected boolean, found 2

#### Getter `this`

```ts
const object = {
	x: 4,
	get value(this: { x: number }) {
		return this.x
	},
}

object.value satisfies string
```

- Expected string, found 4

#### Setter `this`

```ts
let a = 2;
const obj = {
	x: 5,
	set value(v) {
		this.x = v;
	}
}

obj.value = "some value";
obj.x satisfies 5;
```

- Expected 5, found "some value"

#### Setter assignment type

```ts
const obj = {
   set value(v: string) { }
}

obj.value = 5;
```

- Type 5 does not meet property constraint string

#### Setter side effect

```ts
let a = 2;
const obj = {
	x: 5,
	set value(v) {
		a = v;
	}
}

obj.value = "some value";
a satisfies 2;
```

- Expected 2, found "some value"

#### Setter return

> Returns the RHS not the return type
> TODO warning in the setter

```ts
const result = ({ set value(a) { return { a: 3 } }}).value = 5;
result satisfies string;
```

- Expected string, found 5

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

### Excess property

> The following work through the same mechanism as forward inference
> Thanks to pull request: #139

#### Excess property at declaration

```ts
interface MyObject { property: string }

const a: MyObject = { property: "hello", another: 2 }
```

- 'another' is not a property of MyObject

#### Excess property at argument

```ts
interface MyObject { property: string }

function process(param: MyObject) {}

process({ property: "hello", another: 2 })
```

- 'another' is not a property of MyObject

#### Excess property at return type

```ts
interface MyObject { property: string }

function returnNewObject(): MyObject {
	return { property: "hello", another: 67 }
}
```

- 'another' is not a property of MyObject

#### Excess property checks through spread and condition

```ts
type MyObject = { foo: number; bar?: number };

const b: MyObject = {
  foo: 1,
  ...{
    bar: 2,
    invalid: 3,
  },
};

declare let condition: boolean;

const c: MyObject = {
  foo: 1,
  ...(condition ? {
    bar: 2,
    non_existent: 3,
  } : {}),
};
```

- 'invalid' is not a property of MyObject
- 'non_existent' is not a property of MyObject

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
(6 >= 2) satisfies string;
(6 <= 2) satisfies 5;
```

- Expected true, found false
- Expected number, found true
- Expected string, found true
- Expected 5, found false

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

#### Default parameter value type check

> Thanks to #132

```ts
function outer(a: number) {
    function inner(b: string = Math.floor(a)) {
    }
}
```

- Cannot use a default value of type number for parameter of type string

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

#### Variadic function parameter type

> aka rest parameter type

```ts
function variadic(...r: string[]) {
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

#### String internal `this` unbinding error

> Thanks to `this` checking in #127

```ts
const { toUpperCase } = "hi";

toUpperCase();
```

- The 'this' context of the function is expected to be string, found undefined

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

#### Optional parameter type

> Effectively optional parameter with the default value being undefined

```ts
function optionally(p?: number) {
  p satisfies string;
}
```

- Expected string, found number | undefined

#### Calling optional parameter type

```ts
function optionally(p?: number) {
  return p
}

// Fine
optionally() satisfies undefined;
optionally(5) satisfies 5;

optionally("hello world");
```

- Argument of type "hello world" is not assignable to parameter of type number | undefined

#### Tagged template literal

```ts
function myTag(static_parts: Array<string>, name: string) {
	return static_parts[0] + name
}

const name = "Ben";
myTag`${name}Hello ` satisfies "Hi Ben"
```

- Expected "Hi Ben", found "Hello Ben"

#### Default parameter side effect on parameter

```ts
function doThing(a, b = (a += 2)) {
	return a
}

doThing(3) satisfies 2;
doThing(6, 1) satisfies 6;
```

- Expected 2, found 5

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

- Invalid assignment to parameter

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
a satisfies string;
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

### Collections

> Some of these are built of exiting features.
> But they are important enough in code for them to have their own usage tests

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
[1, 2, 3].find(x => x % 2 === 0) satisfies 4;

// [1, 2, 3].includes(6) satisfies string;
```

- Expected 4, found 2
<!-- - Expected string, found false -->

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
type MyObj = { a: string };

"hi" satisfies MyNumber;
4 satisfies MyNumber;

declare let obj: MyObj;
obj.a satisfies string;
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
const regexp = /hi/ satisfies string;
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

#### Catch annotation

> Thanks to #131

```ts
try {
	throw 3
} catch (err: string) {
    console.log(err)
}
```

- Cannot catch type string because the try block throws 3

#### Object destructuring assignment

> Added in #127

```ts
const o = { a: 1, b: { c: 3 } };

let a, b, c;
({
  c = o.a++,
  b: { c: b = 7 },
  a,
} = o);

a satisfies string;
b satisfies boolean;
c satisfies 3;
```

- Expected string, found 2
- Expected boolean, found 3
- Expected 3, found 1

#### `import.meta`

> Unfortunately because of bundling `url` and `resolve` cannot have known results so just `string`.

```ts
import.meta.url satisfies number;
import.meta.resolve("./lib/helper.js") satisfies string;

import.meta.env.production satisfies boolean;
```

- Expected number, found string
- Expected boolean, found string | undefined

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

#### Class `this` unbinding

> Thanks to `this` checking added in #127

```ts
class X {
    method() {
        return this;
    }
}

const { method } = new X();
method();
```

- The 'this' context of the function is expected to be X, found undefined

#### Property keys

> Property keys are synthesised once and their effects run once (as opposed to their value)

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

#### Property field side effects

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

#### Mix of property fields and assigned

> Property fields are assigned first

```ts
let global: number = 0;

class X {
	prop1 = ++global;

	constructor() {
		this.prop2 = ++global;
	}
}

const x = new X();
x.prop1 satisfies string;
x.prop2 satisfies boolean;
```

- Expected string, found 1
- Expected boolean, found 2

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

#### Use before defined

> Declared same as `let` and `const`

```ts
const x = new X;
class X { }
```

- Variable 'X' used before declaration

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

### Types

#### Non existent type

```ts
type X = number;
const a: Y = 2;
```

- Cannot find type Y

#### Type shadowing

> TODO maybe should test loops, functions, function parameters etc...

```ts
type X = string;
{
	type X = number;
	const a: X = "hello world";
}
```

- Type "hello world" is not assignable to type X

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

const b: X = 2;
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

getA({ a: 2 })
```

- Argument of type { a: 2 } is not assignable to parameter of type T

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

- Expected string, found void
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
const numbers1: Array<number> = [1, 2, "3"],
      numbers2: Array<string> = ["hi", "3"],
      numbers3: Array<string> = 4;
```

- Type [1, 2, "3"] is not assignable to type Array\<number>
- Type 4 is not assignable to type Array\<string>

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

#### Generic condition

```ts
declare function isNumber<T>(t: T): T extends number ? "yeess" : "nno";

isNumber(5) satisfies "yeess";
isNumber("5") satisfies number;
```

- Expected number, found "nno"

#### More accurate generic

```ts
declare function unwrap<T>(a: T | { item: T }): T;

unwrap({ item: 5 }) satisfies string;
unwrap(16) satisfies 16;
```

- Expected string, found 5

#### Across alias

```ts
type WithLabel<T> = { label: string, item: T };

declare function getItem<T>(a: WithLabel<T>): T;

getItem({ label: "item 1", item: 5 }) satisfies string;
```

- Expected string, found 5

#### Double generics

> Really want to only have one covariant and one contravariant but want to keep TSC semantics

```ts
declare function what<T>(a: T, b: T): T;

what(2, 3) satisfies string;
```

- Expected string, found 2 | 3

### Mapped types

> Aka generic property keys

#### Simple key

```ts
type Record2<K extends string, T> = { [P in K]: T }

declare let myRecord: Record2<"hi", number>;

myRecord.hi satisfies string;
myRecord.hello;
```

- Expected string, found number
- No property 'hello' on { [\"hi\"]: number }

#### Assignment

```ts
type Record2<K extends string, T> = { [P in K]: T }

const x: Record2<"test", boolean> = { no: false },
      y: Record2<"test", boolean> = { test: 6 },
      z: Record2<"test", boolean> = { test: false };
```

- 'no' is not a property of { [\"test\"]: boolean }
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

#### Computed generics from collection

```ts
const x = [1, 2, 3];
x.map(a => (a satisfies string, 2))
```

- Expected string, found 1 | 2 | 3

### Object constraint

> Any references to a annotated variable **must** be within its LHS type. These test that it carries down to objects.

#### Object property constraints

```ts
const my_obj: { a: number } = { a: 2 }
my_obj.a = "hello world"
```

- Type "hello world" does not meet property constraint number

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
export default const;
```

- Found reserved identifier

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

### Extras

> This contains new features. Most are WIP

#### JSX type

```tsx
function JSXH(tag_name: string, attributes: any, children: any) {
  return { tag_name, attributes, children }
}

const x = <h1 title="Example text">Hello World</h1> satisfies string;
```

- Expected string, found { tag_name: "h1", attributes: { title: "Example text" }, children: ["Hello World"] }

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

#### Errors carries

> Note only one error raised. This prevents the compiler presenting loads of errors if an origin is invalid

```ts
const obj = { prop: 2 };
console.log(obj.a.b.c);

function x() {
	return y
}

x().nothing
```

- Could not find variable 'y' in scope
- No property 'a' on { prop: 2 }
