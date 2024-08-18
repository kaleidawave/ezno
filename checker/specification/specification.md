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
a satisfies number
```

- Expected number, found "hello world"

#### Variable references does not exist

```ts
const exists = 2;
nexists
```

- Could not find variable 'nexists' in scope

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

#### TDZ in statements

```ts
let first = second;
let second = 2;
```

- Variable 'second' used before declaration

#### `var` can be reregistered

```ts
{
	// Fine
	var x = 2;
	x satisfies 2;
	var x = 3;
	x satisfies 3;
}

{
	let b = 2;
	var b = 2;
}
```

- Cannot redeclare variable 'b'

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
let global: number = 0;
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
	set value(this: { x: number }, v) {
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

> TODO could the error be better?

- Argument of type 5 is not assignable to parameter of type string (in setter)

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
const obj: { a?: number, b?: number } = { a: 2 }

function setProperty(key: "a" | "b", value: number) {
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

#### `Object.keys`, `Object.values`, `Object.entries`

```ts
Object.keys({ a: 1, b: 2 }) satisfies ["a", "b"];
Object.values({ a: 1, b: 2 }) satisfies [1, 2];
Object.entries({ a: 1, b: 2 }) satisfies boolean;
```

- Expected boolean, found [["a", 1], ["b", 2]]

#### Spread condition

```ts
declare let condition: boolean;

const obj = {
	foo: 1,
	...(condition ? {
		bar: 2,
		non_existent: 3,
	} : {}),
};

obj.foo satisfies number;
obj.bar satisfies string;
```

- Expected string, found 2 | undefined

#### And on properties

> Note that it keeps it as a `and`. It does not join the properties into a single typ

```ts
declare type U = { a: 2 } & { b: 3 }
declare let x: U;
x.b satisfies 3;

({ a: 2, b: 3 } satisfies U); 
({ b: 3 } satisfies U); 
```

- Expected U, found { b: 3 }

#### Properties on or

```ts
declare let key: "a" | "b";

const object = { a: "apple", b: "banana" };
object[key] satisfies boolean;
```

- Expected boolean, found "apple" | "banana"

#### Properties on big or

> TODO this creates a fat or type

```ts
const array = [1, 2, 3];
array[Math.random()] satisfies string;
```

- Expected string, found 1 | 2 | 3 | undefined

#### Properties matched against continous type become conditional

> I think this is TSC behavior under indexed access

```ts
declare let strings: { [a: string]: number };
declare let record: Record<string, number>;
declare let d: { [a: "a" | "b"]: number };

strings.a satisfies number | undefined;
record.a satisfies boolean;
d.a satisfies number;
```

- Expected boolean, found number | undefined

#### Un-delete-able property

> TODO in a function as well

```ts
const x: { a?: number } = { a: 4 };
// Fine
delete x.a;

const y: { a: number } = { a: 4 };
// Bad
delete y.a;

const z = {};
Object.defineProperty(z, "a", { value: 4 });
delete z.a;
```

- Cannot delete from object constrained to { a: number }
- Cannot delete from non-configurable property

#### Order of numerical properties

> TODO test could be better using `for in` or `Object.keys` etc

```ts
let x = {}; x.something = null; x[4] = null; x["eight"] = null; x["2"] = null;
x satisfies string;
```

- Expected string, found { 2: null, 4: null, something: null, eight: null }

#### Order of properties after assignment

> TODO this is because setting properties are simply appended. There are two straightforward fixes, but I am unsure which one is better...

```ts
const obj = { a: 1, b: 2 };
obj.a = 2; obj.c = 6; obj.b = 4;
obj satisfies boolean;
```

- Expected boolean, found { a: 2, b: 4, c: 6 }

#### Assigning to getter

```ts
const obj = { get prop() { return 2 } };
obj.prop = "hi";
obj.prop satisfies 2;
```

- Cannot write to property 'prop' as it is a getter

#### Assinging to non existent property

> Allowing this could break objects passed to functions

```ts
const obj = { prop: 1 };
// Fine
obj.non_existent = 6;

function func(param: { prop: number }) {
	param.notProp = 5;
}
```

- Cannot write to non-existent property 'notProp'

#### Function and class name

> TODO should also check that it is readonly

```ts
function a() { }
class B { }
let c = class { }

a.name satisfies "a"
B.name satisfies "B"
c.name satisfies "sea"
```

- Expected "sea", found "c"

#### Getters AND setter

> This involves property lookup skipping setter and getters

```ts
let global: number = 0;
const object = {
	get value() {
		return global
	},
	set value(newValue: number) {
		global = newValue;
	}
}

object.value satisfies string;
object.value = 10;
object.value satisfies 10;
global satisfies 10
```

- Expected string, found 0

#### Getters AND setter can be type via `Object.defineProperty`

> TODO parameter checking as well

```ts
function func(get: () => number) {
	const obj = {};
	Object.defineProperty(obj, "value", { get });
	obj.value satisfies string
}
```

- Expected string, found number

#### `enumerable` in for in

```ts
const obj = { n: 1, b: 2 };
Object.defineProperty(obj, "c", { value: 3, enumerable: false });
Object.defineProperty(obj, "d", { value: 4, enumerable: true });

let keys: string = "";
for (const key in obj) {
	keys += key;
}
keys satisfies boolean
```

- Expected boolean, found "nbd"

#### `Object.freeze`

> TODO seal & preventExtensions

```ts
const obj = {}
let result = Object.freeze(obj);
(obj === result) satisfies true;
obj.property = 2;
Object.isFrozen(obj) satisfies true;
```

> TODO maybe error should say that whole object is frozen

- Cannot write to property 'property'

#### `Object.defineProperty` writable

> TODO defineProperties

```ts
const obj = {};
Object.defineProperty(obj, 'property', {
	value: 42,
	writable: false,
});
obj.property satisfies string;
obj.property = 70;
```

- Expected string, found 42
- Cannot write to property 'property'

#### Descriptor carries across assignments

> TODO defineProperties

```ts
const obj = {};
Object.defineProperty(obj, 'property', {
	value: 42,
	enumerable: false,
	// needed as all properties default to false
	writable: true,
});
obj.property = 70;
Object.getOwnPropertyDescriptor(obj, 'property') satisfies string;
```

- Expected string, found { value: 70, writable: true, enumerable: false, configurable: false }

#### `Object.defineProperty` getter and setter

> TODO setter

```ts
const obj = {};
let b = 0;
Object.defineProperty(obj, 'property', {
	get: () => b,
});
obj.property satisfies 0;
b++;
obj.property satisfies string;
```

- Expected string, found 1

#### `Object.defineProperty` configurable

```ts
const obj = {};
Object.defineProperty(obj, 'property', { value: 6 });
Object.defineProperty(obj, 'property', { value: "hi" });
```

- Property 'property' not configurable

#### `Object.getOwnPropertyDescriptor`

> TODO getOwnPropertyDescriptors

```ts
const obj = { a: "something" };
Object.defineProperty(obj, 'b', { value: 42 });

Object.getOwnPropertyDescriptor(obj, 'a') satisfies string;
Object.getOwnPropertyDescriptor(obj, 'b').writable satisfies false;
```

> Order is also important

- Expected string, found { value: "something", writable: true, enumerable: true, configurable: true }

#### `Object.assign`

> TODO multiple RHS

```ts
const obj = { a: 1 };
Object.assign(obj, { b: 2, c: 3 });
obj satisfies string;
```

- Expected string, found { a: 1, b: 2, c: 3 }
s
### Excess properties

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

process({ property: "hello", another: 2 });

function func<T>(a: T) { }

func<MyObject>({ property: "hello", "something else": 2 })
```

- 'another' is not a property of MyObject
- 'something else' is not a property of MyObject

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

#### Function hoisting

> `getString` can be used and has a type before it has been synthesised
> TODO actual calling before defined (this currently only works bc of free-variables)

```ts
function x() {
	getString(3)
}

function y() {
	getString("something") satisfies string;
}

function getString(param: string): string {
	return "hi"
}
```

- Argument of type 3 is not assignable to parameter of type string

### Inferred return types

#### Simple

```ts
function id(a: number) {
	return a
}

function simple() {
	return "hello world"
}

id satisfies (n: number) => number;
simple satisfies () => number;
```

- Expected () => number, found () => "hello world"

#### Conditional

```ts
function func<T extends boolean>(condition: T) {
	if (condition) {
		return 4
	} else {
		return 3
	}
}

func satisfies string;
```

> There are some issues around printing here, when to include the generic etc

- Expected string, found \<T\>(condition: T) => T ? 4 : 3

#### Early return

```ts
function func(value: number) {
	if (value === 3) {
		return "is three"
	}
	console.log("hi")
	return "another"
}

loop satisfies (a: number) => "is three" | "another";

function loop(value: number) {
	for (let i = 0; i < 10; i++) {
		if (value === i) {
			return "something"
		}
	}
	return "another"
}

loop satisfies (a: number) => "something" | "another";

function sometimes(a: boolean) {
	if (a) {
		return "sometimes"
	}
}

sometimes satisfies (a: boolean) => string;
```

- Expected (a: boolean) => string, found (a: boolean) => "sometimes" | undefined

#### `throw` in body

```ts
function throwSomething() {
	throw "to implement!"
}

throwSomething satisfies string;
```

- Expected string, found () => never

> #TODO try-catch, Promise

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

#### Calling `new` on a function

```ts
function MyClass(value) {
	this.value = value
}

new MyClass("hi").value satisfies "hello"
```

- Expected "hello", found "hi"

#### `new` on function prototype

```ts
function MyClass(value) {
	this.value = value
}

MyClass.prototype.other = 2;

const object = new MyClass("hi");
object.value satisfies "hi";
object.other satisfies "hello";
```

- Expected "hello", found 2

#### Checking with function prototype

```ts
function MyClass(this: { other: string }) { }

MyClass.prototype.other = 2;

const m = new MyClass();
```

- The 'this' context of the function is expected to be { other: string }, found { other: 2 }

#### Arguments in to rest parameter

```ts
function myRestFunction(...r: string[]) {
	return r
}

myRestFunction("hello ", "world") satisfies number;
```

- Expected number, found ["hello ", "world"]

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
function doThing(a: number = (b += 2)) {
	return a
}

doThing(7);
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

#### Default parameter side effect on parameter

```ts
function doThing(a: number, b: number = (a += 2)) {
	return a
}

doThing(3) satisfies 2;
doThing(6, 1) satisfies 6;
```

- Expected 2, found 5

#### Canceled generics

> aka remove generic arguments if they don't match the structure

```ts
declare function func<T>(prop: { a: number, b: T, c: string } | { a: number, b: string, c: T }): T;

func({ a: 3, b: "hi", c: false }) satisfies string;
```

- Expected string, found false

#### Template literal calling error

```ts
function myTag(static_parts: Array<string>, count: number) {
}

myTag`Count is ${"not a number!!"}`;
```

- Argument of type \"not a number!!\" is not assignable to parameter of type number (in template literal)

#### Generics on dependent type

> Weird annotation here is to work around [#165](https://github.com/kaleidawave/ezno/issues/165)

```ts
function createNew(cb: { f<T>(t: T): { a: T }}["f"]) {
	return cb(4)
}

createNew satisfies string;
```

- Expected string, found (cb: \<T\>(t: T) => { a: T }) => { a: 4 }

#### Builder pattern

> Testing for `this` returning

```ts
class StringBuilder {
	s: string = ""

	append(s: string) {
	this.s += s;
	return this
	}

	finish() {
	return this.s
	}
}

(new StringBuilder).append("Hello ").append("Ben").finish() satisfies number
```

- Expected number, found "Hello Ben"

#### Dependent operations

```ts
function isFive(a: number): boolean {
	return a === 5
}

isFive(5) satisfies true;
isFive(6) satisfies string;

function hasPropertyX(obj: object): boolean {
	return "x" in obj;
}

hasPropertyX({ a: 2 }) satisfies false;
hasPropertyX({ x: 5 }) satisfies number;
```

- Expected string, found false
- Expected number, found true

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

#### TDZ errors through nested getter

```ts
function func(obj: { prop: number }) {
	return obj.prop
}

func({ get prop() { return b } });

let b: number = 0;
```

- Variable 'b' used before declaration

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

- Invalid assignment through parameter

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

#### Unconditional throw warning

```ts
function safeDivide(num: number, denom: number) {
	if (denom === 0) {
		throw new Error("Cannot divide by zero");
	}
	return num / denom
}

function func() {
	safeDivide(8, 4) satisfies 2;
	// ahh
	safeDivide(10, 0);
}
```

- Conditional '[Error] { message: \"Cannot divide by zero\" }' was thrown in function

#### Unreachable statement

```ts
function throwGreeting() {
	throw "Hello";
	return "Unreachable!"
}

function doSomething() {
	throwGreeting()
	const unreachable = 2;
}
```

> One is for `return 5` the other is for `const x = 2;`

- Unreachable statement
- Unreachable statement

#### `throw` short-circuit

```ts
let x: number = 2;

function func(cb: () => void) {
	try {
		cb();
		x = 10;
		return "not-thrown"
	} catch {
		return "thrown"
	}
}

func(() => { throw "error" }) satisfies "thrown";
x satisfies string;
```

- Expected string, found 2

#### `delete` as an effect

```ts
function dewete(param: { prop?: string }) {
	const { prop } = param;
	delete param.prop;
	return prop
}

const obj = { prop: "hi" };
dewete(obj);

obj.prop;
```

> This error *should* be from the last statement

- No property 'prop' on {}

#### Optional effect key

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

#### Effects across functions

```ts
let value: number = 2;
function a() { value = 8; }
function b() { a() }

let func = () => {};

function c() { b() }
function d(newCb: () => void, then: () => void) { func = newCb; then() }

value satisfies 2;
d(a, c);
value satisfies boolean;
```

- Expected boolean, found 8

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

#### Class constructors

```ts
function func(a: number, b: number) {
	return class {
		value: number;

		constructor() {
			this.value = a;
		}

		plusB() {
			return this.value + b
		}
	}
}

const c1 = new (func(1, 2));
c1.plusB() satisfies 3;

const c2 = new (func(6, 8));
c2.plusB() satisfies string;
```

- Expected string, found 14

#### Getters closures

```ts
function Closure(n: string) {
	return { get value() { return n }, set value(newValue: string) { n = newValue;	} };
}

let b = Closure("hi");
b.value = "something";
b.value satisfies number;
```

- Expected number, found "something"

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
let a: number = 1, i: number = 0;
while (i < 5) {
	a *= 2;
	i++;
}

a satisfies 8;
```

- Expected 8, found 32

#### While loop event in the condition

```ts
let a: number = 1, i: number = 0;
while (i++ < 5) {
	a *= 2;
}

a satisfies 8;
```

- Expected 8, found 32

#### Do while loop

```ts
let a: number = 0;
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
	let i: number = 1;
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
let a: number = 2, i: number = 0;
while (i++ < 10) {
	a *= 2;
	if (a > 5) {
		break;
	}
}

a satisfies 2;
```

- Expected 2, found 8

#### Continue in a while loop

> With the continue the update to `a` only happens on even runs (5 times)

```ts
let a: number = 2, i: number = 0;
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

### Exceptions and `try-catch-finally`

#### Try-catch and throw

```ts
try {
	throw 2
} catch (err) {
	err satisfies string
}

console.log("Error caught!")
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

console.log("Error caught!")
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

console.log("Error caught!")
```

- Cannot catch type string because the try block throws 3

#### Function effect

```ts
function exceptionToResult(cb: () => number) {
	try {
	return cb()
	} catch (e) {
	return e
	}
}

exceptionToResult(() => 6) satisfies 6;
exceptionToResult(() => { throw 12 }) satisfies 8;
console.log("Error caught!")
```

- Expected 8, found 12

#### Checked thrown type from callback

```ts
function exceptionToResult(cb: () => number) {
	try {
	cb()
	} catch (e: number) {
	return e
	}
}

exceptionToResult(() => { throw "not a number" });
console.log("Error caught!")
```

- Cannot throw "not a number" in block that expects number

#### Internal function effect

```ts
function exceptionToResult(s: string) {
	try {
	return JSON.parse(s)
	} catch (e: number) {
	return e
	}
}
console.log("Error caught!")
```

- Cannot catch type number because the try block throws SyntaxError

#### Conditional throw

> This emits a warning if a throw was created in a conditional branch

```ts
// no complex numbers :(
function checkedLn(x: number) {
	if (x > 0) {
	return Math.log(x)
	} else {
	throw new Error("Cannot log")
	}
}

// Fine
try { checkedLn(Math.E ** 3) satisfies 3 } catch {}
// Will throw
try { checkedLn(-5) } catch {}
```

- Conditional '[Error] { message: \"Cannot log\" }' was thrown in function

#### Throw through internal callback

```ts
try {
	[1, 2, 3].map((x: number) => {
		if (x === 2) {
			throw "error"
		}
	});
	console.log("unreachable")
} catch (e) {
	e satisfies number;
}
```

- Conditional '"error"' was thrown in function
- Unreachable statement
- Expected number, found "error"

### Collections

> Some of these are built of exiting features.
> But they are important enough in code for them to have their own usage tests

#### Array push

```ts
const x = [1];
x.push("hi");
x[1] satisfies 3;
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

#### Array map

> TODO other arguments (index and `this`)

```ts
[6, 8, 10].map(x => x + 1) satisfies [7, 8, 11];
```

- Expected [7, 8, 11], found [7, 9, 11]

#### Mutation

> This is part of [assignment mismatch](https://github.com/kaleidawave/ezno/issues/18)

```ts
function fakeRead(a: Array<string | number>) {
	a.push(2)
}

const array1: Array<string> = []
fakeRead(array1)
```

- Invalid assignment through parameter

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

#### Non-null assertions

> TODO this currently only works on conditionals

```ts
declare const global: { property?: string };

global.property satisfies string | undefined;
global.property! satisfies number;
```

- Expected number, found string

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

#### Object destructuring assignment

> Added in #127

```ts
const o = { a: 1, b: { c: 3 } };

let a, b, d;
({
	d = o.a++,
	b: { c: b = 7 },
	a,
} = o);

a satisfies string;
b satisfies boolean;
d satisfies 3;
```

- Expected string, found 2
- Expected boolean, found 3
- Expected 3, found 1

#### `import.meta`

> Unfortunately because of bundling `url` and `resolve` cannot have known results so just `string`

```ts
import.meta.url satisfies number;
import.meta.resolve("./lib/helper.js") satisfies string;

import.meta.env.production satisfies boolean;
```

- Expected number, found string
- Expected boolean, found string | undefined

#### `instanceof` operator

> TODO dependent version

```ts
([] instanceof Array) satisfies true;
({} instanceof Map) satisfies 4;

class X {}

(new X instanceof X) satisfies true;
([] instanceof X) satisfies false;
```

- Expected 4, found false

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

#### Called without new

> Declared same as `let` and `const`

```ts
class X { }
const x = X();
```

- Class constructor must be called with new

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

#### `super` call

```ts
let b: number = 0;
class Y {
	constructor(a) {
	this.a = a;
	b++;
	}
}

class X extends Y {
	constructor(a) {
	super(a);
	}
}

const x = new X("hi");
x.a satisfies "hello";
b satisfies 1;
```

- Expected "hello", found "hi"

#### Nominal-ness

```ts
class X { a: number = 2 }
class Y { a: number = 2}

function doThingWithX(x: X) {}

doThingWithX(new X());
doThingWithX(new Y())
```

- Argument of type [Y] { a: 2 } is not assignable to parameter of type X

### Types

#### Non existent type

```ts
type X = number;
const a: Y = 2;
```

- Could not find type 'Y'

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
function getProp(obj: { prop: 3, prop2: 6 } | { prop: 2 }) {
	obj.prop2;
	return obj.prop
}

getProp satisfies string
```

- No property 'prop2' on { prop: 3, prop2: 6 } | { prop: 2 }
- Expected string, found (obj: { prop: 3, prop2: 6 } | { prop: 2 }) => 3 | 2

#### Generic extends

```ts
function getA<T extends { a: string }>(p: T) {
	return p.a
}

getA({ a: 2 })
```

- Argument of type { a: 2 } is not assignable to parameter of type T

> I think reasons contains more information for the T parameter

#### Function parameter subtyping

```ts
// Perfectly fine
const x: (a: number) => string = (p: string | number) => "hi"
// Bad
const y: (a: number | string) => string = (p: number) => "hi"
```

- Type (p: number) => "hi" is not assignable to type (a: number | string) => string

> I think reasons contains more information

#### Function parameter excess allowed

```ts
// Perfectly fine
const x: (a: number, b: string) => string = (p: number) => "hi"
// Bad
const y: (a: string) => string = (p: number, q: string) => "hi"
```

- Type (p: number, q: string) => "hi" is not assignable to type (a: string) => string

> I think reasons contains more information

#### Function return type subtyping

```ts
const x: (a: number) => number = p => 4;
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

> This is not TS behavior:

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

#### Cyclic type alias

```ts
type Node<T> = { parent: Node<T>, value: T } | null;

null satisfies Node<number>;
({ parent: { parent: { parent: null, value: 2 }, value: 6 }, value: 2 } satisfies Node<number>);
({ parent: { parent: { parent: null, value: "hi" }, value: 6 }, value: "hi" } satisfies Node<string>);
```

- Expected { parent: Node\<string>, value: string } | null, found { parent: { parent: { parent: null, value: "hi" }, value: 6 }, value: "hi" }

#### Cyclic type alias check

```ts
type X = Y;
type Y = X;

// test usage doesn't blow up subtyping
const x: X = 2;
```

- Circular type reference

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

#### `never` subtyping

```ts
function getSpecialNumber(): number {
	throw "to implement!"
}

getSpecialNumber satisfies string;
```

- Expected string, found () => number

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

#### Union with never

```ts
declare function func<T>(): T | string;

func<number>() satisfies string | number;
func<never>() satisfies boolean;
```

- Expected boolean, found string

#### Infer and extends distribution

```ts
type ElementOf<T> = T extends Array<infer U> ? U : never;

declare let elementOfNumber: ElementOf<Array<number>>;
declare let elementOfNumberOrString: ElementOf<"not array" | Array<number>>;

elementOfNumber satisfies number;
elementOfNumberOrString satisfies string;

declare let n: never;
n satisfies ElementOf<"not array">;
```

- Expected string, found number

#### `keyof` type annotation

```ts
interface X {
	a: string,
	b: string
}

"a" satisfies keyof X; "b" satisfies keyof X; "c" satisfies keyof X;
```

- Expected keyof X, found "c"

#### Template literal types

```ts
type Introduction = `Hello ${string}`;

const first: Introduction = "Hello Ben";
const second: Introduction = "Hi Ben";
```

- Type "Hi Ben" is not assignable to type Introduction

#### Assigning to types as keys

```ts
const obj = { a: 1, b: 2, c: 3 };
obj satisfies { [s: string]: number };
obj satisfies { [s: string]: boolean };
```

- Expected { [string]: boolean }, found { a: 1, b: 2, c: 3 }

#### String slice matching pattern

```ts
type GetPrefix<S, End> = S extends `${infer T} ${End}` ? T : false;

4 satisfies GetPrefix<"Hello Ben", "Ben">;
```

- Expected "Hello", found 4

#### `infer ... extends ...`

```ts
type X<T> = T extends { a: infer I extends string } ? I : string;

declare let a: X<{ a: 4 }>;
declare let b: X<{ a: "hello" }>;

a satisfies number;
b satisfies "hello";
```

- Expected number, found string

#### TSC string intrinsics

```ts
const a: Uppercase<"something" |"hi"> = "HI";
const b: Uppercase<string> = "hi"
```

- Type \"hi\" is not assignable to type Uppercase\<string\>

#### `NoInfer`

```ts
declare function func<T>(a: T, b: NoInfer<T>): T;

func("hi", "hello") satisfies number;
```

> but not `| "hello"` !!!

- Expected number, found "hi"

#### Subtyping edge cases

```ts
"hi" satisfies { length: 3 };
"hi" satisfies { length: 2 };
(() => {}) satisfies Function;
```

- Expected { length: 3 }, found "hi"

### Generic types

#### Generic interface

```ts
interface Wrapper<T> {
	internal: T
}

({ internal: "hi" } satisfies Wrapper<number>);
({ internal: "hi" } satisfies Wrapper<string>);
```

- Expected Wrapper\<number>, found { internal: \"hi\" }

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

#### Excess generic arguments

```ts
declare function generic<T>(a: T);

generic<string, number>("something");
```

- Expected 1 type argument, but got 2

#### Passing generic type to non-generic function

```ts
declare function func();

func<string>();
```

- Cannot pass a type argument to a non-generic function

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

> Only works for `Record` atm. And while defined in the root environment, want to test the definition here as well

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
const x: Record<"test", boolean> = { no: false },
		y: Record<"test", boolean> = { test: 6 },
		z: Record<"test", boolean> = { test: false };
```

- 'no' is not a property of { [\"test\"]: boolean }
- Type { no: false } is not assignable to type { [\"test\"]: boolean }
- Type { test: 6 } is not assignable to type { [\"test\"]: boolean }

#### Union and types as keys

```ts
declare let obj1: Record<"hi" | "hello", boolean>;

obj1.hi satisfies boolean;
obj1.hello satisfies boolean;

obj1.bye;
```

- No property 'bye' on { ["hi" | "hello"]: boolean }

#### Readonly and optionality carries through

```ts
type Mapped<T> = {
	[P in keyof T]: T[P]
}

interface Y { readonly a: string, b?: number }
declare let x: Mapped<Y>;

x.a = "hi";
x.b satisfies number;
```

- Cannot write to property 'a'
- Expected number, found number | undefined

#### Specialisation

```ts
type Pick<T, K extends keyof T> = {
	[P in K]: T[P];
};

interface X { a: number, b: string, c: string }

({ a: 5 } satisfies Pick<X, "a">);
({ a: 5 } satisfies Pick<X, "a" | "b">);
({ a: 5, b: "hi" } satisfies Pick<X, "a" | "b">);
({ a: 5, b: 6 } satisfies Pick<X, "a" | "b">);

declare let y: Pick<X, "b" | "c">;
y.b satisfies string;
y.a;
```

- Expected { ["a" | "b"]: X["a" | "b"] }, found { a: 5 }
- Expected { ["a" | "b"]: X["a" | "b"] }, found { a: 5, b: 6 }
- No property 'a' on { ["b" | "c"]: X["b" | "c"] }

#### Optional

```ts
type Partial<T> = {
	[P in keyof T]?: T[P];
};

({ a: 3 } satisfies Partial<{ a: number, b: string }>);
({ a: "hi" } satisfies Partial<{ a: number, b: string }>)
```

- Expected { [keyof { a: number, b: string }]?: { a: number, b: string }[keyof { a: number, b: string }] }, found { a: \"hi\" }

#### Negated optionality

```ts
type Required<T> = {
	[P in keyof T]-?: T[P];
};

({ a: 3 } satisfies Required<{ a?: number }>);
// Bad
({ } satisfies Required<{ a?: number }>);
```

- Expected { [keyof { a?: number }]: { a?: number }[keyof { a?: number }] }, found {}

#### Readonly

```ts
type Mutable<T> = {
	readonly [P in keyof T]: T[P];
};

interface Y { a: string }
declare let x: Mutable<Y>;
x.a = "hi";
```

- Cannot write to property 'a'

#### Negated readonly

```ts
type Mutable<T> = {
	-readonly [P in keyof T]: T[P];
};

interface Y { readonly a: string }
declare let x: Mutable<Y>;
x.a = "hi";
x.a = 4;
```

- Type 4 does not meet property constraint "hi"

#### `as` rewrite

```ts
type PrefixKeys<T> = {
	[P in ((keyof T) & string) as `property_${P}`]: T[P];
};

interface X { a: number };

declare let x: PrefixKeys<X>;
x.property_a satisfies number;
x.property_b
```

- No property 'property_b' on { [string]: X[keyof X & string] }

### Readonly and `as const`

> TODO constrained inference

#### Readonly parameter

```ts
function x(p: readonly { a: string }) {
	p.a = "hi";
}
```

- Cannot write to property 'a'

#### Readonly to readonly

```ts
function func1(p: { a: string, b: string }) {
	func2(p)
}
function func2(p: readonly { a: string }) { }

const obj = Object.freeze({ a: "hi" });
func2(obj)
```

- Argument of type { a: string, b: string } is not assignable to parameter of type Readonly<{ a: string }>

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

### `Proxy`

#### Proxy get

```ts
const proxy1 = new Proxy({ a: 2 }, { get(target: { a: number }, prop: string, receiver) {
	if (prop === "a") {
		return target["a"] + 1
	}
} } );

proxy1.a satisfies string;
```

- Expected string, found 3

#### Proxy set

```ts
let lastSet: string = "";
const proxy1 = new Proxy({ a: 2 }, { 
	set(target: { a: number }, prop: string, value: number, receiver) {
		lastSet = prop;
	} 
});

proxy1.a = 6;
lastSet satisfies boolean;
```

- Expected boolean, found "a"

#### Proxy handler fallthrough

```ts
const obj = { a: 2 };
const proxy1 = new Proxy(obj, {	});

proxy1.a = 6;
obj.a satisfies 6;
proxy1.a satisfies string;
```

- Expected string, found 6

#### Proxy subtyping

```ts
const proxy1 = new Proxy({}, { get(_target, prop, _recivier) { return prop } });

proxy1 satisfies { a: "a", b: "b" };
proxy1 satisfies { c: "d" };
```

- Expected { c: "d" }, found Proxy [ {}, { get: (_target: any, prop: any, _recivier: any) => any } ]

#### Proxy across functions

```ts
function makeObservable(obj, cb: (kind: string, prop: string, value: any) => void) {
	return new Proxy(obj, {
		get(on, prop: string, _rec) {
			cb("get", prop, on[prop])
		},
		set(on, prop: string, _value, _rec) {
			cb("set", prop, on[prop])
		},
	})
}

let r = null;
const value = makeObservable({ a: 1 }, (k, p, v) => {
	r = { k, p, v };
});

r satisfies null;
value.a = 2;
r satisfies string;
```

- Expected string, found { k: "set", p: "a", v: 1 }

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
export let counter: number = 2;
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

- Cannot find ./two

#### Import where export does not exist

```ts
import b, { a } from "./export";

console.log(a.prop);

// in export.ts
export const c = 2;
```

- Cannot find default export from module './export'
- a not exported from ./export

#### Import conflicts with existing name

```ts
import { x } from "./export1";
import x, { z } from "./export2";

// in export1.ts
export const x = 1;

// in export2.ts
const y = 2;

export default y;
export const z = 2;
```

- Cannot import using conflicting name

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

### Stability

> What to do in the occurance of a type error

#### Use type annotation in the presence of error

> Note x and y are still string and the function still returns string

```ts
const x: string = 5;
const y: string = h;

function getString(a: number): string {
	return a
}

x satisfies string;
y satisfies string;

const z: number = getString(2);
```

- Cannot return number because the function is expected to return string
- Type 5 is not assignable to type string
- Could not find variable 'h' in scope
- Type (error) string is not assignable to type number

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

#### Number intrinsics

```ts
5 satisfies MultipleOf<2>;
4 satisfies MultipleOf<2>;

6 satisfies GreaterThan<2>;
-4 satisfies GreaterThan<2>;

6 satisfies LessThan<2>;
-4 satisfies LessThan<2>;
```

- Expected MultipleOf\<2\>, found 5
- Expected GreaterThan\<2\>, found -4
- Expected LessThan\<2\>, found 6

#### `Not`

```ts
declare let a: number;
4 satisfies Not<4>;
6 satisfies Not<4>;
a satisfies Not<8>;
2 satisfies Not<string>;
"hi" satisfies Not<string>;

declare let b: Not<5> & number;
b satisfies number;
b satisfies string;
b satisfies 5;
```

- Expected Not<4>, found 4
- Expected Not<8>, found number
- Expected Not\<string\>, found "hi"
- Expected string, found Not<5> & number
- Expected 5, found Not<5> & number

#### `Exclusive`

```ts
interface X { a: number }
const x = { a: 1, b: 2 };

x satisfies Exclusive<X>;
({ a: 6 } satisfies Exclusive<X>);
```

- Expected Exclusive\<X\>, found { a: 1, b: 2 }

#### `CaseInsensitive`

```ts
"Hi" satisfies CaseInsensitive<"hi">;
"Hello" satisfies CaseInsensitive<"hi">;

// yeah
type CIWord = "WORD" extends Uppercase<infer T> ? T : never;
"wOrd" satisfies CIWord;
"wood" satisfies CIWord;
```

- Expected CaseInsensitive<"hi">, found "Hello"
- Expected CIWord, found "wood"
