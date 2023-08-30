- This is (automatically) tested against the checker
- Sections at level 3 (`###`), tests at level 4 (`####`), tested code in a `ts` block and errors in a bullet list after in order

## Specification

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

#### Variable exists

```ts
const a = c
```

- Could not find variable c in scope

### Functions

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

- Function is expected to return string but returned 2

#### *Inferred* return type

```ts
function func() {
	return 2
}
func satisfies () => string
```

- Expected () => string, found () => 2

#### Argument type against parameter

```ts
function func(a: number) {}
func("not a number")
```

- Argument of type "not a number" is not assignable to number

#### Parameters are always considered generic

```ts
function id(a) {
	return a
}

const d: 3 = id(2)
```

- Type 2 is not assignable to type 3

#### Type checking function types

```ts
function func(a: string, b: number): boolean {
	return true
}
const a: (a: string, b: number) => boolean = func
const b: (a: string, b: number) => string = func
const c: (a: number, b: number) => boolean = func
```

- Type (a: string, b: number, ) => true is not assignable to type (a: string, b: number, ) => string
- Type (a: string, b: number, ) => true is not assignable to type (a: number, b: number, ) => boolean

#### Get value of property on parameter

```ts
function getA(obj: { a: string }) {
	return obj.a
}

const d: 3 = getA({ a: "hi" })
```

- Type "hi" is not assignable to type 3

#### Assignment to variable in function

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

#### Property updates object outside of function

```ts
const obj: { a: number } = { a: 2 }
function func(value: number) {
	obj.a = value
}

const a: 2 = obj.a
func(4)
const b: 2 = obj.a
```

- Type 4 is not assignable to type 2

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

### Effects

#### Calling and operations with parameter

```ts
function sinPlusB(a: number, b: number) {
	return Math.trunc(a) + b
}

sinPlusB(100.22, 5) satisfies 8
```

- Expected 8, found 105

#### Calling higher order function

```ts
function addTwoToResult(func: number => number) {
	return func(4) + 2
}

addTwoToResult((a: number) => a * 4) satisfies 5
```

- Expected 5, found 18

#### Calling constant higher order function

```ts
function call(func: number => number) {
	return func(9)
}

call(Math.sqrt) satisfies 2
```

- Expected 2, found 3

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

#### Updating property

```ts
const x = { a: 2 }
function updateA(obj: { a: string | number }) {
    obj.a = "hi"
}
updateA(x)
const y: number = x.a
```

- Type "hi" is not assignable to type number

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
(4 < 2) satisfies true;
(4 > 2) satisfies number;
(2 >= 2) satisfies string;
```

- Expected true, found false
- Expected number, found true
- Expected string, found true

#### String operations

```ts
"hi".toUppercase() satisfies number
```

- Expected number, found "HI"

#### Math operations

```ts
Math.cos(0) satisfies 0;
Math.sqrt(16) satisfies 1;
Math.trunc(723.22) satisfies 2
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

- No property with "b" on {"a": 3, }

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

- Type {"a": 2, } is not assignable to type {"b": 3, }

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

#### Arrays

```ts
const x = [1]
x.push("hi")
x[1] satisfies 3
x.length satisfies 4
```

- Expected 3, found "hi"
- Expected 4, found 2

#### Functions create objects

```ts
function newObject() {
	return { a: 2 }
}

const b = newObject();
const c = b;
(b === c) satisfies false;
(b === newObject) satisfies string;
```

- Expected false, found true
- Expected string, found false

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

#### Conditional update

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

- Expected (a: boolean, ) => 5, found (a: boolean, ) => 2 | undefined

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

- Type {"a": 2, "b": false, } is not assignable to type X

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

- Type {"a": "field", "b": false, "c": false, } is not assignable to type X

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
declare var x: number;
declare var y: boolean;
(x && y) satisfies string;
```

- Expected string, found boolean | number

#### Shorthand object literal

```ts
const x = 2
const y = { x }
y.x satisfies 3
```

- Expected 3, found 2

#### Try-catch and throw

```ts
try {
	throw 2
} catch (err) {
	err satisfies string
}
```

- Expected string, found 2

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
