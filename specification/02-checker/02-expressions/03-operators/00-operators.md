#### Checking

#TODO

#### Output

#TODO

- constant mathematics & NaN
- typeof etc

#### Constant compilation

#TODO

> #TODO-link to instrisics

#### Relations - disjoint

#TODO

> Uses disjoint typing #TODO-link

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
(4 !== 5) satisfies string;
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

#### Inequality checks

```ts
function func1(a: GreaterThan<4>) {
	(a > 3) satisfies true;
	(a < 3) satisfies false;
	(a < 10) satisfies string;
}

// thanks narrowing 🙏
function func2(a: number) {
	if (a < 2) {
		(a > 6) satisfies false;
		(a < 7) satisfies true;
		(a > 0) satisfies null;
	}
}
```

With advanced_numbers

- This equality is always false as GreaterThan<4> and 3 have no overlap
- Expected string, found boolean
- This equality is always false as LessThan<2> and 6 have no overlap
- Expected null, found boolean

#### Arithmetic operand check

> This is allowed under non strict casts option (and will return NaN) but the tests run with strict casts on

> This would need to support [Symbol.toPrimitive] + a bunch of error handling

```ts
console + 2
```

> TODO temp diagnostic

- Cannot Console Add 2

#### Inequality operand check

```ts
function isLessThan(a: number) {
	a < console;
}
```

> TODO temp diagnostic

- Cannot number LessThan Console

#### Unary operand check

```ts
function func(a: number, b: boolean) {
	const x = !a;
	const y = ~b;
	(!b), (~a);
}
```

> TODO temp diagnostic

- Cannot LogicalNot number
- Cannot BitwiseNot boolean

### Disjoint equality

> TODO link to types/disjoint

#### Disjoint equality

```ts
function neverEqual(a: string, b: number) {
	(a === b) satisfies false;
}

function sometimes(a: string | number, b: number) {
	(a === b) satisfies string;
}
```

- This equality is always false as string and number have no overlap
- Expected string, found boolean

#### Disjoint equality for number intrinsics

```ts
declare function getNumberBetweenFive(): InclusiveRange<0, 5> & Integer;

getNumberBetweenFive() === 2;
getNumberBetweenFive() === 2.2;
getNumberBetweenFive() === 7;
```

- This equality is always false as GreaterThan<0> & LessThan<5> & Integer | 0 | 5 and 2.2 have no overlap
- This equality is always false as GreaterThan<0> & LessThan<5> & Integer | 0 | 5 and 7 have no overlap

### Known equality

#### Identity equality

> Can only do it not NaN

```ts
function func(a: string, b: number) {
	(a === a) satisfies string;
	(b === b) satisfies null;
}
```

With advanced_numbers

- Expected string, found true
- Expected null, found boolean

#### Disjoint multiple of with range

> TODO need to redo range to use interesection of less than and greater than

```ts
function func1(a: number, b: number) {
  if (a % 8 === 0 && 31 < b && b < 37) {
    const x = a === b;
  }
  if (a % 10 === 0 && 31 < b && b < 37) {
    const x = a === b;
  }
  if (a % 10 === 0 && 31 < b && b < 41) {
    const x = a === b;
  }
}
```

With advanced_numbers

- This equality is always false as MultipleOf<10> and GreaterThan<31> & LessThan<37> have no overlap

#### Disjoint not

```ts
function func1(param: Not<string>) {
  return "hi" === param;
}

function func2(param: Not<string>) {
  return 4 === param;
}

function func3(p1: Not<string>, p2: Not<number>) {
  return p1 === p2;
}
```

- This equality is always false as "hi" and Not\<string> have no overlap

### Constant results

#### NaN

```ts
const value: string = 0 / 0 + 1;
```

- Type NaN is not assignable to type string

#### Always known math

```ts
// True regardless of
function func(a: number) { return a ** 0 }

func satisfies string;

declare let x: NotNotANumber;
(x ** 1 === x) satisfies true;
```

- Expected string, found (a: number) => 1

### Types

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
(x !== 4) satisfies boolean;
(x > 4) satisfies boolean;
(x >= 4) satisfies boolean;
```

- Expected string, found boolean
- Expected Math, found boolean

#### Type of logical operators

```ts
declare var x: number, y: boolean;
(x && y) satisfies string;
```

- Expected string, found boolean | number

### NOT SURE

#### Operators across conditions

```ts
function func(param: boolean) {
    const value = param ? 1 : 2;
    return value + 1;
}

func satisfies string;
```

With advanced_numbers

- Expected string, found (param: boolean) => 2 | 3
