Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Fixes

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

With advanced_number_intrinsics

- This equality is always false as MultipleOf<10> and GreaterThan<31> & LessThan<37> have no overlap

#### Narrowing: Implication from equality

```ts
function func(a: boolean) {
  const x = a ? 1 : 2;
  if (x === 1) {
    a satisfies "hi"
  }
}
```

- Expected "hi", found true

#### Modulo range

```ts
function func(x: number) {
  return x % 5 === 6;
}
```

- This equality is always false as ExclusiveRange<-5, 5> and 6 have no overlap

#### Narrowing in for loop

> Can't do modulo because of post mutation

```ts
for (let i = 0; i < 3; i++) {
  const x = i === 50;
}
```

- This equality is always false as LessThan<3> and 50 have no overlap

#### Transistivity

```ts
function func(a: number, b: number, c: number) {
  if (a < b && b < c)  {
    const cond = (a < c) satisfies 5;
  }
}
```

- Expected 5, found true

### Operators across conditions

```ts
function func(param: boolean) {
    const value = param ? 1 : 2;
    return value + 1;
}

func statisfies string;
```

With advanced_number_intrinsics

- Expected string, found (param: boolean) => 2 | 3

### Broken

#### Template literal edge cases

```ts
const invalidNum1: `${1}` = 1;
const invalidNum2: `${1}` = "1";
const invalidNum3: `${1}` = "2";
```

- Type 1 is not assignable to type "1"
- Type \"2\" is not assignable to type "1"

#### Set prototype of conditional

```ts
const obj = Object.setPrototypeOf(
  {}, 
  Math.random() ? { a: 2 } : { get a() { return 0 } }
);

const result = 'a' in obj;
result satisfies string;
```

- Expected string, found true
