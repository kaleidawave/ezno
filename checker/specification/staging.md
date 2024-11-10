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

- This equality is always false as MultipleOf<10> and LessThan<37> & GreaterThan<31> have no overlap

#### Modulo offsets

```ts
function func(param: Integer) {
  print_type(param - 0.2);
  print_type(param / 2);
  print_type(2 / param);
}

function func2(param: number) {
  print_type((param - 5) + 5);
  print_type((param / 5) * 5);
}
```

With advanced_number_intrinsics

- Hi

#### Narrowing: Implication by

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

- This equality is always false

#### Narrowing in for loop

> Can't do modulo because post mutation

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

### Broken

#### Template literal edge cases

```ts
const invalidNum1: `${1}` = 1;
const invalidNum2: `${1}` = "1";
const invalidNum3: `${1}` = "2";
```

- Type 1 is not assignable to type "1"
- Type \"2\" is not assignable to type "1"
