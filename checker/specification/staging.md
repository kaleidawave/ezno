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
function func(a: number, b: number) {
  if (a % 15 === 0 && 31 < b && b < 37) {
    print_type(a, b)
    print_type(a === b)
  }
}
```

- ?
