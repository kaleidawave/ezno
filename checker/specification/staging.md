Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Constraint inference

> Aka backwards inference

#### From argument

```ts
function cos(a) {
    return Math.cos(a)
}

cos satisfies (a: number) => number;
cos(2);
cos("hi")
```

- Argument of type "hi" is not assignable to parameter of type number

#### Property access

```ts
function printName(a) {
    return a.name
}

printName satisfies string;
```

- Expected string, found (a: { name: any }) => any

#### Function call

```ts
function printName(a) {
    return a(1, 2)
}

printName satisfies string;
```

- Expected string, found (a: (a: 1, b: 2) => any) => any
