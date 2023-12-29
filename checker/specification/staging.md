Currently implementing:

### Iterations

#### Limit to iterations

```ts
let a: number = 0;
while (a++ < 1_000_000) {}

a satisfies string;
```

> The important part is that it doesn't run the loop. Eventually this might be run in a way that is not calling the assign to variable
> function that evaluates `a = a + 1` a million times. There also should be per project, per module, per loop configuration

- Expected string, found number

### Functions

#### Assignment to parameter

```ts
function alterParameter(a: number, b: { prop: string }) {
    a = 2;
    a = "hi";
    b.prop = 6;
}
```

> Assigning straight to `a` might be disallowed by an option in the future. Right now it is allowed by JavaScript and so is allowed

- Type \"hi\" is not assignable to type number

### Statements

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

#### Function return type subtyping

```ts
const x: (a: number) => number = p => 4
const y: (a: number) => number = p => "a number"
```

- Type (p: number) => "a number" is not assignable to type (a: number) => number

#### void return type

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

### Not sure

#### Set property on dependent observed

```ts
function add_property(obj: { prop: number }) {
    obj.prop = 2;
    (obj.prop satisfies 4);
}
```

> Not number

- Expected 4, found 2

### To fix

#### Constant call and operation with a parameter

> An example of the generic constructor type  (namely call and operation)

```ts
function floorPlusB(a: number, b: number) {
	return Math.floor(a) + b
}

floorPlusB(100.22, 5) satisfies 8
```

- Expected 8, found 105
