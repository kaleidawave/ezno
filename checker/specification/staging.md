Currently implementing:

### Things

#### Limit to iterations

```ts
let a: number = 0;
while (a++ < 1_000_000) {}

a satisfies string;
```

> The important part is that it doesn't run the loop. Eventually this might be run in a way that is not calling the assign to variable
> function that evaluates `a = a + 1` a million times. There also should be per project, per module, per loop configuration

- Expected string, found number

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

#### Generic type argument restriction

```ts
function map<T, U>(a: T, b: (t: T) => U) {
	return b(a)
}

map(2, Math.sin)
map("string", Math.sin)
```

- Argument of type "string" is not assignable to parameter of type number

> Because `Math.sin` set T to number

#### Set property on dependent observed

```ts
function add_property(obj: { prop: number }) {
    obj.prop = 2;
    (obj.prop satisfies 4);
}
```

> Not number

- Expected 4, found 2

#### Calling on or type

```ts
type Func1 = () => 3;
type Func2 = () => 2;
function callFunc<T, U>(func: (() => T) | (() => U)): 3 | 2 {
	return func()
}

print_type(callFunc)
```

- Expected "a" | "b" | "c" found "d"

#### This as generic argument

> Was working, now broken for some reason :(

```ts
function callToUpperCase(s: string) {
	return s.toUpperCase()
}

(callToUpperCase("hi") satisfies "HEY")
```

- Expected "HEY", found "HI"
