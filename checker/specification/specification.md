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
const a = c;
```

- Could not find variable c in scope

#### Try catch

```ts
try {
    throw 2
} catch (err) {
    err satisfies string
}
```

- Expected string found 2

#### Getters

```ts
const b = {
    get c() {
        return 2
    }
}
b.c satisfies string
```

- Expected string found 2

### Functions

#### Type of parameter

```ts
function func(a: number) { 
    a satisfies string
}
```

- Expected string found number

#### Return type

```ts
function func(): string { 
    return 2
}
```

- Function is expected to return string but returned 2

#### Argument

```ts
function func(a: number) { }
func("not a number")
```

- Argument of type "not a number" is not assignable to number

#### Automatic generics

```ts
function id(a) { return a }

const d: 3 = id(2)
```

- Type 2 is not assignable to type 3

#### Function as type checking

```ts
function func(a: string, b: number): boolean {
    return true
}
const a: (a: string, b: number) => boolean = func;
const b: (a: string, b: number) => string = func;
const c: (a: number, b: number) => boolean = func;
```

- Type (a: string, b: number, ) => true is not assignable to type (a: string, b: number, ) => string
- Type (a: string, b: number, ) => true is not assignable to type (a: number, b: number, ) => boolean

#### Generics properties

```ts
function getA(obj: { a: string }) { 
    return obj.a 
}

const d: 3 = getA({ a: "hi" })
```

- Type "hi" is not assignable to type 3

#### Variable updates

```ts
let a: number = 0
function func() {
    a = 4
}

func()
let b: 2 = a
```

- Type 4 is not assignable to type 2

#### Variable updates from parameter

```ts
let a: number = 0
function func(c: number) {
    a = c
}

func(4)
let b: 2 = a
```

- Type 4 is not assignable to type 2

#### Property updates from outside

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
function func(p1: number, p2: string) { }

func(4)
```

- Missing argument

#### Excess argument

```ts
function func(p1: number) { }

func(4, "extra")
```

- Excess argument

#### Calling non function

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

- Expected string found 3

### Constant evaluation

#### Arithmetic

```ts
const x: 4 = 2 + 3 
const y: 6 = 2 * 3
const z: 8 = (2 * 3) - 2
```

- Type 5 is not assignable to type 4
- Type 4 is not assignable to type 8

#### Equality

```ts
(4 === 2) satisfies true
```

- Expected true found false

#### String operations

```ts
"hi".toUppercase() satisfies number
```

- Expected number found "HI"

#### Math operations

```ts
Math.cos(0) satisfies 0
```

- Expected 0 found 1

### Objects

#### Property exists

```ts
let my_obj = { a: 3 }
const a = my_obj.a;
const b = my_obj.b;
```

- No property with "b" on {"a": 3,  }

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

- Type {"a": 2,  } is not assignable to type {"b": 3,  }

#### Arrays

```ts
const x = [1]
x.push("hi")
x[1] satisfies 3
x.length satisfies 4
```

- Expected 3 found "hi"
- Expected 4 found 2
