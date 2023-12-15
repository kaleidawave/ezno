Currently implementing:

### Iteration

#### While loop unrolling

```ts
let a = 1;
let i = 0;
while (i < 5) {
    a *= 2;
    i++;
}

(a satisfies 8);
```

- Expected 8, found 32

#### While loop event in the condition

```ts
let a = 1;
let i = 0;
while (i++ < 5) {
    a *= 2;
}

(a satisfies 8);
```

- Expected 8, found 32

#### Do while loop

```ts
let a = 0;
do {
    a++
} while (a < 3)

(a satisfies 8);
```

- Expected 8, found 3

#### For loop with initialiser and condition

```ts
let a: string = "";
for (let i: number = 0; i < 10; i++) {
    a = a + i;
}

(a satisfies number)
```

- Expected number, found "0123456789"

#### While loop with unknown number of iterations

```ts
declare let i: number;
let a: number = 0;
while (a < i) {
    a++;
}

(a satisfies string)
```

- Expected string, found number

> Important that type is widened to 'number' (think it is an open poly in this case)

#### While loop unrolling as an effect

```ts
function loop(n: number, c: string) {
    let a: string = c;
    let i: number = 0;
    while (i++ < n) {
        a += c
    }
    return a
}

(loop(10, "!") satisfies number);
```

- Expected number, found "!!!!!!!!!!"

#### Break in a while loop

```ts
let a = 2;
let i = 0;
while (i++ < 10) {
    a *= 2;
    if (a > 5) {
        break;
    }
}

(a satisfies 2);
```

- Expected 2, found 8

#### Continue in a while loop

> With the continue the update to `a` only happens on even runs (5 times)

```ts
let a = 2;
let i = 0;
while (i++ < 10) {
    if (i % 2) {
        continue;
    }
    a *= 2;
}

(a satisfies 2);
```

- Expected 2, found 64

### Events

#### TDZ from free variable (across function)

```ts
function getX() {
    return x
}

(getX satisfies () => number);

getX();

let x: number = 5;
```

- Variable x used before declaration

#### Assignment to union

> Solves the common subtyping issue between read and write properties

```ts
let myObject: { a: number } = { a: 4 }

function setAtoString(someObject: { a: number | string }) {
    someObject.a = "hi";
}

setAtoString({ a: 6 });
setAtoString(myObject);
```

> Error could be better. Full one contains labels with more information

- Assignment mismatch

#### Mutating an object by a function

> This is where the object loses its constant-ness

```ts
function doThingWithCallback(callback: (obj: { x: number }) => any) {
    const obj: { x: number } = { x: 8 };
    callback(obj);
    (obj.x satisfies 8);
    return obj;
}

const object = doThingWithCallback((obj: { x: number }) => obj.x = 2);
(object.x satisfies string);
```

- Expected 8, found number
- Expected string, found 2

### Statements

#### TDZ in statements

```ts
let x = y;

let y = 2;
```

- Variable y used before declaration

### Types

#### Index into dependent array

```ts
function getFirst(array: number[]) {
    return array[0]
}

(getFirst satisfies boolean);
```

- Expected boolean, found (array: Array\<number>) => number | undefined

#### Index into dependent string

```ts
function getSecondCharacter(s: string) {
    return s[1]
}

(getSecondCharacter satisfies boolean);
(getSecondCharacter("string") satisfies "b");
```

- Expected boolean, found (s: string) => string | undefined
- Expected "b", found "t"

#### Index into string

```ts
("something"[2] satisfies number);
```

- Expected number, found "m"
