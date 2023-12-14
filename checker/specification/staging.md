Currently implementing:

### Loops

#### While loop unrolling

```ts
let a = 2;
let i = 0;
while (i < 5) {
    a *= 2;
    i++;
}

(a satisfies 8);
```

- Expected 8, found 64

#### While loop condition effects

```ts
let a = 2;
let i = 0;
while (i++ < 5) {
    a *= 2;
}

(a satisfies 8);
```

- Expected 8, found 64

#### Do while loop

```ts
let a = 0;
do {
    a++
} while (a < 3)

(a satisfies 8);
```

- Expected 8, found 3

#### For loop (conditional)

```ts
let a: string = "";
for (let i: number = 0; i < 10; i++) {
    a = a + i;
}

(a satisfies number)
```

- Expected number, found "0123456789"

#### While loop unknown iterations

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

#### While loop unrolling across function

```ts
let a: number = 2;

function loop(i: number) {
    while (i < 5) {
        a *= 2;
        i++;
    }
}

loop(1);

(a satisfies 8);
```

- Expected 8, found 32

#### While loop break

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

### Events

#### TDZ in a function

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

> Error is a little different here

- Assignment mismatch

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
