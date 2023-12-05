Currently implementing:

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

#### While loop dependent

```ts
declare let i: number;
let a = 0;
while (i < 5) {
    i--;
    a++;
}

(a satisfies string)
```

- Expected string, found number

> Important that type is widened to 'number' (think it is an open poly in this case)

#### While loop unrolling across function

```ts
let a = 2;

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
