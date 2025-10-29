Iteration is an event

### While loop unrolling

```ts
let a: number = 1, i: number = 0;
while (i < 5) {
	a *= 2;
	i++;
}

a satisfies 8;
```

- Expected 8, found 32

### While loop event in the condition

```ts
let a: number = 1, i: number = 0;
while (i++ < 5) {
	a *= 2;
}

a satisfies 8;
```

- Expected 8, found 32

### Do while loop

```ts
let a: number = 0;
do {
	a++
} while (a < 3)

a satisfies 8;
```

- Expected 8, found 3

### For loop with initialiser and condition

```ts
let a: string = "";
for (let i: number = 0; i < 10; i++) {
	a = a + i;
}

a satisfies number;
```

- Expected number, found "0123456789"

### While loop with unknown number of iterations

```ts
declare let i: number;
let a: number = 0;
while (a < i) {
	a++;
}

a satisfies string;
```

- Expected string, found number

> Important that type is widened to 'number' (think it is an open poly in this case)

### Limit to iterations

> #TODO link to stability

```ts
let a: number = 0;
while (a++ < 1_000_000) {}

a satisfies string;
```

> The important part is that it doesn't run the loop. Eventually this might be run in a way that is not calling the assign to variable
> function that evaluates `a = a + 1` a million times. There also should be per project, per module, per loop configuration

- Expected string, found number

### While loop unrolling as a side-effect

```ts
function loop(n: number, c: string) {
	let a: string = c;
	let i: number = 1;
	while (i++ < n) {
		a += c
	}
	return a
}

loop(10, "!") satisfies number;
```

- Expected number, found "!!!!!!!!!!"

### Break in a while loop

```ts
let a: number = 2, i: number = 0;
while (i++ < 10) {
	a *= 2;
	if (a > 5) {
		break;
	}
}

a satisfies 2;
```

- Expected 2, found 8

### Continue in a while loop

> With the continue the update to `a` only happens on even runs (5 times)

```ts
let a: number = 2, i: number = 0;
while (i++ < 10) {
	if (i % 2) {
		continue;
	}
	a *= 2;
}

a satisfies 2;
```

- Expected 2, found 64
