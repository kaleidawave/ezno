Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Iteration

#### Destructuring with iterating

```ts
const [x, y, z] = {
    [Symbols.iterator]() {
        let i = 0;
        return {
            next() {
                if (i++ === 4) return { done: true, value: i };
                return { done: false, value: i };
            },
            return() {
                return { done: true };
            },
        };
    }
};

({ x, y, z }) satisfies string;
```

- Expected string, found { x: 1, y: 2, z: 3 }

### Collections

#### Map `get` and `set`

```ts
const numberMap = new Map();
numberMap.set(1, 2);
numberMap.get(1) satisfies 2;
numberMap.get(3) satisfies string;
```

- Expected string, found undefined

### Control flow

#### Conditional break

```ts
function getNumber(a: number) {
	for (let i = 0; i < 10; i++) {
		if (i === a) {
			return "found"
		}
	}
	return "not-found"
}

getNumber(4) satisfies "found";
getNumber(100) satisfies boolean;
```

- Expected boolean, found "not-found"

#### *Inconclusive* conditional update

```ts
declare var value: string;
let a: string | number = 0;

function conditional(v: string) {
	if (v === "value") {
		a = "hi"
	}
}
conditional(value);
a satisfies string;
```

- Expected string, found "hi" | 0

#### Break with label

> Note the numbers here, if they are larger they break over the `max_inline` limit and get different results below

```ts
let a: number = 0;
let result;

top: while (a++ < 8) {
	let b: number = 0;
	while (b++ < 8) {
		if (a === 3 && b === 2) {
			result = a * b;
			break top
		}
	}
}

a satisfies string;
result satisfies boolean;
```

- Expected string, found 3
- Expected boolean, found 6
