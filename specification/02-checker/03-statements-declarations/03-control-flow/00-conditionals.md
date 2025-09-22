- Checking dead branches
- Narrowing
- Check each branch
- Unify environment state

### Conditional

```ts
function func<T extends boolean>(condition: T) {
	if (condition) {
		return 4
	} else {
		return 3
	}
}

func satisfies string;
```

> There are some issues around printing here, when to include the generic etc

- Expected string, found \<T>(condition: T) => T ? 4 : 3

### Resolving conditional

```ts
function isNegative(x: number) {
	return x < 0 ? "negative" : "positive"
}
isNegative(-4) satisfies number
isNegative(4) satisfies boolean
```

- Expected number, found "negative"
- Expected boolean, found "positive"

### *Conclusive* conditional update

```ts
let a: number = 0
function conditional(v: string) {
	if (v === "value") {
		a++
	}
}

conditional("x")
a satisfies 2
conditional("value")
a satisfies 3
```

- Expected 2, found 0
- Expected 3, found 1

### If and else (across function)

```ts
function print_number(value: number) {
	if (value === 0) {
		return "zero"
	} else if (value === 1) {
		return "one"
	} else {
		return "some number"
	}
}

print_number(0) satisfies "zero"
print_number(0) satisfies "some number"
print_number(1) satisfies "ONE"
print_number(100) satisfies "100"
print_number(-1) satisfies "TWO"
```

- Expected "some number", found "zero"
- Expected "ONE", found "one"
- Expected "100", found "some number"
- Expected "TWO", found "some number"

### Unknown condition assignment

```ts
let i = 0;
declare let b: boolean;
if (b) {
	i = 1
} else {
	i = 2
}

i satisfies string;
```

- Expected string, found 1 | 2
