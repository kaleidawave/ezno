- Return type checking (+ #TODO link for guards)
- Generics
- Early return (inference)
- throw

### (simple) return type checking

```ts
function func(): string {
	return 2
}
```

- Cannot return 2 because the function is expected to return string

### Return generics mismatch

```ts
function getSecond1<T, U>(p1: T, p2: U): U {
	return p1
}

function getSecond2<T, U>(p1: T, p2: U): U {
	return p2
}
```

- Cannot return T because the function is expected to return U

### Return type annotation is used in constraint

> While could use the returned type (as done in the second example). Using the annotation prevents other code breaking if the body changes
> As shown later, this doesn't affect what is returned if called

```ts
function getNumber1(): number {
	return 4
}

function getNumber2() {
	return 6
}

getNumber1 satisfies () => 4;
getNumber2 satisfies () => 6;

getNumber1() satisfies 4;
getNumber2() satisfies 6;
```

- Expected () => 4, found () => number

### Function return type subtyping

```ts
const x: (a: number) => number = p => 4;
const y: (a: number) => number = p => "a number"
```

- Type (p: number) => "a number" is not assignable to type (a: number) => number
