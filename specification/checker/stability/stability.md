All these examples find errors, it is important that after an error the type checker is still effective. We do not want to show false positives or miss out additional errors.

> A checker that shows one error at a time would be very frustrating to use

#### Use type annotation in the presence of error

> Note x and y are still string and the function still returns string

```ts
const x: string = 5;
const y: string = h;

function getString(a: number): string {
	return a
}

x satisfies string;
y satisfies string;

const z: number = getString(2);
```

- Cannot return number because the function is expected to return string
- Type 5 is not assignable to type string
- Could not find variable 'h' in scope
- Type string is not assignable to type number

#### Errors carries

> Note only one error raised. This prevents the compiler presenting loads of errors if an origin is invalid

```ts
const obj = { prop: 2 };
console.log(obj.a.b.c);

function x() {
	return y
}

x().nothing
```

- Could not find variable 'y' in scope
- No property 'a' on { prop: 2 }
