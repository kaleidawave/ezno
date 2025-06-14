#### Function hoisting

> `getString` can be used and has a type before it has been synthesised
> TODO actual calling before defined (this currently only works bc of free-variables)

```ts
function x() {
	getString(3)
}

function y() {
	getString("something") satisfies string;
}

function getString(param: string): string {
	return "hi"
}
```

- Argument of type 3 is not assignable to parameter of type string

#### Function (and interface) hoisting

> Using functions and interface **before** their position of declaration in the source

```ts
getFive() satisfies 4;

function getFive() {
	return 5
}

let x: X = { a: 3 }

interface X {
	a: 2
}
```

- Expected 4, found 5
- Type { a: 3 } is not assignable to type X
