### Function hoisting

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

### Type hoisting

> Using type references **before** where they are declared in the source

```ts
let x: X = { a: 3 }

interface X {
	a: 2
}
```

- Type { a: 3 } is not assignable to type X
