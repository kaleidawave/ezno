### Readonly property

```ts
function x(p: { readonly a: string, b: string }) {
	p.a = "hi";
	p.b = "hi";
}
```

- Cannot write to property 'a'

### Readonly to readonly

```ts
function func1(p: { a: string, b: string }) {
	func2(p)
}

function func2(p: readonly { a: string }) { }

const obj = Object.freeze({ a: "hi" });
func2(obj)
```

- Argument of type { a: string, b: string } is not assignable to parameter of type Readonly<{ a: string }>