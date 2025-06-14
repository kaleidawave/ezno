#### Interface extends

```ts
interface X {
	a: string
}

interface Y {
	b: string
}

interface Z extends X, Y {
	c: string
}

({ a: "", b: "", c: "hello" }) satisfies Z;
({ a: "", b: 4, c: "hello" }) satisfies Z;
({ c: "hi" }) satisfies Z;
```

- Expected Z, found { a: "", b: 4, c: "hello" }
- Expected Z, found { c: "hi" }
