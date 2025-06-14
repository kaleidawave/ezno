Type checking is a recursive algorithm. It is important that this checking does not get stuck in a lpp[]

#### Cyclic object check

```ts
interface X {
	a: number
	b: X
}

const myObject = { a: 2 };

myObject satisfies X;
// We add a self reference
myObject.b = myObject;
myObject satisfies X;
```

- Expected X, found { a: 2 }

#### Cyclic type alias

```ts
type Node<T> = { parent: Node<T>, value: T } | null;

null satisfies Node<number>;
({ parent: { parent: { parent: null, value: 2 }, value: 6 }, value: 2 } satisfies Node<number>);
({ parent: { parent: { parent: null, value: "hi" }, value: 6 }, value: "hi" } satisfies Node<string>);
```

- Expected { parent: Node\<string>, value: string } | null, found { parent: { parent: { parent: null, value: "hi" }, value: 6 }, value: "hi" }
