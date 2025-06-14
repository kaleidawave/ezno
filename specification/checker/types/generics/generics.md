Generics are "parameters" for types allow abstraction. This abstraction allows us to define more abstract structures and reduce the amount of written definitions.

#### Generics on dependent type

> Weird annotation here is to work around [#165](https://github.com/kaleidawave/ezno/issues/165)

```ts
function createNew(cb: { f<T>(t: T): { a: T }}["f"]) {
	return cb(4)
}

createNew satisfies string;
```

- Expected string, found (cb: \<T>(t: T) => { a: T }) => { a: 4 }

#### Type alias with type parameters

```ts
type X<T> = T;

2 satisfies X<string>;
```

- Expected string, found 2

#### Generic extends

```ts
function getA<T extends { a: string }>(p: T) {
	return p.a
}

getA({ a: 2 })
```

- Argument of type { a: 2 } is not assignable to parameter of type T

> I think reasons contains more information for the T parameter

#### Use of generics in function body

```ts
function setFirst1<T, U>(a: T, b: U) {
	const a2: T = a;
}

function setFirst2<T, U>(a: T, b: U) {
	const a2: U = a;
}
```

- Type T is not assignable to type U

#### Generics as property

```ts
function createObject1<T, U>(a: T, b: U): { a: T, b: U } {
	return { a, b }
}

function createObject2<T, U>(a: T, b: U): { a: U, b: U } {
	return { a, b }
}
```

- Cannot return { a: T, b: U } because the function is expected to return { a: U, b: U }

#### Generic interface

```ts
interface Wrapper<T> {
	internal: T
}

({ internal: "hi" } satisfies Wrapper<number>);
({ internal: "hi" } satisfies Wrapper<string>);
```

- Expected Wrapper\<number>, found { internal: \"hi\" }

#### Across alias

```ts
type WithLabel<T> = { label: string, item: T };

declare function getItem<T>(a: WithLabel<T>): T;

getItem({ label: "item 1", item: 5 }) satisfies string;
```

- Expected string, found 5
