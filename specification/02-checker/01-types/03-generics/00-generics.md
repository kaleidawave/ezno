Generics are "parameters" for types allow abstraction. This abstraction allows us to define more abstract and reusable structures reducing the amount of written definitions.

### Generics on dependent type

```ts
function createNew(cb: <T>(t: T): { a: T }) {
	return cb(4)
}

createNew satisfies string;
```

> Just checking it out it is registered in this test

- Expected string, found (cb: \<T>(t: T) => { a: T }) => { a: 4 }

### Type alias with type parameters

```ts
type X<T> = T;

2 satisfies X<string>;
```

- Expected string, found 2

#### Implementation

Specialising an alias eagerly specialises the type, so we have `Expected string, found 2` here rather than `Expected X<string>, found 2`.

### Generic extends

```ts
function getA<T extends { a: string }>(p: T) {
	return p.a
}

getA({ a: 2 })
```

- Argument of type { a: 2 } is not assignable to parameter of type T

> I think the diagnostic contains more information for the T parameter

#### Implementation

Each generic has a `extends` field. When checking a generic, we check the `extends` type #TODO-link 

### Use of generics in function body

```ts
function setFirst1<T, U>(a: T, b: U) {
	const a2: T = a;
}

function setFirst2<T, U>(a: T, b: U) {
	const a2: U = a;
}
```

> We can use `U` inside the function body (it is not just scoped to the parameters and return type)

- Type T is not assignable to type U

#### Implementation

#TODO something about context here

### Generics as property

```ts
function createObject1<T, U>(a: T, b: U): { a: T, b: U } {
	return { a, b }
}

function createObject2<T, U>(a: T, b: U): { a: U, b: U } {
	return { a, b }
}
```

- Cannot return { a: T, b: U } because the function is expected to return { a: U, b: U }

### Generic interface

> #TODO-location

```ts
interface Wrapper<T> {
	internal: T
}

({ internal: "hi" } satisfies Wrapper<number>);
({ internal: "hi" } satisfies Wrapper<string>);
```

- Expected Wrapper\<number>, found { internal: \"hi\" }

