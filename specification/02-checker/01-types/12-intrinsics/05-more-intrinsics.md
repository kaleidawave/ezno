### Literal special type

```ts
function register(a: Literal<string>) {
	// ...
}

register("something")
// `document.title` is an unknown string, non-literal
register(document.title);

function func(param: object) {
	const obj = { a: 2 };
	const obj1: Literal<object> = obj;
	const obj2: Literal<object> = param;
}
```

- Type object is not assignable to type Literal\<object>
- Argument of type string is not assignable to parameter of type Literal\<string>

> #TODO ors and conditionals

#### Implementation

It checks that the type is a `Constant` and that the larger type matches the argument.

#### Use cases

You may want to disable a function being allowed to take arbitary strings

### `NoInfer`

```ts
declare function func<T>(a: T, b: NoInfer<T>): T;

func("hi", "hello") satisfies number;
```

> but not `| "hello"` !!!

- Expected number, found "hi"

#### Uses case

> #TODO

### `Exclusive`

> #TODO name `exact`

```ts
interface X { a: number }
const x = { a: 1, b: 2 };

x satisfies Exclusive<X>;
({ a: 6 } satisfies Exclusive<X>);
```

- Expected Exclusive\<X>, found { a: 1, b: 2 }

#### Uses case

> #TODO