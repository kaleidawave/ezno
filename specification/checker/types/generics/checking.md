#### Type has no generics

```ts
type X = number;
const a: X<number> = 2;
```

- Cannot pass a type argument to a non-generic type

#### Function is not generic

```ts
declare function func();

func<string>();
```

- Cannot pass a type argument to a non-generic function

#### Missing generics

```ts
interface Pair<K extends string, V> {
	key: K
	value: V
}

let x: BoxedString<string>;
```

- Expected 2 type arguments, but got 1

#### Excess generic arguments

```ts
declare function generic<T>(a: T);

generic<string, number>("something");
```

- Expected 1 type argument, but got 2

#### Interface generic constraint checking

```ts
interface BoxString<T extends string> {
	inner: T
}

type BoxedFour = BoxString<"4">;
type BoxedFive = BoxString<5>;
```

- Generic argument 5 does not match string

#### Explicit generic type argument parameter

```ts
function func<T>(a: T) {}
func<number>("hello world")
```

- Argument of type "hello world" is not assignable to parameter of type number
