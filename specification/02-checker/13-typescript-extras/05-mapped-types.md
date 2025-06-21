> #TODO copy from post

#### Generic condition

```ts
declare function isNumber<T>(t: T): T extends number ? "yeess" : "nno";

isNumber(5) satisfies "yeess";
isNumber("5") satisfies number;
```

- Expected number, found "nno"

#### Simple key

```ts
type Record2<K extends string, T> = { [P in K]: T }

declare let myRecord: Record2<"hi", number>;

myRecord.hi satisfies string;
myRecord.hello;
```

- Expected string, found number
- No property 'hello' on { [\"hi\"]: number }

#### Assignment

```ts
const x: Record<"test", boolean> = { no: false },
		y: Record<"test", boolean> = { test: 6 },
		z: Record<"test", boolean> = { test: false };
```

- 'no' is not a property of { [\"test\"]: boolean }
- Type { no: false } is not assignable to type { [\"test\"]: boolean }
- Type { test: 6 } is not assignable to type { [\"test\"]: boolean }

#### Union and types as keys

```ts
declare let obj1: Record<"hi" | "hello", boolean>;

obj1.hi satisfies boolean;
obj1.hello satisfies boolean;

obj1.bye;
```

- No property 'bye' on { ["hi" | "hello"]: boolean }

#### Readonly and optionality carries through

```ts
type Mapped<T> = {
	[P in keyof T]: T[P]
}

interface Y { readonly a: string, b?: number }
declare let x: Mapped<Y>;

x.a = "hi";
x.b satisfies number;
```

- Cannot write to property 'a'
- Expected number, found number | undefined

#### Specialisation

```ts
type Pick<T, K extends keyof T> = {
	[P in K]: T[P];
};

interface X { a: number, b: string, c: string }

({ a: 5 } satisfies Pick<X, "a">);
({ a: 5 } satisfies Pick<X, "a" | "b">);
({ a: 5, b: "hi" } satisfies Pick<X, "a" | "b">);
({ a: 5, b: 6 } satisfies Pick<X, "a" | "b">);

declare let y: Pick<X, "b" | "c">;
y.b satisfies string;
y.a;
```

- Expected { ["a" | "b"]: X["a" | "b"] }, found { a: 5 }
- Expected { ["a" | "b"]: X["a" | "b"] }, found { a: 5, b: 6 }
- No property 'a' on { ["b" | "c"]: X["b" | "c"] }

#### Optional

```ts
type Partial<T> = {
	[P in keyof T]?: T[P];
};

({ a: 3 } satisfies Partial<{ a: number, b: string }>);
({ a: "hi" } satisfies Partial<{ a: number, b: string }>)
```

- Expected { [keyof { a: number, b: string }]?: { a: number, b: string }[keyof { a: number, b: string }] }, found { a: \"hi\" }

#### Negated optionality

```ts
type Required<T> = {
	[P in keyof T]-?: T[P];
};

({ a: 3 } satisfies Required<{ a?: number }>);
// Bad
({ } satisfies Required<{ a?: number }>);
```

- Expected { [keyof { a?: number }]: { a?: number }[keyof { a?: number }] }, found {}

#### Readonly

```ts
type Immutable<T> = {
	readonly [P in keyof T]: T[P];
};

interface Y { a: string }
declare let x: Immutable<Y>;
x.a = "hi";
```

- Cannot write to property 'a'

#### Negated readonly

```ts
type Mutable<T> = {
	-readonly [P in keyof T]: T[P];
};

interface Y { readonly a: string }
declare let x: Mutable<Y>;
x.a = 4;
x.a = "hi";
```

- Type 4 does not meet property constraint string

#### `as` rewrite

```ts
type PrefixKeys<T> = {
	[P in ((keyof T) & string) as `property_${P}`]: T[P];
};

interface X { a: number };

declare let x: PrefixKeys<X>;
x.property_a satisfies number;
x.property_b
```

- No property 'property_b' on { [`property_${keyof X & string}`]: X[keyof X & string] }

### Readonly and `as const`

> TODO constrained inference
