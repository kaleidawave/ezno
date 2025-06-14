Types satisfy a [commutative ring](https://en.wikipedia.org/wiki/Ring_(mathematics)) *like* structure. With `0 := never`, `_ + _ := _ | _` and `_ * _ := _ & _`.

Properties of a (commutative) ring

- Associativity of addition and multiplication

#### Symmetric or

```ts
function or1<T, U>(obj: T | U): U | T { return obj }

function or2(obj: string | number): number | string { return obj }

// Lack of symmetry
function or3(obj: string | number): number { return obj }
```

- Cannot return string | number because the function is expected to return number

#### Symmetric and

```ts
function and1<T, U>(obj: T & U): U & T { return obj }

// Lack of symmetry
function and2<T, U>(obj: T): U & T { return obj }
```

- Cannot return T because the function is expected to return U & T

#### Distributivity

```ts
function distribute1<T, U, V>(obj: (T | U) & V): (T & V) | (U & V) { return obj }

function distribute2<T, U, V>(obj: V & (T | U)): (T & V) | (U & V) { return obj }

// bad!
function distribute3<T, U, V>(obj: (T | U) & V): (T & U) | (U & V) { return obj }
```

- Cannot return T & V | U & V because the function is expected to return T & U | U & V
