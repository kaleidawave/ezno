In a structural type system we treat types as a collection of properties. We can view them as sets, which follow from propositional logic

They therefore satisfy a [commutative semiring](https://en.wikipedia.org/wiki/Semiring#Commutative_semirings) *like* structure. With `0 := never`, `_ + _ or joint := _ | _` and `_ * _ or meet := _ & _`. #TODO ordering as well

Properties of a (commutative) ring

- Associativity of join and meet
- Commutativity of join and meet
- Join distributes over meet #TODO-check 
### Symmetric union

Here `T | U` is equivalent to `U | T`

```ts
function or1<T, U>(obj: T | U): U | T { return obj }

function or2(obj: string | number): number | string { return obj }

// Lack of symmetry
function or3(obj: string | number): number { return obj }
```

- Cannot return string | number because the function is expected to return number

#### Implementation
#TODO eager thing in subtyping

### Symmetric intersection

Here `T & U` is equivalent to `U & T`

```ts
function and1<T, U>(obj: T & U): U & T { return obj }

// Lack of symmetry
function and2<T, U>(obj: T): U & T { return obj }
```

- Cannot return T because the function is expected to return U & T

#### Implementation
#TODO eager thing in subtyping

### Distributivity

We have that 

```ts
function distribute1<T, U, V>(obj: (T | U) & V): (T & V) | (U & V) { return obj }

function distribute2<T, U, V>(obj: V & (T | U)): (T & V) | (U & V) { return obj }

// bad!
function distribute3<T, U, V>(obj: (T | U) & V): (T & U) | (U & V) { return obj }
```

- Cannot return T & V | U & V because the function is expected to return T & U | U & V

#### Implementation
We distribute during type creation #TODO-link 

#TODO what happens when we don't mutate (through aliases) and what about quadratic blow-up?

### #TODO

> #TODO-link nominal and neutral elements
> #TODO  associativity