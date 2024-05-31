### Properties
- Keyed data (as in JS properties), these are local based and can be throught of a triple (On,Under,As)
- Functions #TODO

## Poly types

### Poly or not poly
- Non-poly types
   	- Constants (strings, )
   	- Known objects with known indexes (/positions) (inc tuples, arrays) (sort of and sometimes)

### Open poly types
Poly types retain information and flow of data through the use of higher poly types.

### Objects
In simple case objects (collections of data) can be considered constant

```js
let x = { a: 1, b: 2 };
console.log(x.a)
```

The difficult comes when dealing with objects outside of the scope.

```js
function func() {
  return { a: 2 }
}

(func() === func()) satisfies false;
```

Unlike constants, these are different objects. They can mutated with different things #TODO explain. This is because unlike constants, something else is going on. If I now rewrite it in a de-sugared way (without a heap, only a stack) it, the problem around what objects are becomes easier.

```js
let stackCounter = 0;

function func() {
  let x = createObject(stackCounter++);
  x.a = 2;
  return x
}
```

Objects are basically dependent on the state of the heap. They are a little bit complicated because they are kind of like open poly but also not #TODO

### `TypeId`s and `TypeStore`

Most `Type`s are created at runtime via user declarations (`interface ...`, `type ... = ...` and other synthesis) or `.d.ts` files (including from caches).

Types are stored in `TypeStore`. It currently uses a single `Vec<Type>` and there are pointers into this buffer are from `struct TypeId(u16);`.

However there are some types are the are set ahead of time. You can see them in the `impl TypeId`. These types are used inside the checker for specific things.
- `string` and `number` needs to be referenced by the backing id for their respective literals `"hello world"`, `42`
- `Array` needs to be set as the prototype for array literals `[1, 2, 3]`
- `ReadOnly` needs to know that you cannot write to it
- Some like `1`, `0`, `null` and `undefined` are also used inside the checker + reduces lookup + the amount of types created

Because some of these types are created using the usual methods and don't get added to the `named_types` field on `Context` by default.

> `TypeStore` is subject to change in #120
