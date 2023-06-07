#TODO some of this will be made into a post :0

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

A few differences
- They are not subject to specialization (#TODO explain why)

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

assertType<false>(func() === func())
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
