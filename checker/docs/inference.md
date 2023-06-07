### Regular inference
- The type of a variable comes from its RHS (right of the `=`) (this happens anyway as Ezno can mutate variable types and so this doesn't really happen. To follow the rules of JS a variable without an annotation is free to have any type).
- Function return types (similarly can be inferred from what is returned). Again the return type annotation loses information and so *it is always inferred*

### Parameter inference
This is the big one. Hegel has this feature. Ezno can do more + works for *mutable* closed over variables (implicit parameters).

The idea is as follows, untyped parameters (or ones typed with `any`) are made into generic types which begin by extending any. These are marked as having a modify-able backing (extend) in the context (**important that this information is in the context as cannot modify outside the function**).

The important thing to note is that to avoid complications elsewhere in the project **`Type` structs are immutable** (they do not change structure from once they were created).

So the idea is that types that can have information mutated have to reference it. This #TODO should be done on `RootPolyType`s as some variants don't have a base. Instead their properties are referenced based on the context. This sort of follows `TypeStore` is append only. `Context` appends but in a different way where it changes its ideas of things, rather than going back and modifying existing data.

To distinguish them we have `PolyBase` and with it... #TODO

### Nesting / deep changes
For higher order poly types (types based on generics, in Rust known as 'generic associated data types'), flow through a root. For mutable ones these cannot have fixed properties, these have to change their parents. On the other hand higher poly types based only of fixed ones can

### Parameter constraint inference is NOT event aware
For example, the following

```js
function print_properties(obj) {
  console.log(obj.x)
  console.log(obj.y)
  console.log(obj.z)
}
```

Will cause the constraint of parameter `obj` to be inferred as `{ x: any, y: any, z: any }`. This object will be checked at function parameter pairing time and if that current object does not have the properties then it will raise a type mismatch error. This may be a problem if the following is done

```js
print_properties({ x: 1, get y() { Reflect.set(this, "z", 3); return 2 } })
```

This may be solved in the future by a different way of checking parameters, but for now it is recognised as a use case that should be avoided.

### #TODO
- Mutable and immutable bases in `Context`
- Modify RootPoly so only open poly and Typed generic and parameter can have a aliases, rest is handled by Context
- Think about how function constraints work (they may not exist under `Type`)

### Inferable properties
There are two types of inference, type based *coalescence*:

```js
interface X {
    readable: boolean
}

interface Y {
    name: string
}

function x(p: X) {}
function y(p: Y) {}

function z(param1) {
    x(param1)
    y(param1)
}
```

Here `param1` becomes `X & Y`. When matched against a type, it picks up the restrictions.
