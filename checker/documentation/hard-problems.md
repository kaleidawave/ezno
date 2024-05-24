#### Solved

##### Overhead of annotations in the definition files

Solved by binary definition files and the cache mechanism (no parsing,checking and complex synthesis on first start)

##### Possible mutated objects

Solved by the `find_possible_mutations` functions and overrides on contexts (`possibly_mutated_objects`) which prevent the normal constant property lookup.

##### Collection generics

Solved by `LookupGenerics`

> This is better than TS inference, which is more strict. Also creating `StructureGeneric` to match properties would not be aware of future mutations and be overhead.

##### Import mutations

Special import object which looks up values

##### TDZ and union assignment

Done by checking in event application

##### Cyclic mutable types

Don't use `Rc`. Can use `Gc`, but better to have a linear array of types and graph things using maps (such as in the `LocalInformation` struct)

#### Work in progress

##### Calling functions under `or`

```ts
let x: number = 0;
(Date.now() % 2 ? 
    (function () { return x++ }) 
    : (function () { return --x }) 
)();
```

I think when calling a `Logical::Or` it needs to call it in conditional contexts

<!-- ##### Free variable prototypes (prototype mutation in loop and others) -->

#### Unsolved

##### Partial free variables

```ts
const x = {};
let y = 0;

function func() {
    x, y;

    x === x satisfies true;
}
```

#TODO `x` is not the same type as the referenced in scope. It is treated as root poly type free variable. Here `x` could be a possibly mutated variables? I don't think it works because we only know the variable `x` contains the object during synthesis, not at hoisting (hoisting does not look at the RHS).

##### Always true in the loop

```ts
let x: boolean = true;
while (i++ < 100) {
    if (x) {
        // ... no mutations to `x` ...
    }
    // ... also no mutations to `x` ...
}
```

Here Ezno has to assume that `x` is a boolean. It won't give any warnings.
