When synthesizing (aka checking) a function a few thing happen:
- Parameters are synthesized
- Ones that are not already generic are made into generics

### Implicit parameters
There are two ways for the function send data to a function aside from the parameters. They are
- `this` argument
- outside arguments / closed over variables

The difficulty here is they **have to be done lazily**. If a function created these all at the start of the function it would be really slow.

> Could do in a separate pass but not great either. `this` could be done eagerly but it's easier to have the same implementation as the closed over variables

When looking up a (root) reference (`this` or a variable name) in the context, it will check whether the reference is above a dynamic boundary (where variables might have different values, such as a function or loop).

There are cases
1. Variable does not cross a dynamic boundary, does not change between runs
2. Variable does cross a dynamic boundary but has some restriction, so create a fixed poly type
3. Variable does cross a dynamic boundary but has does not have a restriction so is inferred

A few things
- It picks the closest boundary #TODO why
- Once in the boundary it should be cached to prevent further lookup creating a new type. As the parent of a context is immutable, this happens on `TypeStore`
