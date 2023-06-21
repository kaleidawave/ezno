> Building a type checker is hard. Here are some tricks that Ezno uses to keep the codebase small and simple

### Specializing generics
Finding the values for generics is done during subtype checking #TODO

### Object keys are types
When `x.t` a `"t"` type is created.
#TODO how does casting work

### Operators are functions
#TODO

## Internal #TODO
- `Environment::parents_iter`
- `get_ctx!`
- `Logical`
