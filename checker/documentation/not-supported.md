While Ezno is have a high JS compatibility, the following compromise the fundamentals of Ezno and are also advised as bad features to use.

Note all of these are not supported in TSC either

Not supporting in JavaScript:
- dynamic import with a non-constant value. Can introduce unknown and unreliable side effects.
   	- Also just avoid as they slow down things in the checker
   	- Want something [this to be added](https://github.com/tc39/proposal-defer-import-eval) for lazy loading that is statically analyzable
- `super` calls in a conditional context of a constructor
   	- Really difficult to add the checker without adding enormous complexity and overhead
   	- Means that reference errors
- `eval` just don't
- `with`

Not supporting from TypeScript:
- Function overloading syntax (use generics and matchings)
