> This is the start
- variable assignments (including TDZ, catches lazy assignment (bad code style))

#### Enabling a new model for variable values ⭐

> #TODO link to context
Removing (as) assertions and satisfies

#### Catching the exterior assignment ⭐

problem with how subtyping works and mutability.
We can check if the property being assigned meets the constraints of the object > #TODO-link
> Catches things that TS cannot check, whilest still allowing the pattern that flow does not

#### Handling properties on new objects

We want objects to be unique for disjoint analysis. We need to append, this uses the same mechanism but flags that this event is an assignment.

```ts
function construct(value) { return { value } }
```
	- TODO

#### Running setters

> #TODO example here
