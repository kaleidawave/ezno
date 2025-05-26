Parameter values are inferred using an *expected parameter type*

```ts
const x: (s: string) => void = (s) => s satisfies number;
// TODO function + satisfies
```

> I called this *forwards* because the type -> (forward) implies the type. *backwards* refers to usage of a type being modifying <- the constraint

> TODO problems with generics
