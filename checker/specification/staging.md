Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Types

#### Generics scope

> Generics are scoped to the function they are declared in

```ts
function x<YEA>() {}

type B = YEA;
```

- Could not find type 'YEA'
