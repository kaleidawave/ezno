Functions are classes are regular objects.

### Function and class name

> TODO should also check that it is readonly

```ts
function a() { }
class B { }
let c = class { }

a.name satisfies "a"
B.name satisfies "B"
c.name satisfies "sea"
```

- Expected "sea", found "c"
