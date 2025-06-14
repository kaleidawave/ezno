#### `typeof` operator

```ts
function func() {}

(typeof 5) satisfies "number";
(typeof "hi") satisfies "string";
(typeof func) satisfies "function";

declare let someNumber: number;
(typeof someNumber) satisfies "function";
```

- Expected "function", found "number"
