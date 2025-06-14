This is implemented through disjoint testing

#### `Not`

```ts
declare let a: number;
4 satisfies Not<4>;
6 satisfies Not<4>;
a satisfies Not<8>;
2 satisfies Not<string>;
"hi" satisfies Not<string>;

declare let b: Not<5> & number;
b satisfies number;
b satisfies string;
b satisfies 5;
```

- Expected Not<4>, found 4
- Expected Not<8>, found number
- Expected Not\<string>, found "hi"
- Expected string, found Not<5> & number
- Expected 5, found Not<5> & number
