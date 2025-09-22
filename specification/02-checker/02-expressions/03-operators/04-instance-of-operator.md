### `instanceof` operator

```ts
([] instanceof Array) satisfies true;
({} instanceof Map) satisfies 4;

class X {}

(new X instanceof X) satisfies true;
([] instanceof X) satisfies false;

function isArray(param: any): boolean {
	return param instanceof Array;
}

declare let myArray: Array<string>;

isArray([1, 2, 3]) satisfies true;
isArray(myArray) satisfies string;
isArray({ }) satisfies null;
```

- Expected 4, found false
- Expected string, found true
- Expected null, found false

#### Implementation

This looks at the prototype of objects. #TODO-check it also descends
