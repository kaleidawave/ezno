Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Feats

#### `Object.freeze`

> When `Object.freeze` is called, the object's `isSealed` is inferred as `true`

```ts
const obj = {}
let result = Object.freeze(obj);
(obj === result) satisfies true;
obj.property = 2;
Object.isSealed(obj) satisfies true;
```

- Cannot write to property 'property'

#### `Object.seal`

> When `Object.seal` is called, the object's `isFrozen` and `isSealed` are inferred as `true`

```ts
const obj = {}
let result = Object.seal(obj);
(obj === result) satisfies true;
obj.property = 2;
Object.isFrozen(obj) satisfies true;
Object.isSealed(obj) satisfies true;
```

- Cannot write to property 'property'

#### `Object.preventExtensions`

> When `Object.preventExtensions` is called, the object's `isFrozen` and `isSealed` are inferred as `true`

```ts
const obj = {}
let result = Object.preventExtensions(obj);
(obj === result) satisfies true;
obj.property = 2;
Object.isFrozen(obj) satisfies true;
Object.isSealed(obj) satisfies true;
```

- Cannot write to property 'property'

#### `Object.isExtensible`

> The object that has been applied `Object.seal`, `Object.freeze` and `Object.preventExtensions` returns `false` by `Object.isExtensible`, otherwise returns `true`

```ts
const obj = {}
Object.isExtensible(obj) satisfies true;
Object.preventExtensions(obj);
Object.isExtensible(obj) satisfies false;
Object.seal(obj);
Object.isExtensible(obj) satisfies false;
Object.freeze(obj);
Object.isExtensible(obj) satisfies false;
```
