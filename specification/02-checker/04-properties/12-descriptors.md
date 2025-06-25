In JavaScript, each property can have additional information on top of properties known as [descriptors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperty#description).

This file tests some setup of this and use of it.

> TODO defineProperties

### `Object.defineProperty` writable

```ts
const obj = {};
Object.defineProperty(obj, 'property', {
	value: 42,
	writable: false,
});
obj.property satisfies string;
obj.property = 70;
```

- Expected string, found 42
- Cannot write to property 'property'

### Descriptor carries across assignments

When assigning, the descriptors carry over. This checks that this assignment is not overwritten.

```ts
const obj = {};
Object.defineProperty(obj, 'property', {
	value: 42,
	enumerable: false,
	// needed as all properties default to false
	writable: true,
});
obj.property = 70;
Object.getOwnPropertyDescriptor(obj, 'property') satisfies string;
```

- Expected string, found { value: 70, writable: true, enumerable: false, configurable: false }

### `Object.defineProperty` getter and setter

> TODO setter

```ts
const obj = {};
let b = 0;
Object.defineProperty(obj, 'property', {
	get: () => b,
});
obj.property satisfies 0;
b++;
obj.property satisfies string;
```

- Expected string, found 1

### `Object.defineProperty` configurable

```ts
const obj = {};
Object.defineProperty(obj, 'property', { value: 6 });
Object.defineProperty(obj, 'property', { value: "hi" });
```

- Property 'property' not configurable

### `Object.getOwnPropertyDescriptor`

> TODO getOwnPropertyDescriptors

```ts
const obj = { a: "something" };
Object.defineProperty(obj, 'b', { value: 42 });

Object.getOwnPropertyDescriptor(obj, 'a') satisfies string;
Object.getOwnPropertyDescriptor(obj, 'b').writable satisfies false;
```

> Order is also important

- Expected string, found { value: "something", writable: true, enumerable: true, configurable: true }

### `Object.assign`

> TODO multiple RHS

```ts
const obj = { a: 1 };
Object.assign(obj, { b: 2, c: 3 });
obj satisfies string;
```

- Expected string, found { a: 1, b: 2, c: 3 }

### `Object.freeze`

> When `Object.freeze` is called, the object's `isSealed` is inferred as `true`

```ts
const obj = {}
let result = Object.freeze(obj);
(obj === result) satisfies true;
obj.property = 2;
Object.isSealed(obj) satisfies true;
```

- Cannot write to property 'property'

### `Object.seal`

> When `Object.seal` is called, the object's `isFrozen` and `isSealed` are inferred as `true`

```ts
const obj = { a: 2 }
let result = Object.seal(obj);
(obj === result) satisfies true;

// Allowed
obj.a = 4;
// Not allowed
obj.property = 2;

Object.isSealed(obj) satisfies true;
Object.isFrozen(obj) satisfies false;
```

- Cannot write to property 'property'

### `Object.preventExtensions`

> When `Object.preventExtensions` is called, the object's `isFrozen` and `isSealed` are inferred as `true`

```ts
const obj = { a: 2 }
let result = Object.preventExtensions(obj);
(obj === result) satisfies true;

// Allowed
obj.a = 4;
// Not allowed
obj.property = 2;

Object.isFrozen(obj) satisfies false;
Object.isSealed(obj) satisfies false;
```

- Cannot write to property 'property'

### `Object.isExtensible`

> The object that has been applied `Object.seal`, `Object.freeze` and `Object.preventExtensions` returns `false` by `Object.isExtensible`, otherwise returns `true`

```ts
{
	const obj = {}
	Object.isExtensible(obj) satisfies true;
	Object.preventExtensions(obj);
	Object.isExtensible(obj) satisfies false;
}
{
	const obj = {}
	Object.seal(obj);
	Object.isExtensible(obj) satisfies false;
}
{
	const obj = {}
	Object.freeze(obj);
	Object.isExtensible(obj) satisfies 5;
}
```

- Expected 5, found false

### `enumerable` in for-in

> #TODO-link

```ts
const obj = { n: 1, b: 2 };
Object.defineProperty(obj, "c", { value: 3, enumerable: false });
Object.defineProperty(obj, "d", { value: 4, enumerable: true });

let keys: string = "";
for (const key in obj) {
	keys += key;
}
keys satisfies boolean
```

- Expected boolean, found "nbd"
