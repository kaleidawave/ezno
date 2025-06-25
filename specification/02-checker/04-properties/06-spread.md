The `...` operator appends all properties of the operand into the 'currently being formed object'

### Object spread

```ts
const obj1 = { a: 2, b: 3 };
const obj2 = { b: 4, ...obj1, a: 6 };

obj2.b satisfies 100;
obj2.a satisfies boolean;
```

> We also check overwriting here

- Expected 100, found 3
- Expected boolean, found 6

### Spread condition

```ts
declare let condition: boolean;

const obj = {
	foo: 1,
	...(condition ? {
		bar: 2,
		non_existent: 3,
	} : {}),
};

obj.foo satisfies number;
obj.bar satisfies string;
```

- Expected string, found 2 | undefined

### Implementation

#TODO
