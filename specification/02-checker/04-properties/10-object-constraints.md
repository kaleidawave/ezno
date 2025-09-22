> #TODO explain why objects have constraints

### Object property constraints

```ts
const my_obj: { a: number } = { a: 2 }
my_obj.a = "hello world"
```

- Type "hello world" does not meet property constraint number

### Nested constraint

```ts
const obj1 = { a: 5 };
const obj2: { prop: { a: number } } = { prop: obj1 }

obj1.a = 6;
obj1.a = "hello";
```

- Type "hello" does not meet property constraint number

### And object constraint

```ts
{
	const obj = { a: true, b: false };
	const x: { a: boolean } = obj, y: { b: boolean } = obj;

	obj.a = "yo";
	obj.b = "wassup";
}

{
	// and in the same assignment through a cycle
	const obj = { a: 2, b: 3 }; obj.c = obj;
	const something: { a: number, c: { b: number } } = obj;

	obj.a = "hi";
	obj.b = "hello";
}
```

- Type "yo" does not meet property constraint boolean
- Type "wassup" does not meet property constraint boolean
- Type "hi" does not meet property constraint number
- Type "hello" does not meet property constraint number
