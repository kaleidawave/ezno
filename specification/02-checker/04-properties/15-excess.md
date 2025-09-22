> The following work through the same mechanism as forward inference
> Thanks to pull request: #139

This is more of a linting feature. It emits an error when an object literal specifies a property it was not expecting

### Excess property at declaration

```ts
interface MyObject { property: string }

const a: MyObject = { property: "hello", another: 2 }
```

- 'another' is not a property of MyObject

### Excess property at argument

```ts
interface MyObject { property: string }

function process(param: MyObject) {}

process({ property: "hello", another: 2 });

function func<T>(a: T) { }

func<MyObject>({ property: "hello", "something else": 2 })
```

- 'another' is not a property of MyObject
- 'something else' is not a property of MyObject

### Excess property at return type

```ts
interface MyObject { property: string }

function returnNewObject(): MyObject {
	return { property: "hello", another: 67 }
}
```

- 'another' is not a property of MyObject

### Excess property checks through spread and condition

> For some reason TSC does not support this

```ts
type MyObject = { foo: number; bar?: number };

const b: MyObject = {
	foo: 1,
	...{
		bar: 2,
		invalid: 3,
	},
};

declare let condition: boolean;

const c: MyObject = {
	foo: 1,
	...(condition ? {
		bar: 2,
		non_existent: 3,
	} : {}),
};
```

- 'invalid' is not a property of MyObject
- 'non_existent' is not a property of MyObject
